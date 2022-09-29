/*
 * |-----------------------------------------------------------|
 * | Copyright (c) 1990 MIPS Computer Systems, Inc.            |
 * | All Rights Reserved                                       |
 * |-----------------------------------------------------------|
 * |          Restricted Rights Legend                         |
 * | Use, duplication, or disclosure by the Government is      |
 * | subject to restrictions as set forth in                   |
 * | subparagraph (c)(1)(ii) of the Rights in Technical        |
 * | Data and Computer Software Clause of DFARS 52.227-7013.   |
 * |         MIPS Computer Systems, Inc.                       |
 * |         950 DeGuigne Drive                                |
 * |         Sunnyvale, CA 94086                               |
 * |-----------------------------------------------------------|
 */
#ident	"$Header: clock.c,v 1.37.1.7.1.4.1.2 90/11/02 18:00:05 beacker Exp $"

/*	Copyright (c) 1984 AT&T	*/
/*	  All Rights Reserved  	*/

/*	THIS IS UNPUBLISHED PROPRIETARY SOURCE CODE OF AT&T	*/
/*	The copyright notice above does not evidence any   	*/
/*	actual or intended publication of such source code.	*/

#include "sys/types.h"
#include "sys/param.h"
#include "sys/tuneable.h"
#include "sys/reg.h"
#include "sys/psw.h"
#include "sys/sysmacros.h"
#include "sys/systm.h"
#include "sys/sysinfo.h"
#include "sys/callo.h"
#include "sys/fs/s5dir.h"
#include "sys/signal.h"
#include "sys/sbd.h"
#include "sys/pcb.h"
#include "sys/immu.h"
#include "bsd/sys/time.h"
#include "sys/user.h"
#include "sys/conf.h"
#include "sys/region.h"
#include "bsd/sys/time.h"
#include "sys/proc.h"
#include "sys/var.h"
#include "sys/cmn_err.h"
#include "sys/map.h"
#include "sys/swap.h"
#include "sys/fixpoint.h"
#include "sys/debug.h"
#include "sys/cpu_board.h"
#include "sys/limits.h"
#ifdef PROFILING
#include "sys/prof.h"
#endif
#include "sys/getpages.h"
#undef iowait

#define BOUND(x, min, max) \
	( (x)<(min) ? (min):  ((x)>(max)?(max):(x))  )

/*
 * clock is called straight from
 * the real time clock interrupt.
 *
 * Functions:
 *	reprime clock
 *	implement callouts
 *	maintain user/system times
 *	maintain date
 *	profile
 *	alarm clock signals
 *	jab the scheduler
 */

#define	PRF_ON	01
unsigned	prfstat;
extern struct sit cicsit;
extern int	vhand();
extern int	switching;

/*
 * Bump a timeval by a small number of usec's.
 */
#define BUMPTIME(t, usec) { \
	register struct timeval *tp = (t); \
 \
	tp->tv_usec += (usec); \
	if (tp->tv_usec >= 1000000) { \
		tp->tv_usec -= 1000000; \
		tp->tv_sec++; \
	} \
}

struct timeval	boottime;		/* time since 1970 of last boot */
struct timeval	time;			/* time since 1970 */
struct timezone	tz;			/* timezone */
time_t		lbolt;			/* time in HZ since last boot */
int	lticks;
int	tick = 1000000/HZ;		/* usecs per clock tick */

int	one_sec = 1;

uint	sxbrkcnt;	/* count of procs whose current stat is SXBRK	*/

void	(*poll_funct)();	/* TODO: ditch this */

extern struct callout callout[];	/* callout table */
struct callout *callfree = 0;		/* free list of callout entries */
struct callout calltodo = {0};		/* next item to do */
extern short ncallout;			/* # of the beasties */

extern char qrunflag;			/* need to run streams queues */

/*
 * The cpu usage estimator (p_cpu) is increased each tick and
 * decays once a second.  The cpu usage estimator ramps up quite 
 * quickly when the process is running (linearly), and decays away 
 * exponentially, at a rate which is proportionally slower when the 
 * system is busy.  The basic principal is that the system will 90% forget
 * that a process used a lot of CPU time in 5*loadav seconds.
 * This causes the system to favor processes which haven't run
 * much recently, and to round-robin among other processes.
 *
 * Filter() calculates a scale that gives ~90% decay in 5*loadav seconds:
 *
 * 	2*loadav / ( 2*loadav + 1 ).
 *
 * The cpu_decay variable determines which element of loadavg is used
 * in the filter calculation.
 */
#   define filter(loadav) DIV_2FIX( MUL_2FIX(TO_FIX(2), (loadav)), \
				MUL_2FIX(TO_FIX(2), (loadav)) + TO_FIX(1))
extern fix avenrun[];	/* Array of fix8 load averages. (60,300, 900 secs) */
extern fix sq_avenrun[];/* Like avenrun, but rolling average. */
extern ufix loadavg[];	/* Bigger array.  (1, 5, 20, 60, 300, 900 secs) */
int cpu_decay = 2;	/* Loadavg[cpu_decay] is used for cpu decay. */

extern int tickdelta;	/* Values for adjtime().  See kern_time.c. */
extern int timedelta;

#if (FBITS != 8)
# include "Error: need to redefine ccpu decay constant."
#endif
fix	ccpu		= 244;		/* (1<<8)*exp(-1/20) */
fix	one_ccpu	= 12;		/* (1<<8)*(1 - exp(-1/20)) */

#ifndef POLL_CONST
#define POLL_CONST 4 			/* Clock rate is sped up by this rate */
#endif
					/* MUST match rate in todc.c */
#define MAX_INTERVAL POLL_CONST		/* Poll MAX_INTERVAL times every POLL_CONST ticks */
#define MIN_INTERVAL 1			/* Only poll once every POLL_CONST ticks */

#define INVAL_INT	1
#define NO_RESOURCE	2

#define POLL_TBL_SIZE	5

struct entry {
	void 	(*func)();
};

struct entry poll_tbl[MAX_INTERVAL-MIN_INTERVAL + 2][POLL_TBL_SIZE]; 
				/* 0th slot is wasted */

clock(ep)
int *ep;
{

	    /*
	     * The floating point interrupt code enable hardclock and
	     * execptions so that time will be charged to the system
	     * rather than the user.  However, a second fp execption 
	     * may come in before the first one is finished.  Since the
	     * clock bit hasn't yet been reset (because fp execptions
	     * get handled before clock ints), and clock interrupts
	     * are enabled, the clock interrupt will get handled
	     * after the second fp exception.  However, when control
	     * is returned to intr() it has saved away the fact that
	     * there was a clock interrupt and will hence call clock again.
	     * So we check here to make sure that the cause register
	     * really indicates that there is a clock int pending before
	     * proceeding.
	     */
	if (!IS_R3030 && !IS_R6300 && !IS_I2000) {
	    if ( !(get_cause() & CAUSE_IP5) ) {
	    	return;
	    }
	}

#ifdef PIXIE
	{
		extern char	pixie[];
		extern int	pixie_size, pixie_clr;

		if (pixie_clr) {
			pixie_clr = 0;
			bzero(pixie, pixie_size);
		}
	}

#endif PIXIE

	/* TODO inline this? */
	ackrtclock();  

	if (IS_R2400) {
		static int s;
		register int i;
		static int s_ticks = 0;		/* scheduled ticks */
		static int poll_tick = 1;	/* which tick is this? */

		if (s_ticks++ > 1)
			return (0);
	
		for (i=0; (i<POLL_TBL_SIZE) && (poll_tbl[poll_tick][i].func != NULL); i++){
				poll_tbl[poll_tick][i].func();
		}

		if ((poll_tick += s_ticks) > MAX_INTERVAL){
			poll_tick -= (MAX_INTERVAL);
			s_ticks = 0;
			return(real_clock(ep));
		}
		s_ticks = 0;
		return(0);
	}
	else{
		return(real_clock(ep));
	}
}

void poll_init()
{
	register int i,j;

	for (j=MIN_INTERVAL;j<=MAX_INTERVAL;j++){
		for (i=0;i<POLL_TBL_SIZE;i++){
			poll_tbl[j][i].func = NULL;
		}
	}
}

int	vmmeter_hz = HZ;
extern int	splnet();

/*
 * This is the polling routine for the vmmeter routine instead
 * of calling this each second at splclock
 */
void vmmeter_poll() {

	vmmeter();
	timeout(vmmeter_poll, 0, vmmeter_hz, splnet);
}

/*
 * Interrupt handler for the scheduling clock.
 */
real_clock(ep)
int *ep;
{
	caddr_t pc = (caddr_t)ep[EF_EPC];
	psw_t ps = ep[EF_SR];
	psw_t sbc_ivectmask = ep[EF_IVECTMASK];
	register struct proc *pp;
	register a, i;
	register cpu;
	register int s, retval;
	static rqlen, sqlen;
	extern int idleflag;
	fix scale;


	retval = 0;

	/*
	 * if panic stop clock
	 */

	if (panicstr) {
		clkreld();
		return(retval);
	}


	{
		register struct callout *p1 = calltodo.c_next;
		if (p1 != 0		/* if any callout active, */
		    && --p1->c_time <= 0)	/* update 1st non-zero time */
			timepoke();	/* and force soft clock if needed */
	}

#ifndef KGCLOCK
	/*
	 * If the profiling clock is not enabled, then gather kernel
	 * profiling statistics here instead.
	 */
#ifdef PROFILING
	{
	u_int s, effective_pc;
	static int reset_pc_buckets;

	if (reset_pc_buckets) {
	    extern struct phdr phdr;
	    bzero( phdr.pc_buf, phdr.pc_bytes );
	    reset_pc_buckets = 0;
	}

	effective_pc = (u_int)pc;
#ifdef R6000
	{
	    extern char ssplx[], esplx[];
	    /*
	     *  If the PC lies within splx(), then the RA
	     *  holds the really interesting value.
	     */
	    if (effective_pc >= (u_int)&ssplx[0]  &&
		effective_pc <  (u_int)&esplx[0]) {
		effective_pc = ep[EF_RA];
	    }
	}
#endif R6000
	s = (effective_pc - s_lowpc);
	if (s < s_textsize) {
		extern struct phdr phdr;
		char *k = (char *)phdr.pc_buf;
		(*(u_int *)(k + ((s >> 3) << 2)))++;
		}
	}
#endif PROFILING
	if (prfstat & PRF_ON)
		prfintr(pc, ps);
#endif

	pp = u.u_procp;
	if (USERMODE(ps)) {
		a = CPU_USER;
		if (!(IS_R3030)) {
			u.u_utime++;
		}
		BUMPTIME(((struct timeval *) &u.u_ru.ru_utime), tick);
		if (u.u_prof.pr_scale) {
			pp->p_flag |= SOWEUPC;
			u.u_pcb.pcb_resched = 1;
			retval = 1;
		}
		/*
		 * CPU was in user state.  Increment
		 * user time counter, and process process-virtual time
		 * interval timer. 
		 */
		if (timerisset(&u.u_timer[ITIMER_VIRTUAL].it_value) &&
		    itimerdecr(&u.u_timer[ITIMER_VIRTUAL], tick) == 0)
			psignal(u.u_procp, SIGVTALRM);
	} else {
		if (idleflag) {
			if (syswait.iowait+syswait.swap+syswait.physio) {
				a = CPU_WAIT;
				if (syswait.iowait)
					sysinfo.wait[W_IO]++;
				if (syswait.swap)
					sysinfo.wait[W_SWAP]++;
				if (syswait.physio)
					sysinfo.wait[W_PIO]++;
			} else
				if (sxbrkcnt)
					a = CPU_SXBRK;
				else
					a = CPU_IDLE;
		} else {
			a = CPU_KERNEL;
			if (!(IS_R3030)) {
				u.u_stime++;
			}
			BUMPTIME(((struct timeval *) &u.u_ru.ru_stime), tick);
			if (server()) dinfo.serve++;	/* ticks servicing remote */
		}
	}
	sysinfo.cpu[a]++;

	/*
	 * If the cpu is currently scheduled to a process, then
	 * charge it with resource utilization for a tick, updating
	 * statistics which run in (user+system) virtual time,
	 * such as the cpu time limit and profiling timers.
	 * This assumes that the current process has been running
	 * the entire last tick.
	 */
	if (a == CPU_KERNEL || a == CPU_USER ) {
		if ((u.u_ru.ru_utime.tv_sec+u.u_ru.ru_stime.tv_sec+1) >
		    	u.u_rlimit[BSD43_RLIMIT_CPU].rlim_cur) {
			psignal(u.u_procp, BSD43_SIGXCPU);
			if (u.u_rlimit[BSD43_RLIMIT_CPU].rlim_cur <
			    u.u_rlimit[BSD43_RLIMIT_CPU].rlim_max)
				u.u_rlimit[BSD43_RLIMIT_CPU].rlim_cur += 5;
		}
		if (timerisset(&u.u_timer[ITIMER_PROF].it_value) &&
		    itimerdecr(&u.u_timer[ITIMER_PROF], tick) == 0)
			psignal(u.u_procp, SIGPROF);
	}

	/* update memory usage for the currently running process */

	if (pp->p_stat==SRUN)
	{	register preg_t	*prp;
		register reg_t	*rp;
		int	s = 0;

		for (prp = pp->p_region; rp = prp->p_reg; prp++) {
			if (rp->r_type == RT_PRIVATE)
				u.u_mem += rp->r_nvalid;
			else if (rp->r_refcnt)
				u.u_mem += rp->r_nvalid/rp->r_refcnt;
			switch (prp->p_type) {
			case PT_STACK:
				u.u_ru.ru_isrss += rp->r_nvalid;
				s += rp->r_nvalid;
				break;
			case PT_DATA:
				u.u_ru.ru_idrss += rp->r_nvalid;
				s += rp->r_nvalid;
				break;
			default:
				if (rp->r_type == RT_PRIVATE) {
					u.u_ru.ru_idrss +=
						rp->r_nvalid;
					s += rp->r_nvalid;
				} else if (rp->r_refcnt) {
					register int t;
					t = ((rp->r_nvalid +
						(rp->r_refcnt / 2)) /
						    rp->r_refcnt);
					u.u_ru.ru_ixrss += t;
					s += t;
				};
				break;
			};
		};
		if (s > u.u_ru.ru_maxrss)
			u.u_ru.ru_maxrss = s;
	}
	if (!switching) {
		pp->p_cpticks++;
		if (pp->p_cpu < 255) 
			pp->p_cpu++;
	};
	lbolt++;	/* time in ticks */
	if (--lticks <= 0)
		runrun++;

	/* "double" long arithmatic for minfo.freemem */
	{	unsigned long ofrmem;

		ofrmem = minfo.freemem[0];
		minfo.freemem[0] += freemem;
		if (minfo.freemem[0] < ofrmem)
			minfo.freemem[1]++;
	}

	/*
	 * Bump the time, taking into account a skew from adjtime.
	 * "timedelta" is the total amount of time to adjust, and "tickdelta"
	 * is the amount to move per tick.
	 */
	if (timedelta == 0) {
		BUMPTIME(&time, tick);
	}
	else {
		if ( abs(tickdelta) > abs(timedelta) )
			tickdelta = timedelta;
		timedelta -= tickdelta;
		BUMPTIME(&time, tick+tickdelta);
	}

	if (--one_sec <= 0) {
		/* see that the CPU in-use LEDS change once a second --
		 * even if the CPU is stuck in some sort of loop at
		 * a nonzero priority (i.e. call before check for BASEPRI).
		 */
		cpu_bound_leds();

		/* TODO this test was BASEPRI before, ??? */
		if (!BASEPRI((IS_M6000) ? sbc_ivectmask : ps))
			return(retval);
		one_sec += HZ;

		minfo.freeswap = 0;
		{	int	i;
			swpt_t	*st;
			for (i=0, st=swaptab; i<MSFILES; i++, st++)
			{	if (st->st_ucnt == NULL)
					continue;
				minfo.freeswap += st->st_nfpgs << DPPSHFT;
			}
		}
		++runrun;

		cpu_decay = BOUND(cpu_decay, 0, 6-1);
		scale = max( filter(loadavg[cpu_decay]), filter(1) );
		rqlen = sqlen = 0;

		for (pp = &proc[0]; pp < (struct proc *)v.ve_proc; pp++)
		if (pp->p_stat) {
			if (pp->p_time != 127)
				pp->p_time++;
			if (pp->p_stat == SSLEEP || pp->p_stat == SSTOP)
				if (pp->p_slptime != 127)
					pp->p_slptime++;

			/*
			 * p_pctcpu is only for ps.
			 */
			pp->p_pctcpu = MUL_2FIX(pp->p_pctcpu, ccpu) +
					(one_ccpu * pp->p_cpticks) / HZ;
			pp->p_cpticks = 0;

			cpu = FIX_TO_INT(scale * (pp->p_cpu - (pp->p_nice - NZERO))) + 
				(pp->p_nice - NZERO);
			pp->p_cpu = BOUND(cpu, 0, 255);

			if (pp->p_pri >= (PUSER-NZERO))
				pp->p_pri = calcppri(pp);

			if (pp->p_stat == SRUN)
				if (pp->p_flag & SLOAD)
					rqlen++;
				else
					sqlen++;
		}

		if (rqlen) {
			sysinfo.runque += rqlen;
			sysinfo.runocc++;
		}
		if (sqlen) {
			sysinfo.swpque += sqlen;
			sysinfo.swpocc++;
		}

		/*
		 * Compute load average and copy 60, 300 & 900 sec loadavg 
		 * values into avenrun for BSD compatibility.
		 */
		{
			static count = 0;
			loadav(loadavg, rqlen + sqlen, &count);
		}
		for (i=0; i<3; ++i) 
			avenrun[i] = loadavg[i+3];
		if ( (time.tv_sec%5) == 0 )
			sq_loadav(sq_avenrun, rqlen + sqlen);

		if (runin!=0) {
			runin = 0;
			setrun(&proc[0]);
		}

		if (bclnlist != NULL)
		  	wakeup((caddr_t) vhand);

		wakeup((caddr_t) &lbolt);
	}
	return(retval);
}

/*
 * Calculate user process priority.
 * This is like the BSD equation, but cpu/7 instead of 4 to take into
 * account HZ=100 instead of HZ=60 like BSD.
 * The old SYSV one was (cpu/2 + nice).)
 */
calcppri(pp)
	register struct proc *pp;
{
	register int p;

	ASSERT(pp);
	p = CALCCPRI(pp);
	return(p);
}

/*
 * loadav(avg, n, countp) 
 *
 * Loadav is called once a second to update an array avg of fixed point 
 * averages to include the new data point n.  The array contains exponentially 
 * decaying averages with decay-periods of 1 sec, 5 sec, 20 sec, 1 min, 
 * 5 min, 15 min.
 *
 * The basic formula for each element in the avg array is.
 *
 *	avg = new_data*(1 - exp(-1/N)) + avg*exp(-1/N);
 *
 * where N is the number of intervals over which decay should take place.
 * (That is, after N intervals, influence decays to 1/e the original value.)
 *
 * To avoid nasty computation, we use the cexp array of precomputed 
 * exponentiations.
 *
 * Averages are normally kept as fix8 (that is, an int with 8 binary digits 
 * to the right of the decimal).  During the computation they are sometimes
 * fix15.  The cexp[] array is fix15.  This is confusing.
 *
 * To avoid underflow, different decay rates are updated at different
 * periods.
 */
struct cexp_t {
	ufix	e;		/* Precomputed fixed-point exponentiation. */
	ufix	p;		/* Period for updating. */
} cexp[] = {
	{12055,	1},	/* exp(-1/1) << 15 		 1 sec  @ 1 sec */
	{26828,	1},	/* exp(-1/5) << 15 		 5 secs @ 1 sec */
	{31170,	1},	/* exp(-1/(20) << 15 		20 secs @ 1 sec */
	{29650,	6},	/* exp(-1/(60/6)) << 15 	 1 min  @ 6 sec */
	{30860,	18},	/* exp(-1/(300/18)) << 15 	 5 mins @ 18 sec */
	{31483,	36}	/* exp(-1/(900/36)) << 15 	15 mins @ 36 sec */
};

#if (FBITS != 8)
# include "Error: Output conversion here assumes FBITS of 8."
#endif

loadav(avg, n, countp)
ufix *avg;
uint n;
uint *countp;
{
	/*
	 * Avg starts as a fix8 value, becomes fix15 during the
	 * computation, and gets converted back to fix8 at the end.
	 * Cexp[] contains fix15 values.
	 *
	 * WARNING: There is only 8 bits of integer at one point in the
	 * calculation below.  So load average > 512 breaks this.
	 */
	int i;

	++(*countp);
	for (i=0; i<6; ++i) {
		if ((*countp)%cexp[i].p == 0) {
			avg[i] = ( avg[i] * cexp[i].e ) >> 8;	/* to fix15  */
			avg[i] += n * ( (1<<15) - cexp[i].e );	/* still 15  */
			avg[i] >>= 7;				/* back to 8 */
		}
	}
}

/*
 * This function computes a the square load average instead of an
 * exponentially decaying one.  (i.e. a rolling average.)  It isn't 
 * used in the kernel, but some users prefer this sort of averaging.
 *
 * It gets called every five seconds and computes values for 1, 5, 15
 * minutes.  (I don't call this once a second because of the amount of
 * data that would be required for the 15 minute average.)
 */
#define interval1 (60/5)
#define interval5 (interval1*5)
#define interval15 (interval1*15)
sq_loadav(avg, n)
	register fix *avg;
	int n;
{
	register int i;
	static int data[interval15];	/* interval 5 points of data. */
	static int sum[3];		/* Rolling sums for each interval. */
	static int datap = 0;		/* Current data point in data[]. */
	register int lastp;		/* Data point to remove in data[]. */
	static int interval[3] = {	/* The 3 intervals to compute for. */
		interval1, interval5, interval15
	};
	static int factor[3] = { 	/* Divisors.  The 256 gives extra */
	    (256<<FBITS)/interval1, 	/* precision. */
	    (256<<FBITS)/interval5,
    	    (256<<FBITS)/interval15
	};

	for (i=0; i < 3; i++) {
	    lastp = datap - interval[i];
	    if (lastp < 0 ) 
		lastp += interval[i];
	    sum[i] += n - data[lastp];
	    avg[i] = (sum[i]*factor[i])>>8;	/* >>8 gets rid of extra 256. */
	}

	data[datap] = n;
	if (++datap >= interval5 ) 
	    datap = 0;
}


#ifdef KGCLOCK
/*
 * Interrupt handler for the profiling clock.
 * If the profiling clock is enabled (KGCLOCK defined), then
 * gather kernel profiling statistics here at profile
 * clock interupt.  Otherwise gather statistics in clock().
 */
prof_intr(ep)
int *ep;
{
	caddr_t pc = (caddr_t)ep[EF_EPC];
	psw_t ps = ep[EF_SR];

	ackkgclock();

	/*
	 * Gather kernel profiling statistics.
	 */
#ifndef PROFILING
	if (prfstat & PRF_ON)
		prfintr(pc, ps);
#else
	s = pc - s_lowpc;
	if (s < s_textsize) {
		extern struct phdr phdr;
		char *k = (char *)phdr.pc_buf;
		(*(u_int *)(k + ((s >> 3) << 2)))++;
		}
#endif PROFILING
}
#endif KGCLOCK

/*
 * timeout is called to arrange that fun(arg) is called in tim/HZ seconds.
 * An entry is sorted into the callout structure.
 * The time in each structure entry is the number of HZ's more
 * than the previous entry. In this way, decrementing the
 * first entry has the effect of updating all entries.
 *
 * The panic is there because there is nothing
 * intelligent to be done if an entry won't fit.
 */

int					/* return ID of the entry */
timeout(fun, arg, tim)
register int tim;
int (*fun)();
caddr_t arg;
{
	timeout_spl(fun, arg, tim, 0);
}

int					/* return ID of the entry */
timeout_spl(fun, arg, tim, spl)
register int tim;
int (*fun)();
int (*spl)();
caddr_t arg;
{
	register struct callout *p1, *p2, *pnew;
	register int id;
	int s;

	/* assume that if a negative timeout value was passed in, the
	 * process wanted to be called as soon as possible...make it
	 * ready after the next tick
	 */
	if (tim <= 0)
		tim = 1;

	s = splall();
	pnew = callfree;
	if (pnew == 0) {		/* incrementally initialize table */
		if (ncallout <= 0)
			cmn_err(CE_PANIC,"Timeout table overflow");
		pnew = &callout[--ncallout];
	} else {
		callfree = pnew->c_next;
	}
	pnew->c_arg = arg;
	pnew->c_func = fun;
	if ((id = ++calltodo.c_id) == 0)
	    id = ++calltodo.c_id;
	pnew->c_id = id;
	pnew->c_spl = spl;

	/* insert pnew into correct position in callout list */
	for (p1 = &calltodo; (p2 = p1->c_next) && (p2->c_time <= tim); p1 = p2)
	{
		tim -= p2->c_time;
		/* if tim goes negative, it is because we were very close
		 * to MAXINT and p2->c_time was negative...in that case,
		 * just set us back to MAXINT since that is the maximum
		 * timeout value...that way the test for (p2->c_time <= tim)
		 * won't prematurely succeed; it will start getting
		 * decremented again with the next positive c_time 
		 */
		if (tim < 0) {
			cmn_err(CE_NOTE, 
				"timeout_spl: setting tim to INT_MAX");
			tim = INT_MAX;
		}
	}
	p1->c_next = pnew;
	pnew->c_next = p2;
	pnew->c_time = tim;
	if (p2)
		p2->c_time -= tim;
	splx(s);

	return id;
}


/* given an ID, kill the corresponding time-out
 */
untimeout(id)
register id;
{
	register struct callout *p1, *p2, *p3;
	register s;

	s = splhi();
	for (p1 = &calltodo; (p2 = p1->c_next) != 0; p1 = p2) {
		if (p2->c_id == id) {
			if (p3 = p2->c_next)	/* carry overflow or delta */
				p3->c_time += p2->c_time;
			p1->c_next = p3;
			p2->c_next = callfree;
			callfree = p2;
			break;
		}
	}
	splx(s);
}

/* remove a function timeout call from the callout structure.
 *	This form is used by old, 3.5 code as well as the 4.3BSD TCP/IP
 *	stuff.
 */
untimeout_func(fun, arg)
	register int (*fun)();
	register caddr_t arg;
{
	register struct callout *p1, *p2, *p3;
	register int s;

	s = splall();
	for (p1 = &calltodo; (p2 = p1->c_next) != 0; p1 = p2) {
		if (p2->c_func == fun && p2->c_arg == arg) {
			if (p3 = p2->c_next)	/* carry overflow or delta */
				p3->c_time += p2->c_time;
			p1->c_next = p3;
			p2->c_next = callfree;
			callfree = p2;
			break;
		}
	}
	splx(s);
}


/*
 * Compute number of hz until specified time.
 * Used to compute third argument to timeout() from an
 * absolute time.
 */
hzto(tv)
	struct timeval *tv;
{
	register long ticks;
	register long sec;
	int s = splhi();

	/*
	 * If number of milliseconds will fit in 32 bit arithmetic,
	 * then compute number of milliseconds to time and scale to
	 * ticks.  Otherwise just compute number of hz in time, rounding
	 * times greater than representible to maximum value.
	 *
	 * Delta times less than 25 days can be computed ``exactly''.
	 * Maximum value for any timeout in 10ms ticks is 250 days.
	 */
	sec = tv->tv_sec - time.tv_sec;
	if (sec <= 0x7fffffff / 1000 - 1000)
		ticks = ((tv->tv_sec - time.tv_sec) * 1000 +
			(tv->tv_usec - time.tv_usec) / 1000) / (tick / 1000);
	else if (sec <= 0x7fffffff / HZ)
		ticks = sec * HZ;
	else
		ticks = 0x7fffffff;
	splx(s);
	return (ticks);
}

/*
 * Called via a PIR9 which was set by timepoke() in clock()
 */
timein()
{
	register struct callout *p1, *p2;
	register caddr_t arg;
	register int (*func)();
	register int s, ss;
	register int (*spl)();

	acksoftclock();

	for (;;) {
		s = splhi();
		p1 = calltodo.c_next;
		if (p1 == 0 || p1->c_time > 0) {
			splx(s);
			break;
		}
		if (spl = p1->c_spl) {
			/* We have to invoke the routine at the specified
			 * spl level.  However, we can't invoke the spl
			 * routine directly as it won't lower the privilege.
			 */

			register int	id = p1->c_id;
			splx(s);
			s = (*spl)();
			ss = splhi();
			if ((p2 = calltodo.c_next) == NULL || p2->c_id != id){
				/* We are not the first one on the callout
				 * queue any more.  Try again.  This will
				 * happen only if someone called untimeout.
				 */
				splx(ss);
				splx(s);
				continue;
			}
		}

		arg = p1->c_arg;
		func = p1->c_func;
		p2 = p1->c_next;	/* advance to next item */
		calltodo.c_next = p2;
		if (p2)			/* & carry forward overflow */
			p2->c_time += p1->c_time;
		p1->c_next = callfree;	/* link into freelist */
		callfree = p1;

		if (spl)
			splx(ss);
		(*func)(arg);		/* invoke the function */
		splx(s);	/* it seems to me that timeout fncs should
				 * run at low priority.  However, in case
				 * existing code expects to have S5 conventions,
				 * we ought to stay at hi priority. */
	}

	if (qrunflag)			/* run streams queues */
		queuerun();
}

unselect(p)
	register struct proc *p;
{
	register int s = splhigh();

	switch (p->p_stat) {

	case SSLEEP:
		setrun(p);
		break;

	case SSTOP:
		unsleep(p);
		break;
	}
	splx(s);
}


#define	PDELAY	(PZERO-1)
delay(ticks)
{
  	return(delay2(ticks,PDELAY));
}


delay2(ticks,pri)
{
	int s;

	if (ticks<=0)
		return(0);
	s = splhi();
	(void)timeout(unselect, (caddr_t)u.u_procp, ticks);
	if (sleep((caddr_t)u.u_procp, pri | PCATCH)) {
	  	untimeout_func(unselect, (caddr_t) u.u_procp);
		if (! (pri & PCATCH)) {
		  	splx(s);
			longjmp(u.u_qsav);
		};
		splx(s);
		return(1);
	};
	splx(s);
	return(0);
}


poll_register_me(func,mult)
void (*func)();
int mult;
{
	register int i,y;

	if ((mult < MIN_INTERVAL) || (mult> MAX_INTERVAL)){
		cmn_err(CE_CONT,"poll_register_me: Invalid interval: %d\n",mult);
		return(INVAL_INT);
	}

	for (i=mult;i<=MAX_INTERVAL ;i += mult){
		for (y = 0;((poll_tbl[i][y].func != NULL) && (y < POLL_TBL_SIZE));y++);
		if (y == POLL_TBL_SIZE){
			cmn_err(CE_CONT,"poll_register_me: Table Overflow");
			return(NO_RESOURCE);
		}
		poll_tbl[i][y].func = func;
	}
	return(0);
}

poll_unregister_me(func)
void (*func)();
{
	register int i,y;

	for (i=MIN_INTERVAL;i<=MAX_INTERVAL ;i++){
		for (y = 0;((poll_tbl[i][y].func != NULL) && (y < POLL_TBL_SIZE));y++){
			if (poll_tbl[i][y].func == func){
				poll_tbl[i][y].func = NULL;
			}
		}
	}
}
