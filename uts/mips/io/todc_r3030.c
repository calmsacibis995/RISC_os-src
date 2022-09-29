/*
 * |-----------------------------------------------------------|
 * | Copyright (c) 1989       MIPS Computer Systems, Inc.      |
 * | All Rights Reserved                                       |
 * |-----------------------------------------------------------|
 * | Use, duplication, or disclosure by the Government is      |
 * | subject to restrictions as set forth in                   |
 * | subparagraph (c)(1)(ii) of the Rights in Technical        |
 * | Data and Computer Software Clause of DFARS 52.227-7013.   |
 * |         MIPS Computer Systems, Inc.                       |
 * |         928 Arques Avenue                                 |
 * |         Sunnyvale, CA 94086                               |
 * |-----------------------------------------------------------|
 */

/*	Copyright (c) 1984 AT&T	*/
/*	  All Rights Reserved  	*/

/*	THIS IS UNPUBLISHED PROPRIETARY SOURCE CODE OF AT&T	*/
/*	The copyright notice above does not evidence any   	*/
/*	actual or intended publication of such source code.	*/

#ident	"$Header: todc_r3030.c,v 1.2.1.9 90/05/09 15:04:22 wje Exp $"

#include "sys/sbd.h"
#include "sys/cpu_board.h"
#include "sys/types.h"
#include "sys/param.h"
#include "sys/clock.h"
#include "bsd/sys/time.h"
#include "sys/cmn_err.h"
#include "sys/bc.h"
#include "sys/ctlspace.h"
#include "sys/mk48t02.h"
#include "sys/nvram.h"

#define KILO	1000
#define MEGA	(1000*KILO)

void	intbuf_init();
void	interrupt_at();
extern	clock();
extern	struct timeval	time;
extern	int		showconfig;
extern	int		cpu_config;
extern	unsigned long	r3030_rtodc();
extern int TODC_CLOCK_ADDR[];		/* Lboot'able addresses */

/*
 * for the real time clock counter we have two different clocks, not necessarily
 * aligned; these are the CPU clock, and the COUNTER clock. The difference between
 * them is MULT, where CPU clock = MULT * COUNTER clock. For the Rx3230 this difference
 * is 4, as rambo does a divide by 4 to get the internal clock frequency, and is
 * fed the system clock.
 */

#define	CLOCK_GUESS	25		/* initial guess of system clock */
#define SPIN_TIME	4		/* seconds */
#define MIN_CPU_HZ	10*MEGA		/* in case TOD clock doesn't work */
#define MAX_CPU_HZ	50*MEGA		/* in case TOD clock doesn't work */
#define	MULT		4		/* RAMBO tick to CPU clock tick multiplier */
#define	CLOCK_COUNT	*(volatile ulong *)0xbc000c00 /* RAMBO count reg */
#define	CLOCK_COMPARE	*(volatile ulong *)0xbc000d00 /* RAMBO compare reg */
#define	NUM_INT_BUFS	20

static	ulong rambo_hz 	 = CLOCK_GUESS*MEGA/MULT;	/* init to satisfy config_delay() */
static	ulong rambo_hz_tick_error = 0;		/* extra cpu cycles per tick */
static	ulong rambo_hz_error = 0;		/* total accumulated error cycles */
static	ulong rambo_clock_count;	/* we hold the last clock value of rambo here */
extern	ulong rambo_clock_delay; 	/* this holds the increment we apply to rambo */
static	ulong rambo_clock_error;	/* this holds the error (up to HZ) from delay */
struct	intbuf {
	ulong	time;
	void	(*routine)();
	char	*arg;
	struct	intbuf	*next;
};
static struct intbuf intatbuf[NUM_INT_BUFS];
static struct intbuf *head_ptr = 0;	/* the next interrupt */
static struct intbuf *free_buf = 0;	/* the next free buffer */
	
#ifdef SABLE
int delay_mult = 0;	/* multiplier for DELAY macro, with initial value */
#else
int delay_mult = 16;	/* multiplier for DELAY macro, with initial value */
#endif !SABLE

#define VERIFY_TIME_PERIOD 512 		/* Secs between verify_time() calls */
#define VERIFY_TIME_DELTA  1000000	/* Usec error size to correct */
static long	last_usec_delta = 0;
static long	next_hz_delta=0;
static int	drift_count = 0;
extern int	remote_bp_encountered;

#ifdef	MIPS_LOCAL
int time_fix_debug = 0;
#endif	MIPS_LOCAL

/*
 * Start the real-time clock (really the programable interval clock!).
 */
startrtclock()
{
	register s;
	int	print_hz, mhz, khz100, khz10;
	uint	tod, old_tod, end_count;
	register volatile struct todc_clock *todc = 
		(volatile struct todc_clock *)MACHDEP(TODC_CLOCK_ADDR);

#ifdef SABLE
	rambo_hz = CLOCK_GUESS*MEGA/MULT;		/* too long to compute */
#else
	s = splall();
	old_tod = r3030_rtodc();			/* time now, in seconds */
	if (old_tod == 0) {
	    /*
	     *  NVRAM says time isn't Valid, but we need a nonzero 'seconds'
	     *  time here.  So use inittodr() to set the TOD chip to some
	     *  bogus value, just to get the NVRAM Valid bit turned on.
	     */
	     inittodr(0);
	     old_tod = r3030_rtodc();			/* read it again */
	}
	end_count  = MAX_CPU_HZ*SPIN_TIME;
	if (old_tod) {
		old_tod = todc->todc_secs;
		tod = old_tod;
		while (tod == old_tod) {
			todc->todc_control |= TODC_CONT_READ;
			tod = todc->todc_secs;
			todc->todc_control &= ~TODC_CONT_READ;
		}
		old_tod	   = tod + SPIN_TIME;
		if ((old_tod & 0xf) > 9) {
			old_tod += 6;	/* adjust for BCD representation */
			if (old_tod > 0x59)
				old_tod -= 0x60;
		}
		/*  spin again for N seconds, or until we've chewed up so many  */
		/*  CPU cycles that we can assume the TOD chip doesn't work     */
		CLOCK_COUNT = 0;			/* zero the cycle count */
		while ((old_tod != tod) && (((unsigned)CLOCK_COUNT) < end_count)) {
			todc->todc_control |= TODC_CONT_READ;
			tod = todc->todc_secs;
			todc->todc_control &= ~TODC_CONT_READ;
		}
		end_count = CLOCK_COUNT;
		rambo_hz = end_count/SPIN_TIME;
	} else {
		rambo_hz = 0;	/* error - we'll print out the error below */
	}
	splx( s );

	/*
	 *  Does our TOD chip work?
	 */
	if ((rambo_hz * MULT) < MIN_CPU_HZ  ||  (rambo_hz * MULT) > MAX_CPU_HZ) {
	    cmn_err(CE_WARN, "improperly functioning NVRAM clock chip");
	    rambo_hz = CLOCK_GUESS*MEGA/MULT;		/* invent a value */
	}

#endif !SABLE

	/*
	 *  Recompute delay_mult, now that we have an accurate CPU clock rate.
	 */
	config_delay();

	print_hz = rambo_hz * MULT + 5000;		/* round up for display */
	mhz    = print_hz / MEGA;
	khz100 = (print_hz - (mhz * MEGA)		       ) / (100*KILO);
	khz10  = (print_hz - (mhz * MEGA) - (khz100 * 100*KILO)) /  (10*KILO);
#ifdef MIPS_LOCAL
	{
#else
	if (showconfig) {
#endif
		cmn_err(CE_CONT,"[ CPU clock        = %d.%d%d-MHz ]\n", mhz, khz100, khz10);
		cmn_err(CE_CONT,"[ Delay count      = %d ]\n", delay_mult);
		cmn_err(CE_CONT,"[ rambo add        = %d ]\n", rambo_hz / HZ);
		cmn_err(CE_CONT,"[ rambo_hz         = %d ]\n", rambo_hz);
	}
	remote_bp_encountered = 0;		/* initialize */
	cpu_config |= mhz << P_SPEED_SHIFT;
	s = splall();
	/*
	 * set up the queue for the rambo clock
	 */
	intbuf_init();
	/*
	 *  The RAMBO clock ticks every cycle, and interrupts whenever the
	 *  COUNT value equals the COMPARE value. Set up the interrupt for
	 *  the initial clock period interrupt.
	 *  the clock period delay is calculated as: 1000000 microseconds / HZ
	 */
	rambo_clock_count = CLOCK_COUNT;
	rambo_clock_delay = rambo_hz / HZ;	/* set up for HZ Hz */
	rambo_clock_error = rambo_hz % HZ;	/* set up for HZ Hz */
	interrupt_at(rambo_clock_count + rambo_clock_delay, clock);
	splx( s );
	ailclock();	/* clock is now initialised, so let it rip */
}


/*
 * Disable clocks 
 */
stopclocks()
{
	iilclock();
}


/*
 * Acknowledge the clock interrupt
 */
ackrtclock()
{
	/*
	 *  Always bump the Compare register by a fixed value, and clear
	 *  the external timer interrupt. We keep a track of what the count
	 *  value should have been. This allows us to obtain a more accurate
	 *  interrupt accuracy, as we do not accumulate errors.
	 *
	 * Since the value in rambo_clock_dealy * HZ is not the exact number
	 * of rambo ticks in a second we fold in the difference. In HZ rambo ticks
	 * we would accumulate an error of rambo_clock_error rambo ticks. To
	 * adjust for this on the first rambo_clock_error delays we add 1 to
	 * the delay amount. This gives us the automatic adjustment without much
	 * pain.
	 */
	static unsigned long	count = 0;
	register		s;

	s = splall();
	if (++count >= HZ)
		count = 0;
	if (count > rambo_clock_error)
		interrupt_at(rambo_clock_count += rambo_clock_delay, clock);
	else
		interrupt_at(rambo_clock_count += rambo_clock_delay + 1, clock);
	splx(s);
}


config_delay()
{
#define	DELAY_TEST_TIME 256
	register s;
	unsigned long	start_tick,
			diff_tick;

/*
 * we calculate the delay multiplier as follows:
 * it takes diff_tick rambo ticks to do a 'supposedly'
 * DELAY_TEST_TIME microseconds delay. we generate what
 * the correct answer should have been, ie:
 *
 *	DELAY_TEST_TIME * #of rambo ticks per microsecond
 *
 * with #of rambo ticks per microsecond being generated
 * by:
 *	rambo_hz/MEGA (number of rambo ticks per second/1000000)
 *
 * we then divide the time it should have taken bu the time it
 * did take, and this is the number of times we would have to do
 * the loop to actually do a real delay of the requested time.
 *
 * we have rearranged the calculation to try and ensure that no overflows
 * occur, either overflow, or underflow. We have to be careful, as the
 * basic rambo_hz = 6250000 and this / MEGA is only 6, not 6.25, an error
 * of 4%
 */
#ifndef SABLE
	s = splall();
	delay_mult = 1;
	start_tick = CLOCK_COUNT;
	DELAY(DELAY_TEST_TIME);
	diff_tick = CLOCK_COUNT - start_tick;
	delay_mult = ((DELAY_TEST_TIME/diff_tick)*rambo_hz)/MEGA;
	splx(s);
#else SABLE
	delay_mult = 0;		/* Sable is too slow already -- no delays! */
#endif SABLE
}

/*
 * Return the best possible estimate of the time in the timeval
 * to which tvp points.  We do this by reading the interval count
 * register to determine the time since the last timer interrupt.
 * Just for fun, we guarantee that the time will be greater than 
 * the value obtained by a previous call.
 */
microtime(tvp)
	register struct timeval *tvp;
{
	static struct timeval lasttime;
	unsigned long timer_count;
	unsigned long usecs_since_interrupt;
	int s;

	*tvp = time;
 	s = splhigh();
	timer_count   = CLOCK_COUNT;

/*
 * take care of overflow, and underflow
 */
	usecs_since_interrupt = (((timer_count - (rambo_clock_count - rambo_clock_delay))  * KILO) / (rambo_hz / KILO));

	tvp->tv_usec += usecs_since_interrupt;
	if (tvp->tv_usec > MEGA) {
		tvp->tv_sec++;
		tvp->tv_usec -= MEGA;
	}
	if (tvp->tv_sec == lasttime.tv_sec &&
	    tvp->tv_usec <= lasttime.tv_usec &&
	    (tvp->tv_usec = lasttime.tv_usec + 1) > MEGA) {
		tvp->tv_sec++;
		tvp->tv_usec -= MEGA;
	}
	lasttime = *tvp;
	splx(s);
}

inittodr(base)
	long base;
{
	register long todr;
	long deltat;
	int year = YRREF;
	int verify_time();

	/*
	 * Check once in a while for drift in the system clock.
	 */
	timeout(verify_time, 0, VERIFY_TIME_PERIOD*HZ);

	todr = r3030_rtodc(); /* returns seconds from 1 Jan 1970 */

	/*
	 * if base == -1 then we are diskless - we just believe the time
	 * from the real time clock.
	 */

	if (todr < 0) {
		/*
		 * if the number returned from todr was negative then
		 * it could be that we have an old style time of day chip.
		 * In the old style the BCD representation was trying to
		 * be accurate, (but was too hard to try and keep right)
		 * The 'nominal' adjustment factor between 0 on the old
		 * scale, and '0' on the new scale is 0x56871300 which is
		 * the number of seconds to 00:00:00 1/1/1970 from 00:00:00 1/1/1900
		 *
		 * Since we are restricting the time to being a 31 bit integer
		 * we can check for <0 being new time.
		 */
		time.tv_sec = todr - 0x56871300;
		time.tv_usec = 0;
		if (time.tv_sec > 0) {
			cmn_err(CE_WARN, "Old style time in TODCLK");
			resettodr();
			goto check;
		}
		todr = -1;
	}
	if ( base == -1 ) {
		base = todr;
	}
	if ( base < 5*SECYR) {
		cmn_err(CE_WARN, "preposterous time in file system");
		time.tv_sec = 6*SECYR + 186*SECDAY + SECDAY/2;
		time.tv_usec = 0;
		resettodr();
		goto check;
	}
	/*
	 * TODRZERO is base used to detect loss of power to TODCLK
	 */
	if (todr < TODRZERO) {
		cmn_err(CE_WARN, "lost battery backup clock");
		/* 
		 * Believe the time in the file system for lack 
		 * of anything better, and reset the TODR.
		 */
		time.tv_sec = base;
		time.tv_usec = 0;
		resettodr();
		goto check;
	}

	time.tv_sec = todr;
	time.tv_usec = 0;
	/*
	 * See if we gained/lost two or more days: 
	 * If so, assume something is amiss.
	 */
	deltat = time.tv_sec - base;
	if (deltat < 0)
		deltat = -deltat;
#ifdef SABLE
	return;
#else
	if (deltat < 2*SECDAY) {
		return;
	}
	cmn_err(CE_WARN,"clock %s %d days",
		time.tv_sec < base ? "lost" : "gained", deltat/SECDAY);
#endif !SABLE

check:
	cmn_err(CE_WARN, "CHECK AND RESET THE DATE!");
}

/* 
 * Reset the TODR based on the time value; used when the TODR
 * has a preposterous value and also when the time is reset
 * by the stime system call.
 */

resettodr() {
	write_todc(time);
}

/*
 *	The routines below use the real time clock chip (todc) to
 *	prevent the system time from drifting.
 *
 *	Every VERIFY_TIME_PERIOD seconds verify_time() is called to
 *	determine the real time as specified by the todc, and to adjust
 *	the time -- as adjtime(2) does -- if it is more than
 *	VERIFY_TIME_DELTA microseconds different from the contents of
 *	the global time variable.
 *
 *	Since the real time clock reports time with only 1 second
 *	precision, verify_time() checks the time once a tick until it
 *	changes.  At the point that it changes it should be accurate to
 *	within a tick or so.  When verify_time() is called in this
 *	"call-myself again in one tick" mode, the "timing" argument is
 *	set to 1, otherwise 0.
 *
 *	Verify_time will not try to adjust the time if a timedelta is
 *	already being applied.
 *
 *	The routines read_todc() and write_todc() provide struct timeval
 *	style access to the todc using Epoch based time.
 */
struct timeval read_todc();

verify_time(timing)
{
	static struct timeval last_time;
	struct timeval cur_time;
	int usec_delta;
	int drift;
	int rambo_hz_delta;
	extern int timedelta, tickdelta;

	/*
	 * Wait for verify_time until current timedelta is done.
	 */
	if (timedelta) {
		int wait; 
		/* Wait until VERIFY_TIME_PERIOD, or until current time
		 * adjustment is finished, whichever is sooner.
	   	 */
		wait = min(VERIFY_TIME_PERIOD*HZ, timedelta/tickdelta + 10*HZ);
		timeout(verify_time, 0, wait);
		return;
	}

	/*
	 * If not timing, then initiate timing mode.
	 */
	if (!timing) {
		last_time = read_todc(time);
		timeout(verify_time, 1, 1);
		return;
	}

	/*
	 * Keep "timing" until todc changes values.  
	 * Then cur_time is accurate.
	 */
	cur_time = read_todc(time);
	if (cur_time.tv_sec == last_time.tv_sec) {
		timeout(verify_time, 1, 1);
		return;
	}

#ifdef 	MIPS_LOCAL
	if (time_fix_debug) {
		cmn_err(CE_CONT,
			"verify_time: end timing: time=%d.%d cur_time=%d.%d\n",
			time.tv_sec, time.tv_usec, 
			cur_time.tv_sec, cur_time.tv_usec);
	}
#endif	MIPS_LOCAL

	/*
	 * If time has drifted, fix it up.
	 */
	usec_delta = (cur_time.tv_sec - time.tv_sec)*MEGA
		   + (cur_time.tv_usec - time.tv_usec);

	if (remote_bp_encountered) {
		/*
		 *  We have jumped off to dbgmon to service a breakpoint,
		 *  and dbgmon has [we presume] saved and restored the SBC
		 *  Clock register in order to "freeze" the time state.
		 *  This is convenient for kernel debugging, but wrecks havoc
		 *  on this rambo_hz correction algorithm because the kernel's
		 *  notion of time-of-day is drastically different from the
		 *  time-of-day-chip's notion.  So don't try to adjust the
		 *  rambo_hz value.  Just let adjtime1() adjust things.
		 */
		drift = drift_count = 0;
	} else {
		drift = usec_delta - last_usec_delta;
		drift_count++;
	}

	if ( abs(drift) > 3*MEGA/HZ ) {
		/*
		 *  The time derived from clock ticks is drifting away
		 *  from the time in the time-of-day chip, and the
		 *  difference is more than four clock ticks.
		 *  Adjust what we think of as the CPU clock rate in
		 *  order to slow or stop the drift.
		 */
		rambo_hz_delta = -25*drift / (drift_count*VERIFY_TIME_PERIOD);
		/*
		 * To avoid problems with scheduling & interrupt "jitter"
		 * as well as to avoid other errors, we require that the
		 * computed delta have the same sign twice.  When this
		 * occurs, we take the smaller of the two deltas.
		 * This also helps us at startup time, where the first
		 * computed delta is a somewhat  arbitrary value.
		 */
#ifdef	MIPS_LOCAL
		if (time_fix_debug) {
		  cmn_err(CE_CONT,
			  "verify_time:  hz_delta1 %d hz_delta2 %d  delta %d drift %d drift_count %d\n",
			  next_hz_delta, rambo_hz_delta, 
			  usec_delta, drift,
			  drift_count);
		}
#endif	MIPS_LOCAL
		if ((next_hz_delta > 0) && (rambo_hz_delta >= next_hz_delta) ||
		    (next_hz_delta < 0) && (rambo_hz_delta <= next_hz_delta)) {
		  
		  /* be cautious -- don't move too fast */
		  if (next_hz_delta > 2000) {
		    next_hz_delta = 2000;
		  } else
		    if (next_hz_delta < -2000) {
		      next_hz_delta = -2000;
		    }
#ifdef MIPS_LOCAL
		  if (time_fix_debug) {
		    cmn_err(CE_CONT,
			    "verify_time: adj rambo_hz %d by %d =%d delta %d drift %d drift_count %d\n",
			    rambo_hz, next_hz_delta,
			    rambo_hz+next_hz_delta, usec_delta, drift,
			    drift_count);
		  }
#endif MIPS_LOCAL
		  rambo_hz += next_hz_delta;
/*		  rambo_clock_delay = rambo_hz/HZ; */
		  rambo_hz_tick_error = rambo_hz - HZ * rambo_clock_delay; 
		  cmn_err(CE_CONT,"!Adjust CLOCK Hz by %d to %d \n",
			  next_hz_delta, rambo_hz);
		  next_hz_delta = 0;	/* Reset so don't adjust next time */
		} else
		  next_hz_delta = rambo_hz_delta;
		
		
		/* Restart the drift computation */
		
		last_usec_delta = usec_delta;
		drift_count = 0;
	}

	if ( abs(usec_delta) > VERIFY_TIME_DELTA ) {
		/*
		 *  The time derived from clock ticks is *really* off.
		 *  Do something drastic.
		 */
#ifdef 	MIPS_LOCAL
		if (time_fix_debug) {
			printf("verify_time: fixing time drift (%d usec)\n", 
				usec_delta);
		}
#endif	MIPS_LOCAL

		adjtime1(usec_delta, 0);
	}

	/*
	 * Set up for next check.
	 */
	timeout(verify_time, 0, VERIFY_TIME_PERIOD*HZ);
}

/*
 * Read_todc returns seconds since the Epoch as specified by
 * the todc.
 * 
 */

struct timeval
read_todc(guess)
struct timeval guess;
{
	struct timeval tod;
	int year;

	tod.tv_usec = 0;
	tod.tv_sec = r3030_rtodc();

	if (tod.tv_sec < 0) {
#ifdef MIPS_LOCAL
		printf("read_todc: bogus time, returning guess\n");
#endif MIPS_LOCAL
		return guess;	/* Clock is completely bogus.  Ignore it. */
	}
	return tod;
}

/*
 * Write_todc() sets the real time of day chip to the time specified by
 * the argument.  There are cases -- see adjtime() -- were this is not
 * equal to the current time.
 *
 * Since the todc has only 1 second precision, we must set it at an exact 
 * second to get the time right.  In addition, we set it right away so that
 * people reading before the timeout() fires get a reasonable value.
 * Untimeout() eliminates the conflict that would * occur in case of a 
 * second write_todc before the timeout.
 */
write_todc(newtime)
struct timeval newtime;
{
	static int timeout_id = 0;
	int wait;

	if (timeout_id) {
		untimeout(timeout_id);
		timeout_id = 0;
	}

	wait = MEGA - newtime.tv_usec;	 	/* Time to wait in usec */
	wait *= HZ;
	wait /= MEGA;				/* *(HZ/1000000) gives ticks */

	r3030_wtodc(newtime.tv_sec + 1);
	timeout_id = timeout(r3030_wtodc, newtime.tv_sec + 1, wait);
}

/*
 *	Clock interrupt routines.
 *
 * these routines handle the addition and deletion of items from the clock
 * list.
 *
 * intbuf_init() - initialises the linked list, should be called at boot time.
 *
 * clock_intr_dummy() - should never be called - null routine.
 *
 * handle_clock_intr() - executes the routine and sets up the interrupt for
 * the next routine.
 *
 * interrupt_at() - adds the requested routine to be executed at the
 * requested time.
 */

void
intbuf_init()
{
	register int x;
	register struct intbuf *iptr = &intatbuf[0];

	for(x = 0; x < NUM_INT_BUFS; x++) {
		iptr->next = free_buf;
		free_buf = iptr++;
	}
}

void
interrupt_at(time, routine, arg)
unsigned long time;
void (*routine)();
char *arg;
{
	register struct intbuf *iptr;
	register struct intbuf *pptr;
	register int splsave;
	extern long	ipl_special_set;

	splsave = splall();
	if (free_buf == 0) {
		cmn_err(CE_PANIC, "interrupt_at: out of intbufs");
	}
	iptr = free_buf;
	free_buf = iptr->next;
	iptr->time = time;
	iptr->routine = routine;
	iptr->arg = arg;
/*
 * we check certain things as we insert this item.
 * firstly; we check thatthe time for the interrupt has not already passed,
 * then we check to see that it is not the first thing that has to be executed,
 * and finally we check to see if we can start the insertion search from the
 * last entry we inserted.
 */
	if (((long)(CLOCK_COUNT - time)) > 0) {
		register s;
		register add = 10;
		/*
		 * the time for this interrupt has already passed.
		 * we have to insert this one at the head of the list,
		 * and enable the interrupt, so it gets done right
		 * away. (We set the compare to be the current count plus
		 * a little bit.)
		 */
		s = splall();
		CLOCK_COMPARE = add + CLOCK_COUNT;
		while((long)(CLOCK_COUNT - CLOCK_COMPARE) > 0) {
			CLOCK_COMPARE = add + CLOCK_COUNT;
			add += add;
		}
		splx(s);
		iptr->next = head_ptr;	/* and put it at the head of the list */
		head_ptr = iptr;
		splx(splsave);
		return;
	}
	if (head_ptr == 0 || (long)(head_ptr->time - iptr->time) > 0) {
		/*
		 * iptr has to go at the front of the list,
		 * it is the next interrupt to be scheduled.
		 */
		iptr->next = head_ptr;
		head_ptr = iptr;
		CLOCK_COMPARE = iptr->time;
		splx(splsave);
		return;
	}
	pptr = head_ptr;
	while(pptr->next && ((long)(pptr->next->time - time) < 0))
		pptr = pptr->next;
	iptr->next = pptr->next;
	pptr->next = iptr;
	splx(splsave);
	return;
}

void
clock_intr_dummy()
{
}

handle_clock_intr(ep)
int *ep;
{
	/*
	 * this routine should be called with clock interrupts disabled
	 */
	register struct intbuf	*tmp;

	if (head_ptr == 0) {
		CLOCK_COMPARE = CLOCK_COUNT - 1;
		return;
#if 0
		cmn_err(CE_PANIC, "handle_clock_intr: no routine to call");
#endif
	}
	tmp = head_ptr;
	head_ptr = head_ptr->next;
	(*tmp->routine)(ep, tmp->arg);
	tmp->next = free_buf;
	free_buf = tmp;
	/*
	 * now, set up the time for the next interrupt.
	 * if there is none to do then we should really panic,
	 * but, just in case we'll point to a dummy routine.
	 */
	if (head_ptr != 0) {
		/*
		 * firstly, check to see that the time for the interrupt
		 * has not already passed. If it has then we should just
		 * set the timer to interrupt us again.
		 */
		CLOCK_COMPARE = head_ptr->time;
		if (((long)(CLOCK_COUNT - CLOCK_COMPARE)) > 0) {
			register s;
			register add = 10;
			/*
			 * the time for this interrupt has already passed.
			 * we have to insert this one at the head of the list,
			 * and enable the interrupt, so it gets done right
			 * away. (We set the compare to be the current count plus
			 * a little bit.)
			 */
			s = splall();
			CLOCK_COMPARE = add + CLOCK_COUNT;
			while((long)(CLOCK_COUNT - CLOCK_COMPARE) > 0) {
				CLOCK_COMPARE = add + CLOCK_COUNT;
				add += add;
			}
			splx(s);
		}
	} else {
		/*
		 * there was no routine scheduled, so we set up the delay
		 * for the maximum time and point it to a dummy routine.
		 */
		if (free_buf == 0) {
			cmn_err(CE_PANIC, "handle_clock_intr: out of intbufs");
		}
		head_ptr = free_buf;
		free_buf = free_buf->next;
		head_ptr->routine = clock_intr_dummy;
		CLOCK_COMPARE = head_ptr->time = CLOCK_COUNT;
		head_ptr->next = 0;
	}
}

/*
 * real time clock interface.
 *
 * These routines interface with the real time clock chip.
 *
 * these routines accept and return an unsigned long
 */

#define	SECS_ONE_MIN		60
#define	SECS_ONE_HOUR		(60 * SECS_ONE_MIN)
#define	SECS_ONE_DAY		(24 * SECS_ONE_HOUR)
#define	SECS_ONE_YEAR		(365 * SECS_ONE_DAY)
#define	SECS_ONE_LEAP_YEAR	(366 * SECS_ONE_DAY)
#define	SECS_LEAP_PERIOD	(3 * SECS_ONE_YEAR + SECS_ONE_LEAP_YEAR)

extern prom_getenv(), prom_nv_get();
extern int prom_has_version;
static int	months[] = {
/* jan */	31,
/* feb */	28,
/* mar */	31,
/* apr */	30,
/* may */	31,
/* jun */	30,
/* jul */	31,
/* aug */	31,
/* sep */	30,
/* oct */	31,
/* nov */	30,
/* dec */	31,
};

#define	dec_to_bcd(x)	{ x = ((x / 10) * 16) + (x % 10); }
#define	bcd_to_dec(x)	{ x = (((x >> 4) & 0xf) * 10) + (x & 0xf); }
unsigned long
r3030_rtodc()
{
	register volatile struct todc_clock *todc = 
		(volatile struct todc_clock *)MACHDEP(TODC_CLOCK_ADDR);
	unsigned long	year;
	unsigned long	month;
	unsigned long	day;
	unsigned long	hour;
	unsigned long	min;
	unsigned long	secs;

	/* 
	 * Check if the prom has noticed that battery backup has been lost.
	 *
	 * If we are using 4.0 proms or later, we use prom_nv_get to
	 * get information out of the prom, else we use last known
	 * address for information in nvram.  (Early proms have no "version"
	 * environment variable.)
	 */
	if (prom_has_version) {
		if (((int)(*(char *)prom_nv_get(NVINDEX_STATE))
				 & NVSTATE_TODVALID) == 0)
			return(0);
	} else {
		if (((int)todc->todc_mem[NVADDR_STATE].value
				 & NVSTATE_TODVALID) == 0)
			return(0);
	}
	todc->todc_control |= TODC_CONT_READ;
	year = todc->todc_year;
	month = todc->todc_month & TODC_MONTH_MASK;
	day = todc->todc_daym & TODC_DAYM_MASK;
	hour = todc->todc_hours & TODC_HOUR_MASK;
	min = todc->todc_mins & TODC_MINS_MASK;
	secs = todc->todc_secs & TODC_SECS_MASK;
	todc->todc_control &= ~TODC_CONT_READ;
	bcd_to_dec(year);
	bcd_to_dec(month);
	bcd_to_dec(day);
	bcd_to_dec(hour);
	bcd_to_dec(min);
	bcd_to_dec(secs);
	secs += min * SECS_ONE_MIN;
	secs += hour * SECS_ONE_HOUR;
	secs += (day - 1) * SECS_ONE_DAY;
	if ((year % 4) == 0) {
		months[1] = 29;
	} else
		months[1] = 28;
	for(month--;month;month--)
		secs += months[month-1] * SECS_ONE_DAY;
	secs += ((year / 4) * SECS_LEAP_PERIOD);
	year = year % 4;
	switch(year) {
	case 3:
		secs += SECS_ONE_YEAR;
	case 2:
		secs += SECS_ONE_YEAR;
	case 1:
		secs += SECS_ONE_LEAP_YEAR;
	case 0:
		break;
	}
	return secs;
}

r3030_wtodc(secs)
unsigned long secs;
{
	register volatile struct todc_clock *todc = 
		(volatile struct todc_clock *)MACHDEP(TODC_CLOCK_ADDR);
	unsigned long	year;
	unsigned long	month;
	unsigned long	day;
	unsigned long	hour;
	unsigned long	min;
	unsigned long	i;
	unsigned long	j = secs;
	int s;
	char *state;

	year = (secs / SECS_LEAP_PERIOD) * 4;
	secs =  secs % SECS_LEAP_PERIOD;
	if (secs >= SECS_ONE_LEAP_YEAR) {
		secs -= SECS_ONE_LEAP_YEAR;
		year++;
		months[1] = 28;
		year += secs / SECS_ONE_YEAR;
		secs =  secs % SECS_ONE_YEAR;
	} else
		months[1] = 29;
	month = 1;
	for(i = 0; i < 12; i++) {
		int	month_secs;

		month_secs = months[i] * SECS_ONE_DAY;
		if (secs >= month_secs) {
			month++;
			secs -= month_secs;
		} else
			break;
	}
	day = (secs / SECS_ONE_DAY) + 1;
	secs = secs % SECS_ONE_DAY;
	hour = secs / SECS_ONE_HOUR;
	secs = secs % SECS_ONE_HOUR;
	min =  secs / SECS_ONE_MIN;
	secs = secs % SECS_ONE_MIN;
	if (secs > 59 || min > 59 || hour > 23 || (month == 0 || month > 12) ||
		(day == 0 || day > months[month - 1]) || year > 99) {
		printf("wtodc overflow on seconds = %x\n", j);
		printf("year  = %d\n", year);
		printf("month = %d\n", month);
		printf("day   = %d\n", day);
		printf("hour  = %d\n", hour);
		printf("min   = %d\n", min);
		printf("sec   = %d\n", secs);
	}
	dec_to_bcd(year);
	dec_to_bcd(month);
	dec_to_bcd(day);
	dec_to_bcd(hour);
	dec_to_bcd(min);
	dec_to_bcd(secs);
	s = splall();
	todc->todc_control |= TODC_CONT_WRITE;
	todc->todc_year = year;
	todc->todc_month = month;
	todc->todc_daym = day;
	todc->todc_hours = hour;
	todc->todc_mins = min;
	todc->todc_secs = secs;
	todc->todc_control &= ~TODC_CONT_WRITE;
	if (prom_has_version) { /* if prom version >= 4.0  */
				/* mark the TOD as valid   */
		state = (char *)prom_nv_get(NVINDEX_STATE); 
		*state |= NVSTATE_TODVALID;
		(void) prom_nv_set(NVINDEX_STATE, state); 
	}
	splx(s);
}
