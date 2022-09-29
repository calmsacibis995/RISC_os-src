/* ------------------------------------------------------------------ */
/* | Copyright Unpublished, MIPS Computer Systems, Inc.  All Rights | */
/* | Reserved.  This software contains proprietary and confidential | */
/* | information of MIPS and its suppliers.  Use, disclosure or     | */
/* | reproduction is prohibited without the prior express written   | */
/* | consent of MIPS.                                               | */
/* ------------------------------------------------------------------ */
/* $Header: todc.c,v 1.7.1.2 90/05/09 15:04:12 wje Exp $ */

/*	Copyright (c) 1984 AT&T	*/
/*	  All Rights Reserved  	*/

/*	THIS IS UNPUBLISHED PROPRIETARY SOURCE CODE OF AT&T	*/
/*	The copyright notice above does not evidence any   	*/
/*	actual or intended publication of such source code.	*/

/*#ident	"@(#)kern-port:os/todc.c	10.2"*/
#ident	"$Header: todc.c,v 1.7.1.2 90/05/09 15:04:12 wje Exp $"

#include "sys/sbd.h"
#include "sys/cpu_board.h"
#include "sys/types.h"
#include "sys/param.h"
#include "sys/i8254clock.h"
#include "sys/clock.h"
#include "bsd/sys/time.h"
#include "sys/cmn_err.h"

extern struct timeval time;
extern rtodc(), wtodc();		/* Lboot'able todc routines */
extern int showconfig;

extern int PT_CLOCK_ADDR[], TIM0_ACK_ADDR[], TIM1_ACK_ADDR[];

#ifdef SABLE
int delay_mult = 0;	/* multiplier for DELAY macro, with initial value */
#else
int delay_mult = 6;	/* multiplier for DELAY macro, with initial value */
#endif !SABLE

/* ~~~HITZ
 * I'm not sure what these values should be.  Probably
 * PERIOD depends on DELTA, because the system can only drift
 * so fast.  Maybe I should try to quantify the drift we can
 * expect, and the accuracy which seems reasonable...
 */
#define VERIFY_TIME_PERIOD 10		/* Secs between verify_time() calls */
#define VERIFY_TIME_DELTA  1000000	/* Usec error size to correct */

#ifdef	MIPS_LOCAL
int time_fix_debug = 0;
#endif	MIPS_LOCAL

#ifdef SABLE

/*
 * Sable clock routines.
 */

/*
 * Definition of the sable scheduling/time of day clock.
 */

struct sable_clock {
	unsigned long	sc_command;	/* stop/start scheduling ticks */
	unsigned long	sc_sched_hz;	/* rate of scheduling tick */
	unsigned long	sc_prof_hz;	/* rate of profiling tick */
	unsigned long	sc_todr;	/* time of day register. READONLY */
};

#define SC_SCHED_START	1
#define SC_SCHED_ACK	2
#define SC_PROF_START	4
#define SC_PROF_ACK	8
#define SABLE_CLOCK_BASE	(struct sable_clock *)(0x1f002000+K1BASE)

#define MASTER_PERIOD	3600
#define	SCHED_PERIOD	MASTER_PERIOD/HZ
#define	PROF_PERIOD	36		/* 100 hertz for now */
#define	KGHERTZ		100		/* must match PROF_PERIOD */

/*
 * Start the real-time clock (really the programable interval clock!).
 */

startrtclock()
{
	struct sable_clock *scp = SABLE_CLOCK_BASE;
	register s;

	s = splall();
	scp->sc_sched_hz = SCHED_PERIOD;
	scp->sc_command |= SC_SCHED_START;
	splx(s);
}

stopclocks()
{
}

/*
 * Mommy, make that mean 'ol clock interrupt go away!
 */

ackrtclock()
{
	struct sable_clock *scp = SABLE_CLOCK_BASE;

	scp->sc_command |= SC_SCHED_ACK;
}

#ifdef KGCLOCK

/*
 * Kgclock is used as an independent time base for statistics gathering.
 */
startkgclock()
{
	struct sable_clock *scp = SABLE_CLOCK_BASE;

	scp->sc_command |= SC_PROF_START;
}

/*
 * Acknowledge the profiling clock tick
 */
ackkgclock()
{
	struct sable_clock *scp = SABLE_CLOCK_BASE;

	scp->sc_command |= SC_PROF_ACK;
}

#endif KGCLOCK

config_delay()
{
	delay_mult = 0;		/* Sable doesn't need more delays */
}

#else !SABLE

/*
 * Clock routines.
 */

/*
 * The todc chip has three counters.  We use #0 for scheduling and #1 for
 * kernel profiling.  Counter #2 is a master which drives the other two.
 * The master clock is decremented at 3686400 hz (1024*3600).  The other two
 * clocks are decremented whenever the master clock reaches 0.  An interrupt
 * is generated whenever clock #0 or #1 reach 0.
 *
 * Each clock has a value (its period) to which it is reset when it hits 
 * zero.  The period of the profiling clock should be chosen so that it 
 * does not sync up with the scheduling clock.  (i.e. PROF_PERIOD shouldn't 
 * divide evenly into SCHED_PERIOD.)
 *
 * Currently the profiling clock is about 10 times faster than the scheduling
 * clock.  This allows data to be gathered more quickly than a slower 
 * profiling clock would, but does have a larger performance impact.
 *
 * I haven't measured the performance impact of profiling.  Have no idea 
 * if it is significant or not.
 */
#ifndef POLL_CONST
#define POLL_CONST 	4   
#endif
#define	MASTER_PERIOD	1024		/* sched and prof run at 3600 Hz */
#define	SCHED_PERIOD	(3600/HZ)	/* 36 */
#define	R24_SCHED_PERIOD	(3600/(HZ*POLL_CONST))		/* 9 */
#define	PROF_PERIOD	7		/* About 514 HZ */

#ifdef XXX
#define	PT_CLOCK_ADDR	(struct pt_clock *)PHYS_TO_K1(0x1e001003)
#define	RT_CLOCK_ADDR	(struct rt_clock *)PHYS_TO_K1(0x1e010003)
#define	TIM0_ACK_ADDR	(char *)PHYS_TO_K1(0x1e200003)
#define	TIM1_ACK_ADDR	(char *)PHYS_TO_K1(0x1e200007)
#endif

/*
 * Start the real-time clock (really the programable interval clock!).
 */
startrtclock()
{
	register volatile struct pt_clock *pt = 
		(struct pt_clock *)MACHDEP(PT_CLOCK_ADDR);
	unsigned short rt_period ;
	int ospl;

	if(IS_R2400)
		rt_period = R24_SCHED_PERIOD;
	else
		rt_period = SCHED_PERIOD;

	ospl = splclock();
	pt->pt_control = PTCW_SC(0)|PTCW_16B|PTCW_MODE(MODE_RG);
	pt->pt_counter0 = rt_period;
	wbflush();
	pt->pt_counter0 = rt_period>>8;

	pt->pt_control = PTCW_SC(2)|PTCW_16B|PTCW_MODE(MODE_RG);
	pt->pt_counter2 = MASTER_PERIOD;
	wbflush();
	pt->pt_counter2 = MASTER_PERIOD>>8;
	splx(ospl);
}

/*
 * Disable clocks 
 */
stopclocks()
{
	register volatile struct pt_clock *pt = 
		(struct pt_clock *)MACHDEP(PT_CLOCK_ADDR);

	pt->pt_control = PTCW_SC(0)|PTCW_16B|PTCW_MODE(MODE_HROS);

	if (IS_R2400)
		pt->pt_counter0 = R24_SCHED_PERIOD;
	else
		pt->pt_counter0 = SCHED_PERIOD;

	wbflush();

	if (IS_R2400)
		pt->pt_counter0 = R24_SCHED_PERIOD>>8;
	else
		pt->pt_counter0 = SCHED_PERIOD>>8;

	pt->pt_control = PTCW_SC(1)|PTCW_16B|PTCW_MODE(MODE_HROS);
	pt->pt_counter1 = PROF_PERIOD;
	wbflush();
	pt->pt_counter1 = PROF_PERIOD>>8;
	pt->pt_control = PTCW_SC(2)|PTCW_16B|PTCW_MODE(MODE_HROS);
	pt->pt_counter2 = MASTER_PERIOD;
	wbflush();
	pt->pt_counter2 = MASTER_PERIOD>>8;

	ackrtclock();
	ackkgclock();
}

/*
 * Clear cpu board TIM0 acknowledge register
 */
ackrtclock()
{
	register volatile char *tim0ack = 
		(char *)MACHDEP(TIM0_ACK_ADDR);
	static char bucket;

	bucket = *tim0ack;
}

/*
 * Clear cpu board TIM1 acknowledge register
 */
ackkgclock()
{
	register volatile char *tim1ack = 
		(char *)MACHDEP(TIM1_ACK_ADDR);
	static char bucket;

	bucket = *tim1ack;
}

#ifdef KGCLOCK

/*
 * Kgclock is used as an independent time base for statistics gathering.
 */
startkgclock()
{
	volatile struct pt_clock *pt = 
		(struct pt_clock *)MACHDEP(PT_CLOCK_ADDR);
	int ospl;

	ospl = splclock();
	pt->pt_control = PTCW_SC(1)|PTCW_16B|PTCW_MODE(MODE_RG);
	pt->pt_counter1 = PROF_PERIOD;
	wbflush();
	pt->pt_counter1 = PROF_PERIOD>>8;

	pt->pt_control = PTCW_SC(2)|PTCW_16B|PTCW_MODE(MODE_RG);
	pt->pt_counter2 = MASTER_PERIOD;
	wbflush();
	pt->pt_counter2 = MASTER_PERIOD>>8;
	splx(ospl);
}
#endif KGCLOCK

#define	TEST_PERIOD	500

config_delay()
{
	register struct pt_clock *pt = 
		(struct pt_clock *)MACHDEP(PT_CLOCK_ADDR);
	register unsigned cnt_lo, cnt_hi, cnt;
	register s;

	s = splhi();

	delay_mult = 1;

	pt->pt_control = PTCW_SC(2)|PTCW_16B|PTCW_MODE(MODE_STS);
	pt->pt_counter2 = 0xffff; wbflush();
	pt->pt_counter2 = 0xffff>>8; wbflush();

	DELAY(TEST_PERIOD);

	pt->pt_control = PTCW_SC(2)|PTCW_CLCMD;
	wbflush();
	cnt_lo = pt->pt_counter2;
	cnt_hi = pt->pt_counter2;

	stopclocks();
	splx(s);

	cnt = 0xffff - ((cnt_hi << 8) | cnt_lo);
	delay_mult = ((unsigned)TEST_PERIOD*MASTER_FREQ);
	delay_mult = ((delay_mult/1000000) + cnt) / cnt;
	if (showconfig)
		printf("Delay multiplier = %d, cnt = %d\n", delay_mult, cnt);
	if (cnt_hi == 0 && cnt_lo == 0)
		printf("config_delay: WARNING: TEST_PERIOD too long\n");
	if (delay_mult < 1 || delay_mult > 50)
		printf("config_delay: WARNING: unusual DELAY value\n");
	if (delay_mult <= 0)
		delay_mult = 3;
}

#endif !SABLE

/*
 * Return the best possible estimate of the time in the timeval
 * to which tvp points.  We do this by reading the interval count
 * register to determine the time remaining to the next clock tick.
 * We must compensate for wraparound which is not yet reflected in the time
 * (which happens when the counter hits 0 and wraps after the splhigh(),
 * but before the counter latch command).  Also check that this time is
 * no less than any previously-reported time, which could happen around
 * the time of a clock adjustment.  Just for fun, we guarantee that
 * the time will be greater than the value obtained by a previous call.
 */
#define MASTER_HZ	(3686400/MASTER_PERIOD)
extern int tick;

microtime(tvp)
	register struct timeval *tvp;
{
	struct pt_clock *pt = 
		(struct pt_clock *)MACHDEP(PT_CLOCK_ADDR);
	static struct timeval lasttime;
	unsigned cnt_lo, cnt_hi;
	register long t;
	int s;

	*tvp = time;
 	s = splhigh();
#ifndef SABLE
	pt->pt_control = PTCW_SC(0)|PTCW_CLCMD;
	wbflush();
	cnt_lo = pt->pt_counter0;
	cnt_hi = pt->pt_counter0;
	/*
	 * convert remaining counter 0 units (which are in 1/MASTER_HZ units)
	 * to usecs
	 */
	t = (1000000/MASTER_HZ) * (MASTER_HZ/HZ - ((cnt_hi << 8) | cnt_lo));

	if ((t < (tick/2)) && (get_cause() & CAUSE_IP5))
		t += tick;
	tvp->tv_usec += t;
	if (tvp->tv_usec > 1000000) {
		tvp->tv_sec++;
		tvp->tv_usec -= 1000000;
	}
	if (tvp->tv_sec == lasttime.tv_sec &&
	    tvp->tv_usec <= lasttime.tv_usec &&
	    (tvp->tv_usec = lasttime.tv_usec + 1) > 1000000) {
		tvp->tv_sec++;
		tvp->tv_usec -= 1000000;
	}
#endif !SABLE
	lasttime = *tvp;
	splx(s);
}

inittodr(base)
	long base;
{
	register uint todr;
	long deltat;
	int year = YRREF;
	int checktodr();
	int verify_time();

	/*
	 * Once a day check for clock rollover.
	 * Check once in a while for drift in the system clock.
	 */
	timeout(checktodr, 0, SECDAY*HZ);
	timeout(verify_time, 0, VERIFY_TIME_PERIOD*HZ);

	todr = rtodc(); /* returns seconds from year start + TODRZERO */
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
	
	/*
	 * Sneak to within 6 months of the time in the filesystem.
	 */
	time.tv_sec = (todr - TODRZERO);
	while ( time.tv_sec < base-SECYR/2) {
		if (LEAPYEAR(year))
			time.tv_sec += SECDAY;
		year++;
		time.tv_sec += SECYR;
	}
	/*
	 * Real-time clock is always set to 366 day years (leap years).
	 * If we're in that last day, reset the real-time clock to
	 * normalize to the first day of the year.
	 */
	if ( (todr-TODRZERO) >= SECYR+SECDAY)
		resettodr();
	
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
	if (deltat < 2*SECDAY)
		return;
	cmn_err(CE_WARN,"clock %s %d days",
		time.tv_sec < base ? "lost" : "gained", deltat/SECDAY);
#endif !SABLE

check:
	cmn_err(CE_WARN, "CHECK AND RESET THE DATE!");
}

/*
 * checktodr -- check for clock rollover and reset if necessary
 */

checktodr()
{
	timeout(checktodr, 0, SECDAY*HZ);
	if ( (rtodc()-TODRZERO) >= SECYR+SECDAY)
		resettodr();
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
	int delta;
	extern int timedelta, tickdelta;

	/*
	 * Wait for verify_time until current timedelta is done.
	 */
	if (timedelta) {
		int wait; 
#ifdef 	MIPS_LOCAL
		if (time_fix_debug) printf("V");
#endif 	MIPS_LOCAL
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
#ifdef 	MIPS_LOCAL
		if (time_fix_debug) printf("verify_time: Start timing\n");
#endif 	MIPS_LOCAL
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
#ifdef 	MIPS_LOCAL
		if (time_fix_debug) printf(".");
#endif	MIPS_LOCAL
		timeout(verify_time, 1, 1);
		return;
	}

#ifdef 	MIPS_LOCAL
	if (time_fix_debug) {
		printf("verify_time: end timing: time=%d.%d cur_time=%d.%d\n",
			time.tv_sec, time.tv_usec, 
			cur_time.tv_sec, cur_time.tv_usec);
	}
#endif	MIPS_LOCAL

	/*
	 * If time has drifted, fix it up.
	 */
	delta = (cur_time.tv_sec - time.tv_sec)*1000000 
	      + (cur_time.tv_usec - time.tv_usec);

	if ( abs(delta) > VERIFY_TIME_DELTA ) {
#ifdef 	MIPS_LOCAL
		if (time_fix_debug) {
			printf("verify_time: fixing time drift (%d usec)\n", 
				delta);
		}
#endif	MIPS_LOCAL

		adjtime1(delta, 0);
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
 * The todc only keeps track of seconds since the beginning of 
 * the year, so we need "guess" to determine the current year.
 * (Any * guess that is accurate to +- 6 months will work.)
 */

struct timeval
read_todc(guess)
struct timeval guess;
{
	struct timeval tod;
	int year;

	tod.tv_usec = 0;
	tod.tv_sec = rtodc() - TODRZERO;

	if (tod.tv_sec < 0) {
#ifdef MIPS_LOCAL
		printf("read_todc: bogus time, returning guess\n");
#endif MIPS_LOCAL
		return guess;	/* Clock is completely bogus.  Ignore it. */
	}


	year = YRREF;
	while (tod.tv_sec < guess.tv_sec-(SECYR/2) ) {
		tod.tv_sec += SECYR;
		if (LEAPYEAR(year))
			tod.tv_sec += SECDAY;
		++year;
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

	wait = 1000000 - newtime.tv_usec; 	/* Time to wait in usec */
	wait *= HZ;
	wait /= 1000000;			/* *(HZ/1000000) gives ticks */

	wtodc(newtime.tv_sec + 1);
	timeout_id = timeout(wtodc, newtime.tv_sec + 1, wait);
}
