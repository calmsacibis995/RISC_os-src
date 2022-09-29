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
 * |         928 Arques Avenue                                 |
 * |         Sunnyvale, CA 94086                               |
 * |-----------------------------------------------------------|
 */
#ident	"$Header: iop_clock.c,v 1.5.1.3 90/05/10 05:21:51 wje Exp $"
/*
 * $Header: iop_clock.c,v 1.5.1.3 90/05/10 05:21:51 wje Exp $
 */
/*
 * The Jupiter Workstation is a little unusual which respect to the clock
 * interrupt.  It's processed like a device interrupt and so the source
 * is found in the io directory of the kernel.
 */
#include "sys/types.h"
#include "sys/param.h"
#include "sys/cmn_err.h"
#include "sys/edt.h"
#include "sys/sbd.h"
#include "sys/iop.h"
#include "sys/iop_clk.h"
#include "sys/tod.h"
#include "sys/systm.h"
#include "sys/boot.h"
#include "sys/clock.h"
#include "bsd/sys/time.h"

u_long last_tick,lost_tick;	/* monitoring */

/* UGH....watch out below!! */
/* stolen from os/clock.c */

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

extern int tick;		/* a tick = 10ms historically */
extern struct timeval time;	/* the kernel's idea of time */

int clock();
long real_tod_read;

/* XXX~~~
 * This should really read the clock and find out exactly what time
 * it is.
 */
microtime(tvp)
	register struct timeval *tvp;
{
	*tvp = time;
}

clockintr (dummy, ep)
	int dummy, *ep;
{
	register IopClock *ic;
	register u_long itick, ntick, i;
	register u_long big = 0xffffffff;
	IopClock *iop_alloc();
	static int clock_first = 1;

	/* the v50 returns the number of 10ms ticks that */
	/* have gone by. This is not always 1. So we figure out */
	/* from last_tick how many went by and bump the time the */
	/* extra ones and let the clock() routine bump the last one */

	if ((ic = iop_alloc(CLOCKIOCB, sizeof (*ic))) == NULL)
	  cmn_err (CE_PANIC, "ackrtclock: iop_alloc failed\n");

	itick = ic->ic_ticks;	/* read tick count */
	if (clock_first) {	/* if first time set last_tick */
		last_tick = itick - 1;
		clock_first=0;	/* don't do this loop again */
	}

	ntick = 0;
	if (itick > last_tick)
		ntick = (itick - last_tick - 1);
	else if (itick < last_tick) {	/* check for wrap-around */
		if (big >= last_tick)
			ntick = big - last_tick + itick;
	}
	if (ntick > 1000) {	/* 10 secs went by ?? */
		cmn_err (CE_CONT, "preposterous tick count %d\n",ntick);
		ntick = 0;
	}

	lost_tick += ntick;
	for (i=0;i<ntick;i++) {		/* bump the extra number of ticks */
		BUMPTIME(&time,tick);
	}
	
	last_tick = itick;		/* record the tick count */
	clock (ep);			/* do clock and another tick */
}

clockedtinit (edt)
	struct edt *edt;
{
}

startrtclock()
{
	register IopClock *ic;
	IopClock *iop_alloc();
	int status;

	if ((ic = iop_alloc(CLOCKIOCB, sizeof (*ic))) == NULL)
	  cmn_err (CE_PANIC, "startclocks: iop_alloc failed\n");

	ic->ic_cmd = CLOCK_START;
	ic->ic_intr = CLOCK_INTR_ON;

	if (iop_poke (CLOCKIOCB, IOPB_SPIN, K1_TO_IOPB_OFFSET(ic), 0))
	  cmn_err (CE_PANIC, "startclocks: iop_poke failed\n");
	if (iop_wait (CLOCKIOCB, IOPB_SPIN, &status, 0))
	  cmn_err (CE_PANIC, "startclocks: iop_wait failed\n");
	iop_clear (CLOCKIOCB);

	if (status)
	  cmn_err (CE_PANIC, "startclocks: bad status\n");
}

stopclocks()
{
	register IopClock *ic;
	IopClock *iop_alloc();
	int status;

	if ((ic = iop_alloc(CLOCKIOCB, sizeof (*ic))) == NULL)
	  cmn_err (CE_PANIC, "stopclocks: iop_alloc failed\n");

	ic->ic_cmd = CLOCK_STOP;
	ic->ic_intr = CLOCK_INTR_OFF;

	if (iop_poke (CLOCKIOCB, IOPB_SPIN, K1_TO_IOPB_OFFSET(ic), 0))
	  cmn_err (CE_PANIC, "stopclocks: iop_poke failed\n");
	if (iop_wait (CLOCKIOCB, IOPB_SPIN, &status, 0))
	  cmn_err (CE_PANIC, "stopclocks: iop_wait failed\n");
	iop_clear (CLOCKIOCB);

	if (status)
	  cmn_err (CE_PANIC, "stopclocks: bad status\n");
}

ackrtclock()
{
	iop_clear (CLOCKIOCB);
}

int delay_mult = 6;	/* multiplier for DELAY macro */
#define	TEST_PERIOD	0x100000

config_delay()
{
	register IopClock *ic;
	IopClock *iop_alloc();
	register int s, cnt;
	int status;

	s = splhi();
	if ((ic = iop_alloc (CLOCKIOCB, sizeof (*ic))) == NULL)
	  cmn_err (CE_PANIC, "config_delay: iop_alloc failed\n");

	delay_mult = 1;

	ic->ic_cmd = CLOCK_START;
	ic->ic_intr = CLOCK_INTR_OFF;
	ic->ic_ticks = 0;
	if (iop_poke (CLOCKIOCB, IOPB_SPIN, K1_TO_IOPB_OFFSET(ic), 0) ||
	    iop_wait (CLOCKIOCB, IOPB_SPIN, &status, 0))
	  cmn_err (CE_PANIC, "config_delay: can't start command\n");
	iop_clear (CLOCKIOCB);

	DELAY(TEST_PERIOD);

	stopclocks();
	splx(s);

	cnt = ic->ic_ticks * 16667;
	/*
	 * TODO: This isn't right.  Our input clock only
	 * gives a 60Hz freq.  which is 16.6 ms.  This would
	 * give a real rough delay macro on ms timing.
	 */
	delay_mult = TEST_PERIOD / cnt;
	printf ("Delay multiplier = %d, cnt = %d\n",
		 delay_mult, ic->ic_ticks);
	if (delay_mult < 1 || delay_mult > 12)
		printf("config_delay: WARNING: unusual DELAY value\n");
	if (delay_mult <= 0)
		delay_mult = 3;
}


wtodc(newtime)
{
	register struct tod_cmblk *tod;
	int stat;
	
	if ((tod = (struct tod_cmblk *)iop_alloc (TODIOCB, sizeof (*tod))) == NULL)
	  cmn_err (CE_CONT, "wtodc: can't alloc iocb space\n");
	else {
		tod->command = TODWRITE;
		tod->time = newtime;
		if (iop_poke (TODIOCB, IOPB_SPIN, K1_TO_IOPB_OFFSET(tod), 0) ||
		    iop_wait (TODIOCB, IOPB_SPIN, &stat, 0) ||
		    stat != TOD_CMD_OK)
		  cmn_err (CE_CONT, "wtodc: can't set time-of-day\n");
		iop_clear (TODIOCB);
	}
}

rtodc()
{
	register struct tod_cmblk *tod;
	int stat;
	register long year_secs, year;

	if ( (tod = (struct tod_cmblk *)iop_alloc(TODIOCB,
				sizeof(struct tod_cmblk))) == NULL ) {
		printf("could not allocate tod_cmblk\n");
		return(0);
	}
	tod->command = TODREAD;
	if ((iop_poke (TODIOCB, IOPB_SPIN, K1_TO_IOPB_OFFSET(tod), 0) == 0) &&
	    (iop_wait (TODIOCB, IOPB_SPIN, &stat, 0) == 0) &&
	    (stat == TOD_CMD_OK)) {
		iop_clear (TODIOCB);
		year_secs = tod->time;
		real_tod_read = tod->time;
	} else {
		/* something went wrong, should probably do a showconfig
		 *	based message here.
		 */
		iop_clear (TODIOCB);
		return (0);
	}; /* if */


	/*
	 * Whittle the time down to an offset in the current year,
	 * by subtracting off whole years as long as possible.
	 */
	year = YRREF;
	for (;;) {
		register secyr = SECYR;
		if (LEAPYEAR(year))
			secyr += SECDAY;
		if (year_secs < secyr)
			break;
		year_secs -= secyr;
		year++;
	}
	return (year_secs+TODRZERO);
	
}


/* from here is general */

extern struct timeval time;
extern rtodc(), wtodc();		/* Lboot'able todc routines */
extern int showconfig;

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
	if ( base == -1 ) { /* indicates use tod read from tod chip */
		base = real_tod_read;
	}
	if ( base < 5*SECYR ) {
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
	
	time.tv_sec = (todr - TODRZERO);
	time.tv_usec = 0;
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
