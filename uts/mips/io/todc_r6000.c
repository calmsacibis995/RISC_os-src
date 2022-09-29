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
#ident	"$Header: todc_r6000.c,v 1.3.1.11.1.4.1.2 90/12/20 19:28:58 beacker Exp $"

/*	Copyright (c) 1984 AT&T	*/
/*	  All Rights Reserved  	*/

/*	THIS IS UNPUBLISHED PROPRIETARY SOURCE CODE OF AT&T	*/
/*	The copyright notice above does not evidence any   	*/
/*	actual or intended publication of such source code.	*/


#include "sys/sbd.h"
#include "sys/cpu_board.h"
#include "sys/types.h"
#include "sys/param.h"
#include "sys/clock.h"
#include "bsd/sys/time.h"
#include "sys/cmn_err.h"
#include "sys/bc.h"
#include "sys/ctlspace.h"

#define KILO	1000
#define MEGA	(1000*KILO)

extern clock();
extern struct timeval time;
extern int (*ctl_vec_tbl[])();
u_int cpu_hz = 60*MEGA;	/* init to satisfy config_delay() */
u_int cpu_hz_tick = 60*MEGA/HZ;	/* init to satisfy config_delay() */
static u_int cpu_hz_tick_error = 0;	/* extra cpu cycles per tick */
static u_int cpu_hz_error = 0;		/* total accumulated error cycles */
static u_int last_cpu_hz_displayed;

#define CLOCK_COUNT	*(volatile uint *)CSR_COUNT
#define CLOCK_COMPARE	*(volatile uint *)CSR_COMPARE

#ifdef SABLE
int delay_mult = 0;	/* multiplier for DELAY macro, with initial value */
#else
int delay_mult = 30;	/* multiplier for DELAY macro, with initial value */
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

#ifdef DEBUG
int rtclock_roll;	/* Count number of late clock ticks */
#endif DEBUG

/*
 *  Display the clock rate
 */
print_cpu_hz()
{
	u_int print_hz;
	u_int mhz, khz100, khz10;

	last_cpu_hz_displayed = cpu_hz;
	print_hz = cpu_hz + 5000;		/* round up for display */
	mhz    = print_hz / MEGA;
	khz100 = (print_hz - (mhz * MEGA)		       ) / (100*KILO);
	khz10  = (print_hz - (mhz * MEGA) - (khz100 * 100*KILO)) /  (10*KILO);
#ifdef MIPS_LOCAL
	cmn_err(CE_CONT,"[ CPU clock        = %d.%d%d-MHz ]\n"
		,mhz, khz100, khz10 );
#else
	cmn_err(CE_CONT,"![ CPU clock        = %d.%d%d-MHz ]\n"
		,mhz, khz100, khz10 );
#endif MIPS_LOCAL
}

/*
 * Start the real-time clock (really the programable interval clock!).
 */
#define SPIN_TIME	10		/* seconds */
#define MIN_CPU_HZ	30*MEGA		/* in case TOD clock doesn't work */
#define MAX_CPU_HZ	150*MEGA	/* in case TOD clock doesn't work */
startrtclock()
{
	register s;
	uint	tod, old_tod, base_count;

	/*
	 *  First, use the Time-of-Day chip to calibrate our CPU clock rate.
	 *  (Because of a Sable feature, we must take care not to reset
	 *  CSR_COUNT while we're spinning reading the TODC.)
	 */
	s = splall();
	CLOCK_COUNT = 0;			/* zero the cycle count */
#ifdef SABLE
	cpu_hz = 30*MEGA;			/* too long to compute */
#else
	old_tod = rtodc();			/* time now, in seconds */
	if (old_tod == 0) {
	    /*
	     *  NVRAM says time isn't Valid, but we need a nonzero 'seconds'
	     *  time here.  So use inittodr() to set the TOD chip to some
	     *  bogus value, just to get the NVRAM Valid bit turned on.
	     */
	     inittodr(0);
	     old_tod = rtodc();			/* read it again */
	}
	if (old_tod) {
		while ((tod = rtodc()) == old_tod) {} /* spin until next sec */
	}
	base_count = CLOCK_COUNT;
	tod	  += SPIN_TIME;
	/*  spin again for N seconds, or until we've chewed up so many  */
	/*  CPU cycles that we can assume the TOD chip doesn't work     */
	while (tod != rtodc()  && 
	       (CLOCK_COUNT-base_count) < MAX_CPU_HZ*SPIN_TIME ) {}
	cpu_hz = (CLOCK_COUNT - base_count)/SPIN_TIME;
#endif !SABLE
	splx( s );

	/*
	 *  Does our TOD chip work?
	 */
	if (cpu_hz < MIN_CPU_HZ  ||  cpu_hz > MAX_CPU_HZ) {
	    cpu_hz = 60*MEGA;		/* invent a value */
	    cmn_err(CE_WARN, "improperly functioning TOD chip");
	}

	cpu_hz_tick = cpu_hz/HZ;
	cpu_hz_tick_error = cpu_hz - HZ*cpu_hz_tick;
	/*
	 *  Recompute delay_mult, now that we have an accurate CPU clock rate.
	 */
	config_delay();

	print_cpu_hz();				/* display the value */
	remote_bp_encountered = 0;		/* initialize */

	s = splall();
	/*
	 *  The R6000 clock ticks every cycle, and interrupts whenever the
	 *  COUNT value equals the COMPARE value.
	 */
	CLOCK_COMPARE = CLOCK_COUNT + cpu_hz_tick;
	*(volatile uint *)CSR_IVECTCLR   = CSR_IVECTSET_TIMER;
	*(volatile uint *)CSR_IVECTMASK |= CSR_IVECTSET_TIMER;

	/*
	 *  Enable the real clock interrupt handler.
	 */
	ctl_vec_tbl[ ffs(CSR_IVECTSET_TIMER) - 1 ] = clock;
	splx( s );
}


/*
 * Disable clocks 
 */
stopclocks()
{
	*(volatile uint *)CSR_IVECTMASK &= ~CSR_IVECTSET_TIMER;
	*(volatile uint *)CSR_IVECTCLR   =  CSR_IVECTSET_TIMER;
}


/*
 * Acknowledge the clock interrupt
 */
ackrtclock()
{
	register	s;
	unsigned long	new_timer_compare,
			current_timer_count;

	s = splall();

#ifdef DEBUG
	/*
	 *  Complain a bit if the Clock interrupt bit is still on.  The general
	 *  interrupt handler would have turned this bit off, so it should only
	 *  be on if there has been a delay between turning it off and getting
	 *  to here (e.g., because of a breakpoint) long enough to allow the
	 *  Count value to wraparound again to equal the Compare register.
	 *  In this case the human debugger can ignore the warning message.
	 *
	 *  However, if we have some kind of hardware problem where the SBC bit
	 *  doesn't get turned off....
	 */
	if (*(volatile u_int *)CSR_IVECTSET & CSR_IVECTSET_TIMER) {
	    cmn_err(CE_WARN,"ackrtclock() sees Clock interrupt bit still set!");
	} 
#endif DEBUG
	/*
	 *  Bump the Compare register by a fixed value.
	 */
	new_timer_compare   = CLOCK_COMPARE + cpu_hz_tick;
	if ((cpu_hz_error += cpu_hz_tick_error) > HZ) {
	  new_timer_compare++;
	  cpu_hz_error -= HZ;
	}
	CLOCK_COMPARE	    = new_timer_compare;
	current_timer_count = CLOCK_COUNT;

	/*
	 *  It is possible that timer interrupts have been held off so long
	 *  that the Count has moved beyond the new Compare.  In that case
	 *  we must turn the external timer interrupt back on again.  
	 *  (We could just avoid clearing the interrupt in this case, but I
	 *  think there is a timing window which this algorithm avoids.)
	 *  We need to take Count rollovers into account, so we must presume
	 *  that interrupts won't be held off longer than 30 bits of Count.
	 *
	 *  Since we're used unsigned arithmetic, the only time we shouldn't
	 *  have a pending timer tick is when (compare - count) <= one tick.
	 */

	if ((new_timer_compare - current_timer_count) > cpu_hz_tick) {
	  *(volatile uint *)CSR_IVECTSET = CSR_IVECTSET_TIMER;
#ifdef DEBUG		
	  rtclock_roll++;
#endif DEBUG		
	      }
	{
	extern uint cp0_error_imask;
	if (cp0_error_imask)
		report_soft_parity();
	}
	splx( s );
}

/*
 * This routine may be invoked in order to check for missing real-time
 * clock interrupts.  Normally this shouldn't happen, however SBC has a
 * bug which may cause loss of timer interrupt when clearing another
 * interrupt condition.
 */

rtclock_interrupt_check(check_now)
int check_now;
{
  int s;
  unsigned long timer_compare, timer_count;

  s = splall();
  timer_compare = CLOCK_COMPARE;
  timer_count = CLOCK_COUNT;

  if (((timer_compare - timer_count) <= cpu_hz_tick)  ||
      (*(volatile uint *)CSR_IVECTSET & CSR_IVECTSET_TIMER)) {

    /* Either time has not expired or we have a pending clock interrupt */
    
    splx(s);
    return(0);
    
  } else {
    
    /* We should have a pending clock interrupt, but don't !!! */
    
    *(volatile uint *)CSR_IVECTSET = CSR_IVECTSET_TIMER;
    splx(s);
    
    cmn_err(CE_WARN,
	    "MISSING rtclock interrupt by 0x%x (cnt 0x%x  cmp 0x%x) now %d",
	    timer_count-timer_compare, timer_count, timer_compare, check_now );
    return(1);
  }
}

  
config_delay()
{
#define	DELAY_TEST_TIME 16384
	register s;
	unsigned long	start_time,
			end_time;
	unsigned long	have_good_value;

	s = splall();		/* disable interrupts */
	delay_mult = 1;		/* init test value */
	have_good_value = 0;	/* compute things at least once */
	while (!have_good_value) {
		start_time = CLOCK_COUNT;
		DELAY( DELAY_TEST_TIME );
		end_time = CLOCK_COUNT;
		if (end_time > start_time) {
			/* no timer count rollover -- ok to compute delay */
			delay_mult
			 = DELAY_TEST_TIME	/* supposedly N microsec */
			    / ((end_time - start_time)	/* CPU cycles */
			       /(cpu_hz/MEGA));	/* cycles/usec */
			have_good_value = 1;	/* all done */
			if (delay_mult < 25) {
#ifdef MIPS_LOCAL
			    cmn_err(CE_WARN,"!delay_mult %d too small, default instead to 25\n",delay_mult);
#endif
			    delay_mult = 25;
			}
		} /* if */
	} /* while */
#ifdef SABLE
	delay_mult = 0;		/* Sable is too slow already -- no delays! */
#endif SABLE
	splx(s);		/* restore interrupt state */
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
	unsigned long timer_count, timer_compare;
	unsigned long usecs_since_interrupt;
	int s;

	*tvp = time;
 	s = splhigh();
	timer_count   = CLOCK_COUNT;
	timer_compare = CLOCK_COMPARE;

	usecs_since_interrupt =
	    (timer_count - (timer_compare-cpu_hz_tick))  /  (cpu_hz/MEGA);

#ifdef DEBUG	
	if (usecs_since_interrupt > 2*MEGA/HZ)
	  cmn_err(CE_NOTE, "mtime: usecs_since_int is %d, cnt 0x%x cmp 0x%x",
		  usecs_since_interrupt, timer_count, timer_compare);
#endif DEBUG	
	
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
	register uint todr;
	long deltat;
	int year = YRREF;
	int checktodr();
	int verify_time();
	static char initialized = 0;

	if (!initialized) {
	    initialized = 1;

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
 *	the empirically determined CPU cycles/second value, and to adjust
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
	int cpu_hz_delta;
	uint display_cpu_hz_delta;
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
		 *  on this cpu_hz correction algorithm because the kernel's
		 *  notion of time-of-day is drastically different from the
		 *  time-of-day-chip's notion.  So don't try to adjust the
		 *  cpu_hz value.  Just let adjtime1() adjust things.
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
		cpu_hz_delta = -64*drift / (drift_count*VERIFY_TIME_PERIOD);
		/*
		 * To avoid problems with scheduling & interrupt "jitter"
		 * as well as to avoid other errors, we require that the
		 * computed delta have the same sign twice.  When this
		 * occurs, we take the smaller of the two deltas.
		 * This also helps us at startup time, where the first
		 * computed delta is a somewhat  arbitrary value.
		 */
#ifdef MIPS_LOCAL
		if (time_fix_debug) {
		  cmn_err(CE_CONT,
			  "verify_time:  hz_delta1 %d hz_delta2 %d  delta %d drift %d drift_count %d\n",
			  next_hz_delta, cpu_hz_delta, 
			  usec_delta, drift,
			  drift_count);
		}
#endif MIPS_LOCAL
		if ((next_hz_delta > 0) && (cpu_hz_delta >= next_hz_delta) ||
		    (next_hz_delta < 0) && (cpu_hz_delta <= next_hz_delta)) {
		  
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
			    "verify_time: adj cpu_hz %d by %d =%d delta %d drift %d drift_count %d\n",
			    cpu_hz, next_hz_delta,
			    cpu_hz+next_hz_delta, usec_delta, drift,
			    drift_count);
		  }
#endif MIPS_LOCAL
		  cpu_hz   += next_hz_delta;
		  cpu_hz_tick = cpu_hz/HZ;
		  cpu_hz_tick_error = cpu_hz - HZ*cpu_hz_tick; 
		  /* has cpu_hz drifted enough to justify a logfile message? */
		  if (cpu_hz >= last_cpu_hz_displayed) {
			display_cpu_hz_delta = cpu_hz - last_cpu_hz_displayed;
		  } else {
			display_cpu_hz_delta = last_cpu_hz_displayed - cpu_hz;
		  }
		  if (display_cpu_hz_delta > cpu_hz_tick/10) {
			cmn_err(CE_CONT,"!Adjust CPU Hz by %d to %d \n"
			  	,next_hz_delta, cpu_hz);
			display_cpu_hz_delta = cpu_hz;
		  }
		  next_hz_delta = 0;	/* Reset so don't adjust next time */
		} else
		  next_hz_delta = cpu_hz_delta;
		
		
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

	wait = MEGA - newtime.tv_usec;	 	/* Time to wait in usec */
	wait *= HZ;
	wait /= MEGA;				/* *(HZ/1000000) gives ticks */

	wtodc(newtime.tv_sec + 1);
	timeout_id = timeout(wtodc, newtime.tv_sec + 1, wait);
}
