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
#ident	"$Header: r2000_todc.c,v 1.2.4.2 90/05/10 05:25:34 wje Exp $"

/*	Copyright (c) 1984 AT&T	*/
/*	  All Rights Reserved  	*/

/*	THIS IS UNPUBLISHED PROPRIETARY SOURCE CODE OF AT&T	*/
/*	The copyright notice above does not evidence any   	*/
/*	actual or intended publication of such source code.	*/


#include "sys/sbd.h"
#include "sys/cpu_board.h"
#include "sys/types.h"
#include "sys/param.h"
/* #include "sys/i8254clock.h" */
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

	s = spl7();
	/*
	 *  The Sable clock ticks every ifetch, and here we specify a wakeup
	 *  frequency.
	 *  For example, if the native ifetch rate is 5-Mhz, then HZ==100 means
	 *  a clock interrupt every 50,000 emulated instructions.
	 */
	scp->sc_sched_hz = HZ;
	scp->sc_command |= SC_SCHED_START;
	splx(s);
}


/*
 * Disable clocks 
 */
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
	/*
	 * Sable is so slow that we don't want to spin on any delays.
	 */
	delay_mult = 0;
}

#else !SABLE

/*
 * Clock routines.
 */

/*
 * The todc chip has three counters.  We use #0 for scheduling and #1 for
 * kernel profiling.  Counter #2 is a master which drives the other two.
 * The master clock is decremented at 3686400 hz (1024*3600).  The other two
 * clocks are decremented whenever the master clock reaches 0.  An interupt
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
#define	MASTER_PERIOD	1024		/* sched and prof run at 3600 Hz */
#define	SCHED_PERIOD	(3600/HZ)	/* 36 */
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
	unsigned short rt_period = 3600/HZ;

	spl6();
	pt->pt_control = PTCW_SC(0)|PTCW_16B|PTCW_MODE(MODE_RG);
	pt->pt_counter0 = rt_period;
	wbflush();
	pt->pt_counter0 = rt_period>>8;

	pt->pt_control = PTCW_SC(2)|PTCW_16B|PTCW_MODE(MODE_RG);
	pt->pt_counter2 = MASTER_PERIOD;
	wbflush();
	pt->pt_counter2 = MASTER_PERIOD>>8;
	spl0();
}

#define	TEST_PERIOD	500

config_delay()
{
	register volatile struct pt_clock *pt = 
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
	if (delay_mult < 1 || delay_mult > 12)
		printf("config_delay: WARNING: unusual DELAY value\n");
	if (delay_mult <= 0)
		delay_mult = 3;
}

/*
 * Disable clocks 
 */
stopclocks()
{
	register volatile struct pt_clock *pt = 
		(struct pt_clock *)MACHDEP(PT_CLOCK_ADDR);

	pt->pt_control = PTCW_SC(0)|PTCW_16B|PTCW_MODE(MODE_HROS);
	pt->pt_counter0 = SCHED_PERIOD;
	wbflush();
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

	spl6();
	pt->pt_control = PTCW_SC(1)|PTCW_16B|PTCW_MODE(MODE_RG);
	pt->pt_counter1 = PROF_PERIOD;
	wbflush();
	pt->pt_counter1 = PROF_PERIOD>>8;

	pt->pt_control = PTCW_SC(2)|PTCW_16B|PTCW_MODE(MODE_RG);
	pt->pt_counter2 = MASTER_PERIOD;
	wbflush();
	pt->pt_counter2 = MASTER_PERIOD>>8;
	spl0();
}
#endif KGCLOCK

#endif !SABLE
