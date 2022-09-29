#ident "$Header: pon_timers_6000.c,v 1.4.6.1 90/07/18 14:33:39 huang Exp $"
/* $Copyright$ */

#include "sys/types.h"
#include "machine/cpu.h"
#include "machine/cpu_board.h"
#include "machine/asm.h"
#include "machine/mk48t02.h"
#include "machine/timer.h"
#include "machine/bc.h"
#include "pon.h"
#include "prom.h"

#define	DELAY		0x10000
#define bcd_to_dec(x)	(((((x) >> 4) & 0xf) * 10) + ((x) & 0xf))


extern char success[], failure[];

/*
 * Start the real-time clock (really the programable interval clock!).
 *
 * For now assume that counter 0 is the master counter, counter 1 is the
 * scheduling counter, and counter 2 is the profiling counter.
 */
Pon_Timers()
{
	u_int start_count, target_count;
	u_int delay;
	u_int diff;
	u_int start_secs, end_secs;
	u_char ledcode;
	u_int faultcode = 0;

	pon_set_leds(PON_TIMER);

	pon_puts("Count/Compare Timer Test...");

	/*
	 *  Set the Compare register to 100 million cycles, and reset
	 *  the Count register and the InterruptVector to zero.
	 *  Loop for 200 million cycles, or until the InterruptVector timer
	 *  bit goes on, and verify that both conditions are true.
	 *  We choose a nontrivial cycle count to flush out cases where the
	 *  InterruptVector bit goes on for some other bizarre reason.
	 */
#ifdef SABLE
	target_count =   100000;
#else
	target_count = 200*1000*1000;		/* make it easier to read */
#endif !SABLE
	*(volatile u_int *)CSR_COMPARE = target_count;
	*(volatile u_int *)CSR_COUNT = 0;	/* reset COUNT register */
	*(volatile u_int *)CSR_IVECTCLR = 1;	/* clear timer int bit */
	delay = 0;
	while (     (*(volatile u_int *)CSR_COUNT < target_count)
		&& ((*(volatile u_int *)CSR_IVECTSET & 1) == 0) ) {
		delay++;			/* do something */
	}

	/*
	 *  We've either spun for the minimum number of cycles, or else
	 *  the timer interrupt bit has turned on.  If both haven't
	 *  happened, then we have an error.
	 */
	if (*(volatile u_int *)CSR_COUNT < target_count) {
		pon_puts("spurious timer interrupt!...");
		goto failed;
	}
	if ((*(volatile u_int *)CSR_IVECTSET & 1) == 0) {
		pon_puts("no timer interrupt!...");
		goto failed;
	}
	pon_puts(success);

	pon_puts("Time-of-Day Clock Test...");

#ifndef SABLE
	/*
	 *  First, spin until the TOD Chip time rolls over to a new second.
	 *  Be careful:  maybe the TOD isn't ticking at all!
	 */
	start_secs = get_tod_todc();		/* TOD current seconds value */
	*(volatile u_int *)CSR_COUNT = 0;	/* reset SBC COUNT register */
	delay = 0;
	target_count = 100 * 1000*1000;		/* 1 seconds at 100 MHz */
	while (*(volatile u_int *)CSR_COUNT < target_count	&&
	       start_secs == get_tod_todc()) {
		delay++;			/* do something */
	}
	if (*(volatile u_int *)CSR_COUNT >= target_count) {
		pon_puts("TOD chip not incrementing!...");
		goto failed;
	}
#endif !SABLE
	/*
	 *  Now we're on a new seconds value.  Spin for roughly 10 seconds
	 *  of CPU cycles, and verify that the TOD time has ticked
	 *  approximately that much.
	 */
	start_secs++;				/* TOD current seconds value */
	*(volatile u_int *)CSR_COUNT = 0;	/* reset SBC COUNT register */
#ifdef SABLE
	target_count = 10;			/* something small */
#else
	target_count = 10 * 60*1000*1000;	/* 10 seconds at 60 MHz */
#endif !SABLE
	delay = 0;
	while (*(volatile u_int *)CSR_COUNT < target_count) {
		delay++;			/* do something */
	}
	end_secs = get_tod_todc();
#ifdef SABLE
	diff = 10;
#else
	diff = end_secs - start_secs;
#endif !SABLE

	if ((diff >= 8) && (diff <= 12)) {/* allow +- 20% second variance */
		pon_puts(success);
		return(PASS);
	} else {
		pon_puts("diff=0x");
		pon_puthex(diff);
		pon_puts(", not =0xA...");
	}

failed:	
		pon_puts(failure);
		ledcode = PON_TOD;
		faultcode = PON_FAULT_TOD;
		pon_set_leds(ledcode);
		SetDepend(faultcode);
		return(FAIL);
}
