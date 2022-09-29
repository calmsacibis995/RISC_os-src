#ident "$Header: pon_timers.c,v 1.7.3.1 90/07/18 14:33:32 huang Exp $"
/* $Copyright: |
 * |-----------------------------------------------------------|
 * | Copyright (c) 1988, 1990 MIPS Computer Systems, Inc.      |
 * | All Rights Reserved                                       |
 * |-----------------------------------------------------------|
 * |          Restrictive Rights Legend                        |
 * | Use, duplication, or disclosure by the Government is      |
 * | subject to restrictions as set forth in                   |
 * | subparagraph (c)(1)(ii) of the Rights in Technical        |
 * | Data and Computer Software Clause of DFARS 252.227-7013.  |
 * |         MIPS Computer Systems, Inc.                       |
 * |         928 Arques Avenue                                 |
 * |         Sunnyvale, CA 94086                               |
 * |-----------------------------------------------------------|
 *  $ */

/* ------------------------------------------------------------------ */
/* | Copyright Unpublished, MIPS Computer Systems, Inc.  All Rights | */
/* | Reserved.  This software contains proprietary and confidential | */
/* | information of MIPS and its suppliers.  Use, disclosure or     | */
/* | reproduction is prohibited without the prior express written   | */
/* | consent of MIPS.                                               | */
/* ------------------------------------------------------------------ */

#include "sys/types.h"
#include "machine/cpu.h"
#include "machine/cpu_board.h"
#include "machine/asm.h"
#include "machine/hd146818.h"
#include "machine/mk48t02.h"
#include "machine/i8254clock.h"
#include "machine/timer.h"
#include "pon.h"
#include "prom.h"

#define	DELAY		0x10000
#define	bcd_to_dec(x)	(((((x) >> 4) & 0xf) * 10) + ((x) & 0xf))

extern char success[], failure[], skipped[], crlf[];

/*
 * Start the real-time clock (really the programable interval clock!).
 *
 * For now assume that counter 0 is the master counter, counter 1 is the
 * scheduling counter, and counter 2 is the profiling counter.
 */
Pon_Timers()

{
	register volatile char *tim0ack;
	register volatile char *tim1ack;
	register volatile struct pt_clock *pt;
	register volatile struct todc_clock *tod = 0;
	register volatile struct rt_clock *rt = 0;
	register int secs;
	register int t0ticks;
	register int t1ticks;
	int cause;
	int delay;
	int diff;
	char bucket;
	u_char start_secs;
	u_char end_secs;
	u_char ledcode;
	u_int faultcode = 0;

	if (IS_R3030) {
		return(PASS);
	}
	else {
		pon_set_leds(PON_TIMER);
		pon_puts("8254 Timer Test...");
	}

	switch (machine_type) {
	case BRDTYPE_R2400:
	case BRDTYPE_M180:
		tim0ack = (char *)PHYS_TO_K1(TIM0_ACK_ADDR_R2400);
		tim1ack = (char *)PHYS_TO_K1(TIM1_ACK_ADDR_R2400);
		pt = (struct pt_clock *)PHYS_TO_K1(PT_CLOCK_ADDR_R2400);
		tod = (struct todc_clock *)PHYS_TO_K1(TODC_CLOCK_ADDR_R2400);
		break;

	case BRDTYPE_R2300:
	case BRDTYPE_R2600:
	case BRDTYPE_R2800:
		tim0ack = (char *)PHYS_TO_K1(TIM0_ACK_ADDR_R2300);
		tim1ack = (char *)PHYS_TO_K1(TIM1_ACK_ADDR_R2300);
		pt = (struct pt_clock *)PHYS_TO_K1(PT_CLOCK_ADDR_R2300);
		rt = (struct rt_clock *)PHYS_TO_K1(RT_CLOCK_ADDR_R2300);
		break;

	case BRDTYPE_R3200:
	case BRDTYPE_RB3125:
		tim0ack = (char *)PHYS_TO_K1(TIM0_ACK_ADDR_R3200);
		tim1ack = (char *)PHYS_TO_K1(TIM1_ACK_ADDR_R3200);
		pt = (struct pt_clock *)PHYS_TO_K1(PT_CLOCK_ADDR_R3200);
		tod = (struct todc_clock *)PHYS_TO_K1(TODC_CLOCK_ADDR_R3200);
		break;

	default:
		pon_puts(skipped);
		return(PASS);				/* don't know what's out there */
		break;
	}

	start_secs = (0x7f & (tod ? tod->todc_secs : rt->rt_secs));
							/* grab the TOD current seconds value */
	pt->pt_control = PTCW_SC(2)|PTCW_16B|PTCW_MODE(MODE_RG);
	FlushWB();
	pt->pt_counter2 = MASTER_PERIOD;
	FlushWB();
	pt->pt_counter2 = MASTER_PERIOD >> 8;
	FlushWB();

	pt->pt_control = PTCW_SC(0)|PTCW_16B|PTCW_MODE(MODE_RG);
	FlushWB();
	pt->pt_counter0 = SCHED_PERIOD;
	FlushWB();
	pt->pt_counter0 = SCHED_PERIOD >> 8;
	FlushWB();

	pt->pt_control = PTCW_SC(1)|PTCW_16B|PTCW_MODE(MODE_RG);
	FlushWB();
	pt->pt_counter1 = PROF_PERIOD;
	FlushWB();
	pt->pt_counter1 = PROF_PERIOD >> 8;
	FlushWB();

	bucket = *tim0ack;
	secs = 0;
	do {
		t0ticks = 0;
		do {
			delay = 0;
			do {
				cause = GetCause();
				delay++;
			} while (((cause & CAUSE_IP5) == 0) && (delay != DELAY));

			if (delay == DELAY) {
				pon_puts(failure);
				ledcode = PON_TIMER;
				faultcode = PON_FAULT_TIMER;
				goto failed;
			}

			bucket = *tim0ack;
			t0ticks++;
		} while (t0ticks != 60);

		secs++;
	} while (secs != 3);

	bucket = *tim1ack;
	secs = delay = 0;
	do {
		t1ticks = 0;
		do {
			delay = 0;
			do {
				cause = GetCause();
				delay++;
			} while (((cause & CAUSE_IP7) == 0) && (delay != DELAY));

			if (delay == DELAY) {
				pon_puts(failure);
				ledcode = PON_TIMER;
				faultcode = PON_FAULT_TIMER;
				goto failed;
			}

			bucket = *tim1ack;
			t1ticks++;
		} while (t1ticks != 100);

		secs++;
	} while (secs != 3);

	/*
	 * Disable the counters.
	 */
	pt->pt_control = PTCW_SC(2)|PTCW_16B|PTCW_MODE(MODE_STS);
	FlushWB();
	pt->pt_control = PTCW_SC(1)|PTCW_16B|PTCW_MODE(MODE_STS);
	FlushWB();
	pt->pt_control = PTCW_SC(0)|PTCW_16B|PTCW_MODE(MODE_STS);
	FlushWB();
	bucket = *tim0ack;		/* clear it one last time */
	FlushWB();
	bucket = *tim1ack;		/* clear it one last time */
	FlushWB();

	end_secs = (0x7f & (tod ? tod->todc_secs : rt->rt_secs));
	if (rt) {
		if (end_secs < start_secs) {
			end_secs += 60;
		}

		diff = end_secs - start_secs;
	}
	else {
		if (end_secs < start_secs) {
			end_secs += 0x60;
		}

		diff = bcd_to_dec(end_secs) - bcd_to_dec(start_secs);
	}

	pon_puts(success);

#ifdef	DEBUG
	pon_puts("diff is ");
	pon_puthex(diff);
	pon_puts(crlf);
#endif	DEBUG
	pon_puts("Time-of-Day Clock Test...");
	if ((diff < 5) || (diff > 7)) {	/* allow +- 1 second variance */
		pon_puts(failure);
		ledcode = PON_TOD;
		faultcode = PON_FAULT_TOD;
		goto failed;
	}
	else {
		pon_puts(success);
	}

	return(PASS);

failed:
	/*
	 * Disable the counters.
	 */
	pt->pt_control = PTCW_SC(2)|PTCW_16B|PTCW_MODE(MODE_STS);
	FlushWB();
	pt->pt_control = PTCW_SC(1)|PTCW_16B|PTCW_MODE(MODE_STS);
	FlushWB();
	pt->pt_control = PTCW_SC(0)|PTCW_16B|PTCW_MODE(MODE_STS);
	FlushWB();
	bucket = *tim0ack;		/* clear it one last time */
	FlushWB();
	bucket = *tim1ack;		/* clear it one last time */
	FlushWB();
	FastFlash(ledcode);
	pon_set_leds(ledcode);
	SetDepend(faultcode);
	return(FAIL);
}
