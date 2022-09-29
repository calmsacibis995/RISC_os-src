#ident "$Header: pon_timers_3030.c,v 1.2.1.1 90/07/18 14:33:34 huang Exp $"
/*	%Q%	%I%	%M%	*/
/* $Copyright: |
 * |-----------------------------------------------------------|
 * | Copyright (c) 1990       MIPS Computer Systems, Inc.      |
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

#include "sys/types.h"
#include "machine/cpu.h"
#include "machine/cpu_board.h"
#include "machine/mk48t02.h"
#include "machine/timer.h"
#include "prom/prom.h"
#include "pon.h"

#define	LO_MHZ			(18000000 / 4)	/* 20 MHz - 10% RAMBO clock */
#define	HI_MHZ			(33000000 / 4)	/* 30 MHz + 10% RAMBO clock */

#define	INTERRUPT_HZ		100
#define	TIME			1		/* duration of test in seconds */

#define	SECOND_DELAY		400000
#define	bcd_to_dec(x)		(((((x) >> 4) & 0xf) * 10) + ((x) & 0xf))

#define	increment		end
#define	ticks			secs

extern char success[], failure[], crlf[];

u_int rambo_clock_hz;


Pon_Timers()

{
	register volatile struct cnt_timer *timer = (struct cnt_timer *)PHYS_TO_K1(TIMER_BASE);
	register volatile struct todc_clock *tod = (struct todc_clock *)PHYS_TO_K1(TODC_CLOCK_ADDR_R3030);
	register u_int i;
	register u_int secs;
	register u_int start;
	register u_int end;
	u_int faultcode;

	pon_puts("Timer/Time-of-Day Clock Test...");

#ifdef	DEBUG
	pon_puthex(tod->todc_control);
	pon_puts(crlf);
	pon_puthex(tod->todc_secs);
	pon_puts(crlf);
	pon_puthex(tod->todc_mins);
	pon_puts(crlf);
	pon_puthex(tod->todc_hours);
	pon_puts(crlf);
	pon_puthex(tod->todc_dayw);
	pon_puts(crlf);
	pon_puthex(tod->todc_daym);
	pon_puts(crlf);
	pon_puthex(tod->todc_month);
	pon_puts(crlf);
	pon_puthex(tod->todc_year);
	pon_puts(crlf);
#endif	DEBUG

	tod->todc_control = 0;			/* kernel may leave the read bit set */

	/*
	 * Check the timer frequency using the TOD seconds as a reference.
	 */
	rambo_clock_hz = 0;
	timer->tbreak = 0x7fffffff;
	timer->tcount = 0;
	i = SECOND_DELAY;
	secs = tod->todc_secs;
	while ((secs == tod->todc_secs) && (--i != 0)) {
		;				/* wait for seconds to change */
	}

	start = timer->tcount;
	if (i == 0) {
		faultcode = PON_FAULT_TOD;
#ifdef	DEBUG
		pon_puts("Timed out in TOD 1\r\n");
#endif	DEBUG
		goto failed;
	}

	i = SECOND_DELAY;
	secs = tod->todc_secs;
	while ((secs == tod->todc_secs) && (--i != 0)) {
		;				/* wait for seconds to change */
	}

	end = timer->tcount;
	if (i == 0) {
		faultcode = PON_FAULT_TOD;
#ifdef	DEBUG
		pon_puts("Timed out in TOD 2\r\n");
#endif	DEBUG
		goto failed;
	}

#ifdef	DEBUG
	printf("i %d\n", i);
#endif	DEBUG

	i = end - start;

#ifdef	DEBUG
	printf("diff %d\n", i);
#endif	DEBUG

	/*
	 * Computed frequency should fall between the low and high frequency range.
	 */
	if ((i < LO_MHZ) || (i > HI_MHZ)) {
		faultcode = PON_FAULT_TIMER;
#ifdef	DEBUG
		pon_puts("MHz out of range ");
		pon_puthex(i);
		pon_puts(crlf);
#endif	DEBUG
		goto failed;
	}

	/*
	 * Check the timer interrupts for TIME second.
	 */
	rambo_clock_hz = i;
	increment = rambo_clock_hz / INTERRUPT_HZ;

	Pon_Delay(1000);

	start = 0x7f & tod->todc_secs;
	ticks = 0;
	timer->tbreak = increment;
	timer->tcount = 0;
	do {
		i = HI_MHZ / INTERRUPT_HZ;
		do {
			;
		} while (((GetCause() & CAUSE_IP5) == 0) && (--i != 0));

		if (i == 0) {
#ifdef	DEBUG
			pon_puts("Timed out waiting for interrupt\r\n");
#endif	DEBUG
			faultcode = PON_FAULT_TIMER;
			goto failed;
		}

		timer->tbreak = timer->tbreak + increment;
	} while (++ticks != INTERRUPT_HZ * TIME);

	end = 0x7f & tod->todc_secs;
	if (end < start) {
		end += 0x60;
	}

#ifdef	DEBUG
	printf("hz i %d\n", i);
#endif	DEBUG

	i = bcd_to_dec(end) - bcd_to_dec(start);
	if (i != TIME) {
#ifdef	DEBUG
		pon_puts("Computed seconds ");
		pon_puthex(i);
		pon_puts(crlf);
#endif	DEBUG
		faultcode = PON_FAULT_TOD;
		goto failed;
	}

#ifdef	DEBUG
	printf("ticks %d, diff %d\n", ticks, i);
#endif	DEBUG

	pon_puts(success);
	timer->tbreak = 0x7fffffff;
	return(PASS);

failed:
	pon_puts(failure);
	FastFlash();
	SetDepend(faultcode);
	timer->tbreak = 0x7fffffff;
	return(FAIL);
}
