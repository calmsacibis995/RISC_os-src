#ident "$Header: pon_8254.c,v 1.2.7.1 90/07/18 14:29:05 huang Exp $"
/* $Copyright$ */

#include "machine/cpu.h"
#include "pon.h"
/*
 * Definitions for 8254 programmable interval timer
 *
 * NOTE: counter2 is clocked at MASTER_FREQ (3.6864 MHz), the
 * output of counter2 is the clock for both counter0 and counter1.
 * Counter0 output is tied to Interrupt 2 to act as the scheduling
 * clock and the output of counter1 is tied to Interrupt 4 to act as
 * the profiling clock.
 */

typedef unsigned char u_char;
#define FILL3(x) u_char fill/**/x[3]

struct pt_clock {
	u_char	pt_counter0;		/* counter 0 port */
	FILL3(0);
	u_char	pt_counter1;		/* counter 1 port */
	FILL3(1);
	u_char	pt_counter2;		/* counter 2 port */
	FILL3(2);
	u_char	pt_control;		/* control word */
};

/*
 * control word definitions
 */
#define	PTCW_SC(x)	((x)<<6)	/* select counter x */
#define	PTCW_RBCMD	(3<<6)		/* read-back command */
#define	PTCW_CLCMD	(0<<4)		/* counter latch command */
#define	PTCW_LSB	(1<<4)		/* r/w least signif. byte only */
#define	PTCW_MSB	(2<<4)		/* r/w most signif. byte only */
#define	PTCW_16B	(3<<4)		/* r/w 16 bits, lsb then msb */
#define	PTCW_MODE(x)	((x)<<1)	/* set mode to x */
#define	PTCW_BCD	0x1		/* operate in BCD mode */

/*
 * Mode definitions
 */
#define	MODE_ITC	0		/* interrupt on terminal count */
#define	MODE_HROS	1		/* hw retriggerable one-shot */
#define	MODE_RG		2		/* rate generator */
#define	MODE_SQW	3		/* square wave generator */
#define	MODE_STS	4		/* software triggered strobe */
#define	MODE_HTS	5		/* hardware triggered strobe */

#define	MASTER_FREQ	3686400		/* master frequency */

#define	MASTER_PERIOD	1024		/* sched and prof run at 3600 Hz */
#define	SCHED_PERIOD	60		/* 60 hertz for now */
#define	PROF_PERIOD	36		/* 100 hertz for now */
#define	KGHERTZ		100		/* must match PROF_PERIOD */

#define	PT_CLOCK_ADDR	(struct pt_clock *)PHYS_TO_K1(0x1e001003)
#define	TIM0_ACK_ADDR	(char *)PHYS_TO_K1(0x1e200003)
#define	TIM1_ACK_ADDR	(char *)PHYS_TO_K1(0x1e200007)
#define	NVRAM_DIAG_LOCA	(char *)PHYS_TO_K1(0x1e0100ff)
#define	NVRAM_BOOTMODE_LOCA	(char *)PHYS_TO_K1(0x1e0100f3)
#define	NVRAM_CURR_SECS	(char *)PHYS_TO_K1(0x1e010003)

/*
 * Start the real-time clock (really the programable interval clock!).
 *
 * For now assume that counter 0 is the master counter, counter 1 is
 * the scheduling counter, and counter 2 is the profiling counter.
 */
#define	DELAY	0x10000

Pon_8254()
{
	register volatile char *tim0ack = TIM0_ACK_ADDR;
	register volatile char *tim1ack = TIM1_ACK_ADDR;
	register volatile struct pt_clock *pt = PT_CLOCK_ADDR;
	register volatile char *nvram = NVRAM_DIAG_LOCA;
	register volatile char *csecs = NVRAM_CURR_SECS;
	register volatile char *bootmode = NVRAM_BOOTMODE_LOCA;
	register int secs, t0ticks, t1ticks;
	int cause, delaycnt, diff;
	char buf[32], bucket;
	extern pon_set_leds(), GetCause(), FlushWB();
	unsigned char start_secs, end_secs;
/*	char *pass = "8254 Timer test was SUCCESSFUL...\n\r";	*/
	char *fail = "8254 Timer test FAILED...\n\r";
	char *todfail = "TOD current seconds test FAILED...\n\r";
/*	char *todpass = "TOD current seconds test was SUCCESSFUL...\n\r"; */

	pon_set_leds(PON_TIMER_START);
	start_secs = *csecs;	/* grab the TOD current seconds value */
	pt->pt_control = PTCW_SC(2)|PTCW_16B|PTCW_MODE(MODE_RG);
	FlushWB();
	pt->pt_counter2 = MASTER_PERIOD;
	FlushWB();
	pt->pt_counter2 = MASTER_PERIOD>>8;
	FlushWB();

	pt->pt_control = PTCW_SC(0)|PTCW_16B|PTCW_MODE(MODE_RG);
	FlushWB();
	pt->pt_counter0 = SCHED_PERIOD;
	FlushWB();
	pt->pt_counter0 = SCHED_PERIOD>>8;
	FlushWB();

	pt->pt_control = PTCW_SC(1)|PTCW_16B|PTCW_MODE(MODE_RG);
	FlushWB();
	pt->pt_counter1 = PROF_PERIOD;
	FlushWB();
	pt->pt_counter1 = PROF_PERIOD>>8;
	FlushWB();

	bucket = *tim0ack;
	secs = 0;
	do {
		t0ticks = 0;
		do {
			delaycnt = 0;
			do {
				cause = GetCause();
				delaycnt++;
			} while ((cause & CAUSE_IP5) == 0 && delaycnt != DELAY);
			if (delaycnt == DELAY) {
				pon_puts(fail);
				FastFlash(TIMER_FAIL);
				pon_set_leds(TIMER_FAIL);
				if ( !*nvram ) *nvram = TIMER_FAIL;
				if ( *bootmode != CHAR_d )
					*bootmode = CHAR_e;
				return 1;
			}
			bucket = *tim0ack;
			t0ticks++;
		} while (t0ticks != 60);
		secs++;
	} while (secs != 3);

	bucket = *tim1ack;
	secs = delaycnt = 0;
	do {
		t1ticks = 0;
		do {
			delaycnt = 0;
			do {
				cause = GetCause();
				delaycnt++;
			} while ((cause & CAUSE_IP7) == 0 && delaycnt != DELAY);
			if (delaycnt == DELAY) {
				pon_puts(fail);
				FastFlash(TIMER_FAIL);
				pon_set_leds(TIMER_FAIL);
				if ( !*nvram ) *nvram = TIMER_FAIL;
				if ( *bootmode != CHAR_d )
					*bootmode = CHAR_e;
				return 1;
			}
			bucket = *tim1ack;
			t1ticks++;
		} while (t1ticks != 100);
		secs++;
	} while (secs != 3);
	/* disable the counters */
	pt->pt_control = PTCW_SC(2)|PTCW_16B|PTCW_MODE(MODE_STS);
	pt->pt_control = PTCW_SC(1)|PTCW_16B|PTCW_MODE(MODE_STS);
	pt->pt_control = PTCW_SC(0)|PTCW_16B|PTCW_MODE(MODE_STS);
	bucket = *tim0ack;	/* clear it one last time  */
	bucket = *tim1ack;	/* clear it one last time  */
	end_secs = *csecs;
	if ( end_secs < start_secs ) end_secs += 60;
	diff = end_secs - start_secs;
	if ( (diff < 5) || (diff > 7) ) { /* allow +- 1 second variance */
		pon_puts(todfail);
		FastFlash(TOD_FAIL);
		pon_set_leds(TOD_FAIL);
		if ( !*nvram ) *nvram = TOD_FAIL; /* if no error already */ 
		if ( *bootmode != CHAR_d )
			*bootmode = CHAR_e;
		return 1;
	} /* else pon_puts(todpass);	*/
/*	pon_puts(pass);	*/
	return 0;
}
