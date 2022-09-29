#ident "$Header: i8254clock.h,v 1.6 90/01/23 14:13:48 huang Exp $"
/*	%Q%	%I%	%M%	*/
/* $Copyright$ */

/*
 * Definitions for 8254 programmable interval timer
 *
 * NOTE: counter2 is clocked at MASTER_FREQ (3.6864 MHz), the
 * output of counter2 is the clock for both counter0 and counter1.
 * Counter0 output is tied to Interrupt 2 to act as the scheduling
 * clock and the output of counter1 is tied to Interrupt 4 to act as
 * the profiling clock.
 */

#ifndef FILL3
#define FILL3(x)	char fill_/**/x[3]
#endif

#ifdef LANGUAGE_C
struct pt_clock {
	u_char	pt_counter0;		/* counter 0 port */
	FILL3(0);
	u_char	pt_counter1;		/* counter 1 port */
	FILL3(1);
	u_char	pt_counter2;		/* counter 2 port */
	FILL3(2);
	u_char	pt_control;		/* control word */
};
#endif

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
