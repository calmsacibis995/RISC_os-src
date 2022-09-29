#ident "$Header: timer.h,v 1.4 90/01/23 14:20:53 huang Exp $"
/*	%Q%	%I%	%M%	*/
/* $Copyright$ */

/*
 * Definitions for 8254 programmable interval timer
 *
 */
#ifndef FILL3
#define FILL3(x) char fill/**/x[3]
#endif

#ifdef LANGUAGE_C
struct counter_timer {
	unsigned char counter0;		/* counter 0 port */
	FILL3(0);
	unsigned char counter1;		/* counter 1 port */
	FILL3(1);
	unsigned char counter2;		/* counter 2 port */
	FILL3(2);
	unsigned char control;		/* control word */
};
#endif

#define NTIMERS		3

#define CT_COUNTER0	0x0	/* counter 0 port */
#define CT_COUNTER1	0x4	/* counter 1 port */
#define CT_COUNTER2	0x8	/* counter 2 port */
#define CT_CONTROL	0xc	/* control word */

/*
 * control word definitions
 */
#define	CTCW_SC(x)	((x)<<6)	/* select counter x */
#define	CTCW_RBCMD	(3<<6)		/* read-back command */
#define	CTCW_CLCMD	(0<<4)		/* counter latch command */
#define	CTCW_LSB	(1<<4)		/* r/w least signif. byte only */
#define	CTCW_MSB	(2<<4)		/* r/w most signif. byte only */
#define	CTCW_16B	(3<<4)		/* r/w 16 bits, lsb then msb */
#define	CTCW_MODE(x)	((x)<<1)	/* set mode to x */
#define	CTCW_BCD	0x1		/* operate in BCD mode */
/*
 * Readback Command Definitions
 */
#define CTCW_STATUS_INHIBIT	0x20		/* latch status byte if 0 */
#define CTCW_COUNT_INHIBIT	0x10		/* latch counter if 0 */
#define CTCW_RBSC( n )		(1 << (n) + 1)	/* read back counter select */
#define READBACK( n )		CTCW_RBCMD | CTCW_RBSC( (n) ) 
#define REDBACK_ALL		CTCW_RBCMD | CTCW_RBSC( (0) ) | \
				CTCW_RBSC( (1) ) | CTCW_RBSC( (2) ) 

/*
 * Mode definitions
 */
#define	MODE_ITC	0		/* interrupt on terminal count */
#define	MODE_HROS	1		/* hw retriggerable one-shot */
#define	MODE_RG		2		/* rate generator */
#define	MODE_SQW	3		/* square wave generator */
#define	MODE_STS	4		/* software triggered strobe */
#define	MODE_HTS	5		/* hardware triggered strobe */

/*
 * Readback Status byte defintions
 */
#define RB_OUTPUT	0x80		/* Current state of out pin */
#define RB_NULL_COUNT	0x40		/* =0 if last write loaded */

#define	MASTER_FREQ	3686400		/* master frequency */

#define	MASTER_PERIOD	1024		/* sched and prof run at 3600 Hz */
#define	SCHED_PERIOD	60		/* 60 hertz for now */
#define	PROF_PERIOD	36		/* 100 hertz for now */
#define	KGHERTZ		100		/* must match PROF_PERIOD */
