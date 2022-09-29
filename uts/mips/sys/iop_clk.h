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
/* $Header: iop_clk.h,v 1.2.3.2 90/05/10 06:24:55 wje Exp $ */
/*
 * $Header: iop_clk.h,v 1.2.3.2 90/05/10 06:24:55 wje Exp $
 *
 * iop clock
 */
typedef struct iop_clock {
	u_long ic_ticks;	/* number of ticks since last start */
	u_char ic_cmd;
	u_char ic_intr;
	u_char ic_pad[2];
} IopClock;

/*
 * Commands
 */
#define CLOCK_START	1	/* start clock/counter */
#define CLOCK_STOP	2	/* stop clock/counter */
#define CLOCK_START_DBG 3	/* print time every ten seconds */

/*
 * Interrupt
 */
#define CLOCK_INTR_OFF	0	/* only update ic_ticks */
#define CLOCK_INTR_ON	1	/* enable interrupts with each tick */
/* __EOF__ */
