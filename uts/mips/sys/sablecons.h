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
/* $Header: sablecons.h,v 1.8.3.2 90/05/10 06:34:52 wje Exp $ */

#ifndef	_SYS_SABLECONS_
#define	_SYS_SABLECONS_	1



struct scons_device {
	volatile unsigned long	sc_status;	/* status register */
	volatile unsigned long	sc_command;	/* command register */
	volatile unsigned long	sc_rx;		/* receiver data register */
	volatile unsigned long	sc_tx;		/* transmitter data register */
	volatile unsigned long	sc_txbuf;	/* transmitter data buffer */
};

#define	SC_STAT_RXRDY	1		/* receiver has data available */
#define	SC_CMD_RXIE	1		/* receiver interrupt enable */
#define	SC_CMD_TXFLUSH	2		/* flush any buffered output */

#endif	_SYS_SABLECONS_
