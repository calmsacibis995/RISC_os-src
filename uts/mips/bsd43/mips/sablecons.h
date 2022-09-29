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
/* $Header: sablecons.h,v 1.6.3.2 90/05/10 04:43:20 wje Exp $ */
#ifndef BSD43_
#    include <bsd43/bsd43_.h>
#endif BSD43_


struct bsd43_(scons_device) {
	unsigned long	sc_status;	/* status register */
	unsigned long	sc_command;	/* command register */
	unsigned long	sc_rx;		/* receiver data register */
	unsigned long	sc_tx;		/* transmitter data register */
	unsigned long	sc_txbuf;	/* transmitter data buffer */
};

#define	BSD43_SC_STAT_RXRDY	1		/* reciever has data available */
#define	BSD43_SC_CMD_RXIE	1		/* reciever interrupt enable */
#define	BSD43_SC_CMD_TXFLUSH	2		/* flush any buffered output */

#define BSD43_SCONS0_BASE ((struct bsd43_(scons_device) *)(0x1f000000+BSD43_K1BASE))
#define BSD43_SCONS1_BASE ((struct bsd43_(scons_device) *)(0x1f000014+BSD43_K1BASE))

/*----- NUMIPS: STRIP MACRO PREFIXES (BEGIN) ---------------------------------*/
/* 
 * Strip off "BSD43_" and "bsd43_" for use with standard BSD code.
 * (GENERATED AUTOMATICALLY -- DON'T EDIT BY HAND)
 */
#ifdef SYSTYPE_BSD43
#   define SCONS0_BASE BSD43_SCONS0_BASE
#   define SCONS1_BASE BSD43_SCONS1_BASE
#   define SC_CMD_RXIE BSD43_SC_CMD_RXIE
#   define SC_CMD_TXFLUSH BSD43_SC_CMD_TXFLUSH
#   define SC_STAT_RXRDY BSD43_SC_STAT_RXRDY
#endif SYSTYPE_BSD43
/*----- NUMIPS: STRIP MACRO PREFIXES (END) -----------------------------------*/


