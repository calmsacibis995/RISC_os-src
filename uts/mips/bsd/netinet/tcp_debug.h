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
/* $Header: tcp_debug.h,v 1.7.3.2 90/05/10 04:29:59 wje Exp $ */

#ifndef	_BSD_NETINET_TCP_DEBUG_
#define	_BSD_NETINET_TCP_DEBUG_	1

/*
 * Copyright (c) 1982, 1986 Regents of the University of California.
 * All rights reserved.
 *
 * Redistribution and use in source and binary forms are permitted
 * provided that this notice is preserved and that due credit is given
 * to the University of California at Berkeley. The name of the University
 * may not be used to endorse or promote products derived from this
 * software without specific prior written permission. This software
 * is provided ``as is'' without express or implied warranty.
 *
 *	@(#)tcp_debug.h	7.2 (Berkeley) 12/7/87
 */

struct	tcp_debug {
	n_time	td_time;
	short	td_act;
	short	td_ostate;
	caddr_t	td_tcb;
	struct	tcpiphdr td_ti;
	short	td_req;
	struct	tcpcb td_cb;
};

#define	TA_INPUT 	0
#define	TA_OUTPUT	1
#define	TA_USER		2
#define	TA_RESPOND	3
#define	TA_DROP		4

#ifdef TANAMES
char	*tanames[] =
    { "input", "output", "user", "respond", "drop" };
#endif

#define	TCP_NDEBUG 100
struct	tcp_debug tcp_debug[TCP_NDEBUG];
int	tcp_debx;

#endif	_BSD_NETINET_TCP_DEBUG_
