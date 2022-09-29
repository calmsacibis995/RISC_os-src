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
/* $Header: resolv.h,v 1.4.3.2 90/05/09 19:47:15 wje Exp $ */

#ifndef	_BSD_ARPA_RESOLV_
#define	_BSD_ARPA_RESOLV_	1


/*
 * Copyright (c) 1983 Regents of the University of California.
 * All rights reserved.  The Berkeley software License Agreement
 * specifies the terms and conditions for redistribution.
 *
 *	@(#)resolv.h	5.2 (Berkeley) 7/31/85
 */

/*
 * Global defines and variables for resolver stub.
 */


struct state {
	int	retrans;		/* retransmition time interval */
	int	retry;			/* number of times to retransmit */
	int	options;		/* option flags - see below. */
	struct	sockaddr_in nsaddr;	/* address of name server */
	u_short	id;			/* current packet id */
	char	defdname[MAXDNAME];	/* default domain */
};

/*
 * Resolver options
 */
#define RES_INIT	0x001		/* address initialized */
#define RES_DEBUG	0x002		/* print debug messages */
#define RES_AAONLY	0x004		/* authoritative answers only */
#define RES_USEVC	0x008		/* use virtual circuit */
#define RES_PRIMARY	0x010		/* query primary server only */
#define RES_IGNTC	0x020		/* ignore trucation errors */
#define RES_RECURSE	0x040		/* recursion desired */
#define RES_DEFNAMES	0x080		/* use default domain name */

extern struct state _res;
extern char *p_cdname(), *p_rr(), *p_type(), *p_class();

#endif	_BSD_ARPA_RESOLV_
