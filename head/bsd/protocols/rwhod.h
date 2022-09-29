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
/* $Header: rwhod.h,v 1.4.3.2 90/05/09 19:48:40 wje Exp $ */

#ifndef	_BSD_PROTOCOLS_RWHOD_
#define	_BSD_PROTOCOLS_RWHOD_	1


/*
 * Copyright (c) 1983 Regents of the University of California.
 * All rights reserved.  The Berkeley software License Agreement
 * specifies the terms and conditions for redistribution.
 *
 *	@(#)rwhod.h	5.1 (Berkeley) 5/28/85
 */

/*
 * rwho protocol packet format.
 */
struct	outmp {
	char	out_line[8];		/* tty name */
	char	out_name[8];		/* user id */
	long	out_time;		/* time on */
};

struct	whod {
	char	wd_vers;		/* protocol version # */
	char	wd_type;		/* packet type, see below */
	char	wd_pad[2];
	int	wd_sendtime;		/* time stamp by sender */
	int	wd_recvtime;		/* time stamp applied by receiver */
	char	wd_hostname[32];	/* hosts's name */
	int	wd_loadav[3];		/* load average as in uptime */
	int	wd_boottime;		/* time system booted */
	struct	whoent {
		struct	outmp we_utmp;	/* active tty info */
		int	we_idle;	/* tty idle time */
	} wd_we[1024 / sizeof (struct whoent)];
};

#define	WHODVERSION	1
#define	WHODTYPE_STATUS	1		/* host status */

#endif	_BSD_PROTOCOLS_RWHOD_
