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
/* $Header: cron.h,v 1.5.2.2 90/05/09 15:31:22 wje Exp $ */

/*	Copyright (c) 1984 AT&T	*/
/*	  All Rights Reserved  	*/

/*	THIS IS UNPUBLISHED PROPRIETARY SOURCE CODE OF AT&T	*/
/*	The copyright notice above does not evidence any   	*/
/*	actual or intended publication of such source code.	*/


#define FALSE		0
#define TRUE		1
#define MINUTE		60L
#define HOUR		60L*60L
#define DAY		24L*60L*60L
#define	NQUEUE		26		/* number of queues available */
#define	ATEVENT		0
#define BATCHEVENT	1
#define CRONEVENT	2

#define ADD		'a'
#define DELETE		'd'
#define	AT		'a'
#define CRON		'c'

#define	QUE(x)		('a'+(x))
#define RCODE(x)	(((x)>>8)&0377)
#define TSTAT(x)	((x)&0377)

#define	FLEN	15
#define	LLEN	9

/* structure used for passing messages from the
   at and crontab commands to the cron			*/

struct	message {
	char	etype;
	char	action;
	char	fname[FLEN];
	char	logname[LLEN];
} msgbuf;

/* anything below here can be changed */

#define CRONDIR		"/usr/spool/cron/crontabs"
#define ATDIR		"/usr/spool/cron/atjobs"
#define ACCTFILE	"/usr/lib/cron/log"
#define CRONALLOW	"/usr/lib/cron/cron.allow"
#define CRONDENY	"/usr/lib/cron/cron.deny"
#define ATALLOW		"/usr/lib/cron/at.allow"
#define ATDENY		"/usr/lib/cron/at.deny"
#define PROTO		"/usr/lib/cron/.proto"
#define	QUEDEFS		"/usr/lib/cron/queuedefs"
#define	FIFO		"/usr/lib/cron/FIFO"

#define SHELL		"/bin/sh"	/* shell to execute */

#define CTLINESIZE	1000	/* max chars in a crontab line */
#define UNAMESIZE	20	/* max chars in a user name */
