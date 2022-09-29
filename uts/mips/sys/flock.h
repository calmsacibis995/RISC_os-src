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
/* $Header: flock.h,v 1.7.1.2 90/05/10 06:14:12 wje Exp $ */

#ifndef	_SYS_FLOCK_
#define	_SYS_FLOCK_	1

/*	Copyright (c) 1984 AT&T	*/
/*	  All Rights Reserved  	*/

/*	THIS IS UNPUBLISHED PROPRIETARY SOURCE CODE OF AT&T	*/
/*	The copyright notice above does not evidence any   	*/
/*	actual or intended publication of such source code.	*/

#define USE_PID		1	/* use epid when cleaning locks		*/
#define IGN_PID		2	/* ignore epid when cleaning locks	*/

/*
 * file locking structure (connected to vnode)
 */
#define l_end 		l_len
#define MAXEND  	017777777777

struct	filock	{
	struct	flock set;	/* contains type, start, and end */
	union	{
		int wakeflg;	/* for locks sleeping on this one */
		struct {
			short sysid;
			short pid;
		} blk;			/* for sleeping locks only */
	}	stat;
	struct	filock *prev;
	struct	filock *next;
};

/*
 * file & record locking statistics structure.
 * Note: Record use total may overflow.
 */
struct flckinfo {
	long recs;	/* number of records configured on system */
	long reccnt;	/* number of records currently in use */
	long recovf;	/* number of times system ran out of record locks. */
	long rectot;	/* number of records used since system boot */
};

extern struct flckinfo	flckinfo;
extern struct filock	flox[];

#endif	_SYS_FLOCK_
