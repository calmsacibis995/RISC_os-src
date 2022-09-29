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
 * |         950 DeGuigne Drive                                |
 * |         Sunnyvale, CA 94086                               |
 * |-----------------------------------------------------------|
 */
/* $Header: flock.h,v 1.1.1.1.1.1.1.1 90/11/17 11:42:20 beacker Exp $ */

/* @(#)nfs.cmds:nfs/lockd/flock.h 1.1 */
#ifndef _SYS_FLOCK_H
#define _SYS_FLOCK_H

#define INOFLCK		1	/* Vnode is locked when reclock() is called. */
#define SETFLCK		2	/* Set a file lock. */
#define SLPFLCK		4	/* Wait if blocked. */

#define IGN_PID		-1	/* ignore epid when cleaning locks	*/

/* file locking structure (connected to vnode) */

#define l_end 		l_len
#define MAXEND  	017777777777

typedef struct filock {
	struct	flock set;	/* contains type, start, and end */
#ifdef RISCOS
        /*
         * Global System identifier; Added since the BSD 4.3 flock structure
         * does NOT support the sysid field like System V. This value can be
	 * used as a unique system identifer to distinguish between locks.
         */
        long sysid;
#endif
	union	{
		int wakeflg;	/* for locks sleeping on this one */
		struct {
#ifdef RISCOS
			short sysid;
			short pid;
#else
			long sysid;
			pid_t pid;
#endif
		} blk;			/* for sleeping locks only */
	}	stat;
#ifdef	u3b
	int	wakesem;
#endif
	struct	filock *prev;
	struct	filock *next;
} filock_t;

/* file and record locking configuration structure */
/* record use total may overflow */
struct flckinfo {
	long recs;	/* number of records configured on system */
	long reccnt;	/* number of records currently in use */
	long recovf;	/* number of times system ran out of record locks. */
	long rectot;	/* number of records used since system boot */
};

extern struct flckinfo	flckinfo;

#endif	/* _SYS_FLOCK_H */
