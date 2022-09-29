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
/* $Header: lockf.h,v 1.1.1.1.1.1.1.1 90/11/17 11:43:37 beacker Exp $ */

/* @(#)nfs.cmds:nfs/lockd/lockf.h 1.6 */
/* constants and structures for the locking code ... */

struct data_lock {
        struct data_lock *Next,		/* Next lock in the list           */
                        *Prev,		/* Previous Lock in the list       */
			*NextProc;	/* Link list of process' locks	   */
        struct process_locks *MyProc;	/* Points to process lock list     */
	struct filock	filocks;
	int		granted,        /* The granted flag                */
			color,          /* Used during deadlock search     */
			LockID,         /* ID of this lock                 */
			class;          /* Class of lock (FILE,IO,NET)     */
        };

/* process lock structure holds locks owned by a given process */
struct process_locks {
	long		pid;
	struct process_locks *next;
	struct data_lock *lox;
	};

#define END(l)          ((l)->l_start + (l)->l_len - 1)
 
/* Is TRUE if a is completely contained within b */
#define WITHIN(a,b) (((a)->l_start >= (b)->l_start) && (END(a) <= END(b)))

int local_state;
