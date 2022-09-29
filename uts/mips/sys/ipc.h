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
/* $Header: ipc.h,v 1.6.4.2.1.1.1.2 90/10/05 09:55:49 beacker Exp $ */

#ifndef	_SYS_IPC_
#define	_SYS_IPC_	1


/*	Copyright (c) 1984 AT&T	*/
/*	  All Rights Reserved  	*/

/*	THIS IS UNPUBLISHED PROPRIETARY SOURCE CODE OF AT&T	*/
/*	The copyright notice above does not evidence any   	*/
/*	actual or intended publication of such source code.	*/

/* Common IPC Access Structure */
struct ipc_perm {
	ushort	uid;	/* owner's user id */
	ushort	gid;	/* owner's group id */
	ushort	cuid;	/* creator's user id */
	ushort	cgid;	/* creator's group id */
	ushort	mode;	/* access modes */
	ushort	seq;	/* slot usage sequence number */
	key_t	key;	/* key */
};

/* Common IPC Definitions. */
/* Mode bits. */
#define	IPC_ALLOC	0100000		/* entry currently allocated */
#define	IPC_CREAT	0001000		/* create entry if key doesn't exist */
#define	IPC_EXCL	0002000		/* fail if key exists */
#define	IPC_NOWAIT	0004000		/* error if request must wait */
#define	IPC_PERMANENT	0040000		/* entry is permenantly allocated */

/* Keys. */
#define	IPC_PRIVATE	(key_t)0	/* private key */

/* Control Commands. */
#define	IPC_RMID	0	/* remove identifier */
#define	IPC_SET		1	/* set options */
#define	IPC_STAT	2	/* get options */

#endif	_SYS_IPC_
