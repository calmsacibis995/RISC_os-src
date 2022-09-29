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
/* $Header: file.h,v 1.9.1.5 90/05/10 20:49:43 wje Exp $ */


#ifndef	_SYS_FILE_
#define	_SYS_FILE_	1

#include <sys/fcntl.h>

/*	Copyright (c) 1984 AT&T	*/
/*	  All Rights Reserved  	*/

/*	THIS IS UNPUBLISHED PROPRIETARY SOURCE CODE OF AT&T	*/
/*	The copyright notice above does not evidence any   	*/
/*	actual or intended publication of such source code.	*/


#ifdef KERNEL
/*
 * Descriptor table entry.
 * One for each kernel object.
 */
struct	file {
	int	f_flag;		/* see below */
	short	f_type;		/* descriptor type */
	short	f_count;	/* reference count */
	short	f_msgcount;	/* references from message queue */
	struct	fileops {
		int	(*fo_rw)();
		int	(*fo_ioctl)();
		int	(*fo_select)();
		int	(*fo_close)();
	} *f_ops;
	union {
		struct file  *f_unext;	/* pointer to next entry in freelist */
		caddr_t	f_udata;    /* ptr to specific struct (vnode/socket) */
	} f_up;
	off_t	f_offset;
	struct ucred *f_cred;	/* credentials of user who opened file */
};
#ifndef f_data
#define f_data		f_up.f_udata
#define f_next		f_up.f_unext
#endif

struct	file *getf();
struct	file *falloc();
#endif KERNEL

extern struct file file[];	/* The file table itself */
extern struct file *ffreelist;	/* Head of freelist pool */

/* flags */

#define	FOPEN	0xffffffff
#define	FREAD	0x01
#define	FWRITE	0x02
#define	FNDELAY	0x04
#define	FAPPEND	0x08
#define FSYNC	0x10
#define FNOCTTY	0x20		/* don't acquire controlling terminal in open */
#define FASYNC	0x40		/* signal pgrp when data ready */
#define	FMASK	0x7f		/* FMASK should be disjoint from FNET */
#define FNET	0x80		/* needed by 3bnet */

/* open only modes */

#define	FCREAT	0x100
#define	FTRUNC	0x200
#define	FEXCL	0x400

/* UNIX domain sockets flags */

#define FMARK	0x800		/* mark during gc() */
#define FDEFER	0x1000		/* defer for next gc pass */

/* Other 4.3BSD flags */

#define	FSHLOCK	0x2000		/* shared lock present */
#define	FEXLOCK	0x4000		/* exclusive lock present */

/* Pre-RISC/os 4.50 compatibility flag */

#define	FNDELAY_PRE4_5	0x8000	/* emulate -systype sysv FNDELAY behavoir for */
				/* sockets in pre-RISC/os 5.0 binaries */
/*
 * Select interface to the driver (only one choice passed at a time)
 */
#define	SEL_READ	FREAD		/* check for reading */
#define	SEL_WRITE	FWRITE		/* check for writing */
#define	SEL_EXC		0		/* check for exceptions */

/*
 * Flock call.
 */
#define	LOCK_SH		1	/* shared lock */
#define	LOCK_EX		2	/* exclusive lock */
#define	LOCK_NB		4	/* don't block when locking */
#define	LOCK_UN		8	/* unlock */

/*
 * Lseek call.
 */
#define	L_SET		0	/* absolute offset */
#define	L_INCR		1	/* relative to current offset */
#define	L_XTND		2	/* relative to end of file */

#ifdef KERNEL
#define	GETF(fp, fd) { \
	if ((unsigned)(fd) >= v.v_nofiles || ((fp) = u.u_ofile[fd]) == NULL) { \
		u.u_error = EBADF; \
		return; \
	} \
}

#define	DTYPE_VNODE	1	/* file */
#define	DTYPE_SOCKET	2	/* communications endpoint */

/* Macros to hold and release file pointer */
#define	FP_HOLD(fp)	(fp)->f_count++
#define	FP_RELE(fp)	closef(fp)

#endif KERNEL

#endif	_SYS_FILE_
