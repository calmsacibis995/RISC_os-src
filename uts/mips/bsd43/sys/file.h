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
/* $Header: file.h,v 1.14.1.3 90/05/10 04:51:39 wje Exp $ */
/*
 * Copyright (c) 1982, 1986 Regents of the University of California.
 * All rights reserved.  The Berkeley software License Agreement
 * specifies the terms and conditions for redistribution.
 *
 *	@(#)file.h	2.1 88/05/18 4.0NFSSRC SMI
 *	@(#)file.h	7.1 (Berkeley) 6/4/86
 */
/*----- COPYRIGHT (END) -----------------------------------------------------*/

#ifndef _BSD43_SYS_FILE_
#define _BSD43_SYS_FILE_ 1

#ifndef BSD43_
#    include <bsd43/bsd43_.h>
#endif BSD43_

#ifdef KERNEL
#include "bsd43/sys/fcntl.h"
#else KERNEL
#include <bsd43/sys/types.h>
#include <bsd43/sys/fcntl.h>
#endif KERNEL

#ifdef KERNEL
/*
 * Descriptor table entry.
 * One for each kernel object.
 */
struct	bsd43_(file) {
	int	f_flag;		/* see below */
	short	f_type;		/* descriptor type */
	short	f_count;	/* reference count */
	short	f_msgcount;	/* references from message queue */
	struct	bsd43_(fileops) {
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
	struct bsd43_(ucred) *f_cred;	/* credentials of user who opened file */
};
#ifndef f_data
#define f_data		f_up.f_udata
#define f_next		f_up.f_unext
#endif

#endif KERNEL

/*
 * flags- also for fcntl call.
 */
#define	BSD43_FOPEN		(-1)
#define	BSD43_FREAD		00001		/* descriptor read/receive'able */
#define	BSD43_FWRITE		00002		/* descriptor write/send'able */
#ifndef	BSD43_F_DUPFD
#define	BSD43_FNDELAY		00004		/* no delay */
#define	BSD43_FAPPEND		00010		/* append on each write */
#endif
#define	BSD43_FMARK		00020		/* mark during gc() */
#define	BSD43_FDEFER		00040		/* defer for next gc pass */
#ifndef	BSD43_F_DUPFD
#define	BSD43_FASYNC		00100		/* signal pgrp when data ready */
#endif
#define	BSD43_FSHLOCK		00200		/* shared lock present */
#define	BSD43_FEXLOCK		00400		/* exclusive lock present */

/* bits to save after open */
#define	BSD43_FMASK		00113
#define	BSD43_FCNTLCANT	(BSD43_FREAD|BSD43_FWRITE|BSD43_FMARK|BSD43_FDEFER|BSD43_FSHLOCK|BSD43_FEXLOCK)

/*
 * User definitions.
 */

/*
 * Flock call.
 */
#define	BSD43_LOCK_SH		1	/* shared lock */
#define	BSD43_LOCK_EX		2	/* exclusive lock */
#define	BSD43_LOCK_NB		4	/* don't block when locking */
#define	BSD43_LOCK_UN		8	/* unlock */

/*
 * Lseek call.
 */
#define	BSD43_L_SET		0	/* absolute offset */
#define	BSD43_L_INCR		1	/* relative to current offset */
#define	BSD43_L_XTND		2	/* relative to end of file */

#ifdef KERNEL
#define	BSD43_GETF(fp, fd) { \
	if ((unsigned)(fd) >= NOFILE || ((fp) = u.u_ofile[fd]) == NULL) { \
		u.u_error = EBADF; \
		return; \
	} \
}

#define	BSD43_DTYPE_VNODE	1	/* file */
#define	BSD43_DTYPE_SOCKET	2	/* communications endpoint */
#endif

/*----- NUMIPS: STRIP MACRO PREFIXES (BEGIN) ---------------------------------*/
/* 
 * These defines strip the "BSD43_" and "bsd43_" prefixes from the names
 * above so that these files may be included in standard BSD code.
 */
#ifdef SYSTYPE_BSD43
#   define FOPEN	BSD43_FOPEN
#   define FREAD	BSD43_FREAD
#   define FWRITE	BSD43_FWRITE
#ifndef BSD43_F_DUPFD
#   define FNDELAY	BSD43_FNDELAY
#   define FAPPEND	BSD43_FAPPEND
#endif BSD43_F_DUPFD
#   define FMARK	BSD43_FMARK
#   define FDEFER	BSD43_FDEFER
#ifndef BSD43_F_DUPFD
#   define FASYNC	BSD43_FASYNC
#endif BSD43_F_DUPFD
#   define FSHLOCK	BSD43_FSHLOCK
#   define FEXLOCK	BSD43_FEXLOCK
#   define FMASK	BSD43_FMASK
#   define FCNTLCANT	BSD43_FCNTLCANT
#   define LOCK_SH	BSD43_LOCK_SH
#   define LOCK_EX	BSD43_LOCK_EX
#   define LOCK_NB	BSD43_LOCK_NB
#   define LOCK_UN	BSD43_LOCK_UN
#   define L_SET	BSD43_L_SET
#   define L_INCR	BSD43_L_INCR
#   define L_XTND	BSD43_L_XTND
#   define GETF		BSD43_GETF
#   define DTYPE_VNODE	BSD43_DTYPE_VNODE
#   define DTYPE_SOCKET	BSD43_DTYPE_SOCKET
#endif SYSTYPE_BSD43
/*----- NUMIPS: STRIP MACRO PREFIXES (END) ----------------------------------*/

#endif _BSD43_SYS_FILE_
