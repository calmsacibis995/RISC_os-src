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
/* $Header: uio.h,v 1.8.1.2 90/05/10 04:58:25 wje Exp $ */
/*
 * Copyright (c) 1982, 1986 Regents of the University of California.
 * All rights reserved.  The Berkeley software License Agreement
 * specifies the terms and conditions for redistribution.
 *
 *	@(#)uio.h	7.1 (Berkeley) 6/4/86
 */
/*----- COPYRIGHT (END) ------------------------------------------------------*/

#ifndef BSD43_
#    include <bsd43/bsd43_.h>
#endif BSD43_


#ifndef _BSD43_SYS_UIO_
#define	_BSD43_SYS_UIO_

struct bsd43_(iovec) {
	caddr_t	iov_base;
	int	iov_len;
};

struct bsd43_(uio) {
	struct	bsd43_(iovec) *uio_iov;
	int	uio_iovcnt;
	off_t	uio_offset;
	short	uio_segflg;
#define	uio_seg	uio_segflg	/* obsolete */
	short	uio_fmode;
	int	uio_resid;
};

enum	bsd43_(uio_rw) { BSD43_UIO_READ, BSD43_UIO_WRITE };

/*
 * Segment flag values (should be enum).
 */
#define BSD43_UIO_USERSPACE	0		/* from user data space */
#define BSD43_UIO_SYSSPACE	1		/* from system space */
#define BSD43_UIO_USERISPACE	2		/* from user I space */
#endif _BSD43_SYS_UIO_

/*----- NUMIPS: STRIP MACRO PREFIXES (BEGIN) ---------------------------------*/
/* 
 * Strip off "BSD43_" and "bsd43_" for use with standard BSD code.
 * (GENERATED AUTOMATICALLY -- DON'T EDIT BY HAND)
 */
#ifdef SYSTYPE_BSD43
#   define UIO_SYSSPACE BSD43_UIO_SYSSPACE
#   define UIO_USERISPACE BSD43_UIO_USERISPACE
#   define UIO_USERSPACE BSD43_UIO_USERSPACE
#   define UIO_READ BSD43_UIO_READ
#   define UIO_WRITE BSD43_UIO_WRITE
#   define _UIO_ BSD43__UIO_
#endif SYSTYPE_BSD43
/*----- NUMIPS: STRIP MACRO PREFIXES (END) -----------------------------------*/


