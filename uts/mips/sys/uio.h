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
/* $Header: uio.h,v 1.2.1.2 90/05/10 06:43:34 wje Exp $ */


#ifndef _SYS_UIO_
#define _SYS_UIO_	1

/*	@(#)uio.h	2.1 88/05/18 4.0NFSSRC SMI;	from UCB 7.1 6/4/86	*/
/*
 * Copyright (c) 1982, 1986 Regents of the University of California.
 * All rights reserved.  The Berkeley software License Agreement
 * specifies the terms and conditions for redistribution.
 */

#ifndef _UIO_
#define	_UIO_

struct iovec {
	caddr_t	iov_base;
	int	iov_len;
};

struct uio {
	struct	iovec *uio_iov;
	int	uio_iovcnt;
	off_t	uio_offset;
	short	uio_segflg;
#define	uio_seg	uio_segflg	/* obsolete */
	short	uio_fmode;
	int	uio_resid;
};

enum	uio_rw { UIO_READ, UIO_WRITE };

/*
 * Segment flag values (should be enum).
 */
#define UIO_USERSPACE	0		/* from user data space */
#define UIO_SYSSPACE	1		/* from system space */
#define UIO_USERISPACE	2		/* from user I space */
#endif

#endif	_SYS_UIO_
