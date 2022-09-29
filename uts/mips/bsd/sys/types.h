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
/* $Header: types.h,v 1.9.1.3 90/05/10 04:38:30 wje Exp $ */

#ifndef _BSD_SYS_TYPES_
#define _BSD_SYS_TYPES_	1

/*
 * Copyright (c) 1982, 1986 Regents of the University of California.
 * All rights reserved.  The Berkeley software License Agreement
 * specifies the terms and conditions for redistribution.
 *
 *	@(#)types.h	7.1 (Berkeley) 6/4/86
 */

/*
 * So far the kernel uses only novel definitions such as fd_mask and
 * FD_SETSIZE, below.  The major/minor definitions and other standard
 * macros and types may be incompatible with our types.h, so we exclude
 * them for now.
 */
#include "../../sys/types.h"

/*
 * Basic system types and major/minor device constructing/busting macros.
 */

/* major part of a device */
#ifndef major
#define	major(x)	((int)(((unsigned)(x)>>8)&0377))
#endif

/* minor part of a device */
#ifndef minor
#define	minor(x)	((int)((x)&0377))
#endif

/* make a device number */
#ifndef makedev
#define	makedev(x,y)	((dev_t)(((x)<<8) | (y)))
#endif

#ifdef vax
typedef	struct	_physadr { int r[1]; } *physadr;
typedef	struct	label_t	{
	int	val[14];
} label_t;
#endif
#ifndef mips	/* these get defined in ../../sys/types.h for mips */
typedef	struct	_quad { long val[2]; } quad;
typedef	long	swblk_t;
typedef	u_short	uid_t;
typedef	u_short	gid_t;
#endif

#ifndef NBBY
#define	NBBY	8		/* number of bits in a byte */
#endif
/*
 * Select uses bit masks of file descriptors in longs.
 * These macros manipulate such bit fields (the filesystem macros use chars).
 * FD_SETSIZE may be defined by the user, but the default here
 * should be >= NOFILE (param.h).
 */
#ifndef	FD_SETSIZE
#define	FD_SETSIZE	256
#endif

typedef long	fd_mask;
#define NFDBITS	(sizeof(fd_mask) * NBBY)	/* bits per mask */
#ifndef howmany
#define howmany(x, y)   (((unsigned)((x)+((y)-1)))/(unsigned)(y))
#endif

typedef	struct fd_set {
	fd_mask	fds_bits[howmany(FD_SETSIZE, NFDBITS)];
} fd_set;

#define	FD_SET(n, p)	((p)->fds_bits[(n)/NFDBITS] |= (1 << ((n) % NFDBITS)))
#define	FD_CLR(n, p)	((p)->fds_bits[(n)/NFDBITS] &= ~(1 << ((n) % NFDBITS)))
#define	FD_ISSET(n, p)	((p)->fds_bits[(n)/NFDBITS] & (1 << ((n) % NFDBITS)))
#define FD_ZERO(p)	bzero((char *)(p), sizeof(*(p)))

#endif _BSD_SYS_TYPES_
