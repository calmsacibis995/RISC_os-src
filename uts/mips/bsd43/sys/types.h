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
/* $Header: types.h,v 1.8.1.4 90/05/10 04:58:11 wje Exp $ */
/*
 * Copyright (c) 1982, 1986 Regents of the University of California.
 * All rights reserved.  The Berkeley software License Agreement
 * specifies the terms and conditions for redistribution.
 *
 *	@(#)types.h	7.1 (Berkeley) 6/4/86
 */
/*----- COPYRIGHT (END) ------------------------------------------------------*/

#ifndef BSD43_
#    include <bsd43/bsd43_.h>
#endif BSD43_


#ifndef _BSD43_SYS_TYPES_
#define	_BSD43_SYS_TYPES_
/*
 * Basic system types and major/minor device constructing/busting macros.
 */

/* major part of a device */
#define	bsd43_major(x)	((int)(((unsigned)(x)>>8)&0377))

/* minor part of a device */
#define	bsd43_minor(x)	((int)((x)&0377))

/* make a device number */
#define	bsd43_makedev(x,y)	((dev_t)(((x)<<8) | (y)))


#ifdef vax
typedef	struct	bsd43_(_physadr) { int bsd43_(r)[1]; } *bsd43_(physadr);
typedef	struct	bsd43_(label_t)	{
	int	val[14];
} bsd43_(label_t);
#endif
#ifdef mips
typedef	struct	bsd43_(_physadr) { int bsd43_(r)[1]; } *bsd43_(physadr);
/*
 * WARNING:
 * this must match the definition of kernel jmpbuf's in machine/pcb.h
 */
typedef	struct	bsd43_(label_t)	{
	int	val[12];
} bsd43_(label_t);
#endif mips
typedef	struct	_quad { long val[2]; } quad;

#if !defined(_U_CHAR)			/* common types between SysV & BSD */
#define _U_CHAR
typedef	unsigned char	u_char;
typedef	unsigned short	u_short;
typedef	unsigned int	u_int;
typedef	unsigned long	u_long;
typedef	long		daddr_t;
typedef	char *		caddr_t;
typedef	unsigned short	ushort;		/* sys III compat */
typedef unsigned char	unchar;
typedef	unsigned int	uint;
typedef	unsigned long	ulong;
typedef	unsigned long	ino_t;
typedef	long		time_t;
typedef	short		dev_t;
typedef	long		off_t;
typedef	unsigned short	uid_t;
typedef	unsigned short	gid_t;
#endif 	/* _U_CHAR */

/* BSD basic types not used in SysV */
typedef	long	bsd43_(size_t);
#ifndef SWBLK_T
#define SWBLK_T
typedef	long	swblk_t;
#endif

#define	BSD43_NBBY	8		/* number of bits in a byte */
/*
 * Select uses bit masks of file descriptors in longs.
 * These macros manipulate such bit fields (the filesystem macros use chars).
 * FD_SETSIZE may be defined by the user, but the default here
 * should be >= NOFILE (param.h).
 */
#ifndef	BSD43_FD_SETSIZE
#define	BSD43_FD_SETSIZE	256
#endif

typedef long	bsd43_(fd_mask);
#define BSD43_NFDBITS	(sizeof(bsd43_(fd_mask)) * BSD43_NBBY)	/* bits per mask */
#ifndef bsd43_howmany
#define bsd43_howmany(x, y)   	(((unsigned)((x)+((y)-1)))/(unsigned)(y))
#endif

typedef	struct bsd43_(fd_set) {
	bsd43_(fd_mask)	fds_bits[bsd43_howmany(BSD43_FD_SETSIZE, BSD43_NFDBITS)];
} bsd43_(fd_set);

#define	BSD43_FD_SET(n, p)	((p)->fds_bits[(n)/BSD43_NFDBITS] |= (1 << ((n) % BSD43_NFDBITS)))
#define	BSD43_FD_CLR(n, p)	((p)->fds_bits[(n)/BSD43_NFDBITS] &= ~(1 << ((n) % BSD43_NFDBITS)))
#define	BSD43_FD_ISSET(n, p)	((p)->fds_bits[(n)/BSD43_NFDBITS] & (1 << ((n) % BSD43_NFDBITS)))
#define BSD43_FD_ZERO(p)	bzero((char *)(p), sizeof(*(p)))


/*----- NUMIPS: STRIP MACRO PREFIXES (BEGIN) ---------------------------------*/
/* 
 * Strip off "BSD43_" and "bsd43_" for use with standard BSD code.
 * (GENERATED AUTOMATICALLY -- DON'T EDIT BY HAND)
 */
#ifdef SYSTYPE_BSD43
#   define FD_CLR BSD43_FD_CLR
#   define FD_ISSET BSD43_FD_ISSET
#   define FD_SET BSD43_FD_SET
#   define FD_SETSIZE BSD43_FD_SETSIZE
#   define FD_ZERO BSD43_FD_ZERO
#   define NBBY BSD43_NBBY
#   define NFDBITS BSD43_NFDBITS
#   define howmany bsd43_howmany
#   define major bsd43_major
#   define makedev bsd43_makedev
#   define minor bsd43_minor
#endif SYSTYPE_BSD43
/*----- NUMIPS: STRIP MACRO PREFIXES (END) -----------------------------------*/


#endif _BSD43_SYS_TYPES_
