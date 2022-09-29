#ident "$Header: types.h,v 1.4 90/03/19 19:04:13 hal Exp $"
/*	%Q%	%I%	%M%	*/
/* $Copyright: |
 * |-----------------------------------------------------------|
 * | Copyright (c) 1987, 1990 MIPS Computer Systems, Inc.      |
 * | All Rights Reserved                                       |
 * |-----------------------------------------------------------|
 * |          Restrictive Rights Legend                        |
 * | Use, duplication, or disclosure by the Government is      |
 * | subject to restrictions as set forth in                   |
 * | subparagraph (c)(1)(ii) of the Rights in Technical        |
 * | Data and Computer Software Clause of DFARS 252.227-7013.  |
 * |         MIPS Computer Systems, Inc.                       |
 * |         928 Arques Avenue                                 |
 * |         Sunnyvale, CA 94086                               |
 * |-----------------------------------------------------------|
 *  $ */

/*	types.h	6.1	83/07/29	*/

#ifndef _SYS_TYPES_
#define _SYS_TYPES_
/*
 * Basic system types and major/minor device constructing/busting macros.
 */

/* major part of a device */
#define	major(x)	((int)(((unsigned)(x)>>8)&0377))

/* minor part of a device */
#define	minor(x)	((int)((x)&0377))

/* make a device number */
#define	makedev(x,y)	((dev_t)(((x)<<8) | (y)))

typedef	unsigned char	u_char;
typedef	unsigned short	u_short;
typedef	unsigned int	u_int;
typedef	unsigned long	u_long;
typedef	unsigned short	ushort;		/* sys III compat */

#ifdef vax
typedef	struct	_physadr { int r[1]; } *physadr;
typedef	struct	label_t	{
	int	val[14];
} label_t;
#endif
#ifdef mips
typedef	struct	_physadr { int r[1]; } *physadr;
/*
 * WARNING:
 * this must match the definition of kernel jmpbuf's in machine/pcb.h
 */
typedef	struct	label_t	{
	int	val[12];
} label_t;
#endif
typedef	struct	_quad { long val[2]; } quad;
typedef	long	daddr_t;
typedef	char *	caddr_t;
typedef u_long	paddr_t;
typedef	u_long	ino_t;
typedef	long	swblk_t;
typedef	int	size_t;
typedef	int	time_t;
typedef	short	dev_t;
typedef	int	off_t;

typedef	struct	fd_set { int fds_bits[1]; } fd_set;
#endif _SYS_TYPES_
