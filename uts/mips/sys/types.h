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
/* $Header: types.h,v 1.13.1.3.1.1.1.2 90/10/16 10:14:17 beacker Exp $ */

#ifndef	_SYS_TYPES_
#define	_SYS_TYPES_	1


/*	Copyright (c) 1984 AT&T	*/
/*	  All Rights Reserved  	*/

/*	THIS IS UNPUBLISHED PROPRIETARY SOURCE CODE OF AT&T	*/
/*	The copyright notice above does not evidence any   	*/
/*	actual or intended publication of such source code.	*/


typedef	struct { int r[1]; } *	physadr;

#if !defined(_U_CHAR)			/* common types between SysxV & BSD */
#define _U_CHAR
typedef	unsigned char	u_char;		/* (Needed for 4.2 driver ports) */
typedef	unsigned short	u_short;	/* (Needed for 4.2 driver ports) */
typedef	unsigned int	u_int;		/* (Needed for 4.2 driver ports) */
typedef	unsigned long	u_long;		/* (Needed for 4.2 driver ports) */
typedef	long		daddr_t;	/* <disk address> type */
typedef	char *		caddr_t;	/* ?<core address> type */
typedef	unsigned short	ushort;
typedef	unsigned char	unchar;
typedef	unsigned int	uint;
typedef	unsigned long	ulong;
typedef	unsigned long	ino_t;		/* <inode> type */
typedef	long		time_t;		/* <time> type */
typedef	short		dev_t;		/* <old device number> type */
typedef	long		off_t;		/* ?<offset> type */
typedef	unsigned short	uid_t;
typedef	unsigned short	gid_t;
#endif _U_CHAR

/* SysV basic types not used in BSD */
typedef	short		cnt_t;		/* ?<count> type */
typedef	int		label_t[12];	/* sync with NJBREGS in pcb.h */
typedef	unsigned long	paddr_t;	/* <physical address> type */
typedef	int		key_t;		/* IPC key type */
typedef	unsigned char	use_t;		/* use count for swap.  */
typedef	short		sysid_t;
typedef	short		index_t;
typedef	short		lock_t;		/* lock work for busy wait */
#ifndef _SIZE_T
#define _SIZE_T
typedef	unsigned int	size_t;		/* len param for string funcs */
#endif


#ifdef KERNEL
typedef	struct	_quad { long val[2]; } quad;
#ifndef SWBLK_T
#define SWBLK_T
typedef	long	swblk_t;
#endif

/* This is defined in lib/libc/rpc/types.h == ???/rpc/types.h in Sun-land */
#ifndef bool_t
#define	bool_t	int
#endif /* bool_t */

#endif KERNEL

/*
 * Distributed UNIX hook 
 */
typedef struct cookie {
	long	c_sysid;
	long 	c_rcvd;
} *cookie_t;

#endif	_SYS_TYPES_
