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
/* $Header: types.h,v 1.6.1.5 90/05/10 06:04:01 wje Exp $ */

#ifndef	_POSIX_SYS_TYPES_
#define	_POSIX_SYS_TYPES_	1

/*	Copyright (c) 1984 AT&T	*/
/*	  All Rights Reserved  	*/

/*	THIS IS UNPUBLISHED PROPRIETARY SOURCE CODE OF AT&T	*/
/*	The copyright notice above does not evidence any   	*/
/*	actual or intended publication of such source code.	*/

#ifndef	_POSIX_SOURCE
typedef	unsigned int	u_int;
typedef	unsigned short	u_short;
typedef	unsigned char	u_char;

typedef	char *		caddr_t;
#endif	/* !_POSIX_SOURCE */

#ifndef _SSIZE_T
#define _SSIZE_T
typedef	int		ssize_t;
#endif

#ifndef _SIZE_T
#define _SIZE_T
typedef	unsigned int	size_t;
#endif

typedef	unsigned long	ino_t;		/* <inode> type */
typedef	short		dev_t;		/* <old device number> type */
typedef	long		off_t;		/* ?<offset> type */

typedef	unsigned short	mode_t;		/* file attributes */
typedef short		nlink_t;	/* link counts */

typedef int		pid_t;		/* process and process group IDs */

#ifndef	_UID_T
#define _UID_T
typedef unsigned short	uid_t;		/* user IDs */
#endif

#ifndef	_GID_T
#define _GID_T
typedef unsigned short	gid_t;		/* group IDs */
#endif

#ifndef	_TIME_T
#define _TIME_T
typedef long		time_t;		/* time type */
#endif

#endif	_POSIX_SYS_TYPES_
