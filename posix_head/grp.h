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
/* $Header: grp.h,v 1.2.1.3 90/05/10 04:07:50 wje Exp $ */

#ifndef	_POSIX_GRP_
#define	_POSIX_GRP_	1

/*	Copyright (c) 1984 AT&T	*/
/*	  All Rights Reserved  	*/

/*	THIS IS UNPUBLISHED PROPRIETARY SOURCE CODE OF AT&T	*/
/*	The copyright notice above does not evidence any   	*/
/*	actual or intended publication of such source code.	*/

#ifndef	_GID_T
#define _GID_T
typedef	unsigned short	gid_t;
#endif

struct	group {	/* see getgrent(3) */
	char	*gr_name;
	char	*gr_passwd;
	short	pad;	/* to make structure same size as sysv structure */
	gid_t	gr_gid;
	char	**gr_mem;
};

extern struct group *getgrgid(), *getgrnam();

#endif	_POSIX_GRP_
