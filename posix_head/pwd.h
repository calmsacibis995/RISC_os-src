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
/* $Header: pwd.h,v 1.3.1.4 90/05/10 04:08:21 wje Exp $ */

#ifndef	_POSIX_PWD_
#define	_POSIX_PWD_	1

/*	Copyright (c) 1984 AT&T	*/
/*	  All Rights Reserved  	*/

/*	THIS IS UNPUBLISHED PROPRIETARY SOURCE CODE OF AT&T	*/
/*	The copyright notice above does not evidence any   	*/
/*	actual or intended publication of such source code.	*/

#ifndef	_UID_T
#define _UID_T
typedef unsigned short	uid_t;		/* user IDs */
#endif

#ifndef	_GID_T
#define _GID_T
typedef unsigned short	gid_t;		/* group IDs */
#endif

/* padding added to make this structure align with the sysv structure 
 * (sysv pw_uid and pw_gid are ints, whereas in posix these are shorts) 
 */
struct passwd {
	char	*pw_name;
	char	*pw_passwd;
	short	pw_pad1;	/* pad structure to be the same size as */
	uid_t	pw_uid;
	short	pw_pad2; 	/* the sysv passwd structure */
	gid_t	pw_gid;
	char	*pw_age;
	char	*pw_comment;
	char	*pw_gecos;
	char	*pw_dir;
	char	*pw_shell;
};

extern struct passwd *getpwnam(), *getpwuid();

#endif	_POSIX_PWD_
