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
/* $Header: acct.h,v 1.8.1.2 90/05/10 06:04:20 wje Exp $ */

#ifndef	_SYS_ACCT_
#define	_SYS_ACCT_	1


/*	Copyright (c) 1984 AT&T	*/
/*	  All Rights Reserved  	*/

/*	THIS IS UNPUBLISHED PROPRIETARY SOURCE CODE OF AT&T	*/
/*	The copyright notice above does not evidence any   	*/
/*	actual or intended publication of such source code.	*/

/*
 * Accounting structures
 */

typedef	ushort comp_t;		/* "floating point" */
		/* 13-bit fraction, 3-bit exponent  */

struct	acct
{
	char	ac_flag;		/* Accounting flag */
	char	ac_stat;		/* Exit status */
	ushort	ac_uid;			/* Accounting user ID */
	ushort	ac_gid;			/* Accounting group ID */
	dev_t	ac_tty;			/* control typewriter */
	time_t	ac_btime;		/* Beginning time */
	comp_t	ac_utime;		/* acctng user time in clock ticks */
	comp_t	ac_stime;		/* acctng system time in clock ticks */
	comp_t	ac_etime;		/* acctng elapsed time in clock ticks */
	comp_t	ac_mem;			/* memory usage */
	comp_t	ac_io;			/* chars transferred */
	comp_t	ac_rw;			/* blocks read or written */
	char	ac_comm[8];		/* command name */
};	

extern	struct	acct	acctbuf;
extern	struct	vnode	*acctp;		/* vnode of accounting file */

#define	AFORK	01		/* has executed fork, but no exec */
#define	ASU	02		/* used super-user privileges */
#define	ACOMPAT	0004		/* used compatibility mode */
#define	ACORE	0010		/* dumped core */
#define	AXSIG	0020		/* killed by a signal */
#define	ACCTF	0300		/* record type: 00 = acct */

#endif	_SYS_ACCT_
