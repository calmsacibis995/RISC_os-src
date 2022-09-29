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
/* $Header: acct.h,v 1.7.1.2 90/05/10 04:48:33 wje Exp $ */
/*
 * Copyright (c) 1982, 1986 Regents of the University of California.
 * All rights reserved.  The Berkeley software License Agreement
 * specifies the terms and conditions for redistribution.
 *
 *	@(#)acct.h	7.1 (Berkeley) 6/4/86
 */
/*----- COPYRIGHT (END) ------------------------------------------------------*/

#ifndef BSD43_
#    include <bsd43/bsd43_.h>
#endif BSD43_


/*
 * Accounting structures;
 * these use a comp_t type which is a 3 bits base 8
 * exponent, 13 bit fraction ``floating point'' number.
 * Units are 1/AHZ seconds.
 */
typedef	u_short bsd43_(comp_t);

struct	bsd43_(acct)
{
	char	ac_comm[10];		/* Accounting command name */
	bsd43_(comp_t)	ac_utime;		/* Accounting user time */
	bsd43_(comp_t)	ac_stime;		/* Accounting system time */
	bsd43_(comp_t)	ac_etime;		/* Accounting elapsed time */
	time_t	ac_btime;		/* Beginning time */
	uid_t	ac_uid;			/* Accounting user ID */
	gid_t	ac_gid;			/* Accounting group ID */
	short	ac_mem;			/* average memory usage */
	bsd43_(comp_t)	ac_io;			/* number of disk IO blocks */
	dev_t	ac_tty;			/* control typewriter */
	char	ac_flag;		/* Accounting flag */
};

#define	BSD43_AFORK	0001		/* has executed fork, but no exec */
#define	BSD43_ASU	0002		/* used super-user privileges */
#define	BSD43_ACOMPAT	0004		/* used compatibility mode */
#define	BSD43_ACORE	0010		/* dumped core */
#define	BSD43_AXSIG	0020		/* killed by a signal */

/*
 * 1/AHZ is the granularity of the data encoded in the various
 * comp_t fields.  This is not necessarily equal to hz.
 */
#define BSD43_AHZ 64

#ifdef KERNEL
struct	bsd43_(acct)	bsd43_(acctbuf);
struct	vnode	*bsd43_(acctp);
#endif

/*----- NUMIPS: STRIP MACRO PREFIXES (BEGIN) ---------------------------------*/
/* 
 * Strip off "BSD43_" and "bsd43_" for use with standard BSD code.
 * (GENERATED AUTOMATICALLY -- DON'T EDIT BY HAND)
 */
#ifdef SYSTYPE_BSD43
#   define ACOMPAT BSD43_ACOMPAT
#   define ACORE BSD43_ACORE
#   define AFORK BSD43_AFORK
#   define AHZ BSD43_AHZ
#   define ASU BSD43_ASU
#   define AXSIG BSD43_AXSIG
#endif SYSTYPE_BSD43
/*----- NUMIPS: STRIP MACRO PREFIXES (END) -----------------------------------*/


