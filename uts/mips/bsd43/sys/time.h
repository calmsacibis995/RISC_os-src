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
/* $Header: time.h,v 1.9.1.2 90/05/10 04:56:58 wje Exp $ */
/*
 * Copyright (c) 1982, 1986 Regents of the University of California.
 * All rights reserved.  The Berkeley software License Agreement
 * specifies the terms and conditions for redistribution.
 *
 *	@(#)time.h	7.1 (Berkeley) 6/4/86
 */
/*----- COPYRIGHT (END) ------------------------------------------------------*/

#ifndef BSD43_
#    include <bsd43/bsd43_.h>
#endif BSD43_

#ifndef BSD43_SYS_TIME_
#define BSD43_SYS_TIME_

/*
 * Structure returned by gettimeofday(2) system call,
 * and used in other calls.
 */
struct bsd43_(timeval) {
	long	tv_sec;		/* seconds */
	long	tv_usec;	/* and microseconds */
};

struct bsd43_(timezone) {
	int	tz_minuteswest;	/* minutes west of Greenwich */
	int	tz_dsttime;	/* type of dst correction */
};
#define	BSD43_DST_NONE	0	/* not on dst */
#define	BSD43_DST_USA		1	/* USA style dst */
#define	BSD43_DST_AUST	2	/* Australian style dst */
#define	BSD43_DST_WET		3	/* Western European dst */
#define	BSD43_DST_MET		4	/* Middle European dst */
#define	BSD43_DST_EET		5	/* Eastern European dst */
#define	BSD43_DST_CAN		6	/* Canada */

/*
 * Operations on timevals.
 *
 * NB: timercmp does not work for >= or <=.
 */
#define	bsd43_timerisset(tvp)		((tvp)->tv_sec || (tvp)->tv_usec)
#define	bsd43_timercmp(tvp, uvp, cmp)	\
	((tvp)->tv_sec cmp (uvp)->tv_sec || \
	 (tvp)->tv_sec == (uvp)->tv_sec && (tvp)->tv_usec cmp (uvp)->tv_usec)
#define	bsd43_timerclear(tvp)		(tvp)->tv_sec = (tvp)->tv_usec = 0

/*
 * Names of the interval timers, and structure
 * defining a timer setting.
 */
#define	BSD43_ITIMER_REAL	0
#define	BSD43_ITIMER_VIRTUAL	1
#define	BSD43_ITIMER_PROF	2

struct	bsd43_(itimerval) {
	struct	bsd43_(timeval) it_interval;	/* timer interval */
	struct	bsd43_(timeval) it_value;	/* current value */
};

#ifndef KERNEL
#include <time.h>
#endif

/*----- NUMIPS: STRIP MACRO PREFIXES (BEGIN) ---------------------------------*/
/* 
 * Strip off "BSD43_" and "bsd43_" for use with standard BSD code.
 * (GENERATED AUTOMATICALLY -- DON'T EDIT BY HAND)
 */
#ifdef SYSTYPE_BSD43
#   define DST_AUST BSD43_DST_AUST
#   define DST_CAN BSD43_DST_CAN
#   define DST_EET BSD43_DST_EET
#   define DST_MET BSD43_DST_MET
#   define DST_NONE BSD43_DST_NONE
#   define DST_USA BSD43_DST_USA
#   define DST_WET BSD43_DST_WET
#   define ITIMER_PROF BSD43_ITIMER_PROF
#   define ITIMER_REAL BSD43_ITIMER_REAL
#   define ITIMER_VIRTUAL BSD43_ITIMER_VIRTUAL
#   define timerclear bsd43_timerclear
#   define timercmp bsd43_timercmp
#   define timerisset bsd43_timerisset
#endif SYSTYPE_BSD43
/*----- NUMIPS: STRIP MACRO PREFIXES (END) -----------------------------------*/


#endif BSD43_SYS_TIME_
