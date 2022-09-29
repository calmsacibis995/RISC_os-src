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
/* $Header: resource.h,v 1.7.3.2 90/05/10 04:55:05 wje Exp $ */
/*
 * Copyright (c) 1982, 1986 Regents of the University of California.
 * All rights reserved.  The Berkeley software License Agreement
 * specifies the terms and conditions for redistribution.
 *
 *	@(#)resource.h	7.1 (Berkeley) 6/4/86
 */
/*----- COPYRIGHT (END) ------------------------------------------------------*/

#ifndef BSD43_
#    include <bsd43/bsd43_.h>
#endif BSD43_

#ifndef BSD43_SYS_RESOURCE_
#define BSD43_SYS_RESOURCE_

/*
 * Process priority specifications to get/setpriority.
 */
#define	BSD43_PRIO_MIN	-20
#define	BSD43_PRIO_MAX	20

#define	BSD43_PRIO_PROCESS	0
#define	BSD43_PRIO_PGRP	1
#define	BSD43_PRIO_USER	2

/*
 * Resource utilization information.
 */

#define	BSD43_RUSAGE_SELF	0
#define	BSD43_RUSAGE_CHILDREN	-1

struct	bsd43_(rusage) {
	struct bsd43_(timeval) ru_utime;	/* user time used */
	struct bsd43_(timeval) ru_stime;	/* system time used */
	long	ru_maxrss;
#define	bsd43_ru_first	ru_ixrss
	long	ru_ixrss;		/* integral shared memory size */
	long	ru_idrss;		/* integral unshared data " */
	long	ru_isrss;		/* integral unshared stack " */
	long	ru_minflt;		/* page reclaims */
	long	ru_majflt;		/* page faults */
	long	ru_nswap;		/* swaps */
	long	ru_inblock;		/* block input operations */
	long	ru_oublock;		/* block output operations */
	long	ru_msgsnd;		/* messages sent */
	long	ru_msgrcv;		/* messages received */
	long	ru_nsignals;		/* signals received */
	long	ru_nvcsw;		/* voluntary context switches */
	long	ru_nivcsw;		/* involuntary " */
#define	bsd43_ru_last		ru_nivcsw
};

/*
 * Resource limits
 */
#define	BSD43_RLIMIT_CPU	0		/* cpu time in milliseconds */
#define	BSD43_RLIMIT_FSIZE	1		/* maximum file size */
#define	BSD43_RLIMIT_DATA	2		/* data size */
#define	BSD43_RLIMIT_STACK	3		/* stack size */
#define	BSD43_RLIMIT_CORE	4		/* core file size */
#define	BSD43_RLIMIT_RSS	5		/* resident set size */

#define	BSD43_RLIM_NLIMITS	6		/* number of resource limits */

#define	BSD43_RLIM_INFINITY	0x7fffffff

struct bsd43_(rlimit) {
	int	rlim_cur;		/* current (soft) limit */
	int	rlim_max;		/* maximum value for rlim_cur */
};

/*----- NUMIPS: STRIP MACRO PREFIXES (BEGIN) ---------------------------------*/
/* 
 * Strip off "BSD43_" and "bsd43_" for use with standard BSD code.
 * (GENERATED AUTOMATICALLY -- DON'T EDIT BY HAND)
 */
#ifdef SYSTYPE_BSD43
#   define PRIO_MAX BSD43_PRIO_MAX
#   define PRIO_MIN BSD43_PRIO_MIN
#   define PRIO_PGRP BSD43_PRIO_PGRP
#   define PRIO_PROCESS BSD43_PRIO_PROCESS
#   define PRIO_USER BSD43_PRIO_USER
#   define RLIMIT_CORE BSD43_RLIMIT_CORE
#   define RLIMIT_CPU BSD43_RLIMIT_CPU
#   define RLIMIT_DATA BSD43_RLIMIT_DATA
#   define RLIMIT_FSIZE BSD43_RLIMIT_FSIZE
#   define RLIMIT_RSS BSD43_RLIMIT_RSS
#   define RLIMIT_STACK BSD43_RLIMIT_STACK
#   define RLIM_INFINITY BSD43_RLIM_INFINITY
#   define RLIM_NLIMITS BSD43_RLIM_NLIMITS
#   define RUSAGE_CHILDREN BSD43_RUSAGE_CHILDREN
#   define RUSAGE_SELF BSD43_RUSAGE_SELF
#   define ru_first bsd43_ru_first
#   define ru_last bsd43_ru_last
#endif SYSTYPE_BSD43
/*----- NUMIPS: STRIP MACRO PREFIXES (END) -----------------------------------*/


#endif BSD43_SYS_RESOURCE_

