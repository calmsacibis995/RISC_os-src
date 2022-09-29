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
/* $Header: vlimit.h,v 1.6.3.2 90/05/10 05:00:05 wje Exp $ */
/*
 * Copyright (c) 1982, 1986 Regents of the University of California.
 * All rights reserved.  The Berkeley software License Agreement
 * specifies the terms and conditions for redistribution.
 *
 *	@(#)vlimit.h	7.1 (Berkeley) 6/4/86
 */
/*----- COPYRIGHT (END) ------------------------------------------------------*/

#ifndef BSD43_
#    include <bsd43/bsd43_.h>
#endif BSD43_


/*
 * Limits for u.u_limit[i], per process, inherited.
 */
#define	BSD43_LIM_NORAISE	0	/* if <> 0, can't raise limits */
#define	BSD43_LIM_CPU		1	/* max secs cpu time */
#define	BSD43_LIM_FSIZE	2	/* max size of file created */
#define	BSD43_LIM_DATA	3	/* max growth of data space */
#define	BSD43_LIM_STACK	4	/* max growth of stack */
#define	BSD43_LIM_CORE	5	/* max size of ``core'' file */
#define	BSD43_LIM_MAXRSS	6	/* max desired data+stack core usage */

#define	BSD43_NLIMITS		6

#define	BSD43_INFINITY	0x7fffffff

/*----- NUMIPS: STRIP MACRO PREFIXES (BEGIN) ---------------------------------*/
/* 
 * Strip off "BSD43_" and "bsd43_" for use with standard BSD code.
 * (GENERATED AUTOMATICALLY -- DON'T EDIT BY HAND)
 */
#ifdef SYSTYPE_BSD43
#   define INFINITY BSD43_INFINITY
#   define LIM_CORE BSD43_LIM_CORE
#   define LIM_CPU BSD43_LIM_CPU
#   define LIM_DATA BSD43_LIM_DATA
#   define LIM_FSIZE BSD43_LIM_FSIZE
#   define LIM_MAXRSS BSD43_LIM_MAXRSS
#   define LIM_NORAISE BSD43_LIM_NORAISE
#   define LIM_STACK BSD43_LIM_STACK
#   define NLIMITS BSD43_NLIMITS
#endif SYSTYPE_BSD43
/*----- NUMIPS: STRIP MACRO PREFIXES (END) -----------------------------------*/


