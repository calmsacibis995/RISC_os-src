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
/* $Header: trace.h,v 1.6.3.2 90/05/10 04:57:16 wje Exp $ */
/*
 * Copyright (c) 1982, 1986 Regents of the University of California.
 * All rights reserved.  The Berkeley software License Agreement
 * specifies the terms and conditions for redistribution.
 *
 *	@(#)trace.h	7.1 (Berkeley) 6/4/86
 */
/*----- COPYRIGHT (END) ------------------------------------------------------*/

#ifndef BSD43_
#    include <bsd43/bsd43_.h>
#endif BSD43_


/*
 * File system buffer tracing points; all trace <pack(dev, size), bn>
 */
#define	BSD43_TR_BREADHIT	0	/* buffer read found in cache */
#define	BSD43_TR_BREADMISS	1	/* buffer read not in cache */
#define	BSD43_TR_BWRITE	2	/* buffer written */
#define	BSD43_TR_BREADHITRA	3	/* buffer read-ahead found in cache */
#define	BSD43_TR_BREADMISSRA	4	/* buffer read-ahead not in cache */
#define	BSD43_TR_XFODMISS	5	/* exe fod read */
#define	BSD43_TR_XFODHIT	6	/* exe fod read */
#define	BSD43_TR_BRELSE	7	/* brelse */
#define	BSD43_TR_BREALLOC	8	/* expand/contract a buffer */

/*
 * Memory allocator trace points; all trace the amount of memory involved
 */
#define	BSD43_TR_MALL		10	/* memory allocated */

/*
 * Paging trace points: all are <vaddr, pid>
 */
#define	BSD43_TR_INTRANS	20	/* page intransit block */
#define	BSD43_TR_EINTRANS	21	/* page intransit wait done */
#define	BSD43_TR_FRECLAIM	22	/* reclaim from free list */
#define	BSD43_TR_RECLAIM	23	/* reclaim from loop */
#define	BSD43_TR_XSFREC	24	/* reclaim from free list instead of drum */
#define	BSD43_TR_XIFREC	25	/* reclaim from free list instead of fsys */
#define	BSD43_TR_WAITMEM	26	/* wait for memory in pagein */
#define	BSD43_TR_EWAITMEM	27	/* end memory wait in pagein */
#define	BSD43_TR_ZFOD		28	/* zfod page fault */
#define	BSD43_TR_EXFOD	29	/* exec fod page fault */
#define	BSD43_TR_VRFOD	30	/* vread fod page fault */
#define	BSD43_TR_CACHEFOD	31	/* fod in file system cache */
#define	BSD43_TR_SWAPIN	32	/* drum page fault */
#define	BSD43_TR_PGINDONE	33	/* page in done */
#define	BSD43_TR_SWAPIO	34	/* swap i/o request arrives */

/*
 * System call trace points.
 */
#define	BSD43_TR_VADVISE	40	/* vadvise occurred with <arg, pid> */

/*
 * Miscellaneous
 */
#define	BSD43_TR_STAMP	45	/* user said vtrace(VTR_STAMP, value); */

/*
 * This defines the size of the trace flags array.
 */
#define	BSD43_TR_NFLAGS	100	/* generous */

#define	BSD43_TRCSIZ		4096

/*
 * Specifications of the vtrace() system call, which takes one argument.
 */
#define	BSD43_VTRACE		64+51

#define	BSD43_VTR_DISABLE	0		/* set a trace flag to 0 */
#define	BSD43_VTR_ENABLE	1		/* set a trace flag to 1 */
#define	BSD43_VTR_VALUE	2		/* return value of a trace flag */
#define	BSD43_VTR_UALARM	3		/* set alarm to go off (sig 16) */
					/* in specified number of hz */
#define	BSD43_VTR_STAMP	4		/* user specified stamp */
#ifdef KERNEL
#ifdef TRACE
char	bsd43_(traceflags)[BSD43_TR_NFLAGS];
struct	bsd43_(proc) *bsd43_(traceproc);
int	tracebuf[BSD43_TRCSIZ];
unsigned bsd43_(tracex);
int	bsd43_(tracewhich);
#define	bsd43_pack(a,b)	((a)<<16)|(b)
#define	bsd43_trace(a,b,c)	if (bsd43_(traceflags)[a]) trace1(a,b,c)
#else
#define	bsd43_trace(a,b,b)	;
#endif
#endif

/*----- NUMIPS: STRIP MACRO PREFIXES (BEGIN) ---------------------------------*/
/* 
 * Strip off "BSD43_" and "bsd43_" for use with standard BSD code.
 * (GENERATED AUTOMATICALLY -- DON'T EDIT BY HAND)
 */
#ifdef SYSTYPE_BSD43
#   define TRCSIZ BSD43_TRCSIZ
#   define TR_BREADHIT BSD43_TR_BREADHIT
#   define TR_BREADHITRA BSD43_TR_BREADHITRA
#   define TR_BREADMISS BSD43_TR_BREADMISS
#   define TR_BREADMISSRA BSD43_TR_BREADMISSRA
#   define TR_BREALLOC BSD43_TR_BREALLOC
#   define TR_BRELSE BSD43_TR_BRELSE
#   define TR_BWRITE BSD43_TR_BWRITE
#   define TR_CACHEFOD BSD43_TR_CACHEFOD
#   define TR_EINTRANS BSD43_TR_EINTRANS
#   define TR_EWAITMEM BSD43_TR_EWAITMEM
#   define TR_EXFOD BSD43_TR_EXFOD
#   define TR_FRECLAIM BSD43_TR_FRECLAIM
#   define TR_INTRANS BSD43_TR_INTRANS
#   define TR_MALL BSD43_TR_MALL
#   define TR_NFLAGS BSD43_TR_NFLAGS
#   define TR_PGINDONE BSD43_TR_PGINDONE
#   define TR_RECLAIM BSD43_TR_RECLAIM
#   define TR_STAMP BSD43_TR_STAMP
#   define TR_SWAPIN BSD43_TR_SWAPIN
#   define TR_SWAPIO BSD43_TR_SWAPIO
#   define TR_VADVISE BSD43_TR_VADVISE
#   define TR_VRFOD BSD43_TR_VRFOD
#   define TR_WAITMEM BSD43_TR_WAITMEM
#   define TR_XFODHIT BSD43_TR_XFODHIT
#   define TR_XFODMISS BSD43_TR_XFODMISS
#   define TR_XIFREC BSD43_TR_XIFREC
#   define TR_XSFREC BSD43_TR_XSFREC
#   define TR_ZFOD BSD43_TR_ZFOD
#   define VTRACE BSD43_VTRACE
#   define VTR_DISABLE BSD43_VTR_DISABLE
#   define VTR_ENABLE BSD43_VTR_ENABLE
#   define VTR_STAMP BSD43_VTR_STAMP
#   define VTR_UALARM BSD43_VTR_UALARM
#   define VTR_VALUE BSD43_VTR_VALUE
#   define pack bsd43_pack
#   define trace bsd43_trace
#endif SYSTYPE_BSD43
/*----- NUMIPS: STRIP MACRO PREFIXES (END) -----------------------------------*/


