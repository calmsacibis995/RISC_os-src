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
/* $Header: vmsystm.h,v 1.6.3.2 90/05/10 05:00:37 wje Exp $ */
/*
 * Copyright (c) 1982, 1986 Regents of the University of California.
 * All rights reserved.  The Berkeley software License Agreement
 * specifies the terms and conditions for redistribution.
 *
 *	@(#)vmsystm.h	7.1 (Berkeley) 6/4/86
 */
/*----- COPYRIGHT (END) ------------------------------------------------------*/

#ifndef BSD43_
#    include <bsd43/bsd43_.h>
#endif BSD43_


/*
 * Miscellaneous virtual memory subsystem variables and structures.
 */

#ifdef KERNEL
#ifdef mips
int	freemem;		/* remaining blocks of free memory */
int	avefree;		/* moving average of remaining free blocks */
int	avefree30;		/* 30 sec (avefree is 5 sec) moving average */
int	deficit;		/* estimate of needs of new swapped in procs */
int	nscan;			/* number of scans in last second */
extern int bsd43_(multprog);		/* current multiprogramming degree */
int	desscan;		/* desired pages scanned per second */

/* writable copies of tunables */
extern int bsd43_(maxpgio);		/* max paging i/o per sec before start swaps */
extern int bsd43_(maxslp);		/* max sleep time before very swappable */
extern int bsd43_(lotsfree);		/* max free before clock freezes */
extern int bsd43_(minfree);		/* minimum free pages before swapping begins */
extern int bsd43_(desfree);		/* no of pages to try to keep free via daemon */
extern int bsd43_(saferss);		/* no pages not to steal; decays with slptime */
extern int bsd43_(slowscan);		/* slowest scan rate, clusters/second */
extern int bsd43_(fastscan);		/* fastest scan rate, clusters/second */
#else !mips
int	freemem;		/* remaining blocks of free memory */
int	avefree;		/* moving average of remaining free blocks */
int	avefree30;		/* 30 sec (avefree is 5 sec) moving average */
int	deficit;		/* estimate of needs of new swapped in procs */
int	nscan;			/* number of scans in last second */
int	bsd43_(multprog);		/* current multiprogramming degree */
int	desscan;		/* desired pages scanned per second */

/* writable copies of tunables */
int	bsd43_(maxpgio);		/* max paging i/o per sec before start swaps */
int	bsd43_(maxslp);			/* max sleep time before very swappable */
int	bsd43_(lotsfree);		/* max free before clock freezes */
int	bsd43_(minfree);		/* minimum free pages before swapping begins */
int	bsd43_(desfree);		/* no of pages to try to keep free via daemon */
int	bsd43_(saferss);		/* no pages not to steal; decays with slptime */
int	bsd43_(slowscan);		/* slowest scan rate, clusters/second */
int	bsd43_(fastscan);		/* fastest scan rate, clusters/second */
#endif !mips
#endif KERNEL

/*
 * Fork/vfork accounting.
 */
struct	bsd43_(forkstat)
{
	int	cntfork;
	int	cntvfork;
	int	sizfork;
	int	sizvfork;
};
#ifdef KERNEL
struct	bsd43_(forkstat) bsd43_(forkstat);
#endif

/*
 * Swap kind accounting.
 */
struct	bsd43_(swptstat)
{
#ifdef mips
	int	dpteasy;	/* easy data pt swaps */
	int	spteasy;	/* easy stack pt swaps */
	int	dptexpand;	/* data pt expansion swaps */
	int	sptexpand;	/* stack pt expansion swaps */
	int	dptshrink;	/* data pt shrinking swaps */
	int	sptshrink;	/* stack pt shrinking swaps */
#endif
#ifdef vax
	int	pteasy;		/* easy pt swaps */
	int	ptexpand;	/* pt expansion swaps */
	int	ptshrink;	/* pt shrinking swaps */
	int	ptpack;		/* pt swaps involving spte copying */
#endif
};
#ifdef KERNEL
struct	bsd43_(swptstat) bsd43_(swptstat);
#endif

/*----- NUMIPS: STRIP MACRO PREFIXES (BEGIN) ---------------------------------*/
/* 
 * Strip off "BSD43_" and "bsd43_" for use with standard BSD code.
 * (GENERATED AUTOMATICALLY -- DON'T EDIT BY HAND)
 */
#ifdef SYSTYPE_BSD43
#endif SYSTYPE_BSD43
/*----- NUMIPS: STRIP MACRO PREFIXES (END) -----------------------------------*/


