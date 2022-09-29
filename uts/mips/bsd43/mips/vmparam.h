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
/* $Header: vmparam.h,v 1.6.3.2 90/05/10 04:43:34 wje Exp $ */
/*
 * Copyright (c) 1982, 1986 Regents of the University of California.
 * All rights reserved.  The Berkeley software License Agreement
 * specifies the terms and conditions for redistribution.
 *
 *	@(#)vmparam.h	7.1 (Berkeley) 6/5/86
 */
/*----- COPYRIGHT (END) ------------------------------------------------------*/

#ifndef BSD43_
#    include <bsd43/bsd43_.h>
#endif BSD43_



/*
 * Machine dependent constants for MIPS
 */

/*
 * USRTEXT is the start of the user text, USRDATA is the start of the
 * user data, and USRSTACK is the top (end) of the user stack.  
 * LOWPAGES is the number of pages from the beginning of the text region
 * to the beginning of text.
 * HIGHPAGES is the number of pages from the beginning of the stack region
 * to the beginning of the stack.
 */

#define	BSD43_LOWPAGES	0
#define	BSD43_REDZONEPAGES	1
#define	BSD43_HIGHPAGES	BSD43_REDZONEPAGES
#define	BSD43_USRTEXT		0x400000	/* user text starts at 4 MB */
#define BSD43_USRDATA		0x10000000	/* user data starts at 256 MB */
#define	BSD43_EA_SIZE		32		/* EMULATE_AREA size */
#define	BSD43_USRSTACK	(0x80000000-BSD43_REDZONEPAGES*BSD43_NBPG)/* Top of user stack */
#define BSD43_EMULATE_AREA	BSD43_USRSTACK-BSD43_EA_SIZE/* area for bp emulation */

/*
 * Virtual memory related constants, all in bytes
 */
#ifndef BSD43_MAXTSIZ
#define	BSD43_MAXTSIZ		(24*1024*1024)		/* max text size */
#endif
#ifndef BSD43_DFLDSIZ
#define	BSD43_DFLDSIZ		(32*1024*1024)		/* initial data size limit */
#endif
#ifndef BSD43_MAXDSIZ
#define	BSD43_MAXDSIZ		(32*1024*1024)		/* max data size */
#endif
#ifndef	BSD43_DFLSSIZ
#define	BSD43_DFLSSIZ		(1024*1024)		/* initial stack size limit */
#endif
#ifndef	BSD43_MAXSSIZ
#define	BSD43_MAXSSIZ		BSD43_MAXDSIZ			/* max stack size */
#endif

/*
 * Default sizes of swap allocation chunks (see dmap.h).
 * The actual values may be changed in vminit() based on MAXDSIZ.
 * With MAXDSIZ of 16Mb and NDMAP of 38, dmmax will be 1024.
 * All expressed in DEV_BSIZE blocks.
 */
#define	BSD43_DMMIN	32			/* smallest swap allocation */
#define	BSD43_DMMAX	4096			/* largest potential swap allocation */
#define	BSD43_DMTEXT	1024			/* swap allocation for text */

/*
 * Size of the system and user page table.
 */
#define	BSD43_SYSPTSIZE	(0x200000/BSD43_NBPG)	/* allow 2Mb max of buffers */
#define	BSD43_USRPTSIZE 	(3*32*BSD43_MAXUSERS+20*3) /* 8 procs/user + 20 daemons */

/*
 * The size of the clock loop.
 */
#define	BSD43_LOOPPAGES	(bsd43_(maxfree) - bsd43_(firstfree))

/*
 * The time for a process to be blocked before being very swappable.
 * This is a number of seconds which the system takes as being a non-trivial
 * amount of real time.  You probably shouldn't change this;
 * it is used in subtle ways (fractions and multiples of it are, that is, like
 * half of a ``long time'', almost a long time, etc.)
 * It is related to human patience and other factors which don't really
 * change over time.
 */
#define	BSD43_MAXSLP 		20

/*
 * A swapped in process is given a small amount of core without being bothered
 * by the page replacement algorithm.  Basically this says that if you are
 * swapped in you deserve some resources.  We protect the last SAFERSS
 * pages against paging and will just swap you out rather than paging you.
 * Note that each process has at least UPAGES+CLSIZE pages which are not
 * paged anyways (this is currently 8+2=10 pages or 5k bytes), so this
 * number just means a swapped in process is given around 25k bytes.
 * Just for fun: current memory prices are 4600$ a megabyte on VAX (4/22/81),
 * so we loan each swapped in process memory worth 100$, or just admit
 * that we don't consider it worthwhile and swap it out to disk which costs
 * $30/mb or about $0.75.
 */
#define	BSD43_SAFERSS		6		/* nominal ``small'' resident set size
					   protected against replacement */

/*
 * DISKRPM is used to estimate the number of paging i/o operations
 * which one can expect from a single disk controller.
 */
#define	BSD43_DISKRPM		60

/*
 * Klustering constants.  Klustering is the gathering
 * of pages together for pagein/pageout, while clustering
 * is the treatment of hardware page size as though it were
 * larger than it really is.
 *
 * KLMAX gives maximum cluster size in CLSIZE page (cluster-page)
 * units.  Note that KLMAX*CLSIZE must be <= dtoc(DMMIN) in dmap.h.
 */

#define	BSD43_KLMAX	(4/BSD43_CLSIZE)
#define	BSD43_KLSEQL	(2/BSD43_CLSIZE)		/* in klust if vadvise(VA_SEQL) */
#define	BSD43_KLIN	(1/BSD43_CLSIZE)		/* default data/stack in klust */
#define	BSD43_KLTXT	(1/BSD43_CLSIZE)		/* default text in klust */
#define	BSD43_KLOUT	(4/BSD43_CLSIZE)

/*
 * KLSDIST is the advance or retard of the fifo reclaim for sequential
 * processes data space.
 */
#define	BSD43_KLSDIST	2		/* klusters advance/retard for seq. fifo */

/*
 * Paging thresholds (see vm_sched.c).
 * Strategy of 4/22/81:
 *	lotsfree is 512k bytes, but at most 1/4 of memory
 *	desfree is 256K bytes, but at most 1/8 of memory
 *	minfree is 96K bytes, but at most 1/2 of desfree
 */
#define	BSD43_LOTSFREE	(128 * 4096)
#define	BSD43_LOTSFREEFRACT	4
#define	BSD43_DESFREE		(64 * 4096)
#define	BSD43_DESFREEFRACT	8
#define	BSD43_MINFREE		(24 * 4096)
#define	BSD43_MINFREEFRACT	2

/*
 * There are two clock hands, initially separated by HANDSPREAD bytes
 * (but at most all of user memory).  The amount of time to reclaim
 * a page once the pageout process examines it increases with this
 * distance and decreases as the scan rate rises.
 */
#define	BSD43_HANDSPREAD	(2 * 1024 * 4096)

/*
 * The number of times per second to recompute the desired paging rate
 * and poke the pagedaemon.
 */
#define	BSD43_RATETOSCHEDPAGING	4

/*
 * Pages per second to scan when out of memory (targeted toward ~10%
 * of cpu)
 */
#define	BSD43_FASTSCAN	400

/*
 * Scan all of memory no more frequently than SCANSECS
 */
#define	BSD43_SCANSECS	3

/*
 * Believed threshold (in megabytes) for which interleaved
 * swapping area is desirable.
 */
#define	BSD43_LOTSOFMEM	2

/*
 * Paged text files that are less than PGTHRESH bytes may be swapped
 * in instead of paged in.
 */
#define BSD43_PGTHRESH        (100 * 1024)


/*----- NUMIPS: STRIP MACRO PREFIXES (BEGIN) ---------------------------------*/
/* 
 * Strip off "BSD43_" and "bsd43_" for use with standard BSD code.
 * (GENERATED AUTOMATICALLY -- DON'T EDIT BY HAND)
 */
#ifdef SYSTYPE_BSD43
#   define DESFREE BSD43_DESFREE
#   define DESFREEFRACT BSD43_DESFREEFRACT
#   define DFLDSIZ BSD43_DFLDSIZ
#   define DFLSSIZ BSD43_DFLSSIZ
#   define DISKRPM BSD43_DISKRPM
#   define DMMAX BSD43_DMMAX
#   define DMMIN BSD43_DMMIN
#   define DMTEXT BSD43_DMTEXT
#   define EA_SIZE BSD43_EA_SIZE
#   define EMULATE_AREA BSD43_EMULATE_AREA
#   define FASTSCAN BSD43_FASTSCAN
#   define HANDSPREAD BSD43_HANDSPREAD
#   define HIGHPAGES BSD43_HIGHPAGES
#   define KLIN BSD43_KLIN
#   define KLMAX BSD43_KLMAX
#   define KLOUT BSD43_KLOUT
#   define KLSDIST BSD43_KLSDIST
#   define KLSEQL BSD43_KLSEQL
#   define KLTXT BSD43_KLTXT
#   define LOOPPAGES BSD43_LOOPPAGES
#   define LOTSFREE BSD43_LOTSFREE
#   define LOTSFREEFRACT BSD43_LOTSFREEFRACT
#   define LOTSOFMEM BSD43_LOTSOFMEM
#   define LOWPAGES BSD43_LOWPAGES
#   define MAXDSIZ BSD43_MAXDSIZ
#   define MAXSLP BSD43_MAXSLP
#   define MAXSSIZ BSD43_MAXSSIZ
#   define MAXTSIZ BSD43_MAXTSIZ
#   define MINFREE BSD43_MINFREE
#   define MINFREEFRACT BSD43_MINFREEFRACT
#   define PGTHRESH BSD43_PGTHRESH
#   define RATETOSCHEDPAGING BSD43_RATETOSCHEDPAGING
#   define REDZONEPAGES BSD43_REDZONEPAGES
#   define SAFERSS BSD43_SAFERSS
#   define SCANSECS BSD43_SCANSECS
#   define SYSPTSIZE BSD43_SYSPTSIZE
#   define USRDATA BSD43_USRDATA
#   define USRPTSIZE BSD43_USRPTSIZE
#   define USRSTACK BSD43_USRSTACK
#   define USRTEXT BSD43_USRTEXT
#endif SYSTYPE_BSD43
/*----- NUMIPS: STRIP MACRO PREFIXES (END) -----------------------------------*/


