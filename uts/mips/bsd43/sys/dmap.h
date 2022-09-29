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
/* $Header: dmap.h,v 1.7.1.2 90/05/10 04:50:32 wje Exp $ */
/*
 * Copyright (c) 1982, 1986 Regents of the University of California.
 * All rights reserved.  The Berkeley software License Agreement
 * specifies the terms and conditions for redistribution.
 *
 *	@(#)dmap.h	7.1 (Berkeley) 6/4/86
 */
/*----- COPYRIGHT (END) ------------------------------------------------------*/

#ifndef BSD43_
#    include <bsd43/bsd43_.h>
#endif BSD43_


/*
 * Definitions for the mapping of vitual swap
 * space to the physical swap area - the disk map.
 */

#define	BSD43_NDMAP 		38	/* size of the swap area map */

struct	bsd43_(dmap)
{
	swblk_t	dm_size;	/* current size used by process */
	swblk_t	dm_alloc;	/* amount of physical swap space allocated */
	swblk_t	dm_map[BSD43_NDMAP];	/* first disk block number in each chunk */
};
#ifdef KERNEL
struct	bsd43_(dmap) bsd43_(zdmap);
int	bsd43_(dmmin), bsd43_(dmmax), bsd43_(dmtext);
#endif

/*
 * The following structure is that ``returned''
 * from a call to vstodb().
 */
struct	bsd43_(dblock)
{
	swblk_t	db_base;	/* base of physical contig drum block */
	swblk_t	db_size;	/* size of block */
};

/*----- NUMIPS: STRIP MACRO PREFIXES (BEGIN) ---------------------------------*/
/* 
 * Strip off "BSD43_" and "bsd43_" for use with standard BSD code.
 * (GENERATED AUTOMATICALLY -- DON'T EDIT BY HAND)
 */
#ifdef SYSTYPE_BSD43
#   define NDMAP BSD43_NDMAP
#endif SYSTYPE_BSD43
/*----- NUMIPS: STRIP MACRO PREFIXES (END) -----------------------------------*/


