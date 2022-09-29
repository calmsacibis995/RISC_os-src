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
/* $Header: text.h,v 1.7.1.2 90/05/10 04:56:52 wje Exp $ */
/*
 * Copyright (c) 1982, 1986 Regents of the University of California.
 * All rights reserved.  The Berkeley software License Agreement
 * specifies the terms and conditions for redistribution.
 *
 *	@(#)text.h	7.1 (Berkeley) 6/4/86
 */
/*----- COPYRIGHT (END) ------------------------------------------------------*/

#ifndef BSD43_
#    include <bsd43/bsd43_.h>
#endif BSD43_


/*
 * Text structure.
 * One allocated per pure
 * procedure on swap device.
 * Manipulated by text.c
 */
#define	BSD43_NXDAD	48		/* param.h:MAXTSIZ / vmparam.h:dtob(DMTEXT) */

struct bsd43_(text)
{
	struct	bsd43_(text) *x_forw;	/* forward link in free list */
	struct	bsd43_(text) **x_back;	/* backward link in free list */
	swblk_t	x_daddr[BSD43_NXDAD];	/* disk addresses of dmtext-page segments */
	swblk_t	x_ptdaddr;	/* disk address of page table */
	bsd43_(size_t)	x_size;		/* size (clicks) */
	struct bsd43_(proc) *x_caddr;	/* ptr to linked proc, if loaded */
	struct vnode *x_vptr;	/* vnode of prototype */
	short	x_rssize;
	short	x_swrss;
	short	x_count;	/* reference count */
	short	x_ccount;	/* number of loaded references */
	char	x_flag;		/* traced, written flags */
	char	x_slptime;
	short	x_poip;		/* page out in progress count */
};

#ifdef	KERNEL
struct	bsd43_(text) *bsd43_(text), *textNTEXT;
int	ntext;
#endif

#define	BSD43_XTRC	0x01		/* Text may be written, exclusive use */
#define	BSD43_XWRIT	0x02		/* Text written into, must swap out */
#define	BSD43_XLOAD	0x04		/* Currently being read from file */
#define	BSD43_XLOCK	0x08		/* Being swapped in or out */
#define	BSD43_XWANT	0x10		/* Wanted for swapping */
#define	BSD43_XPAGV	0x20		/* Page in on demand from vnode */
#define	BSD43_XUNUSED	0x40		/* unused since swapped out for cache */

/*
 * Text table statistics
 */
struct bsd43_(xstats) {
	u_long	bsd43_(alloc);			/* calls to xalloc */
	u_long	alloc_inuse;		/*	found in use/sticky */
	u_long	alloc_cachehit;		/*	found in cache */
	u_long	alloc_cacheflush;	/*	flushed cached text */
	u_long	alloc_unused;		/*	flushed unused cached text */
	u_long	free;			/* calls to xfree */
	u_long	free_inuse;		/*	still in use/sticky */
	u_long	free_cache;		/*	placed in cache */
	u_long	free_cacheswap;		/*	swapped out to place in cache */
};

/*----- NUMIPS: STRIP MACRO PREFIXES (BEGIN) ---------------------------------*/
/* 
 * Strip off "BSD43_" and "bsd43_" for use with standard BSD code.
 * (GENERATED AUTOMATICALLY -- DON'T EDIT BY HAND)
 */
#ifdef SYSTYPE_BSD43
#   define NXDAD BSD43_NXDAD
#   define XLOAD BSD43_XLOAD
#   define XLOCK BSD43_XLOCK
#   define XPAGV BSD43_XPAGV
#   define XTRC BSD43_XTRC
#   define XUNUSED BSD43_XUNUSED
#   define XWANT BSD43_XWANT
#   define XWRIT BSD43_XWRIT
#endif SYSTYPE_BSD43
/*----- NUMIPS: STRIP MACRO PREFIXES (END) -----------------------------------*/


