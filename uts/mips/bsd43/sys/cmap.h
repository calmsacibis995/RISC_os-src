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
/* $Header: cmap.h,v 1.7.1.2 90/05/10 04:49:11 wje Exp $ */
/*
 * Copyright (c) 1982, 1986 Regents of the University of California.
 * All rights reserved.  The Berkeley software License Agreement
 * specifies the terms and conditions for redistribution.
 *
 *	@(#)cmap.h	7.1 (Berkeley) 6/4/86
 */
/*----- COPYRIGHT (END) ------------------------------------------------------*/

#ifndef BSD43_
#    include <bsd43/bsd43_.h>
#endif BSD43_


/*
 * core map entry
 *
 * Limits imposed by this structure:
 *
 *		limit		     cur. size		fields
 *	Physical memory*		256 MB	c_next, c_prev, c_hlink
 *	size of a process segment	4 Gb	c_page
 *	filesystem size			8 Gb	c_blkno
 *	proc, text table size		64K	c_ndx
 *
 *	* memory can be expanded by converting first three entries
 *	to bit fields of larger than 16 bits, shrinking c_ndx accordingly,
 *	and increasing MAXMEM below.  Also, the type of cmhash
 *	(below) must be changed to long.
 */
#ifndef	LOCORE
struct bsd43_(cmap)
{
unsigned short 	c_next,		/* index of next free list entry */
		c_prev,		/* index of previous free list entry */
		c_hlink;	/* hash link for <vp,blkno> */
unsigned short	c_ndx;		/* index of owner proc or text */
unsigned int	c_page:21,	/* virtual page number in segment */
		c_lock:1,	/* locked for raw i/o or pagein */
		c_want:1,	/* wanted */
		c_intrans:1,	/* intransit bit */
		c_free:1,	/* on the free list */
		c_gone:1,	/* associated page has been released */
		c_type:2,	/* type CSYS or CTEXT or CSTACK or CDATA */
#ifdef USE_IDLE
		c_zero:2,	/* 0=needs zero, 1=is zeroed, 2=in zero */
		:2,		/* to longword boundary */
#else !USE_IDLE
		:4,		/* to longword boundary */
#endif
		c_blkno:24,	/* disk block this is a copy of */
		:8;		/* to longword boundary */
struct vnode	*c_vp;		/* vnode to which c_blkno refers */
#ifdef CACHETRICKS
unsigned short	c_icachecnt,	/* icache stamp when page free'd */
		c_dcachecnt;	/* dcache stamp when page free'd */
#endif CACHETRICKS
};
#else	LOCORE
/*
 * bit offsets of elements in cmap
 */
#define	BSD43_C_INTRANS	87
#define	BSD43_C_FREE		88
#define	BSD43_SZ_CMAP		(sizeof(struct bsd43_(cmap)))

#define	BSD43_MAXMEM		64*1024*4	/* maximum memory, in Kbytes */
#endif	LOCORE

#define	BSD43_CMHEAD	0

/*
 * Shared text pages are not totally abandoned when a process
 * exits, but are remembered while in the free list hashed by <vp,blkno>
 * off the cmhash structure so that they can be reattached
 * if another instance of the program runs again soon.
 */
#define	BSD43_CMHSIZ	2048		/* SHOULD BE DYNAMIC */
#define	BSD43_CMHASH(bn)	((bn)&(BSD43_CMHSIZ-1))

#ifndef	LOCORE
#ifdef	KERNEL
struct	bsd43_(cmap) *bsd43_(cmap);
struct	bsd43_(cmap) *ecmap;
int	bsd43_(ncmap);
struct	bsd43_(cmap) *bsd43_(mfind)();
int	bsd43_(firstfree), bsd43_(maxfree);
int	bsd43_(ecmx);			/* cmap index of ecmap */
u_short	bsd43_(cmhash)[BSD43_CMHSIZ];
#ifdef CACHETRICKS
extern unsigned short bsd43_(icachecnt)[];/* cacheflush count for each icache pg */
extern unsigned short bsd43_(dcachecnt)[];/* cacheflush count for each dcache pg */
#endif CACHETRICKS
extern unsigned int bsd43_(icachemask);	/* mask with pfnum to get icache index */
extern unsigned int bsd43_(dcachemask);	/* mask with pfnum to get dcache index */
#endif

/* bits defined in c_type */

#define	BSD43_CSYS		0		/* none of below */
#define	BSD43_CTEXT		1		/* belongs to shared text segment */
#define	BSD43_CDATA		2		/* belongs to data segment */
#define	BSD43_CSTACK		3		/* belongs to stack segment */

#define	bsd43_pgtocm(x)	(((int) ((x)-bsd43_(firstfree)) / BSD43_CLSIZE) + 1)
#define	bsd43_cmtopg(x)	((((x)-1) * BSD43_CLSIZE) + bsd43_(firstfree))
#endif	LOCORE

/*----- NUMIPS: STRIP MACRO PREFIXES (BEGIN) ---------------------------------*/
/* 
 * Strip off "BSD43_" and "bsd43_" for use with standard BSD code.
 * (GENERATED AUTOMATICALLY -- DON'T EDIT BY HAND)
 */
#ifdef SYSTYPE_BSD43
#   define CDATA BSD43_CDATA
#   define CMHASH BSD43_CMHASH
#   define CMHEAD BSD43_CMHEAD
#   define CMHSIZ BSD43_CMHSIZ
#   define CSTACK BSD43_CSTACK
#   define CSYS BSD43_CSYS
#   define CTEXT BSD43_CTEXT
#   define C_FREE BSD43_C_FREE
#   define C_INTRANS BSD43_C_INTRANS
#   define MAXMEM BSD43_MAXMEM
#   define SZ_CMAP BSD43_SZ_CMAP
#   define cmtopg bsd43_cmtopg
#   define pgtocm bsd43_pgtocm
#endif SYSTYPE_BSD43
/*----- NUMIPS: STRIP MACRO PREFIXES (END) -----------------------------------*/


