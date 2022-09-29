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
/* $Header: pte.h,v 1.6.3.2 90/05/10 04:42:22 wje Exp $ */
#ifndef BSD43_
#    include <bsd43/bsd43_.h>
#endif BSD43_


/*
 * MIPS page table entry
 *
 * There are two major kinds of pte's: those which have ever existed (and are
 * thus either now in core or on the swap device), and those which have
 * never existed, but which will be filled on demand at first reference.
 * There is a structure describing each.  There is also an ancillary
 * structure used in page clustering.
 */

#ifndef LOCORE
struct bsd43_(pte)
{
#ifdef MIPSEB
unsigned int	pg_pfnum:20,		/* HW: core page frame number or 0 */
		pg_n:1,			/* HW: non-cacheable bit */
		pg_m:1,			/* HW: modified (dirty) bit */
		pg_v:1,			/* HW: valid bit */
		pg_g:1,			/* HW: ignore pid bit */
		:4,
		pg_swapm:1,		/* SW: page must be forced to swap */
		pg_fod:1,		/* SW: is fill on demand (=0) */
		pg_prot:2;		/* SW: access control */
#endif MIPSEB
#ifdef MIPSEL
unsigned int	pg_prot:2,		/* SW: access control */
		pg_fod:1,		/* SW: is fill on demand (=0) */
		pg_swapm:1,		/* SW: page must be forced to swap */
		:4,
		pg_g:1,			/* HW: ignore pid bit */
		pg_v:1,			/* HW: valid bit */
		pg_m:1,			/* HW: modified (dirty) bit */
		pg_n:1,			/* HW: non-cacheable bit */
		pg_pfnum:20;		/* HW: core page frame number or 0 */
#endif MIPSEL
};
struct bsd43_(hpte)
{
#ifdef MIPSEB
unsigned int	pg_pfnum:20,
		pg_high:12;		/* special for clustering */
#endif MIPSEB
#ifdef MIPSEL
unsigned int	pg_high:12,		/* special for clustering */
		pg_pfnum:20;
#endif MIPSEL
};
struct bsd43_(fpte)
{
#ifdef MIPSEB
unsigned int	pg_blknohi:20,		/* file system block number */
		:4,			/* overlays v,m,g,n bits */
		pg_blknolo:4,		/* file system block number */
		pg_fileno:1,		/* file mapped from or TEXT or ZERO */
		pg_fod:1,		/* is fill on demand (=1) */
		pg_prot:2;
#endif MIPSEB
#ifdef MIPSEL
unsigned int	pg_prot:2,
		pg_fod:1,		/* is fill on demand (=1) */
		pg_fileno:1,		/* file mapped from or TEXT or ZERO */
		pg_blknolo:4,		/* file system block number */
		:4,			/* overlays v,m,g,n bits */
		pg_blknohi:20;		/* file system block number */
#endif MIPSEL
};
#endif !LOCORE

#define	BSD43_PG_PFNUM	0xfffff000
#define	BSD43_PG_N		0x00000800
#define	BSD43_PG_M		0x00000400
#define	BSD43_PG_V		0x00000200
#define	BSD43_PG_G		0x00000100
#define	BSD43_PG_FILENO	0x00000008
#define	BSD43_PG_SWAPM	0x00000008
#define	BSD43_PG_FOD		0x00000004
#define	BSD43_PG_PROT		0x00000003

#define	BSD43_PG_FZERO	0
#define	BSD43_PG_FTEXT	1
#define	BSD43_PG_FMAX		(BSD43_PG_FTEXT)

#define	BSD43_PG_KR		0x00000000
#define	BSD43_PG_KW		0x00000001
#define	BSD43_PG_URKR		0x00000002
#define	BSD43_PG_UW		0x00000003

#define	BSD43_PROT_KR		0
#define	BSD43_PROT_KW		1
#define	BSD43_PROT_URKR	2
#define	BSD43_PROT_UW		3

#define	BSD43_PTE_PFNSHIFT	12
#define	BSD43_PTE_FILENOSHIFT	3

#define BSD43_DO_CACHE	0x0
#define BSD43_NO_CACHE	BSD43_PG_N

/*
 * deal with hi and lo portions of pg_blkno
 */
#define BSD43_PG_BLKNO(pte)	((((struct bsd43_(fpte) *)(pte))->pg_blknohi<<4) |  \
			((struct bsd43_(fpte) *)(pte))->pg_blknolo)
#define BSD43_PUT_PG_BLKNO(pte, blk)					\
	{ 							\
	register blkno = (blk);					\
	((struct bsd43_(fpte) *)(pte))->pg_blknohi = (blkno) >> 4;	\
	((struct bsd43_(fpte) *)(pte))->pg_blknolo = (blkno);		\
	}

/*
 * Pte related macros
 */
#define	bsd43_dirty(pte)	\
    ((bsd43_(pte))->pg_fod == 0 && (bsd43_(pte))->pg_pfnum && ((bsd43_(pte))->pg_m || (bsd43_(pte))->pg_swapm))

#ifndef LOCORE
#ifdef KERNEL

/* utilities defined in locore.s */
extern	struct bsd43_(pte) BSD43_(Sysmap)[];
extern	struct bsd43_(pte) BSD43_(Usrptmap)[];
extern	struct bsd43_(pte) bsd43_(usrpt)[];
extern	struct bsd43_(pte) bsd43_(msgbufmap)[];
extern	struct bsd43_(pte) bsd43_(camap)[];
extern	struct bsd43_(pte) BSD43_(Forkmap)[];
extern	struct bsd43_(pte) BSD43_(Swapmap)[];
extern	struct bsd43_(pte) BSD43_(Vfmap)[];
#endif KERNEL
#endif !LOCORE

/*----- NUMIPS: STRIP MACRO PREFIXES (BEGIN) ---------------------------------*/
/* 
 * Strip off "BSD43_" and "bsd43_" for use with standard BSD code.
 * (GENERATED AUTOMATICALLY -- DON'T EDIT BY HAND)
 */
#ifdef SYSTYPE_BSD43
#   define DO_CACHE BSD43_DO_CACHE
#   define NO_CACHE BSD43_NO_CACHE
#   define PG_BLKNO BSD43_PG_BLKNO
#   define PG_FILENO BSD43_PG_FILENO
#   define PG_FMAX BSD43_PG_FMAX
#   define PG_FOD BSD43_PG_FOD
#   define PG_FTEXT BSD43_PG_FTEXT
#   define PG_FZERO BSD43_PG_FZERO
#   define PG_G BSD43_PG_G
#   define PG_KR BSD43_PG_KR
#   define PG_KW BSD43_PG_KW
#   define PG_M BSD43_PG_M
#   define PG_N BSD43_PG_N
#   define PG_PFNUM BSD43_PG_PFNUM
#   define PG_PROT BSD43_PG_PROT
#   define PG_SWAPM BSD43_PG_SWAPM
#   define PG_URKR BSD43_PG_URKR
#   define PG_UW BSD43_PG_UW
#   define PG_V BSD43_PG_V
#   define PROT_KR BSD43_PROT_KR
#   define PROT_KW BSD43_PROT_KW
#   define PROT_URKR BSD43_PROT_URKR
#   define PROT_UW BSD43_PROT_UW
#   define PTE_FILENOSHIFT BSD43_PTE_FILENOSHIFT
#   define PTE_PFNSHIFT BSD43_PTE_PFNSHIFT
#   define PUT_PG_BLKNO BSD43_PUT_PG_BLKNO
#   define dirty bsd43_dirty
#endif SYSTYPE_BSD43
/*----- NUMIPS: STRIP MACRO PREFIXES (END) -----------------------------------*/


