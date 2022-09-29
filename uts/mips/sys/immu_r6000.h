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
 * |         950 DeGuigne Drive                                |
 * |         Sunnyvale, CA 94086                               |
 * |-----------------------------------------------------------|
 */
/* $Header: immu_r6000.h,v 1.4.1.4.1.4.1.2 90/10/16 12:19:13 beacker Exp $ */


/*	Copyright (c) 1984 AT&T	*/
/*	  All Rights Reserved  	*/

/*	THIS IS UNPUBLISHED PROPRIETARY SOURCE CODE OF AT&T	*/
/*	The copyright notice above does not evidence any   	*/
/*	actual or intended publication of such source code.	*/


/*  | <-   hard bits   -> |<-  soft  ->|<-hard->|     */
/*  +---------------------+---+--+--+--+-+-+--+-+     */
/*  |        pfn          |lck|sv|cw|nr|n|m|vr|g|     */
/*  +---------------------+---+--+--+--+-+-+--+-+     */
/*             22           1   1  1  3 1 1  1 1      */
/*                                                    */

/*
 *  pfn	- Page frame number
 *  n	- if set is non-cacheable
 *  m	- modified bit if clear a write will cause fault
 *  vr	- Valid and referenced bit.  If clear a fault will occur. Used as
 *	  reference bit also because there is no HW reference bit
 *  g	- Global so ignore pid in translations
 *  lck	- Lock page in cmos
 *  sv	- software valid bit set if page is valid
 *  cw  - software copy-on-write bit
 *  nr  - Reference count. Set to all ones on reference. Decremented
 *        (not below zero) when page is aged by ageregion.
 *        Pages with lower nr will be taken by getpages.
 */

/*	The page table entries are followed by a list of disk block 
 *	descriptors which give the location on disk where a
 *	copy of the corresponding page is found.  If the page
 *	is on swap, it is always a single block.  However, if
 *	it is on a regular file, a single page may correspond
 *	to a number of non-consecutive disk blocks.
 */

typedef union pde {
struct {
#ifdef MIPSEB
	unsigned int	pg_pfn:22,	/* HW: core page frame number or 0 */
			pg_lock:1,	/* SW: lock page */
			pg_sv:1,	/* SW: page valid bit */
			pg_cw:1,	/* SW: copy on write bit */
			pg_nr:3,	/* SW: page reference count */
			pg_n:1,		/* HW: non-cacheable bit */
			pg_m:1,		/* HW: modified (dirty) bit */
			pg_vr:1,	/* HW: valid bit (& SW referenced)*/
			pg_g:1;		/* HW: ignore pid bit */
#endif MIPSEB
#ifdef MIPSEL
	unsigned int	pg_g:1,		/* HW: ignore pid bit */
			pg_vr:1,	/* HW: valid bit (& SW referenced)*/
			pg_m:1,		/* HW: modified (dirty) bit */
			pg_n:1,		/* HW: non-cacheable bit */
			pg_nr:3,	/* SW: page reference count */
			pg_cw:1,	/* SW: copy on write bit */
			pg_sv:1,	/* SW: page valid bit */
			pg_lock:1,	/* SW: lock page */
			pg_pfn:22;	/* HW: core page frame number or 0 */
#endif MIPSEL
		} pgm;
	struct {
		uint	pg_pde;		/* Full page descriptor (table) entry */
		dbd_t	dbd;		/* dbd struct included here */
	} pgi;
	struct {
		uint	pg_pde;
		mmapinfo_t pgmmap;
	} pgmmapinfo;
	struct {
		uint	pg_pde;
		pguselink_t pguse;
	} pguseinfo;
	double	pg_double_alignment;	/* force alignment on 8-byte 	*/
					/* boundary (to page pguse	*/
					/* linkage work)		*/
} pde_t;

/*
 *	Page Table
 */

#define NPGPT		128		/* Nbr of pages per page table (seg). */

typedef union ptbl {
	int page[NPGPT];
} ptbl_t;

/* Page descriptor (table) entry dependent constants */

#define	NBPP		16384		/* Number of bytes per page */
#define	NBPPT		1024		/* Number of bytes per page table */
/* BPTSHFT is used as PNUMSHFT in too many places to change */
#define	BPTSHFT		14 		/* LOG2(NBPPT) if exact */
					/* A disk block is 512 bytes (sector) */
#define DPPSHFT		5		/* Shift for disk blocks per page. */

#define PNUMSHFT	14		/* Shift for page number from addr. */
#define PTE_PNUMSHFT	10		/* Shift for page number from pte. */
#define POFFMASK        0x3FFF		/* Mask for offset into page. */
#define PGFNMASK	0x3FFFFF	/* Mask page frame nbr after shift. */

/*	convert kuseg address to address of pde in kpteseg
 */
pde_t	*svtopde();

/* Page descriptor (table) entry field masks */

#define PG_ADDR		0xFFFFFC00	/* physical page address */
#define PG_PFNUM	PG_ADDR		/* physical page address */
#define PG_N		0x00000008	/* non-cacheable bit */
#define PG_M		0x00000004	/* modified bit	*/
#define PG_VR		0x00000002	/* valid and referenced  bit */
#define PG_G		0x00000001	/* global (ignore pid)  bit */
#define PG_LOCK		0x00000200	/* page lock bit (software) */
#define PG_NDREF	0x00000070	/* reference count (software) */
#define PG_SV		0x00000100	/* software valid bit (software) */
#define PG_CW		0x00000080	/* software copy on write bit */

/*
 * Strategy of 5/30/90:
 *	lotsfree is 2M bytes, but at most 1/4 of memory
 *	desfree is 1M bytes, but at most 1/8 of memory
 *	minfree is 384K bytes, but at most 1/2 of desfree
 */
#define	LOTSFREE	(2*1024 * 1024)
#define	LOTSFREEFRACT	4
#define	DESFREE		(1024 * 1024)
#define	DESFREEFRACT	8
#define	MINFREE		(384 * 1024)
#define	MINFREEFRACT	2

/*
 * Number of pages between the top of the user address space (highest
 * stack page) and the bottom of the kernel address space (KOSEG).
 * It is 32 on the 6000 to avoid having user stacks' tlb line be the
 * same as the u-area's.
 */
#define REDZONEPAGES	32
