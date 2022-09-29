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
/* $Header: immu.h,v 1.28.1.5.1.4.1.2 90/10/16 10:09:52 beacker Exp $ */

#ifndef	_SYS_IMMU_
#define	_SYS_IMMU_	1


/*	Copyright (c) 1984 AT&T	*/
/*	  All Rights Reserved  	*/

/*	THIS IS UNPUBLISHED PROPRIETARY SOURCE CODE OF AT&T	*/
/*	The copyright notice above does not evidence any   	*/
/*	actual or intended publication of such source code.	*/


/*
 * Page Descriptor (Table) Entry Definitions for MIPS CPU
 * The Disk Block Descriptor is included in the structure,
 * so pde's are 2 words long. 
 */

typedef struct dbd {
	uint	dbd_type  :  4;	/* The values for this field are given	*/
				/* below.				*/
	uint	dbd_swpi  :  4;	/* The index into swaptab for the	*/
				/* device this page is on if dbd_type	*/
				/* is DBD_SWAP.				*/
	uint	dbd_blkno : 24;	/* The block number or i_map index.	*/
} dbd_t;

/* mmapinfo struct overlays the dbd to contain information pertinent to
 * a page that is mmapped.  mmapped pages will never need a dbd.
 */
 typedef struct mmapinfo {
	uint	mmap_type  :  4;/* The values for this field should not	
				 * conflict with dbd_type values;
				 * normally, this field should equal
				 * DBD_MMAP				*/
	uint	mmap_fileno: 28;/* The file descriptor associated with	*/
				/* this page */
#define MAX_MMAP_FD 0x3FFFFFF	/* The type field restricts the size of	*/
				/* the above fd */
} mmapinfo_t;

/*
 * page use link structure, which overlays the dbd to contain information 
 * for a page which is in memory (the real dbd is in the pfdat for such a 
 * page).  We assert that the real dbd is in the pfdat whenever 
 * pg_sv is true, and
 * that otherwise the real dbd is in the pde.  Further, when the dbd is
 * in the pfdat, we assert that all pde's which contain the pfdat's
 * page frame number in pg_pfn have a pguselink instead of a dbd,
 * and are threaded from the pfdat's pguselink.  Thus all transitions
 * between the two modes must move the dbd and establish the thread.
 */
typedef struct pguselink {
	uint	pguse_type : 4; /* The values for this field should not 
				 * conflict with dbd_type values;
				 * normally, this field should equal
				 * DBD_PGUSE				*/
	uint	pguse_next : 28; /* kernel address of the next 
				  * pair, shifted right 3 bits, and
				  * masked with 0x0fffffff		*/
} pguselink_t;

/*
 * The bufinfo struct, which also overlays the dbd, contains information
 * pertaining the buffers which map the page. It is of interest only on
 * systems which have a pagesize greater than the file system block size
 * (e.g. R6000). All bufs which map this page are kept in a list headed
 * by buf_bufindx.
 */
typedef struct bufinfo {
	uint	buf_type    : 4,  /* == DBD_BUF */
			    : 4,
		buf_blkmask : 8,  /* free "chunks" of page */
		buf_bufindx : 16; /* first buffer that maps page */
} bufinfo_t;

/*
 * The pdeinfo struct, which also overlays the dbd, contains information
 * pertaining to the page tables contained in the page.
 */
typedef struct pdeinfo {
	uint	pde_type    : 4,  /* == DBD_PDE */
		            : 4,
		pde_blkmask : 16; /* free "chunks" of page */
} pdeinfo_t;

#ifdef R6000
#include "sys/immu_r6000.h"
#else
/*      	   <- hard bits -> |<-  soft bits   ->    */
/*  +---------------------+-+-+--+-+---+--+--+---+---+     */
/*  |        pfn          |n|m|vr|g|lck|sv|cw|   | nr|     */
/*  +---------------------+-+-+--+-+---+--+--+---+---+     */
/*             20          1 1  1  1 1   1  1  2   3       */
/*                                                         */

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
	unsigned int	pg_pfn:20,	/* HW: core page frame number or 0 */
			pg_n:1,		/* HW: non-cacheable bit */
			pg_m:1,		/* HW: modified (dirty) bit */
			pg_vr:1,	/* HW: valid bit (& SW referenced)*/
			pg_g:1,		/* HW: ignore pid bit */
			pg_lock:1,	/* SW: lock page */
			pg_sv:1,	/* SW: page valid bit */
			pg_cw:1,	/* SW: copy on write bit */
			:2,
			pg_nr:3;	/* SW: page reference count */
#endif MIPSEB
#ifdef MIPSEL
	unsigned int	pg_nr:3,	/* SW: page reference count */
			:2,
			pg_cw:1,	/* SW: copy on write bit */
			pg_sv:1,	/* SW: page valid bit */
			pg_lock:1,	/* SW: lock page */
			pg_g:1,		/* HW: ignore pid bit */
			pg_vr:1,	/* HW: valid bit (& SW referenced)*/
			pg_m:1,		/* HW: modified (dirty) bit */
			pg_n:1,		/* HW: non-cacheable bit */
			pg_pfn:20;	/* HW: core page frame number or 0 */
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

#define NPGPT		512		/* Nbr of pages per page table (seg). */

typedef union ptbl {
	int page[NPGPT];
} ptbl_t;

/* Page descriptor (table) entry dependent constants */

#define	NBPP		4096		/* Number of bytes per page */
#define	NBPPT		4096		/* Number of bytes per page table */
#define	BPTSHFT		12 		/* LOG2(NBPPT) if exact */
					/* A disk block is 512 bytes (sector) */
#define DPPSHFT		3		/* Shift for disk blocks per page. */

#define PNUMSHFT	12		/* Shift for page number from addr. */
#define PTE_PNUMSHFT	12		/* Shift for page number from pte. */
#define POFFMASK        0xFFF		/* Mask for offset into page. */
#define PGFNMASK	0xFFFFF		/* Mask page frame nbr after shift. */

/*	convert kuseg address to address of pde in kpteseg
 */
#define svtopde(x) (((uint)(x)>>PNUMSHFT) + (pde_t *)KPTEBASE)

/* Page descriptor (table) entry field masks */

#define PG_ADDR		0xFFFFF000	/* physical page address */
#define PG_PFNUM	PG_ADDR		/* physical page address */
#define PG_N		0x00000800	/* non-cacheable bit */
#define PG_M		0x00000400	/* modified bit	*/
#define PG_VR		0x00000200	/* valid and referenced  bit */
#define PG_G		0x00000100	/* global (ignore pid)  bit */
#define PG_LOCK		0x00000080	/* page lock bit (software) */
#define PG_NDREF	0x00000007	/* reference count (software) */
#define PG_SV		0x00000040	/* software valid bit (software) */
#define PG_CW		0x00000020	/* software copy on write bit */

/*
 * Strategy of 4/22/81:
 *	lotsfree is 512k bytes, but at most 1/4 of memory
 *	desfree is 256K bytes, but at most 1/8 of memory
 *	minfree is 96K bytes, but at most 1/2 of desfree
 */
#define	LOTSFREE	(128 * 4096)
#define	LOTSFREEFRACT	4
#define	DESFREE		(64 * 4096)
#define	DESFREEFRACT	8
#define	MINFREE		(24 * 4096)
#define	MINFREEFRACT	2

/*
 * Number of pages between the top of the user address space (highest
 * stack page) and the bottom of the kernel address space (KOSEG).
 */
#define REDZONEPAGES	1

#endif !R6000

#define NDPP		(NBPP/512)	/* Num 512-byte disk blocks per page */

#define SNUMSHFT	21		/* shift for segment number from addr */
#define SOFFMASK	0x1FFFFF	/* mask for offset into segment (2M) */

#define	NPTPP		16		/* Nbr of page tables per page.	*/
#define	NPTPPSHFT	4		/* Shift for NPTPP. */

#define PTESIZESHFT	3		/* Shift for sizeof(pde_t) */

/* convert pointer to pde to pointer to dbd */
#define pdetodbd(x)	((dbd_t *)((x)->pgi.dbd.dbd_type != DBD_PGUSE \
			? &((x)->pgi.dbd) \
			: &(pdetopfdat(x)->pf_dbd)))
#define pdetorealdbd(x)	((dbd_t *) &((x)->pgi.dbd))

#define pdetopfdat(x)	(pfntopfdat((x)->pgm.pg_pfn))

#define pdetommapinfo(x)	((mmapinfo_t *)&((x)->pgmmapinfo.pgmmap))
#define pdetopguselink(x)	((pguselink_t *)&((x)->pguseinfo.pguse))

#define pdetonextpde(x) (pgusetonextpde(pdetopguselink(x)))

#define pgusetonextpde(x) ((pde_t *)(((x)->pguse_type == DBD_PGUSE && \
				      (x)->pguse_next != 0) \
		? (((x)->pguse_next << 3) | 0x80000000) \
		: 0))

#define pdesetnextpde(x,y) (pgusesetnextpde(pdetopguselink(x),(y)))

#define pgusesetnextpde(x,y) ((x)->pguse_next = (((uint) (y)) >> 3), \
			    (x)->pguse_type = DBD_PGUSE)

#ifdef FRAG_PTS
#define pdetoregion(x) (pfntopfdat(svtop(x))->pf_regions[poff(x) / NBPPT])
#else
#define pdetoregion(x) (pfntopfdat(svtop(x))->pf_region)
#endif

#define pdetobufinfo(x)	((bufinfo_t *)((x)->pgi.dbd.dbd_type != DBD_PGUSE \
			? &((x)->pgi.dbd) \
			: &(pdetopfdat(x)->pf_dbd)))

#define pfdtobufinfo(x)	((bufinfo_t *)&((x)->pf_dbd))

#define pfdtopdeinfo(x)	((pdeinfo_t *)&((x)->pf_dbd))

#define	DBD_NONE	0	/* There is no copy of this page on 	*/
				/* disk.				*/
#define	DBD_SWAP	1	/* A copy of this page is on block nbr	*/
				/* dbd_blkno of the swap file		*/
				/* swptbl[dbd_swpi].			*/
#define	DBD_FILE	2	/* A copy of this page is on the file	*/
				/* described by the inode r_iptr.  The	*/
				/* dbd_blkno field is an index into the	*/
				/* i_map list pointed to by the inode.	*/
				/* It is the start of a list of block	*/
				/* number which contain a copy of the	*/
				/* page.				*/
#define DBD_LSTFILE	3	/* Same as DBD_FILE except that		*/
				/* this is the last page of the region	*/
#define	DBD_DZERO	4	/* This is a demand zero page.  No	*/
				/* space is allocated now.  When a 	*/
				/* fault occurs, allocate a page and	*/
				/* initialize it to all zeros.		*/
#define	DBD_DFILL	5	/* This is a demand fill page.  No	*/
				/* space is allocated now.  When a	*/
				/* fault occurs, allocate a page and	*/
				/* do not initialize it at all.  It	*/
				/* will be initialized by reading in 	*/
				/* data from disk.			*/
#define	DBD_MMAP	8	/* This dbd struct is really used	*/
				/* as a mmapinfo struct			*/
#define DBD_PGUSE	9	/* This dbd struct is really used	*/
				/* as a pguselink struct		*/
#define DBD_SYS		10	/* This dbd is for a system page	*/
#define DBD_PDE		11	/* This dbd is for a page of pde's	*/
				/* (only used in pfdat)			*/
#define DBD_BUF		12	/* This dbd struct is really used	*/
				/* as a bufinfo struct			*/

/* XXX check the use of this bit	*/
#define PG_P		PG_VR		/* page present bit	*/




/* byte addr to virtual page */

#define pnum(X)   ((uint)(X) >> PNUMSHFT) 

/* page offset */

#define poff(X)   ((uint)(X) & POFFMASK)

/* byte addr to segment */

#define snum(X)   ((uint)(X) >> SNUMSHFT)

/* segment offset */

#define soff(X)   ((uint)(X) & SOFFMASK)

/*	Disk blocks (sectors) and pages.
*/

#define	ptod(PP)	((PP) << DPPSHFT)		/* pages to sectors */
#define	dtop(DD)	(((DD) + NDPP - 1) >> DPPSHFT)  /* sectors to pages */

/*	Disk blocks (sectors) and bytes.
*/

#define	btod(BB)	(((BB) + NBPSCTR - 1) >> SCTRSHFT)

/*	Page tables (512 entries == 4096 bytes) to pages.
**	XXXX check this out  (see comment by NPTPP)
*/

#define	pttopgs(X)	((X + NPTPP - 1) >> NPTPPSHFT)
#define	pttob(X)	((X) << BPTSHFT)
#define	btopt(X)	(((X) + NBPPT - 1) >> BPTSHFT)

union ptbl *getptbl();		/* page table allocator */

extern int		nptalloced;
extern int		nptfree;

/* Form page descriptor (table) entry from modes and page frame
** number
*/

#define	mkpde(mode,pfn)	(mode | ((pfn) << PTE_PNUMSHFT))

/*	The following macros are used to check the value
 *	of the bits in a page descriptor (table) entry 
 */

/* Hardware bits: */
#define pg_isnoncache(pde) 	((pde)->pgm.pg_n)

/* Software bits: */
#define pg_isvalid(pde) 	((pde)->pgm.pg_sv)
#define pg_islocked(pde)	((pde)->pgm.pg_lock)

/*	The following macros are used to set the value
 *	of the bits in a page descrptor (table) entry 
 */

/* Hardware bits: */
#define pg_setnoncache(P) ((P)->pgi.pg_pde |= PG_N)	/* Set noncache bit.*/
#define pg_clrnoncache(P) ((P)->pgi.pg_pde &= ~PG_N)	/*Clear noncache bit.*/

#define pg_sethrdvalid(P)	((P)->pgi.pg_pde |= PG_VR) /* Set valid bit.*/
#define pg_clrhrdvalid(P)	((P)->pgi.pg_pde &= ~PG_VR) /*Clear valid bit.*/

#define pg_setmod(P)	((P)->pgi.pg_pde |= PG_M)	/* Set modify bit */
#define pg_clrmod(P)	((P)->pgi.pg_pde &= ~PG_M)	/* Clear modify bit */

#define pg_clrpfn(pde)	((pde)->pgm.pg_pfn = 0)		/* Clr page frame num */

/* the next two macros are used to manipulate copy on write faults	*/
	/* Set copy-on-write bit. 					 */
	/* Also, clear the modify bit so we'll get protection violations */
#define pg_setcw(P)	((P)->pgi.pg_pde |= PG_CW); pg_clrmod(P) 
	/* Clear copy-on-write bit. 					 */
	/* Also, set the modify bit so we won't get protection violations */
#define pg_clrcw(P)	((P)->pgi.pg_pde &= ~PG_CW); pg_setmod(P)

/* Note that the hardware valid and software refernce bits art the same */
#define pg_setref(P)	((P)->pgi.pg_pde |= PG_VR)
#define pg_clrref(P)	((P)->pgi.pg_pde &= ~PG_VR)

#define pg_setglob(P)	( (P)->pgi.pg_pde |= PG_G) /* Set global bit.	*/
#define pg_clrglob(P)	( (P)->pgi.pg_pde &= ~PG_G) /* Clear global bit. */

#define pg_iscw(pde)		((pde)->pgm.pg_cw)
#define pg_ismod(pde)		((pde)->pgm.pg_m)

/* Software bits: */
/* Note: due to lack of reference bits, "valid" bit is vr, nr, and sv */
					/* Set valid bit.*/
#define pg_setvalid(P)	(pg_setsftval(P),(P)->pgi.pg_pde |= PG_VR|PG_NDREF)
					/*Clear valid bit.*/
#define pg_clrvalid(P)	(pg_clrsftval(P),(P)->pgi.pg_pde &= ~(PG_VR|PG_NDREF))

extern pg_setsftval();
extern pg_clrsftval();

#define pg_setndref(P)	((P)->pgi.pg_pde |= PG_NDREF) /* Set need ref bit */
#define pg_clrndref(P)	((P)->pgi.pg_pde &= ~PG_NDREF) /* Clr need ref bit */

#define pg_setlock(P)	((P)->pgi.pg_pde |= PG_LOCK)	/* Set lock bit	*/
#define pg_clrlock(P)	((P)->pgi.pg_pde &= ~PG_LOCK) /* Clear lock bit	*/


/* XXX - can we axe these? */
/* Protection modes */
#define SEG_RO	1		/* read only */
#define SEG_RW	2		/* read/write */

/* Virtual addresses */
#define USRDATA		0x10000000	/* location of user data (256 Mb) */
#define USRTEXT		0x00100000	/* location of user text (4 Mb) */
#define UADDR		0xffffc000	/* location of U area */
#define KERNELSTACK	0xffffe000	/* location of kernel stack */
/* location of user's stack */
#define USERSTACK	(K0SEG - REDZONEPAGES*NBPP)
#define EA_SIZE		32		/* EMULATE_AREA size */
#define EMULATE_AREA	(USERSTACK - EA_SIZE)/* area for bp emulation */
#define UVPN		0x000ffffc	/* virtual page number of U */
#define KUSEG		0		/* kuseg user text/data */
#define K0SEG		0x80000000	/* k0seg kernel text/data cached */
#define K1SEG		0xa0000000	/* k1seg kernel text/data uncached */
#define K2SEG		0xc0000000	/* k2seg kernel mapped data */

/* XXXX what is this?	*/
#define flushaddr(vaddr)	unmaptlb(vaddr)

/*	The following variables describe the memory managed by
**	the kernel.  This includes all memory above the kernel
**	itself.
*/

extern int	kpbase;		/* The address of the start of	*/
				/* the first physical page of	*/
				/* memory above the kernel.	*/
				/* Physical memory from here to	*/
				/* the end of physical memory	*/
				/* is represented in the pfdat.	*/
extern int	syssegs[];	/* Start of the system segment	*/
				/* from which kernel space is	*/
				/* allocated.  The actual value	*/
				/* is defined in the vuifile.	*/
extern int	*win_ublk;	/* A window into which a	*/
				/* u-block can be mapped.	*/
extern pde_t	*kptbl;		/* Kernel page table.  Used to	*/
				/* map sysseg.			*/
extern int	maxmem;		/* Maximum available free	*/
				/* memory.			*/
extern int	maxmem16m;	/* Maximum available free	*/
				/* memory under 16M.		*/
extern int	freemem;	/* Current free memory.		*/
extern int	freemem16m;	/* Current free memory under 16M.	*/
extern int	availrmem;	/* Available resident (not	*/
				/* swapable) memory in pages.	*/
extern int	availsmem;	/* Available swapable memory in	*/
				/* pages.			*/

/*	Conversion macros
*/

/*	convert system address to physical address */
/*	(works only for k0seg and k1seg) */

#define svirtophys(x)	((uint)(x) & ~0xE0000000)

/*	Get page number from system virtual address.  */

#define	svtop(X)	((uint)svirtophys(X) >> PNUMSHFT) 
#define svtopfn(x)	(svtop(x))

/*	Get system virtual address from page number.  */
/*	(converts pfn to address in k0seg, cached)	*/

#define	ptosv(X)	(((uint)(X) << PNUMSHFT) | 0x80000000)
#define phystokv(x)	((x) | 0x80000000)

/*	The following routines are involved with the pfdat
**	table described in pfdat.h
*/

#define	kvtopfdat(kv)	(&pfdat[svtop(kv) - btoc(kpbase)])
#define	pfntopfdat(pfn)	(&pfdat[pfn - btoc(kpbase)])
#define	pfdattopfn(pfd)	(pfd - pfdat + btoc(kpbase))

/*	Test whether a virtual address is in the kernel dynamically
**	allocated area.
**	XXX - should this be in terms of syssegs[]?
*/

#define	iskvir(va)	((caddr_t)(va) >= (caddr_t)K2SEG && \
	(caddr_t)(va) < (caddr_t)(K2SEG + ctob(syssegsz)))

/*	Convert dynamically allocated kernel virtual address 
 *	(in k2seg) to address of kernel page table entry.
 */

#define kvtokptbl(X) (&kptbl[pnum((uint)(X) - (uint)K2SEG)])

/* flags used in ptmemall() call
*/

#define PHYSCONTIG 02
#define NOSLEEP    01

/*
 * flags used in pfree2() call
 */

#define PF_HEAD		0
#define PF_TAIL		1
#define PF_EITHER	2

#if defined(KERNEL) || defined(_RISCOS_VM_SYMBOLS)
/*
 * LOWPAGES is the number of pages from the beginning of the text region
 * to the beginning of text.
 * HIGHPAGES is the number of pages from the beginning of the stack region
 * to the beginning of the stack.
 */

#define	LOWPAGES	0
#define	HIGHPAGES	REDZONEPAGES

/*
 * Virtual memory related constants, all in bytes
 */
#ifndef MAXTSIZ
#define	MAXTSIZ		(24*1024*1024)		/* max text size */
#endif
#ifndef DFLDSIZ
#define	DFLDSIZ		(32*1024*1024)		/* initial data size limit */
#endif
#ifndef MAXDSIZ
#define	MAXDSIZ		(32*1024*1024)		/* max data size */
#endif
#ifndef	DFLSSIZ
#define	DFLSSIZ		(1024*1024)		/* initial stack size limit */
#endif
#ifndef	MAXSSIZ
#define	MAXSSIZ		MAXDSIZ			/* max stack size */
#endif

/*
 * Default sizes of swap allocation chunks (see dmap.h).
 * The actual values may be changed in vminit() based on MAXDSIZ.
 * With MAXDSIZ of 16Mb and NDMAP of 38, dmmax will be 1024.
 * All expressed in DEV_BSIZE blocks.
 */
#define	DMMIN	32			/* smallest swap allocation */
#define	DMMAX	4096			/* largest potential swap allocation */
#define	DMTEXT	1024			/* swap allocation for text */

/*
 * The size of the clock loop.
 */
#define	LOOPPAGES	(maxmem)

/*
 * The time for a process to be blocked before being very swappable.
 * This is a number of seconds which the system takes as being a non-trivial
 * amount of real time.  You probably shouldn't change this;
 * it is used in subtle ways (fractions and multiples of it are, that is, like
 * half of a ``long time'', almost a long time, etc.)
 * It is related to human patience and other factors which don't really
 * change over time.
 */
#define	MAXSLP 		20

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
#define	SAFERSS		6		/* nominal ``small'' resident set size
					   protected against replacement */

/*
 * DISKRPM is used to estimate the number of paging i/o operations
 * which one can expect from a single disk controller.
 */
#define	DISKRPM		60

/*
 * Klustering constants.  Klustering is the gathering
 * of pages together for pagein/pageout, while clustering
 * is the treatment of hardware page size as though it were
 * larger than it really is.
 *
 * KLMAX gives maximum cluster size in CLSIZE page (cluster-page)
 * units.  Note that KLMAX*CLSIZE must be <= dtoc(DMMIN) in dmap.h.
 */

#define	KLMAX	(4/CLSIZE)
#define	KLSEQL	(2/CLSIZE)		/* in klust if vadvise(VA_SEQL) */
#define	KLIN	(1/CLSIZE)		/* default data/stack in klust */
#define	KLTXT	(1/CLSIZE)		/* default text in klust */
#define	KLOUT	(4/CLSIZE)

/*
 * KLSDIST is the advance or retard of the fifo reclaim for sequential
 * processes data space.
 */
#define	KLSDIST	2		/* klusters advance/retard for seq. fifo */


/*
 * There are two clock hands, initially separated by HANDSPREAD bytes
 * (but at most all of user memory).  The amount of time to reclaim
 * a page once the pageout process examines it increases with this
 * distance and decreases as the scan rate rises.
 */
#define	HANDSPREAD	(2 * 1024 * 4096)

/*
 * The number of times per second to recompute the desired paging rate
 * and poke the pagedaemon.
 */
#define	RATETOSCHEDPAGING	4

/*
 * Pages per second to scan when out of memory (targeted toward ~10%
 * of cpu)
 */
#define	FASTSCAN	400

/*
 * Scan all of memory no more frequently than SCANSECS
 */
#define	SCANSECS	3

/*
 * Believed threshold (in megabytes) for which interleaved
 * swapping area is desirable.
 */
/* #define	LOTSOFMEM	2 */

/*
 * Paged text files that are less than PGTHRESH bytes may be swapped
 * in instead of paged in.
 */
#define PGTHRESH        (100 * 1024)

/*
 * Miscellaneous virtual memory subsystem variables and structures.
 */

int	avefree;		/* moving average of remaining free blocks */
int	avefree30;		/* 30 sec (avefree is 5 sec) moving average */
int	deficit;		/* estimate of needs of new swapped in procs */
int	nscan;			/* number of scans in last second */
int 	multprog;		/* current multiprogramming degree */
int	desscan;		/* desired pages scanned per second */
int	desscan16m;		/* desired pages scanned per second under 16M */

/* writable copies of tunables */
extern int maxpgio;		/* max paging i/o per sec before start swaps */
extern int maxslp;		/* max sleep time before very swappable */
extern int lotsfree;		/* max free before clock freezes */
extern int lotsfree16m;	/* max free under 16M before clock freezes */
extern int minfree;		/* minimum free pages before swapping begins */
extern int desfree;		/* no of pages to try to keep free via daemon */
extern int saferss;		/* no pages not to steal; decays with slptime */
extern int slowscan;		/* slowest scan rate, clusters/second */
extern int fastscan;		/* fastest scan rate, clusters/second */

#include <sys/pfdat.h>

#ifdef FRAG_PTS
/*
 * On some systems (especially the r6000), it is to our advantage to put
 * more than 1 page table in a physical page. PFRAG is the size which we
 * allocate multiples of. It corresponds to the r6000 page table size.
 */
#define PFRAG		1024
#define pfragtob(x)	((x) << 10)
#define btopfrag(x)	((x) >> 10) 
#define pde_bits(x)	((1 << btopfrag(x)) - 1)

/*
 * Macro and globals for page table sharing mechanism
 */
pfd_t	*pt_pflist;
int	pt_pfcount;

#define pt_pfadd(pfd)	{ \
	(pfd)->pf_next = pt_pflist; \
	if ((pfd)->pf_next) \
		(pfd)->pf_next->pf_prev = (pfd); \
	(pfd)->pf_prev = NULL; \
	pt_pflist = (pfd); \
	pt_pfcount++; \
}

#define pt_pfdel(pfd)	{ \
	if ((pfd)->pf_next) \
		(pfd)->pf_next->pf_prev = (pfd)->pf_prev; \
	if ((pfd)->pf_prev) \
		(pfd)->pf_prev->pf_next = (pfd)->pf_next; \
	if ((pfd) == pt_pflist) \
		pt_pflist = (pfd)->pf_next; \
	(pfd)->pf_next = NULL; \
	(pfd)->pf_prev = NULL; \
	pt_pfcount--; \
}
#endif

#endif defined(KERNEL) || defined(_RISCOS_VM_SYMBOLS)

#endif	_SYS_IMMU_
