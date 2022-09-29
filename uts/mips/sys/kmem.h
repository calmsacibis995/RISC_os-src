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
/* $Header: kmem.h,v 1.9.1.3 90/05/10 06:26:19 wje Exp $ */

/*
 * Copyright (c) 1987 Regents of the University of California.
 * All rights reserved.
 *
 * Redistribution and use in source and binary forms are permitted
 * provided that the above copyright notice and this paragraph are
 * duplicated in all such forms and that any documentation,
 * advertising materials, and other materials related to such
 * distribution and use acknowledge that the software was developed
 * by the University of California, Berkeley.  The name of the
 * University may not be used to endorse or promote products derived
 * from this software without specific prior written permission.
 * THIS SOFTWARE IS PROVIDED ``AS IS'' AND WITHOUT ANY EXPRESS OR
 * IMPLIED WARRANTIES, INCLUDING, WITHOUT LIMITATION, THE IMPLIED
 * WARRANTIES OF MERCHANTIBILITY AND FITNESS FOR A PARTICULAR PURPOSE.
 *
 *	@(#)malloc.h	7.7 (Berkeley) 6/27/88
 */

#define KMEMSTATS	1

/*
 * flags to malloc
 */
#define M_WAITOK	0x0000
#define M_NOWAIT	0x0001	/* Don't sleep at all */
#define M_FREQUENT	0x0002	/* Implies frequent requests of this size */
#define M_CONTIGUOUS	0x0004	/* Allocate physically contiguous memory */

/*
 * Types of memory to be allocated
 */
#define	M_MISC		0	/* miscllaneous requests */
#define M_MBUF		1	/* mbuf */
#define M_DEVBUF	2	/* device driver memory */
#define	M_SOCKET	3	/* socket structure */
#define	M_PCB		4	/* protocol control block */
#define	M_RTABLE	5	/* routing tables */
#define	M_HTABLE	6	/* IMP host tables */
#define	M_FTABLE	7	/* fragment reassembly header */
#define	M_ZOMBIE	8	/* zombie proc status */
#define	M_IFADDR	9	/* interface address */
#define	M_SOOPTS	10	/* socket options */
#define	M_SONAME	11	/* socket name */
#define M_NAMEI		12	/* namei path name buffer */
#define M_GPROF		13	/* kernel profiling buffer */
#define M_IOCTLOPS	14	/* ioctl data buffer */
#define M_SUPERBLK	15	/* super block data */
#define M_CRED		16	/* credentials */
#define M_BUF		17	/* buffer headers and buffers */
#define M_STREAMS	18	/* stream buffers, etc. */
#define M_PTE		19	/* page of page table entries */
#define M_PT_LIST	20	/* list of page table pages */
#define M_QUOTA		21	/* quota information */
#define M_TEMP		49	/* misc temporary data buffers */
#define M_LAST		50

struct kmemstats {
	long	ks_inuse;	/* # of packets of this type currently in use */
	long	ks_calls;	/* total packets of this type ever allocated */
	long 	ks_memuse;	/* total memory held in bytes */
	u_short	ks_limblocks;	/* number of times blocked for hitting limit */
	u_short	ks_mapblocks;	/* number of times blocked for kernel map */
	long	ks_maxused;	/* maximum number ever used */
	long	ks_limit;	/* most that are allowed to exist */
};


/* Description of a header for a free block (used only when a block is
 * placed on the free list of a buffer).  All chunks within a page are
 * singly linked list on the kf_queue linked list.  All free headers
 * of different pages are singly linked via kf_next field.
 *
 * Note that MINBUCKET is at least as big as the freehdr.
 */

typedef	struct freehdr	freehdr_t;
struct freehdr {
	freehdr_t	*kf_next;	/* next freehdr in the free list */
	caddr_t		kf_queue;	/* pointer to other free chunks */
	int		kf_freecnt;	/* #chuncks free in this page */
	short		kf_pagecnt;	/* #pages in this buffer */
	short		kf_dummy;	/* reserved space for future */
};

/*
 * Array of descriptors that describe the contents of each page
 */

typedef	struct kmemusage	kmemusage_t;
struct kmemusage {
    u_char	ku_indx;	/* bucket index */
    u_char	ku_flags;	/* flags (see below) */
    union {
	u_short	pagecnt;	/* #pages allocated (for big allocation) */
	u_short freehdr;	/* byte offset of header within first page */
    } ku_un;
};
#define	ku_freehdr  ku_un.freehdr
#define	ku_pagecnt  ku_un.pagecnt

/* flags */
#define	KU_QUEUE	0x01	/* some chunks are on bucket list */
#define	KU_UNDER_16M	0x02	/* Physical memory is under 16M */
#define	KU_DEALLOC	0x40	/* page being deallocated */

 
/*
 * Set of buckets for each size of memory block that is retained
 */
struct kmembuckets {
	freehdr_t *kb_next;	/* list of free blocks */
	long	kb_calls;	/* total calls to allocate this size */
	long	kb_total;	/* total number of blocks allocated */
	long	kb_totalfree;	/* # of free elements in this bucket */
	long	kb_elmsz;	/* size of each element in this bucket */
	long	kb_elmpercl;	/* # of elements in this sized allocation */
	long	kb_highwat;	/* high water mark */
	long	kb_avguse;	/* average memory used  */
	long	kb_inuse;	/* # of packets currently in use */
	long	kb_maxused;	/* max used within last interval */
};

/*
 * Sorted list of bucket sizes to bucket index (used to perform a binary
 * search)
 */
#ifdef KERNEL

struct kmembsz {
	uint	kb_index:8;	/* bucket index */
	uint	kb_size:24;	/* bucket size */
};
extern struct kmembsz kmembsz[];


/* Kernel memory allocation info */
typedef struct {
	int	ki_recov;		/* #pages recovered */
	int	ki_dfree;		/* Delayed free pages */
	int	ki_srecovcalls;		/* #calls to soft recover memory */
	int	ki_hrecovcalls;		/* #calls to hard recover memory */
	int	ki_unmapped;		/* #pages unmapped */
} kmeminfo_t;


/* Data structures used to support fast memory allocation. */

#define	KMEMPOOLMAGIC	0x676B6D72

typedef struct kmempool {
	struct kmempool	*km_next;	/* linked list of memory pools */
	caddr_t	km_queue;		/* free chunks in this pool */
	int	km_totalfree;		/* current free count */
	int	km_inuse;		/* how many in use */
	int	km_maxused;		/* maximum ever used */
	int	km_size;		/* size of each chunk */
	int	km_flags;		/* flags to be passed to kmemalloc */
	int	km_chunks;		/* chunks to be allocated */
	int	km_highwater;		/* free threshold mark */
	int	km_limit;		/* max chunks to be ever allocated */
	char	*km_name;		/* name of this local pool */
	int	km_type;		/* type to be passed to kmemalloc */
	int	km_magic;		/* MAGIC number for sanity check */
	int	km_calls;		/* #times fast alloc called */
	int	km_fcalls;		/* #times fast free called */
	int	km_fail;		/* #times failed to alloc a buffer */
} kmempool_t;

extern kmempool_t	*kmempoollist;	/* linked list of local memory pools */

/*
 * Constants for setting the parameters of the kernel memory allocator.
 *
 * 2 ** MINBUCKET is the smallest unit of memory that will be
 * allocated. It must be at least large enough to hold a pointer.
 *
 * Units of memory less or equal to MAXALLOCSAVE will permanently
 * allocate physical memory; requests for these size pieces of
 * memory are quite fast. Allocations greater than MAXALLOCSAVE must
 * always allocate and free physical memory; requests for these
 * size allocations should be done infrequently as they will be slow.
 * Constraints: NBPC <= MAXALLOCSAVE <= 2 ** (MINBUCKET + 14)
 * and MAXALLOCSIZE must be a power of two.
 */
#define MINBUCKET	4		/* 4 => min allocation of 16 bytes */
#define	BUCKETSHFT	MINBUCKET
#define MINALLOCSIZE	(1 << MINBUCKET)

/*
 * Turn virtual addresses into kmem map indicies
 */


#define btokmemx(addr)	((uint)(addr)/NBPC - malloc_base)
#define btokup(addr)	(&kmemusage[((unsigned)(addr)>> BPCSHIFT) -malloc_base])

extern struct kmemstats kmemstats[];
extern kmemusage_t *kmemusage;
extern struct kmembuckets bucket[];
extern caddr_t kmemalloc();
extern caddr_t kmemzalloc();
extern void kmemfree();
extern int maxbucket;
extern int maxallocsave;
extern int fixed_buckets[];

/* Data structures and variables to keep track of kernel memory requests. */

#define	LOG_KMEMREQ	0		/* Log last few kmem requests */

#if LOG_KMEMREQ

typedef struct kmemreq {
	char	kreq_reqtype;		/* type of request */
	char	kreq_reserved;
	short	kreq_seqnum;		/* Sequence number */
	caddr_t	kreq_addr;		/* virtual address */
	int	kreq_args[2];		/* Extra arguments */
} kmemreq_t;

/* kreq_reqtype */

#define	KREQ_NONE	0
#define	KREQ_ALLOC	1		/* args: size flags */
#define	KREQ_FREE	2		/* args: calc-size flags */
#define	KREQ_ZALLOC	3		/* args: size flags */

extern	void log_kmemreq();

#endif

/* The following macros map NFS4.0 memory allocator interface */

#define	NFS4_KMEM_MACROS	1		/* set it to enable macros */

#if	NFS4_KMEM_MACROS

#define	kmem_alloc(amount) kmemalloc(amount, M_MISC, M_WAITOK)
#define	kmem_zalloc(amount) kmemzalloc(amount, M_MISC, M_WAITOK)
#define	kmem_free(ap,nbytes) kmemfree(ap, M_MISC, M_WAITOK)
#define	kmem_fast_alloc(base, size, chunks) \
		kmemalloc(size, M_MISC, M_WAITOK)
#define	kmem_fast_free(base, p) kmemfree(p, M_MISC, M_WAITOK)
#define	kmem_fast_zalloc(base, size, chunks) \
		kmemzalloc(size, M_MISC, M_WAITOK)

#else	!NFS4_KMEM_MACROS

extern caddr_t 	kmem_alloc(/* amount */);
extern caddr_t 	kmem_zalloc(/* amount */);
extern void 	kmem_free(/* ap, nbytes */);
extern caddr_t 	kmem_fast_alloc(/* base, size, chunks */);
extern void 	kmem_fast_free(/* base, p */);
extern caddr_t 	kmem_fast_zalloc(/* base, size, chunks */);

#endif	!NFS4_KMEM_MACROS


/* The following provides interface to old (SGI) memory allocator */

#define	SGI_KMEM_MACROS	1	/* set it to enbale macros */

#if	SGI_KMEM_MACROS

#define kern_malloc(amount)	kmemalloc(amount, M_MISC, M_WAITOK)
#define kern_free(ap)		kmemfree(ap, M_MISC, M_WAITOK)
#define kern_calloc(nelem,elsize)  kmemzalloc((nelem)*(elsize), M_MISC, M_WAITOK)

#else	!SGI_KMEM_MACROS

extern caddr_t	kern_malloc(/* amount */);
extern void	kern_free(/* ap */);
extern caddr_t	kern_calloc(/* nelem, elsize */);

#endif	!SGI_KMEM_MACROS

#endif KERNEL
