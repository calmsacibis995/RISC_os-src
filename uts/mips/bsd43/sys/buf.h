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
/* $Header: buf.h,v 1.8.1.2 90/05/10 04:48:52 wje Exp $ */
/*
 * Copyright (c) 1982, 1986 Regents of the University of California.
 * All rights reserved.  The Berkeley software License Agreement
 * specifies the terms and conditions for redistribution.
 *
 *	@(#)buf.h	7.1 (Berkeley) 6/4/86
 */
/*----- COPYRIGHT (END) -----------------------------------------------------*/

#ifndef BSD43_
#    include <bsd43/bsd43_.h>
#endif BSD43_

/*
 * The header for buffers in the buffer pool and otherwise used
 * to describe a block i/o request is given here.  The routines
 * which manipulate these things are given in bio.c.
 *
 * Each buffer in the pool is usually doubly linked into 2 lists:
 * hashed into a chain by <dev,blkno> so it can be located in the cache,
 * and (usually) on (one of several) queues.  These lists are circular and
 * doubly linked for easy removal.
 *
 * There are currently three queues for buffers:
 *	one for buffers which must be kept permanently (super blocks)
 * 	one for buffers containing ``useful'' information (the cache)
 *	one for buffers containing ``non-useful'' information
 *		(and empty buffers, pushed onto the front)
 * The latter two queues contain the buffers which are available for
 * reallocation, are kept in lru order.  When not on one of these queues,
 * the buffers are ``checked out'' to drivers which use the available list
 * pointers to keep track of them in their i/o active queues.
 */

/*
 * Bufhd structures used at the head of the hashed buffer queues.
 * We only need three words for these, so this abbreviated
 * definition saves some space.
 */
struct bsd43_(bufhd)
{
	long	b_flags;		/* see defines below */
	struct	bsd43_(buf) *b_forw, *b_back;	/* fwd/bkwd pointer in chain */
};
struct bsd43_(buf)
{
	long	b_flags;		/* too much goes here to describe */
	struct	bsd43_(buf) *b_forw, *b_back;	/* hash chain (2 way street) */
	struct	bsd43_(buf) *av_forw, *av_back;	/* position on free list if not BUSY */
#define	bsd43_b_actf	av_forw			/* alternate names for driver queue */
#define	bsd43_b_actl	av_back			/*    head - isn't history wonderful */
	long	b_bcount;		/* transfer count */
	long	b_bufsize;		/* size of allocated buffer */
#define	bsd43_b_active b_bcount		/* driver queue head: drive active */
	short	b_error;		/* returned after I/O */
	dev_t	b_dev;			/* major+minor device name */
	union {
	    caddr_t b_addr;		/* low order core address */
	    int	*b_words;		/* words for clearing */
	    struct bsd43_(fs) *b_fs;		/* superblocks */
	    struct bsd43_(csum) *b_cs;		/* superblock summary information */
	    struct bsd43_(cg) *b_cg;		/* cylinder group block */
	    struct bsd43_(dinode) *b_dino;	/* ilist */
	    daddr_t *b_daddr;		/* indirect block */
	} b_un;
	daddr_t	b_blkno;		/* block # on device */
	long	b_resid;		/* words not transferred after error */
#define	bsd43_b_errcnt b_resid		/* while i/o in progress: # retries */
	struct  bsd43_(proc) *b_proc;	/* proc doing physical or swap I/O */
	int	(*b_iodone)();		/* function called by iodone */
	struct	vnode *b_vp;	/* vnode associated with block */
	int	b_pfcent;		/* center page when swapping cluster */
};

#define	BSD43_BQUEUES		4	/* number of free buffer queues */

#define	BSD43_BQ_LOCKED		0	/* super-blocks &c */
#define	BSD43_BQ_LRU		1	/* lru, useful buffers */
#define	BSD43_BQ_AGE		2	/* rubbish */
#define	BSD43_BQ_EMPTY		3	/* buffer headers with no memory */

#ifdef	KERNEL
#define	BSD43_BUFHSZ	512
#define BSD43_RND	(BSD43_MAXBSIZE/BSD43_DEV_BSIZE)
#if	((BSD43_BUFHSZ&(BSD43_BUFHSZ-1)) == 0)
#define	BSD43_BUFHASH(vp, dblkno)	\
	((struct bsd43_(buf) *)&bsd43_(bufhash)[((u_int)(vp)+(((int)(dblkno))/BSD43_RND))&(BSD43_BUFHSZ-1)])
#else
#define	BSD43_BUFHASH(vp, dblkno)	\
	((struct bsd43_(buf) *)&bsd43_(bufhash)[((u_int)(vp)+(((int)(dblkno))/BSD43_RND)) % BSD43_BUFHSZ])
#endif

struct	bsd43_(buf) *bsd43_(buf);		/* the buffer pool itself */
char	*bsd43_(buffers);
#ifdef vax
int	bsd43_(nbuf);			/* number of buffer headers */
int	bsd43_(bufpages);		/* number of memory pages in the buffer pool */
int	bsd43_(nswbuf);
#endif vax
#ifdef mips
extern	int	bsd43_(nbuf);		/* number of buffer headers */
extern	int	bsd43_(bufpages);	/* number of memory pages in the buffer pool */
extern	int	bsd43_(nswbuf);
#endif mips
struct	bsd43_(buf) *bsd43_(swbuf);		/* swap I/O headers */
struct	bsd43_(bufhd) bsd43_(bufhash)[BSD43_BUFHSZ];	/* heads of hash lists */
struct	bsd43_(buf) bsd43_(bfreelist)[BSD43_BQUEUES];	/* heads of available lists */
struct	bsd43_(buf) bsd43_(bswlist);		/* head of free swap header list */
struct	bsd43_(buf) *bsd43_(bclnlist);		/* head of cleaned page list */

struct	bsd43_(buf) *bsd43_(alloc)();
struct	bsd43_(buf) *bsd43_(realloccg)();
struct	bsd43_(buf) *bsd43_(baddr)();
struct	bsd43_(buf) *bsd43_(getblk)();
struct	bsd43_(buf) *bsd43_(geteblk)();
struct	bsd43_(buf) *bsd43_(getnewbuf)();
struct	bsd43_(buf) *bsd43_(bread)();
struct	bsd43_(buf) *bsd43_(breada)();
struct	vnode *bsd43_(devtovp)();

unsigned bsd43_(minphys)();
#endif

/*
 * These flags are kept in b_flags.
 */
#define	BSD43_B_WRITE		0x000000	/* non-read pseudo-flag */
#define	BSD43_B_READ		0x000001	/* read when I/O occurs */
#define	BSD43_B_DONE		0x000002	/* transaction finished */
#define	BSD43_B_ERROR		0x000004	/* transaction aborted */
#define	BSD43_B_BUSY		0x000008	/* not on av_forw/back list */
#define	BSD43_B_PHYS		0x000010	/* physical IO */
#define	BSD43_B_XXX		0x000020	/* was B_MAP, alloc UNIBUS on pdp-11 */
#define	BSD43_B_WANTED	0x000040	/* issue wakeup when BUSY goes off */
#define	BSD43_B_AGE		0x000080	/* delayed write for correct aging */
#define	BSD43_B_ASYNC		0x000100	/* don't wait for I/O completion */
#define	BSD43_B_DELWRI	0x000200	/* write at exit of avail list */
#define	BSD43_B_TAPE		0x000400	/* this is a magtape (no bdwrite) */
#define	BSD43_B_UAREA		0x000800	/* add u-area to a swap operation */
#define	BSD43_B_PAGET		0x001000	/* page in/out of page table space */
#define	BSD43_B_DIRTY		0x002000	/* dirty page to be pushed out async */
#define	BSD43_B_PGIN		0x004000	/* pagein op, so swap() can count it */
#define	BSD43_B_CACHE		0x008000	/* did bread find us in the cache ? */
#define	BSD43_B_INVAL		0x010000	/* does not contain valid info  */
#define	BSD43_B_LOCKED	0x020000	/* locked in core (not reusable) */
#define	BSD43_B_HEAD		0x040000	/* a buffer header, not a buffer */
#define	BSD43_B_BAD		0x100000	/* bad block revectoring in progress */
#define	BSD43_B_CALL		0x200000	/* call b_iodone from iodone */
#define	BSD43_B_NOCACHE	0x400000	/* don't cache block when released */

/*
 * Insq/Remq for the buffer hash lists.
 */
#define	bsd43_bremhash(bp) { \
	(bp)->b_back->b_forw = (bp)->b_forw; \
	(bp)->b_forw->b_back = (bp)->b_back; \
}
#define	bsd43_binshash(bp, dp) { \
	(bp)->b_forw = (dp)->b_forw; \
	(bp)->b_back = (dp); \
	(dp)->b_forw->b_back = (bp); \
	(dp)->b_forw = (bp); \
}

/*
 * Insq/Remq for the buffer free lists.
 */
#define	bsd43_bremfree(bp) { \
	(bp)->av_back->av_forw = (bp)->av_forw; \
	(bp)->av_forw->av_back = (bp)->av_back; \
}
#define	bsd43_binsheadfree(bp, dp) { \
	(dp)->av_forw->av_back = (bp); \
	(bp)->av_forw = (dp)->av_forw; \
	(dp)->av_forw = (bp); \
	(bp)->av_back = (dp); \
}
#define	bsd43_binstailfree(bp, dp) { \
	(dp)->av_back->av_forw = (bp); \
	(bp)->av_back = (dp)->av_back; \
	(dp)->av_back = (bp); \
	(bp)->av_forw = (dp); \
}

/*
 * Take a buffer off the free list it's on and
 * mark it as being use (B_BUSY) by a device.
 */
#define	bsd43_notavail(bp) { \
	int x = bsd43_(splbio)(); \
	bsd43_bremfree(bp); \
	(bp)->b_flags |= BSD43_B_BUSY; \
	bsd43_(splx)(x); \
}


/*
 * Zero out a buffer's data portion.
 */
#define	bsd43_clrbuf(bp) { \
	bsd43_(blkclr)((bp)->b_un.b_addr, (unsigned)(bp)->b_bcount); \
	(bp)->b_resid = 0; \
}

/*----- NUMIPS: STRIP MACRO PREFIXES (BEGIN) ---------------------------------*/
/* 
 * Strip off "BSD43_" and "bsd43_" for use with standard BSD code.
 * (GENERATED AUTOMATICALLY -- DON'T EDIT BY HAND)
 */
#ifdef SYSTYPE_BSD43
#   define BQUEUES BSD43_BQUEUES
#   define BQ_AGE BSD43_BQ_AGE
#   define BQ_EMPTY BSD43_BQ_EMPTY
#   define BQ_LOCKED BSD43_BQ_LOCKED
#   define BQ_LRU BSD43_BQ_LRU
#   define BUFHASH BSD43_BUFHASH
#   define BUFHSZ BSD43_BUFHSZ
#   define B_AGE BSD43_B_AGE
#   define B_ASYNC BSD43_B_ASYNC
#   define B_BAD BSD43_B_BAD
#   define B_BUSY BSD43_B_BUSY
#   define B_CACHE BSD43_B_CACHE
#   define B_CALL BSD43_B_CALL
#   define B_DELWRI BSD43_B_DELWRI
#   define B_DIRTY BSD43_B_DIRTY
#   define B_DONE BSD43_B_DONE
#   define B_ERROR BSD43_B_ERROR
#   define B_HEAD BSD43_B_HEAD
#   define B_INVAL BSD43_B_INVAL
#   define B_LOCKED BSD43_B_LOCKED
#   define B_NOCACHE BSD43_B_NOCACHE
#   define B_PAGET BSD43_B_PAGET
#   define B_PGIN BSD43_B_PGIN
#   define B_PHYS BSD43_B_PHYS
#   define B_READ BSD43_B_READ
#   define B_TAPE BSD43_B_TAPE
#   define B_UAREA BSD43_B_UAREA
#   define B_WANTED BSD43_B_WANTED
#   define B_WRITE BSD43_B_WRITE
#   define B_XXX BSD43_B_XXX
#   define RND BSD43_RND
#   define b_actf bsd43_b_actf
#   define b_active bsd43_b_active
#   define b_actl bsd43_b_actl
#   define b_errcnt bsd43_b_errcnt
#   define binshash bsd43_binshash
#   define binsheadfree bsd43_binsheadfree
#   define binstailfree bsd43_binstailfree
#   define bremfree bsd43_bremfree
#   define bremhash bsd43_bremhash
#   define clrbuf bsd43_clrbuf
#   define iodone bsd43_iodone
#   define iowait bsd43_iowait
#   define notavail bsd43_notavail
#endif SYSTYPE_BSD43
/*----- NUMIPS: STRIP MACRO PREFIXES (END) -----------------------------------*/


