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
/* $Header: buf.h,v 1.19.1.5.1.3.1.2 90/12/20 19:30:48 beacker Exp $ */

#ifndef	_SYS_BUF_
#define	_SYS_BUF_	1

/*	@(#)buf.h	2.1 88/05/18 4.0NFSSRC SMI;	from UCB 7.1 6/4/86	*/
/*
 * Copyright (c) 1982, 1986 Regents of the University of California.
 * All rights reserved.  The Berkeley software License Agreement
 * specifies the terms and conditions for redistribution.
 */

/* The struct has been slightly modified from the NFS4.0 tape.
 * Three fields were added to provide backward compatibility with
 * all of the drivers which use them (b_start, b_dmaaddr, b_dmalen).
 * In addition, two fields have had their types changed slightly,
 * again to provide backward compatibility (b_bcount, b_resid).
 * Both of these are declared here as u_ints, though the NFS4.0 source
 * has them being longs.
 */

/*
 * The header for buffers in the buffer pool and otherwise used
 * to describe a block i/o request is given here.  The routines
 * which manipulate these things are given in vfs_bio.c.
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
typedef	struct bufhd
{
	long	b_flags;		/* see defines below */
	struct	buf *b_forw, *b_back;	/* fwd/bkwd pointer in chain */
} bufhd_t;
typedef	struct buf
{
	long	b_flags;		/* too much goes here to describe */
	struct	buf *b_forw, *b_back;	/* hash chain (2 way street) */
	struct	buf *av_forw, *av_back;	/* position on free list if not BUSY */
#define	b_actf	av_forw			/* alternate names for driver queue */
#define	b_actl	av_back			/*    head - isn't history wonderful */
	u_int	b_bcount;		/* transfer count in bytes */
	long	b_bufsize;		/* size in bytes of allocated buffer*/
	short	b_error;		/* returned after I/O */
	dev_t	b_dev;			/* major+minor device number */
	union {
	    caddr_t b_addr;		/* low order core address */
	    int	*b_words;		/* words for clearing */
	    struct fs *b_fs;		/* superblocks */
	    struct csum *b_cs;		/* superblock summary information */
	    struct cg *b_cg;		/* cylinder group block */
	    struct dinode *b_dino;	/* ilist */
	    daddr_t *b_daddr;		/* indirect block */
	} b_un;
	daddr_t	b_blkno;		/* block # on device */
	u_int	b_resid;		/* words not transferred after error*/
#define	b_errcnt b_resid		/* while i/o in progress: # retries */
	struct  proc *b_proc;		/* proc doing physical or swap I/O */
	int	(*b_iodone)();		/* function called by iodone */
	struct	vnode *b_vp;		/* vnode associated with block */
	int	b_pfcent;		/* center page when swapping cluster */
	/* ...and some MIPS specials... */
	time_t	b_start;		/* request start time */
	caddr_t	b_dmaaddr;		/* dma address of buffer */
	ushort	b_dmalen;		/* length of dma region */
	u_int	b_dkspare;		/* spare filed used by dkip */
	struct	buf *b_pfchain;		/* list of bufs sharing pages */
} buf_t;

#define	BQUEUES		4		/* number of free buffer queues */

#define	BQ_LOCKED	0		/* super-blocks &c */
#define	BQ_LRU		1		/* lru, useful buffers */
#define	BQ_AGE		2		/* rubbish */
#define	BQ_EMPTY	3		/* buffer headers with no memory */

#define paddr(X)	(paddr_t)(X->b_un.b_addr)

#ifdef	KERNEL
#define	BUFHSZ	512
#ifdef RISCOS
#define RND	(FSMAXBSIZE/DEV_BSIZE)
#else
#define RND	(MAXBSIZE/DEV_BSIZE)
#endif
#define VPRND	32
#define	BUFHASH(dvp, dblkno)	((struct buf *) \
	&bufhash[(((u_int)(dvp)/VPRND)+(((int)(dblkno))/RND)) % BUFHSZ])
extern	struct	buf *buf;		/* the buffer pool itself */
extern	char	*buffers;
extern	int	bufpages;		/* number of memory pages in the buffer pool */
extern	struct	buf *swbuf;		/* swap I/O headers */
extern	int	nswbuf;
extern	daddr_t rablock;                /* block to be read ahead */
extern	int     rasize;                 /* size of block in rablock */
extern	struct	bufhd bufhash[BUFHSZ];	/* heads of hash lists */
extern	struct	buf bfreelist[BQUEUES];	/* heads of available lists */
extern	struct	buf bswlist;		/* head of free swap header list */
extern	struct	buf *bclnlist;		/* head of cleaned page list */

extern	struct	buf *alloc();
extern	struct	buf *realloccg();
extern	struct	buf *baddr();
extern	struct	buf *getblk();
extern	struct	buf *geteblk();
extern	struct	buf *getnewbuf();
extern	struct	buf *bread();
extern	struct	buf *breada();
extern	struct	vnode *bdevvp();

extern	unsigned minphys();
#endif

/*
 * These flags are kept in b_flags.
 */
#define	B_WRITE		0x000000	/* non-read pseudo-flag */
#define	B_READ		0x000001	/* read when I/O occurs */
#define	B_DONE		0x000002	/* transaction finished */
#define	B_ERROR		0x000004	/* transaction aborted */
#define	B_BUSY		0x000008	/* not on av_forw/back list */
#define	B_PHYS		0x000010	/* physical IO */
#define	B_PARTIAL	0x000020	/* Even more scatter/gather stuff */
#define	B_WANTED	0x000040	/* issue wakeup when BUSY goes off */
#define	B_AGE		0x000080	/* delayed write for correct aging */
#define	B_ASYNC		0x000100	/* don't wait for I/O completion */
#define	B_DELWRI	0x000200	/* write at exit of avail list */
#define	B_TAPE		0x000400	/* this is a magtape (no bdwrite) */
#define	B_UAREA		0x000800	/* add u-area to a swap operation */
#define	B_PAGET		0x001000	/* page in/out of page table space */
#define	B_DIRTY		0x002000	/* dirty page to be pushed out async */
#define	B_PGIN		0x004000	/* pagein op, so swap() can count it */
#define	B_CACHE		0x008000	/* did bread find us in the cache ? */
#define	B_INVAL		0x010000	/* does not contain valid info  */
#define	B_LOCKED	0x020000	/* locked in core (not reusable) */
#define	B_HEAD		0x040000	/* a buffer header, not a buffer */
#define	B_LRU		0x080000	/* buffer is in the LRU queue */
#define	B_BAD		0x100000	/* bad block revectoring in progress */
#define	B_CALL		0x200000	/* call b_iodone from iodone */
#define	B_NOCACHE	0x400000	/* don't cache block when released */
#define B_SWAP		0x1000000	/* blk is on swap, paddr is a physadr */
#define B_FORMAT	0x2000000	/* Stolen by device drivers for other uses */
#define B_SPECIAL	0x2000000	/* Official device driver flag */
#define B_VD_SYNC	0x10000000	/* This is a vdisk sync buffer */
#define B_VD_INTERNAL	0x20000000	/* This is a vdisk internal io */
#define B_VDBUF		0x40000000	/* this is a virtual disk buffer */
#define B_VD_IODONE	0x80000000	/* one of the mirror I/O succeeded */

/*
 * Insq/Remq for the buffer hash lists.
 */
#define	bremhash(bp) { \
	(bp)->b_back->b_forw = (bp)->b_forw; \
	(bp)->b_forw->b_back = (bp)->b_back; \
}
#define	binshash(bp, dp) { \
	(bp)->b_forw = (dp)->b_forw; \
	(bp)->b_back = (dp); \
	(dp)->b_forw->b_back = (bp); \
	(dp)->b_forw = (bp); \
}

/*
 * Insq/Remq for the buffer free lists.
 */
#define	bremfree(bp) { \
	(bp)->av_back->av_forw = (bp)->av_forw; \
	(bp)->av_forw->av_back = (bp)->av_back; \
}
#define	binsheadfree(bp, dp) { \
	(dp)->av_forw->av_back = (bp); \
	(bp)->av_forw = (dp)->av_forw; \
	(dp)->av_forw = (bp); \
	(bp)->av_back = (dp); \
}
#define	binstailfree(bp, dp) { \
	(dp)->av_back->av_forw = (bp); \
	(bp)->av_back = (dp)->av_back; \
	(dp)->av_back = (bp); \
	(bp)->av_forw = (dp); \
}

/*
 * Unlink a buffer from the available list and mark it busy.
 * (internal interface).  notavailspl is the same as notavail, but
 * it assumes that the processor is already at splbio.
 */
#define	notavailspl(bp, dp) { \
	(bp)->av_back->av_forw = (bp)->av_forw; \
	(bp)->av_forw->av_back = (bp)->av_back; \
	(bp)->av_forw = (bp); \
	(bp)->av_back = (bp); \
	(bp)->b_flags |= B_BUSY; \
	(dp)->b_bcount--; \
}


/*
 * Zero out a buffer's data portion.
 */
#ifdef RISCOS
#define	clrbuf(bp) { \
	bzero((bp)->b_un.b_addr, (unsigned)(bp)->b_bcount); \
	(bp)->b_resid = 0; \
}
#else
#define	clrbuf(bp) { \
	blkclr((bp)->b_un.b_addr, (unsigned)(bp)->b_bcount); \
	(bp)->b_resid = 0; \
}
#endif

struct	pfree	{
	int	b_flags;
	struct	buf	*av_forw;
};
extern	struct	pfree	pfreelist;
extern	int		pfreecnt;
extern	struct	buf	pbuf[];

#ifdef	INKERNEL
extern	long	bio_memlimit;
extern	long	bio_maxbsize_log2;
extern	long	bio_maxbsize;
extern	long	bio_maxpages;
/*
extern	long	bio_bbsperbucketshift;
extern	long	bio_bbsperbucketminusone;
extern	long	bio_bbsperlbminusone;
*/
extern	unchar	bio_usedmem[];
extern	struct	map bio_memmap[];

#if RISCOS
/* These are included for backwards compatibility since a bunch of drivers
 * use these.  The drivers should be changed eventually.
 */
#define	BTOBB(bytes)	(((bytes) + NBPSCTR - 1) >> SCTRSHFT)
#define	BTOBBT(bytes)	((bytes) >> SCTRSHFT)
#define	BBTOB(bbs)	((bbs) << SCTRSHFT)
#endif /* RISCOS */
#endif /* INKERNEL */

#endif	_SYS_BUF_
