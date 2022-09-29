/* $Header: buf.h,v 1.2 90/01/23 13:46:14 huang Exp $ */
/* $Copyright$ */

#ifndef	_SYS_BUF_
#define	_SYS_BUF_	1


/*	Copyright (c) 1984 AT&T	*/
/*	  All Rights Reserved  	*/

/*	THIS IS UNPUBLISHED PROPRIETARY SOURCE CODE OF AT&T	*/
/*	The copyright notice above does not evidence any   	*/
/*	actual or intended publication of such source code.	*/


/*
 *	Each buffer in the pool is usually doubly linked into 2 lists:
 *	the device with which it is currently associated (always)
 *	and also on a list of blocks available for allocation
 *	for other use (usually).
 *	The latter list is kept in last-used order, and the two
 *	lists are doubly linked to make it easy to remove
 *	a buffer from one list when it was found by
 *	looking through the other.
 *	A buffer is on the available list, and is liable
 *	to be reassigned to another disk block, if and only
 *	if it is not marked BUSY.  When a buffer is busy, the
 *	available-list pointers can be used for other purposes.
 *	Most drivers use the forward ptr as a link in their I/O active queue.
 *	A buffer header contains all the information required to perform I/O.
 *	Most of the routines which manipulate these things are in bio.c.
 */

typedef struct	buf
{
	int	b_flags;		/* see defines below */
	struct	buf *b_forw;		/* headed by d_tab of conf.c */
	struct	buf *b_back;		/*  "  */
	struct	buf *av_forw;		/* position on free list, */
	struct	buf *av_back;		/*     if not BUSY*/
	dev_t	b_dev;			/* major+minor device name */
	unsigned b_bcount;		/* transfer count */
	union {
	    caddr_t b_addr;		/* low order core address */
	    int	*b_words;		/* words for clearing */
	    daddr_t *b_daddr;		/* disk blocks */
#ifdef SV_BFS
	    struct fs *b_fs;	/* superblocks */
	    struct cg *b_cg;	/* cylinder group block */
	    struct bfs_dinode *b_dino;	/* ilist */
#endif
	} b_un;

#define paddr(X)	(paddr_t)(X->b_un.b_addr)

	daddr_t	b_blkno;		/* block # on device */
	char	b_error;		/* returned after I/O */
	unsigned int b_resid;		/* words not transferred after error */
	time_t	b_start;		/* request start time */
	struct  proc  *b_proc;		/* process doing physical or swap I/O */
	ushort	b_length;		/* length of buffer memory */
	caddr_t	b_dmaaddr;		/* dma address of buffer */
	ushort	b_dmalen;		/* length of dma region */
} buf_t;

#define	BQUEUES		2		/* number of free buf queues */
#define	BQ_LRU		0		/* same as old bfreelist */
#define	BQ_EMPTY	1		/* bufs with no mem. */
extern	struct	buf	bfreelist[];	/* head of various free lists */
extern	struct	buf	*buf;		/* base of the buffer pool */

struct	pfree	{
	int	b_flags;
	struct	buf	*av_forw;
};
extern	struct	pfree	pfreelist;
extern	int		pfreecnt;
extern	struct	buf	pbuf[];
extern	char		buffers[];


/*
 *	These flags are kept in b_flags.
 */
#define B_WRITE   0x0000	/* non-read pseudo-flag */
#define B_READ    0x0001	/* read when I/O occurs */
#define B_DONE    0x0002	/* transaction finished */
#define B_ERROR   0x0004	/* transaction aborted */
#define B_BUSY    0x0008	/* not on av_forw/back list */
#define B_PHYS    0x0010	/* Physical IO */
#define B_MAP     0x0020	/* not used */
#define B_WANTED  0x0040	/* issue wakeup when BUSY goes off */
#define B_AGE     0x0080	/* delayed write for correct aging */
#define B_ASYNC   0x0100	/* don't wait for I/O completion */
#define B_DELWRI  0x0200	/* delayed write - wait until buffer needed */
#define B_OPEN    0x0400	/* open routine called */
#define B_STALE   0x0800
#define B_VERIFY  0x1000
#define B_FORMAT  0x2000
#define B_RAMRD   0x4000
#define B_RAMWT	  0x8000
#define B_SWAP	  0x10000	/* block is on swap, paddr is physical addr */

/*
 *	Fast access to buffers in cache by hashing.
 */

#define bhash(d,b)	((struct buf *)&hbuf[((int)(d)+(int)(b))&v.v_hmask])

struct	hbuf
{
	int	b_flags;
	struct	buf	*b_forw;
	struct	buf	*b_back;
};

extern	struct	hbuf	hbuf[];

/*
 * Pick up the device's error number and pass it to the user;
 * if there is an error but the number is 0 set a generalized code
 */
#define geterror(bp) \
{\
\
	if (bp->b_flags&B_ERROR)\
		if ((u.u_error = bp->b_error)==0)\
			u.u_error = EIO;\
}

/*
 * BIO parameterization.  A basic block (BB) is the lowest size of filesystem
 * allocation, and must == NBPSCTR.  Length units given to bio are in
 * BB's
 */
#define	BBSHIFT	9
#define	BBSIZE	(1<<BBSHIFT)
#define	BBMASK	(BBSIZE-1)
#define	BTOBB(bytes)	(((bytes) + BBSIZE - 1) >> BBSHIFT)
#define	BTOBBT(bytes)	((bytes) >> BBSHIFT)
#define	BBTOB(bbs)	((bbs) << BBSHIFT)

#ifdef	INKERNEL
extern	long	bio_memlimit;
extern	long	bio_maxbsize_log2;
extern	long	bio_maxbsize;
extern	long	bio_maxpages;
extern	long	bio_bbsperbucketshift;
extern	long	bio_bbsperbucketminusone;
extern	long	bio_bbsperlbminusone;
extern	unchar	bio_usedmem[];
extern	struct	map bio_memmap[];
#endif

#endif	_SYS_BUF_
