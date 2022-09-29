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
#ident	"$Header: vfs_bio.c,v 1.7.1.17.1.6.1.4 91/01/09 15:54:28 beacker Exp $"
/* Original include file list:
*  ../machine/pte.h param.h systm.h user.h buf.h conf.h
*  proc.h seg.h vm.h trace.h vnode.h
*
* RISC/OS 5.0 support for NFS 4.0 included the following changes:
* 1. Adding the proper global variable declarations
* 2. Added support for the following interfaces:
*    bdevwait, btoss, bio_dcacheinval
* 3. The 4 queue management scheme employed by NFS 4.0 required changes in
*    binit, bio_freemem, btoss and getnewbuf. The page handling differs
*    from the vax/sun so changes were made by adapting bio_reallocmem to
*    provide equivalent functionality of allocbuf.
* 4. Added in bdflush.
* 5. Changed all splhigh() to splclock().
*/

#include "sys/types.h"
#include "sys/signal.h"		/* needed by user.h */
#include "sys/pcb.h"		/* needed by user.h */
#include "sys/user.h"
#include "sys/sysinfo.h"
#include "sys/buf.h"
#include "sys/conf.h"
#include "sys/errno.h"
#include "sys/var.h"
#include "sys/cmn_err.h"
#include "sys/map.h"
#include "sys/cpu_board.h"
#include "sys/systm.h"
#include "sys/immu.h"
#include "sys/region.h"
#include "sys/param.h"
#ifdef RISCOS
#include "sys/tuneable.h"
#define	trace(a,b,c)
#else
#include "sys/trace.h"
#endif
#include "sys/vnode.h"
#include "sys/sysmacros.h"
#include "sys/var.h"
#include "sys/debug.h"

struct bstats {
	int	n_bread;
	int	n_bread_hits;
	int	n_breada;
	int	n_breada_hits1;
	int	n_breada_hits2;
} bstats;

#ifdef RISCOS
extern dbd_t null_dbd;

daddr_t rablock;                /* block to be read ahead */
int     rasize;                 /* size of block in rablock */
struct  bufhd bufhash[BUFHSZ];  /* heads of hash lists */
struct	buf bfreelist[BQUEUES];	/* heads of available lists */

#define TLB_LINE(x)	(((u_int)(x) >>(PNUMSHFT+5)) & 0x3f)	/* R6000 */

#if FSMAXBSIZE < NBPC
/*
 * For machines with a pagesize larger than the largest block size (e.g. R6000),
 * we share "chunks" of physical pages.
 */
#define BUF_CHUNK	8192
#define chnktob(x)	((x) << 13)
#define btochnk(x)	((x) >> 13)
#define buf_bits(x)	((1 << btochnk(x)) - 1)

/*
 * Macro and globals for chunk sharing mechanism
 */
pfd_t	*bio_pffree;
int	bio_pfcount;

#define bio_pfadd(pfd)	{ \
	(pfd)->pf_next = bio_pffree; \
	if ((pfd)->pf_next) \
		(pfd)->pf_next->pf_prev = (pfd); \
	(pfd)->pf_prev = NULL; \
	bio_pffree = (pfd); \
	bio_pfcount++; \
}

#define bio_pfdel(pfd)	{ \
	if ((pfd)->pf_next) \
		(pfd)->pf_next->pf_prev = (pfd)->pf_prev; \
	if ((pfd)->pf_prev) \
		(pfd)->pf_prev->pf_next = (pfd)->pf_next; \
	if ((pfd) == bio_pffree) \
		bio_pffree = (pfd)->pf_next; \
	(pfd)->pf_next = NULL; \
	(pfd)->pf_prev = NULL; \
	bio_pfcount--; \
}

/*
 * Bufmap is used to dynamically allocate vm to buffers.
 */
struct map *bufmap;

#else
/*
 * For the other machine (with a 4k pagesize), each buf has its own
 * vm statically allocated at boot time. This is where it is.
 */
caddr_t	bio_vbase;
#define	BUF_VADDR(bp)	(bio_vbase + ((bp)-buf)*bio_maxbsize)
#endif	/* FSMAXBSIZE < NBPC */

/*
 * count and flag for outstanding async writes
 */

int	bio_pageshi;	/* Max pages to allow in buffer cache. */
int	bio_pageslo;	/* Min pages to trim to in the buffer cache. */
int	bio_curpages;	/* Current number of pages in buffer cache. */
int	bio_targpages;	/* Current desired number of pages. */

int	bio_despages;	/* Try to keep the buffer pool to this size, so that */
			/* we could have some pages to steal */
int	bio_lots_io;	/* We don't want to start any more I/O than this */

int	bio_freemem_wanted = 0;	/* someone need a page, but we are booked */
int	bio_freemem_avail = 0;	/* some I/O is done, pages ready to be stolen */
int	bio_lru_min;	/* min LRU queue length */
int	bio_age_max;	/* max AGE queue length */
int	bio_age_min;	/* min AGE queue length */
/* int	bio_age;	/* Age at which pages are freed in vhand. */

extern int	bio_despages_pct;
extern int	bio_lots_io_pct;
extern int	bio_lru_min_pct;
extern int	bio_age_max_pct;
extern int	bio_age_min_pct;
extern int	bio_page_pct;

int bdflush();

extern int	cache_bufs;	/* boot-line option var for cached IO bufs */
extern int	nocache_bufs;	/* boot-line option var for cached IO bufs */

extern int	dcache_size;

/*
 * bfree - Note that no space is currently being used in this buffer.
 */
#define bfree(bp) {bp->b_bcount = 0;}

#endif RISCOS

/*
 * Allocate newlen bytes of memory for the given buffer.
 *
 * Virtual memory is statically allocated at boot time.
 * Physical memory is put under the vm as required.  
 * Bufs should always have exactly enough physical memory to
 * cover their size as specified in bp->b_bufsize.
 *
 * Assumes bp is locked (BUSY) on entry; therefore no one else can 
 * even look at the buffer so we don't need any other locks
 * Returns TRUE on success and FALSE otherwise.
 */
allocbuf(bp, newlen)
	register struct buf *bp;
	int newlen;
{
	register int cur_pages, new_pages;
	register pde_t *pde;
	register int rv;
	register int n;
	register caddr_t vaddr;
	register pfd_t *pfd;
	bufinfo_t *bip;
	int s;

#if FSMAXBSIZE >= NBPC
	if (bp->b_un.b_addr != BUF_VADDR(bp)) {
		if (bp->b_bufsize != 0) {
			cmn_err(CE_WARN, "allocbuf: bp->b_un.b_addr = 0x%x not in range\n",
				bp->b_un.b_addr);
		}
		bp->b_un.b_addr = BUF_VADDR(bp);
	}
#endif
	ASSERT(bp->b_flags & B_BUSY);
	ASSERT(newlen <= bio_maxbsize);

	cur_pages = btoc(bp->b_bufsize);
	new_pages = btoc(newlen);

	/*
	 *	This code is not responsible for reducing the size
	 *	of the buffer cache, but if we are at or past the
	 *	upper limit, we must not increase the size.
	 */

	/*
	 *	Fix up the buffer's length right now, so that 
	 *	cache will remain consistent if we sleep.
	 */
	bp->b_bufsize = newlen;
	bp->b_bcount = newlen;

#if FSMAXBSIZE < NBPC
	/*
	 *	Allocate or free shared pages.
	 *	N.B. we assume that a chunk covers any length
	 */
	ASSERT(newlen <= BUF_CHUNK);
	if (cur_pages == 0 && new_pages != 0 && bpfalloc(bp))
		return (1);
	if (cur_pages != 0 && new_pages == 0 && bpffree(bp))
		return (1);
#endif
	/*
	 *	Allocate pages or free pages as required.
	 */
	if (cur_pages == new_pages) {
		/*
		 *	Buf is fine.  Don't touch.
		 */
	} 
	else if (cur_pages > new_pages) {
		/*
		 *	Buf has too many pages.  Free the extras.
		 */
		memlock();
		for (n=new_pages; n < cur_pages; ++n) {
			vaddr = bp->b_un.b_addr + ctob(n);
			pde = kvtokptbl(vaddr);
			if (! pg_isvalid(pde))
			  	continue;
			pfd = pdetopfdat(pde);
			ASSERT(pfd->pf_type == DBD_BUF && \
			       pfd->pf_rawcnt == 1 && \
			       pfd->pf_use == 1);
			bip = pdetobufinfo(pde);
			ASSERT(bip->buf_bufindx == bp - buf);
			ASSERT(bp->b_pfchain == NULL);
			pfd->pf_rawcnt = 0;
			pfd->pf_dbd = null_dbd;
			kvirt_inval(pnum(vaddr), 1);
			pfree(&sysreg, pde, &(pfd->pf_dbd), 1);
			/* avail[rs]mem can be modified at interrupt level */
			{   register int s;
			    s = splhi();
			    ++availsmem;
			    ++availrmem;
			    splx(s);
			}
			--bio_curpages;
		}
		memunlock();
#if FSMAXBSIZE < NBPC
		/*
		 *	If we released all physical memory, release
		 *	the virtual as well.
		 */
		if (newlen == 0) {
			mfree(bufmap, btoc(bio_maxbsize),
			    pnum(bp->b_un.b_addr)); 
			bp->b_un.b_addr = NULL;
		}
#endif
		s = splclock();
		if (bio_freemem_wanted) {
			bio_freemem_wanted = 0;
			wakeup(&bio_curpages);
		}
		splx(s);
	} 
	else if (cur_pages < new_pages) {
		/*
		 *	Buf doesn't have enough pages.  Get more.
		 */
#if FSMAXBSIZE < NBPC
		/*
		 *	If buffer doesn't have any vm yet, allocate it.
		 */
		if (bp->b_un.b_addr == NULL) {
again:
			n = malloc(bufmap, btoc(bio_maxbsize));
			if (n == 0) {
				/* shouldn't happen */
				cmn_err(CE_NOTE, "allocbuf: waiting for vm\n");
				mapwant(bufmap)++;
				sleep((caddr_t)bufmap, PRIBIO);
				goto again;
			}
#ifdef R6000
			/* avoid tlb clashes with the u-area */
			if (TLB_LINE(ctob(n)) == TLB_LINE(UADDR))
				goto again;
#endif
			bp->b_un.b_addr = (caddr_t)ctob(n);
		}
#endif
		/*
		 *	Don't take any more pages if the buffer
		 *	cache already has too many.
		 */
		s = splclock();
		while (bio_curpages > bio_targpages)  {
			bio_freemem_wanted = 1;
			wakeup(bdflush);
			sleep(&bio_curpages, PRIBIO);
		}
		splx(s);
		for (n=cur_pages; n < new_pages; ++n) {
			vaddr = bp->b_un.b_addr + ctob(n);
			pde = kvtokptbl(vaddr);
			if (pg_isvalid(pde))
				continue;
			memlock();
#ifdef R6000
			rv = ptmemall(&sysreg, pde, 1, 
				      REGION_NOT_LOCKED | PAGE_COLOR, 0,
				      btoct(vaddr));
#else
			rv = ptmemall(&sysreg, pde, 1, REGION_NOT_LOCKED, 0,
				      btoct(vaddr));
#endif
			memunlock();
			ASSERT (rv != -1); /* Should have waited. */

			if (cache_bufs) {
				pde->pgi.pg_pde |= PG_G|PG_VR|PG_M|PG_SV;
			} else {
				/* set tlb for non-cached accesses (R2000) */
				pde->pgi.pg_pde |= PG_G|PG_VR|PG_N|PG_M|PG_SV;
			}
			pfd = pdetopfdat(pde);
			ASSERT(pfd->pf_dbd.dbd_type == DBD_NONE);
			bip = pdetobufinfo(pde);
			bip->buf_type = DBD_BUF;
			bip->buf_bufindx = bp - buf;
#if FSMAXBSIZE >= NBPC
			bip->buf_blkmask = 0;
#else
			bip->buf_blkmask = buf_bits(NBPC) & ~1;
			bio_pfadd(pfd);
#endif
			pde_link(pde);
			pfd->pf_rawcnt = 1;
			ASSERT(pfd->pf_use == 1);
			/* avail[rs]mem can be modified at interrupt level */
			{   register int s;
			    s = splhi();
			    --availsmem;
			    --availrmem;
			    splx(s);
			}
			++bio_curpages;
		}
	}
	return (1); /* success */
}

#if FSMAXBSIZE < NBPC

/*
 * Try to allocate shared pages.
 * Returns 1 if successful, 0 if not.
 */
bpfalloc(bp)
	struct buf *bp;
{
	struct buf *hp;
	pfd_t *pfd;
	bufinfo_t *bip;
	u_int want, have;
	int i;

	ASSERT(bp->b_un.b_addr == NULL);
	for (pfd = bio_pffree; pfd; pfd = pfd->pf_next) {
		bip = pfdtobufinfo(pfd);
		ASSERT(bip->buf_blkmask != 0);
		want = buf_bits(BUF_CHUNK);
		have = bip->buf_blkmask;
		for (i = 0; i < NBPC/BUF_CHUNK; i++) {
			if ((want & have) == want) {
				bip->buf_blkmask ^= want;
				if (bip->buf_blkmask == 0)
					bio_pfdel(pfd);
				hp = &buf[bip->buf_bufindx];
				bp->b_pfchain = hp;
				bip->buf_bufindx = bp - buf;
				bp->b_un.b_addr = (caddr_t)
				    ((int)hp->b_un.b_addr & ~POFFMASK) +
				    chnktob(i);
				return (1);
			}
			want <<= 1;
		}
	}
	return (0);
}

/*
 * Try to free a chunk.
 * Returns 0 if page is completely free, 1 if not.
 */
bpffree(bp)
	struct buf *bp;
{
	struct buf *hp;
	pfd_t *pfd, prev;
	caddr_t vaddr;
	pde_t *pde;
	bufinfo_t *bip;
	u_int bits;
	int on_pffree;

	vaddr = bp->b_un.b_addr;
	pde = kvtokptbl(vaddr);
	pfd = pdetopfdat(pde);
	bip = pdetobufinfo(pde);
	ASSERT(bip->buf_type == DBD_BUF);
	on_pffree = bip->buf_blkmask != 0;
	bits = buf_bits(BUF_CHUNK) << btochnk(poff(vaddr));
	ASSERT((bip->buf_blkmask & bits) == 0);
	bip->buf_blkmask |= bits;
	if (bip->buf_blkmask == buf_bits(NBPC)) {
		ASSERT(on_pffree);
		bio_pfdel(pfd);
		return (0);
	}
	bp->b_un.b_addr = NULL;
	hp = &buf[bip->buf_bufindx];
	if (bp == hp) {
		ASSERT(bp->b_pfchain != NULL);
		bip->buf_bufindx = bp->b_pfchain - buf;
	} else
		for (;;) {
			ASSERT(hp != NULL);
			if (hp->b_pfchain == bp) {
				hp->b_pfchain = bp->b_pfchain;
				break;
			}
			hp = hp->b_pfchain;
		}
	bp->b_pfchain = NULL;
	if (!on_pffree)
		bio_pfadd(pfd);
	return (1);
}

#endif	/* FSMAXBSIZE < NBPC */

/*
 * Return buffer address for block IF in memory OTHERWISE return zero.
 */
struct buf *
baddr(vp, blkno, size)
	struct vnode *vp;
	daddr_t blkno;
	int size;
{

	if (incore(vp, blkno))
		return (bread(vp, blkno, size));
	return (0);
}

/*
 * Release the buffer, start I/O on it, but don't wait for completion.
 */
bawrite(bp)
	register struct buf *bp;
{

	bp->b_flags |= B_ASYNC;
	bwrite(bp);
}

/*
 * Tunables:
 *	tune.t_bdflushr (rate bdflush is to be run)
 *	v.v_autoup (life of a dirty buffer)
 *
 */
int bdscan;
int autoup;
int bdflushcnt = 0;
#define RATEOFSCHEDBDFLUSH	4

schedbdflush() {
	int n, pending_io;
	int	valid_buffers;
	int	s;

	timeout(schedbdflush, (caddr_t)0, HZ / RATEOFSCHEDBDFLUSH);

	/*
	 * Resize of the buffer cache.
	 *
	 * Currently, I add up the "extra" memory in the system
	 * (buf cache memory over bio_pageslo and freemem), and allow
	 * buffer cache to use half.
	 *
	 * ~~
	 * Need to think more about this algorithm.
	 * Would be nice to know how many pages in freemem
	 * are useful page cache, and how many are stale.
	 *
	 * Could be put algorithm in master.d/kernel for binary
	 * resetability.
	 */
	s = splbio();
	n = bio_curpages - bio_pageslo;		/* "Extra" from cache */
	n = max(n, 0);
	n += freemem;				/* Extra in sys */
	bio_targpages = bio_pageslo + max(n,0)/2; /* Cache gets half */
	bio_targpages = min(bio_targpages, bio_pageshi);
	bio_targpages = max(bio_targpages, bio_pageslo);

	bio_despages =
		(((bio_targpages - bio_pageslo) * bio_despages_pct) / 100)
		  + bio_pageslo;

	valid_buffers = bfreelist[BQ_LRU].b_bcount +
			bfreelist[BQ_AGE].b_bcount;
	bio_lots_io = (valid_buffers * bio_lots_io_pct) / 100;

	/*
	 * bdflush is gurantee to be run at rate of tune.t_bdflushr
	 */
	if (--bdflushcnt <= 0) {
		bdflushcnt = tune.t_bdflushr * RATEOFSCHEDBDFLUSH;
		autoup = v.v_autoup*HZ;
		if (v.v_autoup < tune.t_bdflushr)
			bdscan = v.v_buf;
		else
			bdscan = v.v_buf / (v.v_autoup / tune.t_bdflushr);
		goto wakeup_bdflush;
	}

	/*
	 * If we are memory limit, and we are not I/O bound
	 * kick bdflush to free up some memory
	 */
	pending_io = v.v_buf - (bfreelist[BQ_LOCKED].b_bcount +
				valid_buffers +
			 	bfreelist[BQ_EMPTY].b_bcount);
	
	/*
	 * If we have some free memory, and someone is waiting for a buffer,
	 * or we are low on free mem and not too busy wake up bdflush
	 */
	if (((bio_curpages < bio_targpages) && (bfreelist[0].b_flags & B_WANTED)) ||
	    ((bio_curpages > bio_despages) && (pending_io < bio_lots_io))) {
wakeup_bdflush:
		wakeup(bdflush);
	}

	splx(s);
}

/*
 * flush delayed writes after time delay
 */
bdflush()
{
	register struct buf *bp;
	register int bdflush_hand;
	register int s, scan;
	register int l, m, n;
	register int pages_to_be_freed;
	int  pending_io;
	int	available_buffers;
	int	age_queue_deficit;
	int	io_deficit;

	bio_pageshi = min(bio_pageshi, (v.v_buf * (bio_maxbsize / NBPP)));
	bio_pageslo = min(bio_pageslo,bio_pageshi);
	bdflushcnt = 1;
	bdflush_hand = 0;

	s = splclock();
	schedbdflush();
	splx(s);

awake:
	s = splclock();
	/*
	 * If I am awakened by brelse, call freemem_proc to release the
	 * pages, which will in trun wakeup the process waiting for it
	 */
	if (n = bio_freemem_avail) {
		bio_freemem_avail -= n;
		splx(s);
		freemem_proc(&bfreelist[BQ_AGE], n);
		splclock();
	}

	/*
	 * If we are memory limited, call bio_freemem to free up some
	 */
	pages_to_be_freed = bio_curpages - bio_despages;
	if (pages_to_be_freed > 0) {
		pending_io = v.v_buf - bfreelist[BQ_LOCKED].b_bcount
				 - bfreelist[BQ_LRU].b_bcount
				 - bfreelist[BQ_AGE].b_bcount
				 - bfreelist[BQ_EMPTY].b_bcount;
		if (pending_io < bio_lots_io) 
			bio_freemem(pages_to_be_freed);
	} else {
		pages_to_be_freed = 0;
		if (bio_freemem_wanted) {
			bio_freemem_wanted = 0;
			wakeup(&bio_curpages);
		}
	}

	if (bio_curpages < bio_targpages) {
		available_buffers = bio_targpages - bio_curpages;
		available_buffers = min(available_buffers,freemem);
		available_buffers =
			((available_buffers * bio_page_pct) / 100);
		available_buffers = min(available_buffers, bfreelist[BQ_EMPTY].b_bcount);
	} else
		available_buffers = 0;
	available_buffers += bfreelist[BQ_AGE].b_bcount;

	bio_lru_min = min(bio_curpages,bio_targpages);
	bio_lru_min = ((((bio_lru_min * bio_page_pct) / 100)
				* bio_lru_min_pct) / 100);

	bio_age_max = ((((bio_targpages * bio_page_pct) / 100)
				* bio_age_max_pct) / 100);

	bio_age_min = ((((bio_targpages * bio_page_pct) / 100)
				* bio_age_min_pct) / 100);

	age_queue_deficit = bio_age_min - available_buffers;
	if (age_queue_deficit < 0)
		age_queue_deficit = 0;
		
	pending_io = v.v_buf - bfreelist[BQ_LOCKED].b_bcount
				 - bfreelist[BQ_LRU].b_bcount
				 - bfreelist[BQ_AGE].b_bcount
				 - bfreelist[BQ_EMPTY].b_bcount;
	io_deficit = bio_lots_io - pending_io;
	if (io_deficit < 0)
		io_deficit = 0;
	splx(s);

	/*
	 * Flush old delayed write disk blocks and steal pages
	 * from invalid buffers.
	 * Write out oldest stuff first.
	 */
	scan = max(bdscan,io_deficit);
	while (scan--) {
		if (bdflush_hand >= v.v_buf)
			bdflush_hand = 0;
		bp = &buf[bdflush_hand++];
		s = splclock();
		if (bp->b_flags & B_BUSY) {
			splx(s);
			continue;
		}
		if (bp->b_bufsize == 0) {
			splx(s);
			continue;
		}
		/*
		 * If buffer is dirty, legit, and old, push it to disk.
		 */
		if ((bp->b_flags & B_DELWRI) && !(bp->b_flags & B_INVAL) &&
		    ((io_deficit > 0) || ((lbolt-bp->b_start) >= autoup))) {
			if (io_deficit > 0) 
				io_deficit--;
			bp->b_flags |= B_ASYNC;
			if (bp->b_flags & B_LRU) {
				notavailspl(bp, &bfreelist[BQ_LRU]);
			} else if (bp->b_flags & B_LOCKED) {
				notavailspl(bp, &bfreelist[BQ_LOCKED]);
			} else {
				notavailspl(bp, &bfreelist[BQ_AGE]);
			}
			splx(s);
			bwrite(bp);
		}
		/*
		 * If we find a stale buffer with memory, throw its memory
		 * away now.  Take it off the freelist so that nobody
		 * else will muck with it in case we need to sleep in
		 * bio_reallocmem.
		 */
		if ((bp->b_flags & B_INVAL) && bp->b_bufsize) {
			if (bp->b_flags & B_LRU) {
				notavailspl(bp, &bfreelist[BQ_LRU]);
			} else if (bp->b_flags & B_LOCKED) {
				notavailspl(bp, &bfreelist[BQ_LOCKED]);
			} else {
				notavailspl(bp, &bfreelist[BQ_AGE]);
			}
			splx(s);
			allocbuf(bp, 0);
			brelse(bp);
			if (bio_curpages < bio_targpages &&
			    age_queue_deficit > 0)
				age_queue_deficit--;
		}
	}

	/*
	 * If age_queue_defice <= 0, but buf is still wanted
	 * we need to move some LRU buf to the AGE queue and do a wakeup
	 * (This is the condition when we have freemem, but not enough buf header
	 */
	if ((age_queue_deficit <= 0) && (bfreelist[0].b_flags & B_WANTED)) {
		age_queue_deficit = max(0, bio_age_min - bfreelist[BQ_AGE].b_bcount);
		if (age_queue_deficit == 0) {
			cmn_err(CE_WARN,"bdflush: buf wanted when it is available\n");
			bfreelist[0].b_flags &= ~B_WANTED;
			wakeup((caddr_t)bfreelist);
		}
	}
	/*
	 * If we are low on clean buffers, move some from the LRU queue
	 * to the AGE queue
	 *
	 * We want to keep a minimum length for the LRU queue, and a
	 * minimum and a maximum for the AGE queue.
	 */
	if (age_queue_deficit > 0 &&
	    bfreelist[BQ_LRU].b_bcount > bio_lru_min) {
		s = splclock();
		for (bp = bfreelist[BQ_LRU].av_forw;
		     bp != &bfreelist[BQ_LRU] &&
			bfreelist[BQ_LRU].b_bcount > bio_lru_min &&
			bfreelist[BQ_AGE].b_bcount < bio_age_max &&
			age_queue_deficit > 0; 
		     age_queue_deficit--) {
			struct buf *sbp = bp->av_forw;

			if (bp->b_flags & B_DELWRI) {
				notavailspl(bp, &bfreelist[BQ_LRU]);
				bp->b_flags |= (B_ASYNC | B_AGE);
				bp->b_flags &= ~B_LRU;
				bwrite(bp);
			} else {
				bremfree(bp);
				bfreelist[BQ_LRU].b_bcount--;
				bp->b_flags &= ~B_LRU;
				binstailfree(bp, &bfreelist[BQ_AGE]);
				bfreelist[BQ_AGE].b_bcount++;
				if (bfreelist[0].b_flags&B_WANTED) {
					bfreelist[0].b_flags &= ~B_WANTED;
					wakeup((caddr_t)bfreelist);
				}
			}
			bp = sbp;
		}
		splx(s);
	}
	sleep(bdflush, PRIBIO);
	goto awake;
}

/*
 * Release the buffer, marking it so that if it is grabbed
 * for another purpose it will be written out before being
 * given up (e.g. when writing a partial block where it is
 * assumed that another write for the same block will soon follow).
 * This can't be done for magtape, since writes must be done
 * in the same order as requested.
 */
bdwrite(bp)
	register struct buf *bp;
{
	sysinfo.lwrite++;
	if ((bp->b_flags&B_DELWRI) == 0) {
	        u.u_ru.ru_oublock++;		/* noone paid yet */
		bp->b_start = lbolt;
	}

#ifndef RISCOS
	/*
	 * This does not work for buffers associated with
	 * vnodes that are remote - they have no dev.
	 * Besides, we don't use bio with tapes, so rather
	 * than develop a fix, we just ifdef this out for now.
	 */
	if (bdevsw[major(bp->b_dev)].d_flags & B_TAPE)
		bawrite(bp);
	else {
		bp->b_flags |= B_DELWRI | B_DONE;
		brelse(bp);
	}
#endif
	bp->b_flags |= B_DELWRI | B_DONE;
	brelse(bp);
}

/*
 * Initialize hash links for buffers.
 */
bhinit()
{
	register int i;
	register struct bufhd *bp;

	v.v_hbuf = BUFHSZ;
	for (bp = bufhash, i = 0; i < BUFHSZ; i++, bp++)
		bp->b_forw = bp->b_back = (struct buf *)bp;
}

/*
 * Initialize the buffer I/O system.
 *
 * Allocate vm for the buffers, put buf headers in free lists,
 * and set all device hash buffer lists to empty.
 */
void
binit()
{
	register struct buf *bp, *dp;
	register short i;
	register int pages, vbase, mapsz;
	static char initialized = 0;

	if (!initialized) {
		/*
		 * Size cache.
		 *
		 * The limits on the number of pages that can be in 
		 * the cache, and the actual number of buffer headers, 
		 * are all determined by functions that can be set 
		 * during lboot.  These values are set in the kernel file 
		 * (master.d/kernel.std) and can thus be modified by system
		 * administrators without source code.
		 */
		bio_pageslo = bio_pageslo_init();
		bio_pageshi = bio_pageshi_init();
		v.v_buf =     bio_nbuf_init();

		bio_curpages = 0;
		bio_targpages = bio_pageshi;

		/*
		 * Get memory for buf headers.
		 */
		buf = (struct buf*)kern_calloc(v.v_buf, sizeof(struct buf));
		if (!buf) {
			cmn_err(CE_PANIC, 
			    "binit: Couldn't allocate mem for buf headers.");
		}

		/* 
		 * Get vm for buffers themselves.
		 * Init the buffer vm allocation map.
		 */
		pages = btoc( v.v_buf * bio_maxbsize );
		vbase = malloc(sptmap, pages);
		if (vbase == 0)
			cmn_err(CE_PANIC, "not enough vm for buffer cache");
#if FSMAXBSIZE < NBPC
		mapsz = MAX(v.v_buf/4, 50);
		bufmap = (struct map *)kern_calloc(mapsz, sizeof(struct map));
		if (!bufmap)
			cmn_err(CE_PANIC, "binit: no mem for bufmap");
		mapinit(bufmap, mapsz);
		mfree(bufmap, pages, vbase);
#else
		bio_vbase = (caddr_t)ctob(vbase);
#endif

		/*
		 * Initilize free lists.
		 */
		for (dp = &bfreelist[0]; dp < &bfreelist[BQUEUES]; ++dp) {
			dp->av_forw = dp->av_back = dp;
			dp->b_forw = dp->b_back = dp;
			dp->b_flags = B_HEAD;
			dp->b_bcount = 0;
		}

		for (i = 0, bp = buf; i < v.v_buf; i++, bp++) {
			bp->b_forw = bp->b_back = bp;
			bp->b_flags = B_BUSY;
			bp->b_dev = NODEV;
#if FSMAXBSIZE < NBPC
			bp->b_un.b_addr = NULL;		/* no vm yet */
#else
			bp->b_un.b_addr = BUF_VADDR(bp);
#endif
			bp->b_bcount = 0;
			bp->b_bufsize = 0;		/* no phys mem yet */
			binshash(bp, &bfreelist[BQ_EMPTY]);
			bp->b_flags = B_BUSY|B_INVAL;
			brelse(bp);
		}

		/* setup raw i/o buffer headers */
		swapinit();

		/* initialize hash buffer headers */
		bhinit();

		/* I/O buffer memory is in KSEG2 space and is accessesed
		 * through tlbs.  On R2000-based machines the tlbs should be
		 * set so that the buffers aren't cached. On machines with
		 * multi-word cache refill, the accesses should be cached to
		 * speed copies. There are boot-line options to explicitly
		 * enable/disable caching (cache_bufs and nocache_bufs) that
		 * take precedence if specified. If not explicitly set, then
		 * the setting is determined by the machine type.
		 */
		if (nocache_bufs) {
			/* don't do anything, cache_bufs should be false */
		} else if (cache_bufs) {
			/* don't do anything, already set */
		} else if (IS_R6300) {
			cache_bufs = 1;
		} else if (IS_M2000_ARCH) {
			cache_bufs = 1;
		} else if (machine_type == BRDTYPE_M180) {
			cache_bufs = 1;
		} else if (IS_R3030) {
			cache_bufs = 1;
		} else {
			cache_bufs = 0;
		}

		if (showconfig)
		{
			printf("Buffer cache: %d pages\n", bio_pageshi);
			printf("              %d bufs\n", v.v_buf);
			printf("              %d bytes buf headers\n",
				v.v_buf*sizeof(struct buf));
			printf("              buffers in %s space\n",
			(cache_bufs==1) ? "cached" : "uncached");
		}
		
		initialized = 1;
	}
}

/*
 * wait for all pending I/O to dev (or NODEV for all) to complete
 */
bdevwait(dev)
     	register dev_t dev;
{
	register struct buf *bp;
	register struct bufhd *hp;
	int	olderrno = u.u_error;
	int s;
#define dp ((struct buf *)hp)

    loop:
	s = splclock();
	for (hp = bufhash; hp < &bufhash[BUFHSZ]; hp++) {
	    for (bp = dp->b_forw; bp != dp; bp = bp->b_forw) {
		/*
		 * If the buffer is legit, and the dev matches or the 
		 * parameter dev is the wildcard, wait for the buffer
		 * I/O to complete.
		 */
		if ( !(bp->b_flags & (B_DONE|B_INVAL)) && 
			    ((dev == NODEV) || (dev == bp->b_dev))) {
		    syswait.iowait++;
		    if ((bp->b_flags&B_DONE)==0) {
			    bp->b_flags |= B_WANTED;
			    (void) sleep((caddr_t)bp, PRIBIO);
		    }
		    (void) splx(s);
		    syswait.iowait--;
		    goto loop;
		}
	    }
	}
#undef dp

	splx(s);
	u.u_error = olderrno;
}


/*
 * Mark I/O complete on a buffer.
 * If someone should be called, e.g. the pageout daemon, do so.
 * Otherwise, wake up anyone waiting for it.
 */
iodone(bp)
	register struct buf *bp;
{
	if (bp->b_flags & B_DONE)
		panic("dup iodone");
	bp->b_flags |= B_DONE;
	if (bp->b_flags & B_CALL) {
		bp->b_flags &= ~B_CALL;
		(*bp->b_iodone)(bp);
		return;
	}
	if (bp->b_flags&B_ASYNC) {
		brelse(bp);
	} else {
		bp->b_flags &= ~B_WANTED;
		wakeup((caddr_t)bp);
	}
}

/*
 * Wait for I/O completion on the buffer; return errors
 * to the user.
 */
iowait(bp)
	register struct buf *bp;
{
	int s;

	syswait.iowait++;
	s = splclock();
	while ((bp->b_flags&B_DONE)==0)
		(void) sleep((caddr_t)bp, PRIBIO);
	(void) splx(s);
	syswait.iowait--;
	if (u.u_error == 0)			/* XXX */
		u.u_error = geterror(bp);
}

/*
 * Insure that no part of a specified block is in an incore buffer.
 */
blkflush(vp, blkno, size)
	struct vnode *vp;
	daddr_t blkno;
	long size;
{
	register struct buf *ep;
	struct buf *dp;
	daddr_t start, last;
	int s;

	start = blkno;
	last = start + btodb(size) - 1;
	dp = BUFHASH(vp, blkno);
loop:
	for (ep = dp->b_forw; ep != dp; ep = ep->b_forw) {
		if (ep->b_vp != vp || (ep->b_flags&B_INVAL))
			continue;
		/* look for overlap */
		if (ep->b_bcount == 0 || ep->b_blkno > last ||
		    ep->b_blkno + btodb(ep->b_bcount) <= start)
			continue;
		s = splclock();
		if (ep->b_flags&B_BUSY) {
			ep->b_flags |= B_WANTED;
			syswait.iowait++;
			(void) sleep((caddr_t)ep, PRIBIO+1);
			syswait.iowait--;
			(void) splx(s);
			goto loop;
		}
		if (ep->b_flags & B_DELWRI) {
			if (ep->b_flags & B_LRU) {
				notavailspl(ep, &bfreelist[BQ_LRU]);
			} else if (ep->b_flags & B_LOCKED) {
				notavailspl(ep, &bfreelist[BQ_LOCKED]);
			} else {
				notavailspl(ep, &bfreelist[BQ_AGE]);
			}
			(void) splx(s);
			bwrite(ep);
			goto loop;
		}
		(void) splx(s);
	}
}

/*
 * Read in (if necessary) the block and return a buffer pointer.
 */
struct buf *
bread(vp, blkno, size)
	struct vnode *vp;
	daddr_t blkno;
	int size;
{
	register struct buf *bp;

	bstats.n_bread++;
	if (size == 0)
		panic("bread: size 0");
	sysinfo.lread++;
	bp = getblk(vp, blkno, size);
	if (bp->b_flags&B_DONE) {
		trace(TR_BREADHIT, vp, blkno);
		bstats.n_bread_hits++;
		return (bp);
	}
	ASSERT(!(bp->b_flags & B_DELWRI));
	bp->b_flags |= B_READ;
	if (bp->b_bcount > bp->b_bufsize)
		panic("bread");
	VOP_STRATEGY(bp);
	trace(TR_BREADMISS, vp, blkno);
	u.u_ru.ru_inblock++;		/* pay for read */
#if RISCOS
	u.u_ior++;
#endif
	sysinfo.bread++;
	iowait(bp);
	return (bp);
}

struct buf *breadma();

struct buf *
breada(vp, blkno, size, rablkno, rabsize)
	struct vnode *vp;
	daddr_t blkno; int size;
	daddr_t rablkno; int rabsize;
{
	if (rablkno)
		return (breadma(vp, blkno, size, 1, &rablkno, &rabsize));
	else
		return (breadma(vp, blkno, size, 0, &rablkno, &rabsize));
}

/*
 * Read in the block, like bread, but also start I/O on the
 * read-ahead block (which is not allocated to the caller)
 */
struct buf *
breadma(vp, blkno, size, nmra, rablkno, rabsize)
	struct vnode *vp;
	daddr_t blkno; int size;
	int nmra;
	daddr_t *rablkno; int *rabsize;
{
	register struct buf *bp, *rabp;
	int i;

	bstats.n_breada++;
	bp = NULL;
	/*
	 * If the block isn't in core, then allocate
	 * a buffer and initiate i/o (getblk checks
	 * for a cache hit).
	 */
	if (!incore(vp, blkno)) {
		sysinfo.lread++;
		bp = getblk(vp, blkno, size);
		if ((bp->b_flags&B_DONE) == 0) {
			ASSERT(!(bp->b_flags & B_DELWRI));
			bp->b_flags |= B_READ;
			if (bp->b_bcount > bp->b_bufsize)
				panic("breada");
			VOP_STRATEGY(bp);
			trace(TR_BREADMISS, vp, blkno);
			u.u_ru.ru_inblock++;		/* pay for read */
#if RISCOS
			u.u_ior++;
#endif
			sysinfo.bread++;
		} else {
			trace(TR_BREADHIT, vp, blkno);
			bstats.n_breada_hits1++;
		}
	}

	/*
	 * If there's a read-ahead block, start i/o
	 * on it also (as above).
	 */
	for (i = 0; i < nmra; i++, rablkno++, rabsize++) {
		if (!incore(vp, *rablkno)) {
			rabp = getblk(vp, *rablkno, *rabsize);
			if (rabp->b_flags & B_DONE) {
				brelse(rabp);
				trace(TR_BREADHITRA, vp, blkno);
				bstats.n_breada_hits2++;
			} else {
				ASSERT(!(rabp->b_flags & B_DELWRI));
				rabp->b_flags |= B_READ|B_ASYNC;
				if (rabp->b_bcount > rabp->b_bufsize)
					panic("breadrabp");
				VOP_STRATEGY(rabp);
				trace(TR_BREADMISSRA, vp, *rablock);
				u.u_ru.ru_inblock++;		/* pay in advance */
#if RISCOS
				u.u_ior++;
#endif
				sysinfo.bread++;
			}
		}
	}

	/*
	 * If block was in core, let bread get it.
	 * If block wasn't in core, then the read was started
	 * above, and just wait for it.
	 */
	if (bp == NULL)
		return (bread(vp, blkno, size));
	iowait(bp);
	return (bp);
}

/*
 * Allocate space associated with a buffer.
 * If can't get space, buffer is released
 * Returns TRUE on success and FALSE otherwise.
 */
brealloc(bp, size, flushongrow)
	register struct buf *bp;
	int size;
	int flushongrow;		/* flush B_DELWRI buffers if growing */
{
	daddr_t start, last;
	register struct buf *ep;
	struct buf *dp;
	int s;

	/*
	 * First need to make sure that all overlaping previous I/O
	 * is dispatched with.
	 */
	if (size == bp->b_bcount)
		return (1);
	if (size < bp->b_bcount) { 
	    if (bp->b_flags & B_DELWRI) {
		bwrite(bp);
		return (0);
	    }
	    if (bp->b_flags & B_LOCKED)
		cmn_err(CE_WARN, 
		    "brealloc: shrinking locked buffer (bp:%x dev:%x blkno:%x)",
		    bp, (unsigned)bp->b_dev, bp->b_blkno);
	    return (allocbuf(bp, size));
	}
	if (flushongrow && (bp->b_flags & B_DELWRI)) {
		bwrite(bp);
		return (0);
	}
	bp->b_flags &= ~B_DONE;
	if (bp->b_vp == (struct vnode *) 0)
		return (allocbuf(bp, size));

	/*
	 * Search cache for any buffers that overlap the one that we
	 * are trying to allocate. Overlapping buffers must be marked
	 * invalid, after being written out if they are dirty. (indicated
	 * by B_DELWRI) A disk block must be mapped by at most one buffer
	 * at any point in time. Care must be taken to avoid deadlocking
	 * when two buffer are trying to get the same set of disk blocks.
	 */
	start = bp->b_blkno;
	last = start + btodb(size) - 1;
	dp = BUFHASH(bp->b_vp, bp->b_blkno);
loop:
	for (ep = dp->b_forw; ep != dp; ep = ep->b_forw) {
		if (ep == bp || ep->b_vp != bp->b_vp || (ep->b_flags&B_INVAL))
			continue;
		/* look for overlap */
		if (ep->b_bcount == 0 || ep->b_blkno > last ||
		    ep->b_blkno + btodb(ep->b_bcount) <= start)
			continue;
		s = splclock();
		if (ep->b_flags&B_BUSY) {
			ep->b_flags |= B_WANTED;
			syswait.iowait++;
			(void) sleep((caddr_t)ep, PRIBIO+1);
			syswait.iowait--;
			(void) splx(s);
			goto loop;
		}
		if (ep->b_flags & B_LRU) {
			notavailspl(ep, &bfreelist[BQ_LRU]);
		} else if (ep->b_flags & B_LOCKED) {
			notavailspl(ep, &bfreelist[BQ_LOCKED]);
		} else {
			notavailspl(ep, &bfreelist[BQ_AGE]);
		}
		if (ep->b_flags & B_DELWRI) {
			(void) splx(s);
			bwrite(ep);
			goto loop;
		}
		ep->b_flags |= B_INVAL;
		(void) splx(s);
		brelse(ep);
	}
	return (allocbuf(bp, size));
}

/*
 * Release the buffer, with no I/O implied.
 */
brelse(bp)
	register struct buf *bp;
{
	register struct buf *flist;
	register s;

	/*
	 * If someone's waiting for the buffer, or
	 * is waiting for a buffer wake 'em up.
	 */
	if (bp->b_flags&B_WANTED)
		wakeup((caddr_t)bp);
	if (bfreelist[0].b_flags&B_WANTED) {
		bfreelist[0].b_flags &= ~B_WANTED;
		wakeup((caddr_t)bfreelist);
	}
	if (bp->b_flags & B_NOCACHE) {
		bp->b_flags |= B_INVAL;
	}
	if (bp->b_flags&B_ERROR)
		if (bp->b_flags & B_LOCKED)
			bp->b_flags &= ~B_ERROR;	/* try again later */
		else
			brelvp(bp);

	/*
	 * Stick the buffer back on a free list.
	 */
	s = splclock();
	if (bp->b_bufsize <= 0) {
		/* block has no buffer ... put at front of unused buffer list */
		bp->b_flags &= ~B_LRU;
		flist = &bfreelist[BQ_EMPTY];
		binsheadfree(bp, flist);
	} else if (bp->b_flags & (B_ERROR|B_INVAL)) {
		/* block has no info ... put at front of most free list */
		bp->b_flags &= ~B_LRU;
		flist = &bfreelist[BQ_AGE];
		binsheadfree(bp, flist);
	} else {
		if (bp->b_flags & B_LOCKED) {
			bp->b_flags &= ~B_LRU;
			flist = &bfreelist[BQ_LOCKED];
		} else if (bp->b_flags & B_AGE) {
			bp->b_flags &= ~B_LRU;
			flist = &bfreelist[BQ_AGE];
		} else {
			bp->b_flags |= B_LRU;
			flist = &bfreelist[BQ_LRU];
		}
		binstailfree(bp, flist);
	}
	bp->b_flags &= ~(B_WANTED|B_BUSY|B_ASYNC|B_AGE|B_NOCACHE);
	flist->b_bcount++;
	if ((flist == &bfreelist[BQ_AGE]) && bio_freemem_wanted) {
		bio_freemem_avail += btoc(bp->b_bufsize);
		wakeup(bdflush);
	}
	(void) splx(s);
}

/*
 * Make sure all write-behind blocks
 * associated with vp (or the whole cache if vp == 0)
 * are flushed out.
 * (from sync)
 */
bflush(vp)
	struct vnode *vp;
{
	register struct buf *bp;
	register struct buf *flist;
	int s;

loop:
	s = splclock();
	for (flist = bfreelist; flist < &bfreelist[BQ_EMPTY]; flist++)
	for (bp = flist->av_forw; bp != flist; bp = bp->av_forw) {
		if ((bp->b_flags & B_DELWRI) == 0)
			continue;
		if (vp == bp->b_vp || vp == (struct vnode *) 0) {
			bp->b_flags |= B_ASYNC;
			notavailspl(bp, flist);
			(void) splx(s);
			bwrite(bp);
			goto loop;
		}
	}
	(void) splx(s);
}

/*
 * Invalidate in core blocks belonging to closed or umounted filesystem
 *
 * This is not nicely done at all - the buffer ought to be removed from the
 * hash chains & have its dev/blkno fields clobbered, but unfortunately we
 * can't do that here, as it is quite possible that the block is still
 * being used for i/o. Eventually, all disc drivers should be forced to
 * have a close routine, which ought ensure that the queue is empty, then
 * properly flush the queues. Until that happy day, this suffices for
 * correctness.						... kre
 * This routine assumes that all the buffers have been written.
 */
binval(vp)
	struct vnode *vp;
{
	register struct buf *bp;
	register struct bufhd *hp;
#define dp ((struct buf *)hp)

loop:
	for (hp = bufhash; hp < &bufhash[BUFHSZ]; hp++)
		for (bp = dp->b_forw; bp != dp; bp = bp->b_forw)
			if (bp->b_vp == vp && (bp->b_flags & B_INVAL) == 0) {
				bp->b_flags |= B_INVAL;
				brelvp(bp);
				goto loop;
			}
#undef dp
}

/*
 * Invalidate blocks associated with vp which are on the freelist.
 * Make sure all write-behind blocks associated with vp are flushed out.
 */
binvalfree(vp)
	struct vnode *vp;
{
	register struct buf *bp;
	register struct buf *flist;
	int s;

loop:
	s = splclock();
	for (flist = bfreelist; flist <= &bfreelist[BQ_EMPTY]; flist++)
	for (bp = flist->av_forw; bp != flist; bp = bp->av_forw) {
		if (vp == bp->b_vp || vp == (struct vnode *) 0) {
			if (bp->b_flags & B_DELWRI) {
				bp->b_flags |= B_ASYNC;
				notavailspl(bp, flist);
				(void) splx(s);
				bwrite(bp);
			} else {
				bp->b_flags |= B_INVAL;
				brelvp(bp);
				(void) splx(s);
			}
			goto loop;
		}
	}
	(void) splx(s);
}

/*
 * RISC/OS ADDED PROCEDURE
 * Invalidate physical data cache entries for the physical addresses of the
 * I/O buffer area into which I/O is done. For R2000, R3000 the buffer's
 * memory is contiguous in kernel virtual space but not (necessarily)
 * physically contiguous, so invalidation must be done piecewise per page.
 *
 * The buffer should be busy because invalidation must be done while the
 * buffer is mapped into kernel virtual space so we can find the pages.
 * Note that the most common case, a disk-cache buffer, is a simple case
 * and that this code tries to do a reasonable job for other cases as well.
 * 
 * [The current strategy is to call this procedure after I/O has completed
 * but before it is actually unmapped or I/O marked done.]
 */
void
bio_dcacheinval(bp)
	register struct buf *bp;
{
	register pde_t *pde;
	register caddr_t kvaddr;	/* virtual addr of buffer pages */
	register caddr_t k0_inval_addr;	/* kseg0 addr to give cache inval rtn */
	int	inval_count;		/* count to give to cache inval rtn */
	int	inval_offset;		/* offset in page where inval starts */
	int	remaining_count;	/* byte cnt remaining to be inval */

	ASSERT(bp->b_flags & B_BUSY);
	ASSERT(bp->b_bufsize <= bio_maxbsize);

	/* Look at the virtual range of the memory attached
	 * to the buffer.  For each physical page, find a
	 * physical address, and the corresponding k0 address.
	 * Clean the portion of the page that was transferred.
	 */
	kvaddr = bp->b_dmaaddr;
	remaining_count = bp->b_bcount;
	while (remaining_count > 0) {
		pde = kvtokptbl(kvaddr);
		ASSERT(pde->pgm.pg_pfn); /* Page better be allocated */
		inval_offset = (long)kvaddr & (NBPC-1);
		k0_inval_addr = (caddr_t)ptosv(pde->pgm.pg_pfn);
		k0_inval_addr += inval_offset;
		inval_count = min( NBPC-inval_offset, remaining_count );
		clean_dcache(k0_inval_addr,inval_count);
		remaining_count -= inval_count;
		kvaddr += inval_count;
	}
}

/*
 * RISC/OS ADDED PROCEDURE
 * Note that this procedure assumes only two queues, an empty and lru queue
 * where the nfs 4.0 code manipulates four queues.
 *
 * Take away specified number of pages from buffers in buffer cache.
 * Only mem when convenient.  Don't sleep if there aren't even any bufs 
 * in the free lists.
 */
bio_freemem(pages)
int pages;
{
	register struct buf *dp;
	int pages_remain;

	dp = &bfreelist[BQ_AGE];
	pages_remain = freemem_proc (dp, pages);

	if ( pages_remain > 0 && bio_curpages > bio_pageslo) {
	        dp = &bfreelist[BQ_LRU];
		pages_remain = freemem_proc (dp, pages_remain);
	}
}

/*
 * Take away specified number of pages from buffers in buffer cache.
 * Only mem when convenient.  Don't sleep if there aren't even any bufs 
 * in the free lists.
 */
int
freemem_proc(dp, pages)
struct buf *dp;
int pages;
{
	register struct buf *bp;
	int s;

	while ( pages > 0 && bio_curpages > bio_pageslo ) {
		s = splclock();
		if (dp->av_forw == dp) {
			splx(s);
			return (pages);
		}
		bp = dp->av_forw;
		notavailspl(bp, dp);
		splx(s);

#if FSMAXBSIZE >= NBPC
		pages -= btoc(bp->b_bufsize);
#else
		/*
		 * Pages allocated to buffer cache only goes
		 * down if this is the only buffer mapping
		 * the page.
		 */
		if (bp->b_pfchain == NULL) {
			pde_t *pde;
			bufinfo_t *bip;

			pde = kvtokptbl(bp->b_un.b_addr);
			bip = pdetobufinfo(pde);
			if (bp == &buf[bip->buf_bufindx])
				pages--;
		}
#endif

		/*
		 * Write delayed-write buf.
		 */
		if (bp->b_flags & B_DELWRI) {
			/* AGE block for immediate reuse. */
			bp->b_flags |= B_ASYNC | B_AGE;
			bwrite(bp);
			continue;
		}

		/*
		 * Steal this buf's mem.
		 */
		bp->b_flags |= B_INVAL;
		allocbuf(bp, 0);
		brelse(bp);
	}
	return (pages);
}


/*
 * Release the vnode associated with the buffer.
 */
brelvp(bp)
	struct buf *bp;
{
	struct vnode *vp;

	if (bp->b_vp == (struct vnode *) 0) {
		return;
	}
	vp = bp->b_vp;		/* save vp because VN_RELE may sleep */
	bp->b_vp = (struct vnode *) 0;
	VN_RELE(vp);
}

/*
 * Release the vnode associated with the buffer IF one is stored in the buffer.
 * Next lock the new vode and save it's address in the buffer descriptor.
 */
bsetvp(bp, vp)
	struct buf *bp;
	struct vnode *vp;
{
	if (bp->b_vp) {
		brelvp(bp);
	}
	VN_HOLD(vp);
	bp->b_vp = vp;
}

/*
 * RISC/OS ADDED PROCEDURE
 * Note that this procedure assumes only two queues, an empty and lru queue
 * where the nfs 4.0 code manipulates four queues.
 *
 * Remove blocks "blkno" through "blkno + len - 1" from the cache
 * only delete exact subsets of (blkno, blkno+len-1)
 * This is done to:
 * (a) keep the cache clean of detritus from removed files and
 * (b) to cancel i/o operations on deleted file data.
 * Hopefully files with a short lifetime will never go out to disk!
 *
 * XXX fix code to not rescan when freelist is broken
 * XXX if buffer is being chopped off at the end, we should just adjust
 * XXX its length with allocbuf(), once it can handle it.
 */
void
btoss(dev, blkno, len)
	register dev_t dev;
	register daddr_t blkno;
	int len;
{
	register struct buf *bp;
	register struct buf *dp;
	register daddr_t endblkno;
	register int oldlen;
	int s;

	endblkno = blkno + len;			/* just past end */

loop:
	s = splclock();
	for (dp = &bfreelist[BQ_AGE]; dp > bfreelist; dp--)
	for (bp = dp->av_forw; bp != dp; bp = bp->av_forw) {
		if ((bp->b_dev == dev) &&
		    ((bp->b_flags & (B_BUSY | B_INVAL)) == 0) &&
		    ((bp->b_blkno >= blkno) &&
		     (bp->b_blkno + btod(bp->b_bufsize) <= endblkno))) {
			/*
			 * Buffer is wholly contained within the
			 * freed blocks.  Invalidate it and free its
			 * memory.
			 */
			notavailspl(bp, dp);
			splx(s);
			bp->b_flags |= B_INVAL;
			bp->b_flags &= ~B_DELWRI;
			allocbuf(bp, 0);
			brelse(bp);
			/*
			 * Since we just messed up the freelist linkage,
			 * start over at the top (UGH).
			 */
			goto loop;
		}
	}
	splx(s);
}

/*
 * Write the buffer, waiting for completion.
 * Then release the buffer.
 */
bwrite(bp)
	register struct buf *bp;
{
	register flag;
	int s;

	sysinfo.lwrite++;
	flag = bp->b_flags;
	bp->b_flags &= ~(B_READ | B_DONE | B_ERROR | B_DELWRI);
	if ((flag&B_DELWRI) == 0)
		u.u_ru.ru_oublock++;		/* noone paid yet */
	trace(TR_BWRITE, bp->b_vp, bp->b_blkno);
	if (bp->b_bcount > bp->b_bufsize)
		panic("bwrite");
	VOP_STRATEGY(bp);
#if RISCOS
	u.u_iow++;
#endif
	sysinfo.bwrite++;

	/*
	 * If the write was synchronous, then await i/o completion.
	 * If the write was "delayed", then we put the buffer on
	 * the q of blocks awaiting i/o completion status.
	 */
	if ((flag&B_ASYNC) == 0) {
		iowait(bp);
		brelse(bp);
	} else if (flag & B_DELWRI) {
		s = splclock();
		bp->b_flags |= B_AGE;
		splx(s);
	}
}

/*
 * Assign a buffer for the given block.  If the appropriate block is already
 * associated, return it; otherwise search for the oldest non-busy buffer and
 * reassign it.
 *
 * We use splx here because this routine may be called
 * on the interrupt stack during a dump, and we don't
 * want to lower the ipl back to 0.
 */
struct buf *
getblk(vp, blkno, size)
	struct vnode *vp;
	daddr_t blkno;
	int size;
{
	register struct buf *bp, *ep, *dp;
	int s;
	daddr_t start, last;

#ifdef RISCOS
	if (size > FSMAXBSIZE)
#else
	if (size > MAXBSIZE)
#endif
		panic("getblk: size too big");
	/*
	 * To prevent overflow of 32-bit ints when converting block
	 * numbers to byte offsets, blknos > 2^32 / DEV_BSIZE are set
	 * to the maximum number that can be converted to a byte offset
	 * without overflow. This is historic code; what bug it fixed,
	 * or whether it is still a reasonable thing to do is open to
	 * dispute. mkm 9/85
	 */
	if ((unsigned)blkno >= 1 << (sizeof(int)*NBBY-DEV_BSHIFT))
		blkno = 1 << ((sizeof(int)*NBBY-DEV_BSHIFT) + 1);
	/*
	 * Search the cache for the block.  If we hit, but
	 * the buffer is in use for i/o, then we wait until
	 * the i/o has completed.
	 */
	dp = BUFHASH(vp, blkno);
loop:
	for (bp = dp->b_forw; bp != dp; bp = bp->b_forw) {
		if (bp->b_blkno != blkno || bp->b_vp != vp ||
		    bp->b_flags&B_INVAL)
			continue;
		s = splclock();
		if (bp->b_flags&B_BUSY) {
			bp->b_flags |= B_WANTED;
			syswait.iowait++;
			(void) sleep((caddr_t)bp, PRIBIO+1);
			syswait.iowait--;
			(void) splx(s);
			goto loop;
		}
		if (bp->b_flags & B_LRU) {
			notavailspl(bp, &bfreelist[BQ_LRU]);
		} else if (bp->b_flags & B_LOCKED) {
			notavailspl(bp, &bfreelist[BQ_LOCKED]);
		} else {
			notavailspl(bp, &bfreelist[BQ_AGE]);
		}
		(void) splx(s);
		if (bp->b_bcount != size && brealloc(bp, size, 1 /* flush B_DELWRI buffers if growing */) == 0)
			goto loop;
		bp->b_flags |= B_CACHE;
		return (bp);
	}
	/*
	 * Allocate the resource, then check to see the buffer pool again.
	 * If we lost the race, release the buffer and try again.
	 */
	bp = getnewbuf();
	bfree(bp);
	bremhash(bp);
	/*
	 * hash it on something in case we lost the race
	 */
	binshash(bp, &bfreelist[BQ_EMPTY]);
	bsetvp(bp, vp);
	bp->b_dev = vp->v_rdev;
	bp->b_blkno = blkno;
	bp->b_error = 0;
	bp->b_resid = 0;
	if (allocbuf(bp, size) == 0)
		goto loop;

	start = bp->b_blkno;
	last = start + btodb(size) - 1;

	for (ep = dp->b_forw; ep != dp; ep = ep->b_forw) {
		if (ep == bp || ep->b_vp != bp->b_vp || (ep->b_flags&B_INVAL))
			continue;
		/* look for overlap */
		if (ep->b_bcount == 0 || ep->b_blkno > last ||
		    ep->b_blkno + btodb(ep->b_bcount) <= start)
			continue;
		s = splclock();
		if (ep->b_flags&B_BUSY) {
			bp->b_flags |= B_INVAL;
			brelse(bp);
			ep->b_flags |= B_WANTED;
			syswait.iowait++;
			(void) sleep((caddr_t)ep, PRIBIO+1);
			syswait.iowait--;
			(void) splx(s);
			goto loop;
		}
		if (ep->b_flags & B_LRU) {
			notavailspl(ep, &bfreelist[BQ_LRU]);
		} else if (ep->b_flags & B_LOCKED) {
			notavailspl(ep, &bfreelist[BQ_LOCKED]);
		} else {
			notavailspl(ep, &bfreelist[BQ_AGE]);
		}
		if (ep->b_flags & B_DELWRI) {
			bp->b_flags |= B_INVAL;
			brelse(bp);
			(void) splx(s);
			bwrite(ep);
			goto loop;
		}
		ep->b_flags |= B_INVAL;
		(void) splx(s);
		brelse(ep);
	}
	bremhash(bp);
	binshash(bp, dp);
	return (bp);
}

/*
 * get an empty block,
 * not assigned to any particular device
 */
struct buf *
geteblk(size)
	int size;
{
	register struct buf *bp, *flist;

#ifdef RISCOS
	if (size > FSMAXBSIZE)
#else
	if (size > MAXBSIZE)
#endif
		panic("geteblk: size too big");
loop:
	bp = getnewbuf();
	bp->b_flags |= B_INVAL;
	bfree(bp);
	bremhash(bp);
	flist = &bfreelist[BQ_AGE];
	brelvp(bp);
	bp->b_error = 0;
	bp->b_resid = 0;
	binshash(bp, flist);
	if (brealloc(bp, size, 0 /* don't flush B_DELWRI buffers if growing */) == 0)
		goto loop;
	return (bp);
}

/*
 * Pick up the device's error number and pass it to the user;
 * if there is an error but the number is 0 set a generalized code.
 */
geterror(bp)
	register struct buf *bp;
{
	int error = 0;

	if (bp->b_flags&B_ERROR)
		if ((error = bp->b_error)==0)
			return (EIO);
	return (error);
}

/*
 * getnewbuf - riscos adapted buffer manager
 * Get a block off the freelists. 
 * Algorithm used to select a buffer is as follows:
 *
 * If we have enough pages, get buf from empty list.
 * OR if there are free AGE bufs, grab one of them.
 * OR sleep waiting for free buf, and let user try again.
 */
struct buf*
getnewbuf()
{
	register struct buf *bp, *dp;
	int s;

loop:
	s = splclock();
#if FSMAXBSIZE >= NBPC
	if ((bio_curpages < bio_targpages)
#else
	if ((bio_curpages < bio_targpages || bio_pfcount != 0)
#endif
	     && ((dp = &bfreelist[BQ_EMPTY]) != bfreelist[BQ_EMPTY].av_forw)) {
		bp = dp->av_forw;
	} else {
		dp = &bfreelist[BQ_AGE];
		for (bp = dp->av_forw; bp != dp; bp = bp->av_forw) {
			if (!(bp->b_flags & B_DELWRI))
				goto found;
		}
		dp = bfreelist;
		dp->b_flags |= B_WANTED;
		syswait.iowait++;
		(void) sleep((caddr_t)dp, PRIBIO+1);
		syswait.iowait--;
		splx(s);
		goto loop;
	}
found:
	notavailspl(bp, dp);
	splx(s);
	trace(TR_BRELSE, bp->b_vp, bp->b_blkno);
	bp->b_flags = B_BUSY;  /* Clears B_INVAL among other flags */
	return (bp);
}

/*
 * See if the block is associated with some buffer
 * (mainly to avoid getting hung up on a wait in breada)
 */
incore(vp, blkno)
	struct vnode *vp;
	daddr_t blkno;
{
	register struct buf *bp;
	register struct buf *dp;

	dp = BUFHASH(vp, blkno);
	for (bp = dp->b_forw; bp != dp; bp = bp->b_forw)
		if (bp->b_blkno == blkno && bp->b_vp == vp &&
		    (bp->b_flags & B_INVAL) == 0)
			return (1);
	return (0);
}


/*	make sure all buffers are flushed before shutdown
*/

bflushall()
{
	register struct buf *bp;
	register struct bufhd *hp;
	register unsigned flags;
	register i,n;
	int s;
#define dp ((struct buf *)hp)

	for(i = 0; i < 10000; i++) {
		n = 0;
		s = splclock();
		for (hp = bufhash; hp < &bufhash[BUFHSZ]; hp++) {
			for (bp = dp->b_forw; bp != dp; bp = bp->b_forw) {
				extern struct vnodeops nfs_vnodeops;
				if (bp->b_vp && (bp->b_vp->v_op == &nfs_vnodeops))
					continue;
				flags = bp->b_flags;
				if(!(flags & (B_DONE|B_INVAL)))
					n++;
			}
		}
		splx(s);
		if(n == 0) {
			break;
		} else {
			DELAY(1024);
		}
	}
	if(i >= 10000)
		cmn_err(CE_CONT,"bflushall: Dirty buffers ignored before shutdown\n");
#undef dp
}

