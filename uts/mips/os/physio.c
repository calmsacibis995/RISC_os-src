/*
 * |-----------------------------------------------------------|
 * | Copyright (c) 1990 MIPS Computer Systems, Inc.            |
 * | All Rights Reserved                                       |
 * |-----------------------------------------------------------|
 * |          Restrictive Rights Legend                        |
 * | Use, duplication, or disclosure by the Government is      |
 * | subject to restrictions as set forth in                   |
 * | subparagraph (c)(1)(ii) of the Rights in Technical        |
 * | Data and Computer Software Clause of DFARS 52.227-7013.   |
 * |         MIPS Computer Systems, Inc.                       |
 * |         928 Arques Avenue                                 |
 * |         Sunnyvale, CA 94086                               |
 * |-----------------------------------------------------------|
 */
#ident	"$Header: physio.c,v 1.25.1.5.1.1.1.4 90/11/08 18:36:47 beacker Exp $"

/*	Copyright (c) 1984 AT&T	*/
/*	  All Rights Reserved  	*/

/*	THIS IS UNPUBLISHED PROPRIETARY SOURCE CODE OF AT&T	*/
/*	The copyright notice above does not evidence any   	*/
/*	actual or intended publication of such source code.	*/

#include "sys/types.h"
#include "sys/sysmacros.h"
#include "sys/signal.h"		/* needed by user.h */
#include "sys/pcb.h"		/* needed by user.h */
#include "sys/user.h"
#include "sys/immu.h"		/* needed by proc.h */
#include "sys/sbd.h"		/* needed by proc.h */
#include "sys/region.h"		/* needed by proc.h */
#include "sys/proc.h"
#include "sys/sysinfo.h"
#include "sys/errno.h"
#include "sys/cmn_err.h"
#include "sys/debug.h"
#include "sys/buf.h"
#include "sys/getpages.h"
#include "sys/swap.h"
#include "sys/conf.h"
#include "sys/var.h"
#include "sys/getpages.h"
#include "sys/tuneable.h"
#include "sys/cpu_board.h"
#include "bsd43/sys/vmmeter.h"
#include "sys/vnode.h"
#include "sys/uio.h"

int icache_size;
int dcache_size;

/* Max. number of pages to swap per I/O */

#define NPAGE NPGPT

/*
 * swap I/O
 */

/*
 *	Swap the pages specified in the pglptr page list to or from disk.
 *
 *	This routine writes all buffers at once to take advantage of
 *	the fact the swap space is usually allocated sequentially on disk.
 *	A clever disk driver could write all buffers in one chunk.
 *
 *	If there are more pages to do than there are disk buffers available,
 *	then the algorithm grabs as many buffers as it can.  The code isn't
 *	optimized for this case because it should be rare.
 */
swdone();
extern vhand();

swap(pglptr, npage, rw)
pglst_t	*pglptr;
{
	int	pgl_index;	/* First unprocessed page in pglptr */
	buf_t	*bufs[MAXSPGLST];
	buf_t	*bp;
	int	buf_index;	/* Index of first unused slot of page_bufs */
	pfd_t	*pfd;
	pde_t	*pd;
	dbd_t	*dbd;
	dev_t	dev;
	int	ospl;
	int	i;
	int	async = (rw & B_ASYNC);
	int	rw_direction = (rw & (B_READ | B_WRITE));
	struct vnode *vp;
	int resid;

	ASSERT((! async) || (rw_direction == B_WRITE));
	rw &= ~B_ASYNC;
	pgl_index = 0;
	XPR3(XPR_VM,"SWAP %d %s", npage, rw_direction==B_READ?"in":"out");

	/*
	 *	Read/write pages to/from disk as long as there are
	 *	pages left.
	 */
	while (npage) {
		buf_index = 0;

		/*
		 *	Grab as many buffers as we need.
		 *	(Or as many as we can if there aren't enough.)
		 */
		while (pfreelist.av_forw == NULL) {
			ASSERT(! async); /* caller must assure buffers */
			pfreelist.b_flags |= B_WANTED;
			sleep(&pfreelist, PRIBIO);
		};
		while ( npage && pfreelist.av_forw != NULL ) {
			bufs[buf_index] = pfreelist.av_forw;
			pfreelist.av_forw = bufs[buf_index]->av_forw;
			++buf_index;
			--npage;
		}
		if (npage) {
			ASSERT(! async);
			cmn_err(CE_NOTE, 
				"swap() used up all physical disk buffers.");
			cmn_err(CE_CONT, 
				"\tconsider increasing NPBUF.\n");
		}

		/*
		 *	Fill in required data for each buffer
		 *	and clear the cache for the page being read.
		 */
		for( i = 0;  i < buf_index;  ++i ) {
			bp  = bufs[i];
			pd = pglptr[pgl_index].pl_pde;
			pfd  = pglptr[pgl_index++].pl_pfdat;
			ASSERT(rw || (pfd->pf_type == DBD_SWAP));
			if (pd)
				dbd = pdetodbd(pd);
			else
				dbd = &(pfd->pf_dbd);
			dev = swaptab[dbd->dbd_swpi].st_vp->v_rdev;
			vp = swaptab[dbd->dbd_swpi].st_vp;
			bp->b_flags = B_BUSY | B_PHYS | B_SWAP | rw;
			bp->b_dev = dev;
			bp->b_bcount = ctob(1);
			bp->b_un.b_addr = (caddr_t)ctob(pfdattopfn(pfd));
			bp->b_blkno = dbd->dbd_blkno;
			bp->b_proc = u.u_procp;
			bp->b_vp = vp;
			if (async) {
				bp->b_iodone = swdone;
				bp->b_flags |= B_CALL;
			};
			if (IS_R6300) {
			    if (rw == B_READ) {
				/* write-back and invalidate s-cache */
				invalidate_scache( PHYS_TO_K0(bp->b_un.b_addr)
						  ,NBPC );
			    } else {
				/* writing: just write-back */
				writeback_cache( PHYS_TO_K0(bp->b_un.b_addr)
						,NBPC );
			    }
			} else {
			    if (rw == B_READ) {
				clean_cache(PHYS_TO_K0(bp->b_un.b_addr), NBPC);
			    }
			}
			u.u_iosw++;
			if (rw_direction == B_READ) {
				++sysinfo.swapin;
				++sysinfo.bswapin;
			} else {
				++sysinfo.swapout;
				++sysinfo.bswapout;
			}
		} /* for all buffers */

		/*
		 *	Initiate disk io on all buffers.
		 *	This loop could be combined with the previous
		 *	one, but I want to get these blocks down
		 *	to the driver all at once so that it can
		 *	write them in groups, if possible.
		 *
		 *	XXX How can I force the driver to wait so that
		 *	it doesn't initiate the first block before the
		 *	rest have been queued up?
		 */
		for( i = 0;  i < buf_index;  ++i ) {
			bp  = bufs[i];
                        if (bp->b_vp && bp->b_vp->v_type == VBLK) {
                                (*bdevsw[bmajor(bp->b_dev)].d_strategy)(bp);
                        }
                        else {
                                if (bp->b_flags & B_PHYS)
                                        bp->b_un.b_addr = (caddr_t) PHYS_TO_K0(bp->b_un.b_addr);
                                VOP_STRATEGY(bp);
                        }
		}

		if (async)
			break;

		/*
		 *	Wait for io to complete on all buffers.
		 *	Free buffers once the io is complete.
		 */
		ospl = splclock();
		for( i = 0;  i < buf_index;  ++i ) {
			bp = bufs[i];
			bufs[i] = NULL;
			while ((bp->b_flags & B_DONE) == 0)
				sleep(bp, PRIBIO);
			if (bp->b_flags & B_ERROR)
				cmn_err(CE_PANIC,"swap - i/o error in swap");
			if (rw_direction == B_READ) {
				++sysinfo.swapin;
				++sysinfo.bswapin;
				cnt.v_pgin++;
				cnt.v_pgpgin += bp->b_bcount / NBPP;
			} else {
				++sysinfo.swapout;
				++sysinfo.bswapout;
				cnt.v_pgout++;
				cnt.v_pgpgout += bp->b_bcount / NBPP;
			};
			free_swap_buf(bp);
		}
		splx(ospl);

	} /* while more pages */
	XPR1(XPR_VM,"swap-done\n");
}


/*
 * Put a buffer on the clean list after I/O is done.
 * Called from iodone.
 */
swdone(bp)
	register struct buf *bp;
{
	register int s;

	if (bp->b_flags & B_ERROR)
		cmn_err(CE_PANIC,"swap - i/o error in swap");
	s = splbio();
	bp->av_forw = bclnlist;
	bclnlist = bp;
	++sysinfo.swapout;
	++sysinfo.bswapout;
	cnt.v_pgout++;
	cnt.v_pgpgout += bp->b_bcount / NBPP;
	wakeup((caddr_t)vhand);
	splx(s);
}


/*
 * Free a swap buf header
 */

free_swap_buf(bp)
	register struct buf *bp;
{
	register int s;

	bp->b_flags = 0;
	bp->av_forw = pfreelist.av_forw;
	pfreelist.av_forw = bp;
	if (pfreelist.b_flags & B_WANTED) {
		pfreelist.b_flags &= ~B_WANTED;
		wakeup((caddr_t)vhand);
		wakeup((caddr_t)&pfreelist);
	};
}


/*
 * Raw I/O. The arguments are
 * The strategy routine for the device
 * A buffer, which is usually NULL, or special buffer
 *   header owned exclusively by the device for this purpose
 * The device number
 * Read/write flag
 */

physio(strat, bp, dev, rw)
register struct buf *bp;
int (*strat)();
{	register int		count;
	register caddr_t	base;
	register caddr_t	lbase;
	register int		hpf;
	int			ospl;

	count = u.u_count;
	base = u.u_base;
	if (server()) {
#ifdef LATER			/* server() always fails */
		if ((lbase = kseg(btoc(count))) == 0)
			return;
		if (rw == B_WRITE) {
			if (remio(base, lbase, count)) {
				unkseg(lbase);
				return;
			}
			u.u_segflg = 1;	/*you're now dealing with kernel space*/
		}
		u.u_base = lbase;
#endif LATER
	} else {
		if (userdma(base, count, rw) == NULL) {
			if (u.u_error == 0)
				u.u_error = EFAULT;
			return;
		}
	}
	ASSERT(syswait.physio >= 0);
	syswait.physio++;
	if (rw)
		sysinfo.phread++;
	else
		sysinfo.phwrite++;


	hpf = (bp == NULL);
	if (hpf) {
		/*	Get a buffer header off of the
		 *	free list.
		 */

		while (pfreelist.av_forw == NULL) {
			pfreelist.b_flags |= B_WANTED;
			sleep(&pfreelist, PRIBIO);
		};
		bp = pfreelist.av_forw;
		pfreelist.av_forw = bp->av_forw;
	}

	bp->b_flags = B_BUSY | B_PHYS | rw;
	bp->b_error = 0;
	bp->b_proc = u.u_procp;
	bp->b_dev = dev;
	bp->b_un.b_addr = u.u_base;
	bp->b_blkno = btod(u.u_offset);
	bp->b_bcount = u.u_count;

	(*strat)(bp);

	ospl = splclock();
	while ((bp->b_flags & B_DONE) == 0) {
		sleep(bp, PRIBIO);
	}

	splx(ospl);
	if (server()) {
#ifdef LATER
		if (rw == B_READ) {
			u.u_segflg = 0;
			if (unremio(lbase, base, count))
				printf("unremio failed: err=%d\n",u.u_error);
		}
		unkseg(lbase);
#endif LATER
	}
	else
		undma(base, count, rw, NULL);
	bp->b_flags &= ~(B_BUSY|B_PHYS);

	if (bp->b_flags&B_ERROR)
		if ((u.u_error = bp->b_error)==0)
			u.u_error = EIO;
	u.u_count = bp->b_resid;
	u.u_base = u.u_base + (bp->b_bcount - bp->b_resid);

	if (hpf)
		free_swap_buf(bp);
	ASSERT(syswait.physio);
	syswait.physio--;
}


/* XXX fix to not do nblocks << SCTRSHFT -- will cause bad truncation */
physck(nblocks, rw)
daddr_t nblocks;
{
	register unsigned over;
	register off_t upper, limit;
	struct a {
		int	fdes;
		char	*cbuf;
		unsigned count;
	} *uap;

	limit = nblocks << SCTRSHFT;
	if (u.u_offset >= limit) {
		if (u.u_offset > limit || rw == B_WRITE)
			u.u_error = ENXIO;
		return(0);
	}
	upper = u.u_offset + u.u_count;
	if (upper > limit) {
		over = upper - limit;
		u.u_count -= over;
		uap = (struct a *)u.u_ap;
		uap->count -= over;
	}
	return(1);
}


/*
 *	Initialize free list of swap() and physio() buffer headers
 */

swapinit()
{
	buf_t	*bp;

	pfreelist.av_forw = bp = pbuf;
	for (; bp < &pbuf[v.v_pbuf-1]; bp++)
		bp->av_forw = bp+1;
	bp->av_forw = NULL;
}

