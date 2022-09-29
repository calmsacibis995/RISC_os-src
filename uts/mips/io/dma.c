/*
 * |-----------------------------------------------------------|
 * | Copyright (c) 1989       MIPS Computer Systems, Inc.      |
 * | All Rights Reserved                                       |
 * |-----------------------------------------------------------|
 * | Use, duplication, or disclosure by the Government is      |
 * | subject to restrictions as set forth in                   |
 * | subparagraph (c)(1)(ii) of the Rights in Technical        |
 * | Data and Computer Software Clause of DFARS 52.227-7013.   |
 * |         MIPS Computer Systems, Inc.                       |
 * |         928 Arques Avenue                                 |
 * |         Sunnyvale, CA 94086                               |
 * |-----------------------------------------------------------|
 */
/* $Header: dma.c,v 1.7.1.4.1.2.1.2 90/11/20 14:56:22 beacker Exp $ */

/*
 * Dma utilities.
 *
 * Written by: Kipp Hickman
 */
#include "sys/debug.h"
#include "sys/param.h"
#include "sys/types.h"
#include "sys/sysmacros.h"
#include "sys/buf.h"
#include "sys/sbd.h"
#include "sys/immu.h"
#include "sys/region.h"
#include "bsd/sys/time.h"
#include "sys/proc.h"
#include "sys/dir.h"
#include "sys/signal.h"
#include "sys/errno.h"
#include "sys/pcb.h"
#include "sys/user.h"
#include "sys/map.h"

extern int cache_bufs;		/* true if I/O buffer mem should be cached */
extern int dcache_size;		/* so we don't redundantly invalidate */

/*
 * Bits we should turn on to make the virtual address kernel addressable
 */
#define	GOOD	(PG_G | PG_M | PG_N | PG_SV | PG_VR)

/*
 * Allocate space in the system segment page table.  Copy the physical page
 * numbers from the segments underlying the buffer into the system segment
 * page table.  Setup b_dmaaddr to point to the system segment where the
 * buffer is now mapped.  Insure that the memory in the system segment is
 * accessible read/write by the kernel so as to allow the drivers to copy
 * data in and out if they need to.
 */
iomap(bp)
	register struct buf *bp;
{
	register pde_t *to, *from;
	register caddr_t baddr;
	register int i;
	register int pages;
	register long kmap;
	register long offset;
	unsigned long writeback_addr, writeback_bcount, remaining_bcount;
	int s;

	ASSERT(bp->b_bcount);
	ASSERT(bp->b_dmaaddr == 0);
	ASSERT(bp->b_dmalen == 0);

	baddr = bp->b_un.b_addr;
	offset = ((long) baddr) & (NBPC - 1);
	pages = btoc(offset + bp->b_bcount);
	bp->b_dmalen = pages;
	/*
	 * The buffer cache is already mapped into KSEG2, so we don't need
	 * to do anything here.
	 */
	if (!(bp->b_flags & B_SWAP) && IS_KSEG2(baddr)) {
		bp->b_dmaaddr = baddr;
		/*
		 *  Writeback the cache on a page by page basis, and only
		 *  those bytes involved in the I/O transfer.
		 */
		remaining_bcount = bp->b_bcount;
		for (i=1; i<=pages; i++) {
		    writeback_addr = ptosv(kvtokptbl(baddr)->pgm.pg_pfn)
					+ offset;
		    writeback_bcount = min( NBPC-offset, remaining_bcount );
		    writeback_cache( writeback_addr, writeback_bcount );
		    offset = 0;		/* next writeback at page boundary */
		    remaining_bcount -= writeback_bcount;
		    baddr += writeback_bcount;
		} /* writeback_cache for each physical page */
		return;
	}

	/*
	 * Allocate system segment space
	 */
	s = splhi();
	while ((kmap = malloc(sptmap, pages)) == 0) {
		mapwant(sptmap) = 1;
		sleep((caddr_t)sptmap, PRIBIO);
	}
	splx(s);
	bp->b_dmaaddr = (caddr_t) ctob(kmap) + offset;
	to = kvtokptbl(bp->b_dmaaddr);

	/*
	 * Copy pte info from page tables underneath buffer to system
	 * page table.  The tests here are ordered in terms of most-likely
	 * to least-likely under normal conditions.
	 */
	if (bp->b_flags & B_SWAP) {
		/*
		 * Swap request.  For now, they always come one at a time.
		 */
		ASSERT(pages == 1);
		to->pgi.pg_pde = mkpde(GOOD, btoc(baddr));
		writeback_cache( ptosv(to->pgm.pg_pfn) + offset, bp->b_bcount );
	} else
	if (IS_KUSEG(baddr)) {
		/*
		 * Physical I/O to/from current process.
		 */
		/* try to make sure this isn't called at interrupt time */
		ASSERT(u.u_procp == bp->b_proc);
		remaining_bcount = bp->b_bcount;
		for (i = pages; --i >= 0; ) {
			from = (pde_t *) vtop((unsigned)baddr, u.u_procp);
			ASSERT(from);
			ASSERT(from->pgm.pg_pfn);
			to->pgi.pg_pde = mkpde(GOOD, from->pgm.pg_pfn);
			writeback_bcount = min( NBPC-offset, remaining_bcount );
			writeback_cache( ptosv(from->pgm.pg_pfn) + offset
					,writeback_bcount );
			remaining_bcount -= writeback_bcount;
			offset = 0;
			to++;
			baddr += NBPC;
		}
	} else
	if (IS_UAREA(baddr)) {
		/*
		 * I/O to/from udot
		 */
		from = u.u_procp->p_ubptbl + (int)btoc(baddr - (caddr_t)&u);
		remaining_bcount = bp->b_bcount;
		for (i = pages; --i >= 0; ) {
			ASSERT(from->pgm.pg_pfn);
			to->pgi.pg_pde = mkpde(GOOD, from->pgm.pg_pfn);
			writeback_bcount = min( NBPC-offset, remaining_bcount );
			writeback_cache( ptosv(to->pgm.pg_pfn) + offset
					,writeback_bcount );
			remaining_bcount -= writeback_bcount;
			offset = 0;
			to++;
			from++;
		}
	} else {
		/*
		 * I/O to/from kernel memory.  This code assumes that
		 * kernel physical and kernel virtual are mapped one-to-one.
		 */
		register uint pfn;

		pfn = svtopfn(baddr);
		remaining_bcount = bp->b_bcount;
		for (i = pages; --i >= 0; ) {
			ASSERT(pfn);
			to->pgi.pg_pde = mkpde(GOOD, pfn);
			writeback_bcount = min( NBPC-offset, remaining_bcount );
			writeback_cache( ptosv(pfn) + offset,writeback_bcount );
			remaining_bcount -= writeback_bcount;
			offset = 0;
			to++;
			pfn++;
		}
	}
}

/*
 * Release mapping resources assigned above.
 */
iounmap(bp)
	register struct buf *bp;
{
	long kmap;
	long offset;
	long pages;
	int s;
	register caddr_t baddr;
	int remaining_bcount;
	register pde_t *pde;
	unsigned long inval_addr,inval_bcount,inval_offset;

	/*
	 * If buffer has no dma resources, just return.  This happens
	 * when buffers have errors in them.
	 */
	if (bp->b_dmalen == 0) {
		ASSERT(bp->b_dmaaddr == 0);
		return;
	}
	offset = ((long) bp->b_dmaaddr) & (NBPC - 1);
	pages = btoc(offset + bp->b_bcount);
	ASSERT(bp->b_dmalen == pages);

	/*
	 *  If we've finished a Read into a cached buffer, then we must
	 *  write-back and invalidate the caches in order to avoid stale data.
	 */
	if (bp->b_flags & B_READ) {
	    ASSERT(bp->b_bcount <= 0x7fffffff);    /* will convert to int */
	    baddr = bp->b_dmaaddr;
	    pde = kvtokptbl(baddr);
	    ASSERT(pde->pgm.pg_pfn);	/* page better be allocated! */
	    /* If iomap used the original K2 address mapping then if the
	     * buf is not cached, no need to invalidate.  If iomap
	     * re-mapped the region, then we may need to invalidate
	     * the original virtual address (re-map address is always PG_N).
	     */
	    if ((baddr == bp->b_un.b_addr) && (pde->pgi.pg_pde & PG_N)) {
		goto out;
	    }
	    remaining_bcount = (int)bp->b_bcount;
	    invalidate_virt_dcache( bp->b_un.b_addr, remaining_bcount );
	    inval_offset = offset;
	    if (baddr != bp->b_un.b_addr  &&  remaining_bcount < dcache_size
		&& !(pde->pgi.pg_pde & PG_N)) {
		/*
		 *  iomap() mapped the original baddr into a K2 address, and
		 *  the driver might have mucked with the Read buffer through
		 *  that address.  So invalidate that virtual cache section,
		 *  also (unless the previous invalidate was big enough to
		 *  cleanse the entire d-cache).
		 */
	        invalidate_virt_dcache( (long)baddr, remaining_bcount );
	    }
	    /*
	     *  Now invalidate the physical cache, page by page, but only
	     *  as much as necessary.
	     */
	    while (remaining_bcount > 0) {
		pde = kvtokptbl(baddr);
		ASSERT(pde->pgm.pg_pfn);	/* page better be allocated! */
		inval_addr = ptosv(pde->pgm.pg_pfn) + inval_offset;
		inval_bcount = min( NBPC-inval_offset, remaining_bcount );
		invalidate_scache( inval_addr, inval_bcount ); /* 6000     */
		clean_dcache(	   inval_addr, inval_bcount ); /* non-6000 */
		remaining_bcount -= inval_bcount;
		baddr += inval_bcount;
		inval_offset = 0;
	    } /* while */
	} /* if B_READ */
out:
	/*
	 * If buffer was mapping KSEG2, then life is easy - there is
	 * nothing to release.
	 */
	if (!(bp->b_flags & B_SWAP) && IS_KSEG2(bp->b_un.b_addr)) {
		ASSERT(bp->b_un.b_addr == bp->b_dmaaddr);
	} else {
		/*
		 * Compute map location and release space back into sptmap
		 */
		kmap = btoct(bp->b_dmaaddr);

		ASSERT((((long) bp->b_un.b_addr) & (NBPC - 1)) == offset);
		sptfree(bp->b_dmaaddr, pages, 0);
	}
	bp->b_dmaaddr = 0;
	bp->b_dmalen = 0;
}

/*
 * Map a range of user virtual addresses to kernel virtual addresses.
 * Used by drivers that want to copy to locked down pages at interrupt time
 * that don't have access to a buffer header
 */
caddr_t
maputokv(uvaddr, count)
register caddr_t uvaddr;
register int count;
{
	register pde_t *to, *from;
	register int i;
	register int pages;
	register long kmap;
	register long offset;
	register caddr_t kvaddr;
	int s;

	ASSERT(IS_KUSEG(uvaddr));

	offset = ((long) uvaddr) & (NBPC - 1);
	pages = btoc(offset + count);

	/*
	 * Allocate system segment space
	 */
	s = splhi();
	while ((kmap = malloc(sptmap, pages)) == 0) {
		mapwant(sptmap) = 1;
		sleep((caddr_t)sptmap, PRIBIO);
	}
	splx(s);

	kvaddr = (caddr_t) ctob(kmap) + offset;
	to = kvtokptbl(kvaddr);

	/*
	 * Physical I/O to/from current process.
	 */
	for (i = pages; --i >= 0; ) {
		from = (pde_t *) vtop((unsigned)uvaddr, u.u_procp);
		ASSERT(from);
		ASSERT(from->pgm.pg_pfn);
		to->pgi.pg_pde = mkpde(GOOD, from->pgm.pg_pfn);
		to++;
		uvaddr += NBPC;
	}
	return(kvaddr);
}

/*
 * Unmap the request
 */
unmaputokv(kvaddr, count)
register caddr_t kvaddr;
register int count;
{
	long kmap;
	long offset;
	long pages;

	ASSERT(IS_KSEG2(kvaddr));

	offset = ((long) kvaddr) & (NBPC - 1);
	pages = btoc(offset + count);

	sptfree(kvaddr, pages, 0);
}


/*
 * Perform a CPU cache writeback, possibly spanning physical memory page
 * boundaries.
 *
 * The dmaaddr may be K0, K1, or K2.  We will flush data from the dcache
 * and scache which correspond to the specified virtual address range.
 *
 * If the caller specified a K1 (non-cacheable) address, we will still flush
 * the corresponding K0 addresses.  The assumption is that the caller may be
 * using both K0 and K1 addresses to reference the page and happened to pass
 * us the K1 address.  If the caller were certain that all references were
 * through K1, then it doesn't need to call this routine.
 *
 * This routine is intended primarily for I/O device drivers which don't
 * really care about memory page boundaries or the kind of cache(s) the CPU
 * has.
 */

writeback_virtual_data( dmaaddr, dmasize )
unsigned long dmaaddr;
unsigned long dmasize;
{
#ifdef R6000  
  unsigned long bcount, k0addr;

  if (IS_KUSEG(dmaaddr)) {
    
    /* Address is a user address.  Not allowed ! */
    cmn_err(CE_PANIC, "KUSEG address not supported!");
    
  } else if (IS_KSEG2(dmaaddr)) {
    
    /*
     * Address is K2.  We must map the virtual address to the physical
     * address before flushing the cache.  Do this a page at a time.
     */
    while (dmasize > 0) {
      k0addr = ptosv(kvtokptbl(dmaaddr)->pgm.pg_pfn) |	poff(dmaaddr);
      bcount = NBPC - poff(dmaaddr);
      if (bcount > dmasize)
	bcount = dmasize;
      writeback_cache( k0addr, bcount );
      dmasize -= bcount;
      dmaaddr += bcount;
    }
  } else {
    
    /*
     * Address is either K0 or K1.  In either case, make certain that the
     * K0 addresses corresponding to the requested physical addresses are
     * flushed to physical memory. Do this one page at a time.
     */
    while (dmasize > 0) {
      bcount = NBPC - poff(dmaaddr);
      if (bcount > dmasize)
	bcount = dmasize;
      writeback_cache( dmaaddr, bcount );
      dmasize -= bcount;
      dmaaddr += bcount;
    }
  }
#else
    wbflush();
#endif R6000
}



/*
 * Perform a CPU cache invalidate, possibly spanning physical memory page
 * boundaries.
 *
 * The dmaaddr may be K0, K1, or K2.  We will invalidate data from the dcache
 * and scache which correspond to the specified virtual address range.
 *
 * If the caller specified a K1 (non-cacheable) address, we will still
 * invalidate the corresponding K0 addresses.  The assumption is that the
 * caller may be using both K0 and K1 addresses to reference the page and
 * happened to pass us the K1 address.  If the caller were certain that all
 * references were through K1, then it doesn't need to call this routine.
 *
 * This routine is intended primarily for I/O device drivers which don't
 * really care about memory page boundaries or the kind of cache(s) the CPU
 * has.
 */


invalidate_virtual_data( dmaaddr, dmasize )
unsigned long dmaaddr;
unsigned long dmasize;
{
  unsigned long bcount, k0addr;
  /*
   * Invalidate CPU cache so next access to will obtain the
   * latest data following the I/O operation.
   */

  if (dmaaddr & 0x03) {
    /* Some cache routines (i.e. 6000) require word addresses.  Fix
     * caller's address & count to be word aligned & word multiple.
     */
    dmasize = (dmasize + 7) & 0xfffffffc;	/* round up to mult of 4 */
    dmaaddr &= 0xfffffffc;		/* round to word address */
  }
  if (IS_KUSEG(dmaaddr)) {
    
    /* Address is a user address.  Not allowed ! */
    cmn_err(CE_PANIC, "KUSEG address not supported!");
    
  } else if (IS_KSEG2(dmaaddr)) {
    
    /*
     * Address is K2.  We must map the virtual address to the physical
     * address before flushing the cache.  Do this a page at a time.
     */
    while (dmasize > 0) {
      k0addr = ptosv(kvtokptbl(dmaaddr)->pgm.pg_pfn) |	poff(dmaaddr);
      bcount = NBPC - poff(dmaaddr);
      if (bcount > dmasize)
	bcount = dmasize;
#ifdef R6000      
      invalidate_virt_dcache( dmaaddr, bcount );
      invalidate_scache( k0addr, bcount );
#else
      clean_dcache( k0addr, bcount );
#endif R6000      
      dmasize -= bcount;
      dmaaddr += bcount;
    }
  } else {
    
    /*
     * Address is either K0 or K1.  In either case, make certain that the
     * K0 addresses corresponding to the requested physical addresses are
     * flushed to physical memory. Do this one page at a time.
     *
     * Note: It's OK to pass "virtual" address to invalidate_scache
     *       here since it also corresponds to the "physical" address.
     */
#ifndef R6000
    if (IS_KSEG1(dmaaddr))	/* clean_dcache doesn't like K1 address */
      return;
#endif R6000
    while (dmasize > 0) {
      bcount = NBPC - poff(dmaaddr);
      if (bcount > dmasize)
	bcount = dmasize;
#ifdef R6000
      invalidate_virt_dcache( dmaaddr, bcount );   /* dmaaddr is virt addr */
      invalidate_scache( dmaaddr, bcount );	/* dmaaddr is phys addr too */
#else
      clean_dcache( dmaaddr, dmasize );
#endif R6000      
      dmasize -= bcount;
      dmaaddr += bcount;
    }
  }
}

