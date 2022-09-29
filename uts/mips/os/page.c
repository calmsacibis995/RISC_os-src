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
#ident	"$Header: page.c,v 1.36.1.9.1.5.1.2 90/10/16 10:05:14 beacker Exp $"

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
#include "sys/pfdat.h"
#include "sys/systm.h"
#include "sys/map.h"
#include "sys/cmn_err.h"
#include "sys/swap.h"
#include "sys/cpu_board.h"
#include "sys/debug.h"
#include "sys/vnode.h"
#include "sys/vfs.h"

int			wait16mcnt;	/* #processes waiting for 16M mem */
int			mem_lock;
int			firstfree, maxfree, freemem, freemem16m;
int			availrmem;
int			availsmem;
int			dcache_size;
extern unsigned int	sxbrkcnt;
int			maxmem16m;	/* max free memory under 16M */

extern	int	_riscos_idle_clear_pages;

dbd_t null_dbd = { DBD_NONE, 0, 0 };

/*	Lock memory allocation.
 */
memlock()
{
	register int s = splhi(); 

	while (mem_lock)
		sleep(&mem_lock, PMEM);
	mem_lock = 1;
	splx(s);
}

/* lock memory allocation without sleeping 
 */
int					/* 0=failed 1=locked */
memlock_i()
{
	register int s = splhi(); 

	if (mem_lock) {
		splx(s);
		return 0;
	}
	mem_lock = 1;
	splx(s);
	return 1;
}




/*	Unlock memory allocation.
 */
memunlock()
{
	register int s = splhi(); 

	ASSERT(mem_lock);
	mem_lock = 0;
	wakeup(&mem_lock);
	splx(s);
}

memlocked()
{
	return(mem_lock);
}


/*
 *	getcpages(npgs,nosleep) - gets physically continguous pages, 
 *		the kernel virtual address of the pages allocated.
 *		is returned.
 *	npgs = number of pages to be allocated.
 *	wait for pages only if nosleep =0.
 *
 *	Remark: pages allocated are not mapped to sysreg
 */

pde_t *
getcpages(npgs, nosleep)
{
	register int	pfn;

	do {
		if (npgs <= 0) {
			cmn_err(CE_WARN, "!getcpages - negative or 0 page request");
			return(NULL);
		}
		memlock();
		pfn = contmemall(npgs, 0);
		memunlock();
		if (pfn != NULL)
			return((pde_t *)phystokv(ctob(pfn)));
		if (nosleep) {
			nomemmsg("getcpages", npgs, 1, 0);
			return(NULL);
		}
		cmn_err(CE_NOTE,
			"!getcpages - waiting for %d contiguous pages",
			npgs);
		u.u_procp->p_stat = SXBRK;
		++sxbrkcnt;
		if (runout) {
			runout = 0;
			wakeup(&runout);
		}
		swtch();
	} while (1);
}

/*
 * freepage(pfn)
 */

freepage(pfn)
{
	pde_t base;

	base.pgi.pg_pde = mkpde(PG_P, pfn);
	base.pgi.dbd = null_dbd;
	memlock();
	pfree3(NULL,&base,NULL,1,PF_EITHER,1 /* assume_pfn_is_valid */);
	memunlock();
}

/*
 * contmemall(npgs, flag)
 *
 * allocate phycally contiguous pages
 */

contmemall(npgs, flag)
{
	register struct pfdat	*pfd, *pfd1,*top;
	register int numpages;
	register int pages16m;

	/*
	 * Need contiguous memory
	 */
	
	ASSERT(memlocked());

	if (freemem < npgs)
		return(NULL);

	top = pfntopfdat(maxclick);
	if (flag & PAGE_UNDER_16M) {
		if (freemem16m < npgs)
			return(NULL);
		if (maxclick > btoc(MEM16M))
		    top = pfntopfdat(btoc(MEM16M)); 
	}
	if(npgs == 1) {
		/*
		 * If only one page wanted (e.g. for page table),
		 * take from head of freelist.
		 */
		pfd = phead.pf_next;
		pfd1 = pfd + 1;
	} else {
		pfd = pfntopfdat(btoc(kpbase));

		for (; pfd < top; pfd++) {
			if (pfd->pf_flags & P_QUEUE) {
				for (pfd1 = pfd + 1, numpages = npgs ;
				    pfd1 <= top && --numpages > 0 ;
				    pfd1++)
					if (!(pfd1->pf_flags & P_QUEUE))
						break;
				if (numpages == 0)
					break;
				pfd = pfd1;
			}
		}

		if (pfd >= top)
			return(NULL);
	}

	ASSERT(pfd1 <= top);
	ASSERT(pfd1 - pfd == npgs);

	/*
	 * Take pages *pfd .. *(--pfd1)
	 */

	for (pages16m=0; --pfd1 >= pfd;) {
		(pfd1->pf_prev)->pf_next = pfd1->pf_next;
		(pfd1->pf_next)->pf_prev = pfd1->pf_prev;
		if (pfd1 == pclearp)
			pclearp = &phead;
		pfd1->pf_next = NULL;
		pfd1->pf_prev = NULL;
		if (pfd1->pf_flags&P_HASH)
			premove(pfd1);
		pfd1->pf_dbd = null_dbd;
		pfd1->pf_use = 1;
		pfd1->pf_flags &= P_CLEARED;	/* clear all but P_CLEARED */
		pfd1->pf_rawcnt = 0;
		if (pfdattopfn(pfd1) < btoc(MEM16M))
			pages16m++;	/* count #pages under 16M */
	}
	freemem -= npgs;
	freemem16m -= pages16m;
	return(pfdattopfn(pfd));
}

#ifdef PERFECT_COLORING
/*
 *	getcpages_scc(npgs,nosleep,vpn) - gets physically continguous pages,
 *		colored to secondardy cache color of vpn,
 *		the kernel virtual address of the pages allocated.
 *		is returned.
 *	npgs = number of pages to be allocated.
 *	wait for pages only if nosleep =0.
 *
 *	Remark: pages allocated are not mapped to sysreg
 */

pde_t *
getcpages_scc(npgs, nosleep,vpn)
int npgs;
unsigned int nosleep;
unsigned int vpn;     
{
	register int	pfn;

	do {
		if (npgs <= 0) {
			cmn_err(CE_WARN, "!getcpages - negative or 0 page request");
			return(NULL);
		}
		memlock();
		pfn = contmemall_scc(npgs, vpn);
		memunlock();
		if (pfn != NULL)
			return((pde_t *)phystokv(ctob(pfn)));
		if (nosleep) {
			nomemmsg("getcpages_scc", npgs, 1, 0);
			return(NULL);
		}
		cmn_err(CE_NOTE,
			"!getcpages_scc - waiting for %d contiguous pages",
			npgs);
		u.u_procp->p_stat = SXBRK;
		++sxbrkcnt;
		if (runout) {
			runout = 0;
			wakeup(&runout);
		}
		swtch();
	} while (1);
}

/*
 * contmemall_scc(npgs, vpn)
 *
 * allocate phycally contiguous pages on secondary cache colored boundary
 */

contmemall_scc(npgs, vpn)
int npgs;
unsigned int vpn;     
{
	register struct pfdat	*pfd, *pfd1,*top;
	register int numpages;
	extern unsigned int	scachemask;

	/*
	 * Need contiguous memory
	 */
	
	ASSERT(memlocked());

	if (freemem < npgs)
		return(NULL);

	top = pfntopfdat(maxclick);
	pfd = pfntopfdat(btoc(kpbase));

	for (; pfd < top; pfd++) {
	  if ((pfd->pf_flags & P_QUEUE)  &&
	      ((pfdattopfn(pfd) & scachemask) == (vpn & scachemask)))
	    {
	      for (pfd1 = pfd + 1, numpages = npgs ;
		   pfd1 <= top && --numpages > 0 ;  pfd1++)
		
		if (!(pfd1->pf_flags & P_QUEUE))
		  break;
	      if (numpages == 0)
		break;
	      pfd = pfd1;
	    }
	}

	if (pfd >= top)
	  return(NULL);

	ASSERT(pfd1 <= top);
	ASSERT(pfd1 - pfd == npgs);

	/*
	 * Take pages *pfd .. *(--pfd1)
	 */

	for (; --pfd1 >= pfd;) {
		(pfd1->pf_prev)->pf_next = pfd1->pf_next;
		(pfd1->pf_next)->pf_prev = pfd1->pf_prev;
		if (pfd1 == pclearp)
			pclearp = &phead;
		pfd1->pf_next = NULL;
		pfd1->pf_prev = NULL;
		if (pfd1->pf_flags&P_HASH)
			premove(pfd1);
		pfd1->pf_dbd = null_dbd;
		pfd1->pf_use = 1;
		pfd1->pf_flags &= P_CLEARED;	/* clear all but P_CLEARED */
		pfd1->pf_rawcnt = 0;
	}
	freemem -= npgs;
	return(pfdattopfn(pfd));
}
#endif PERFECT_COLORING

/*
 * Allocate pages and fill in page table
 *	rp		-> region pages are being added to.
 *	base		-> address of page table
 *	size		-> # of pages needed
 *	flag (bits)	-> VALIDATE:	Mark pages valid if set.
 *			-> PAGE_COLOR:	try for "nice" cache placement if set.
 *	nosleep		-> wait for pages only if nosleep is 0.
 *	vpn		-> virtual page number used for page coloring
 * returns:
 *	0	Memory allocated immediately.
 *	1	Had to unlock region and go to sleep before
 *		memory was obtained.  After awakening, the
 *		page was valid or pfree'd so no page was
 *		allocated.
 *	-1	Not enough pages to cover the request.
 *
 * Called with mem_lock set and returns the same way.
 */

int page_color_hit = 0;		/* Tried to color, and it worked. */
int page_color_miss = 0;	/* Tried to color, and it missed. */
int page_color_na = 0;		/* Didn't even try to color.      */
int page_color_on = 1;		/* BOBJ: for debugging and benchmarking
				 * (remove eventually)
				 */

int page_allocation_count = 0;

ptmemall(rp, base, size, flag, nosleep, vpn)
reg_t		*rp;
register pde_t	*base;
register int	flag;
unsigned int	vpn;
{
	register struct pfdat	*pfd;
	register int		i, j;
	register unsigned int	cachemask;
	register unsigned int	pg_color;
	register int		maxcachecolor;	/* max # free pgs to check */
	extern unsigned int	icachemask;
	extern unsigned int	dcachemask;
	extern unsigned int	scachemask;

	/*	Check for illegal size.
	 */

	ASSERT(size > 0);
	ASSERT((rp == NULL) || (rp->r_flags & RG_LOCK) || (flag & REGION_NOT_LOCKED));
	ASSERT( !(base->pgm.pg_pfn) );	/* Don't double allocate. */

#ifdef PERFECT_COLORING
	flag |= PAGE_COLOR;
	if (vpn == 0)
		cmn_err(CE_WARN, "Need nonzero vpn for coloring");
#endif PERFECT_COLORING

loop:
	switch (memreserve(rp, size, nosleep, (flag & REGION_NOT_LOCKED))) {
		case  0: break;
		case -1: return(-1);
		case  1: 
			if (base->pgm.pg_pfn)
			{
				freemem += size;
				return(1);
			}
			break;
	 }

	/*
	 * Even before allocating pages, make sure we have enough 
	 * number of pages of the requested type.
	 */
	if (flag & PAGE_UNDER_16M) {
	    /* Until the coloring algorithm is updated, disallow allocating a
	     * page under 16M and coloring.
	     */
	    if (flag & PAGE_COLOR)
		cmn_err(CE_PANIC, "ptmemall: page under 16M & coloring");

	    /*
	     * If the system has less than 16M bytes of memory, then
	     * reset the PAGE_UNDER_16M flag as any memory we allocate
	     * will always be under 16M bytes address range.
	     */
	    if (maxclick <= btoc(MEM16M))
		flag &= ~PAGE_UNDER_16M;
	    else if (freemem16m < size) {
		register struct proc *p;

		freemem += size;
		if (nosleep)
		    return(-1);

		p = u.u_procp;
		memunlock();
		if (rp != NULL && !(flag & REGION_NOT_LOCKED))
		    regrele(rp);
		wait16mcnt++;
		sleep(&wait16mcnt, PZERO-1);
		if (rp != NULL && !(flag & REGION_NOT_LOCKED))
		    reglock(rp);
		memlock();
		goto loop;
	    }
	}

	/*
	 * Take pages from head of queue
	 */

	if (page_color_on && (flag & PAGE_COLOR)) {
		if (IS_R6300) {
			cachemask = scachemask;
		} else {
		    if (rp != NULL &&
			rp->r_type == RT_STEXT)
			cachemask = icachemask;
		    else
			cachemask = dcachemask;
		}
		/* 
		 *  Compute the max number of pages to look at on the
		 *  free list before giving up.
		 *  Remember:  'freemem' has already been decremented by
		 *  'size', so the last page to look at is really
		 *	(freemem+size)		the original freemem
		 *	 - (size-1)		if need 'size' pages, then
		 *				 this is last candidate for
		 *				 the first of those pages
		 */
#ifdef PERFECT_COLORING
		maxcachecolor = freemem+1;
#else
#ifdef R6000
		/* try harder for the R6000 because the benefits are so great */
		maxcachecolor = min(8*MAXCACHECOLOR, freemem+1);
#else
		maxcachecolor = min(MAXCACHECOLOR, freemem+1);
#endif R6000
#endif
	}

	/* If we are asked to allocate physically contiguous pages,
	 * then we can't really honor the free list ordering in
	 * reusing pages.  Instead, we look for contiguous chunk
	 * from the beginning.  A possible enhancement is to
	 * maintain a bit mask of free pages, so that we can
	 * quickly locate the contiguous chunk.
	 */

	if ((flag & CONTIGUOUS_PAGES) && size > 1) {
	    register int	pfn;

	    freemem += size;
#ifdef PERFECT_COLORING
	    if ((pfn = contmemall_scc(size, vpn)) == 0) {
#else
	    if ((pfn = contmemall(size, flag)) == 0) {
#endif
		register struct proc *p;

		if (nosleep)
		    return(-1);

		/* We don't have contiguous memory.  The best bet is to
		 * steal selected pages.  For now, we just assume that
		 * we are running low on memory, and let the scheduler
		 * swap out processes, if necessary.  This approach is
		 * not guaranteed to work alwasys.
		 */
		p = u.u_procp;
		memunlock();
		if (rp != NULL && !(flag & REGION_NOT_LOCKED))
		    regrele(rp);
		p->p_stat = SXBRK;
		    ++sxbrkcnt;
		if (runout) {
		    runout = 0;
		    wakeup(&runout);
		}
		swtch();
		if (rp != NULL && !(flag & REGION_NOT_LOCKED))
		    reglock(rp);
		memlock();
		goto loop;
	    }

	    /* Do other bookkeeping and initialize PDEs */
	    if (rp != NULL && ! (flag & NO_CHANGE_NVALID))
		reg_change_nvalid(rp,size);

	    for (i = 0; i < size; i++, base++) {
		base->pgm.pg_pfn = pfn++;
		pg_clrmod(base);

		if (flag & VALIDATE)
			pg_setvalid(base);
		
	    }
	    return(0);
	}

	i = 0;
	while (i < size) {
		register pfd_t	*pfd_next;

		/* with page coloring scan from beginning each time */
		pfd = phead.pf_next;

		
		/*
		 * If asked to allocate pages under 16M, then skip all
		 * pages above 16M address.  This algorithm can be optimized
		 * in several ways, but for now, the cost is not too bad.
		 *
		 * Note that we have already checked that there are enough 
		 * number of pages under 16M bytes.
		 */
		if (flag & PAGE_UNDER_16M)
		    while (pfdattopfn(pfd) >= btoc(MEM16M))
			pfd = pfd->pf_next;

		ASSERT (pfd != &phead);
		ASSERT(pfd->pf_flags&P_QUEUE);
		ASSERT(pfd->pf_use == 0);

		/* Delink page from free queue and set up pfd
		 */

		if (page_color_on && (flag & PAGE_COLOR)) {
			if (IS_R6300)
#ifdef PERFECT_COLORING
			    /* align based strictly on vpn */
			    pg_color = vpn & cachemask;
#else
			    /* aligned based upon vpn and processID */
			    pg_color = (vpn + up->u_procp->p_pid) & cachemask;
#endif PERFECT_COLORING
			else
			if (rp == &sysreg || rp == NULL)
			    /* align based strictly on vpn */
			    pg_color = vpn & cachemask;
			else
			    /* try to align based on vpn, region index */
			    pg_color = (vpn + (rp - &region[0])) & cachemask;

			for (j = 0; j < maxcachecolor; j++) {
				if ((pfdattopfn(pfd) & cachemask) == pg_color) {
/* BOBJ: add minfo stuff (pgcolorhit) here */
					++page_color_hit;
					break;			/* success */
				} else {
					/* march down free list, trying */
					pfd = pfd->pf_next;
				}
			}
			if (j == maxcachecolor) {
#ifdef PERFECT_COLORING
				int color_counts[64];
				cmn_err(CE_CONT,
				    "Can't color, freemem %d  pg_color 0x%x  maxcachecolor %d \n",
				    freemem, pg_color, maxcachecolor);
				for (j = 0; j < 64; j++)
					color_counts[j] = 0;
				pfd = phead.pf_next;
				for (j = 0; j < maxcachecolor; j++) {
					color_counts[ pfdattopfn(pfd) &
							cachemask ]++;
					pfd = pfd->pf_next;
				}
				for (j = 0; j <= cachemask; j++) {
					cmn_err( CE_CONT, "%d: %d   ",
						 j, color_counts[j] );
					if ((j & 7) == 7)
					    cmn_err( CE_CONT, "\n" );
				}
				cmn_err( CE_PANIC, "Can't color page!" );
#endif PERFECT_COLORING
				/* no match. take from head of free list */
				pfd = phead.pf_next;
/* BOBJ: add minfo stuff (pgcolormiss) here */
				++page_color_miss;
			}

			maxcachecolor--;
		}
		else {
			++page_color_na;   /* Page coloring not attempted. */
		}
		pfd_next = pfd->pf_next;
		if (pfd == pclearp)
			pclearp = &phead;
		pfd->pf_prev->pf_next = pfd_next;
		pfd_next->pf_prev = pfd->pf_prev;
		page_allocation_count++;
		pfd->pf_next = NULL;
		pfd->pf_prev = NULL;
		if (pfd->pf_flags&P_HASH)
			premove(pfd);
		pfd->pf_dbd = null_dbd;
		pfd->pf_use = 1;
		pfd->pf_flags &= P_CLEARED;	/* clear all but P_CLEARED */
		pfd->pf_rawcnt = 0;
		if (pfdattopfn(pfd) < btoc(MEM16M))
			freemem16m--;
		if (rp != NULL &&
		    ! (flag & NO_CHANGE_NVALID))
			reg_change_nvalid(rp,1);

		/*
		 * Insert in page table
		 */

		base->pgm.pg_pfn = pfdattopfn(pfd);
		pg_clrmod(base);

		if (flag & VALIDATE)
			pg_setvalid(base);
		
		i++;
		base++;
		vpn++;
	}
	return(0);
}

/*
 * Shred page table and update accounting for swapped
 * and resident pages.
 *	rp	-> ptr to the region structure.
 *	pt	-> ptr to the first pte to free.
 *	dbd	-> ptr to disk block descriptor.
 *		(must be the one associated to pt by pdetodbd(pt) or NULL)
 *	size	-> nbr of pages to free.
 *
 * Called with mem_lock set and returns the same way.
 *
 * Added pfree2 to give a way for pages to be explicitly returned to TAIL of
 * the free page list.  Used by kmem_alloc and kern_mbuf.
 *
 * Added pfree3 to provide avoid having to set pages valid to free them,
 * to avoid possible utlbmiss races.
 */

pfree(rp, pt, dbd, size)
	reg_t		*rp;
	register pde_t	*pt;
	register dbd_t	*dbd;
	int		size;
{
	return(pfree3(rp, pt, dbd, size, PF_EITHER,0));
}


pfree2(rp, pt, dbd, size, where)
	reg_t		*rp;
	register pde_t	*pt;
	register dbd_t	*dbd;
	int		size;
	int		where;		/* PF_HEAD, PF_TAIL, PF_EITHER */
{
	return(pfree3(rp, pt, dbd, size, where,0));
}


pfree3(rp, pt, dbd, size, where, assume_pfn_is_valid)
	reg_t		*rp;
	register pde_t	*pt;
	register dbd_t	*dbd;
	int		size;
	int		where;		/* PF_HEAD, PF_TAIL, PF_EITHER */
	int		assume_pfn_is_valid;
{
	register struct pfdat	*pfd;
	register int		k;


	/* 
	 * Zap page table entries
	 */

	ASSERT(memlocked());
	ASSERT(where == PF_HEAD || where == PF_TAIL || where == PF_EITHER);

	for (k = 0; k < size; k++, pt++) {
		if(dbd)
			dbd = pdetodbd(pt);

		/* Release swap space, if any assigned */
		if (rp && dbd  &&  dbd->dbd_type == DBD_SWAP) {
			if (swfree1(dbd) == 0) {
#ifdef DEBUG
				/*
				 * check on number of references to this page
				 */
				pde_t *next_pde;
				
				if (pg_isvalid(pt) ||
				    assume_pfn_is_valid) {
				    pfd = pdetopfdat(pt);
				    /*
				     * assert for pf_use is at least one,
				     * and there is at most one pde chained to
				     * this pfdat
				     * pf_use could be bigger because it is a
				     * reservation count.
				     */
				    ASSERT(pfd->pf_use >= 1);
				    next_pde = pgusetonextpde(&(pfd->pf_use_link));
				    ASSERT((next_pde == NULL) || \
					(pdetonextpde(next_pde) == NULL));
				};				
#endif DEBUG
				pbremove(rp, dbd);
			}
		}

		if ((assume_pfn_is_valid || pg_isvalid(pt)) &&
		    (dbd == NULL ||
		     dbd->dbd_type != DBD_MMAP)) {
			pfd = pdetopfdat(pt);

			/* Free pages that aren't being used
			 * by anyone else
			 */
			if (pg_isvalid(pt)) {
				pg_clrvalid(pt);
				if (dbd)
					dbd = pdetodbd(pt);
			};
			pg_clrpfn(pt);
			pfree_pfd(pfd,dbd,where);

			if (rp != NULL)
				reg_change_nvalid(rp,-1);
		}
		if (pg_isvalid(pt)) {
			pg_clrvalid(pt);
			if (dbd)
				dbd = pdetodbd(pt);
		};

		/*
		 * Change to zero pte's.
		 */

		pt->pgi.pg_pde = 0;
		if (dbd) 
			*dbd = null_dbd;
	}
}


pfree_pfd(pfd,dbd,where)
	register pfd_t *pfd;
	register dbd_t *dbd;
	int where;
{
	int	did_lock;

	did_lock = memlock_i(); /* hold off interrupt level allocators */

	ASSERT(pfd->pf_use > 0);
	if (--pfd->pf_use == 0) {
		/*
		 * Break disk association if swap page and the swap
		 * page is deallocated
		 */
		if (pfd->pf_dbd.dbd_type == DBD_SWAP &&
		    swpuse(&(pfd->pf_dbd)) == 0) {
			if (pfd->pf_flags & P_HASH)
				premove(pfd);
			pfd->pf_dbd = null_dbd;
		};
		/* Pages that are associated with disk
		 * go to end of queue in hopes that they
		 * will be reused.  All others go to
		 * head of queue so they will be reused
		 * quickly.
		 */
		if ((dbd == NULL || dbd->dbd_type == DBD_NONE ||
		     where == PF_HEAD) && where != PF_TAIL) {
			if (_riscos_idle_clear_pages != 0) {
			    /*
			     * put at head 
			     */
			    pfd->pf_next = phead.pf_next;
			    pfd->pf_prev = &phead;
			    phead.pf_next = pfd;
			    pfd->pf_next->pf_prev = pfd;
			} else {
			    /*
			     * Put page after the already zeroed
			     * pages.  The zeroed pages start at
			     * the beginning of the freelist and go
			     * until pclearp (inclusive).
			     */
			    pfd->pf_next = pclearp->pf_next;
			    pfd->pf_prev = pfd->pf_next->pf_prev;
			    pfd->pf_prev->pf_next = pfd;
			    pfd->pf_next->pf_prev = pfd;
			}
		} else {
			/*
			 * put at tail 
			 */
			pfd->pf_prev = phead.pf_prev;
			pfd->pf_next = &phead;
			phead.pf_prev = pfd;
			pfd->pf_prev->pf_next = pfd;
		}
		pfd->pf_flags |= P_QUEUE;
		pfd->pf_flags &= ~P_CLEARED;
		freemem++;
		if (pfdattopfn(pfd) < btoc(MEM16M)) {
			freemem16m++;
			while (wait16mcnt) {
				wait16mcnt = 0;
				wakeup(&wait16mcnt);
			}
		}
	}
#ifdef DEBUG
	else {
		ASSERT(pfd->pf_dbd.dbd_type != DBD_SWAP || \
		       swpuse(&(pfd->pf_dbd)) > 0);
	}
#endif DEBUG

	if (did_lock)
		memunlock();
}

ulong
effdev(vp)
register struct vnode *vp;
{
	if (vp->v_type == VCHR || vp->v_type == VBLK) 
		return((ulong)vp->v_rdev);
	return((ulong)vp->v_vfsp);
}

/*
 * Find page by looking on hash chain
 *	dbd	-> Ptr to disk block descriptor being sought.
 * returns:
 *	0	-> can't find it
 *	pfd	-> ptr to pfdat entry
 */

struct pfdat *
pfind(rp, dbd)
register reg_t	*rp;
register dbd_t	*dbd;
{
	register ulong		devid;
	register daddr_t	blkno;
	register pfd_t		*pfd;
	register long		hashindx;
	register ulong		nodeid;

	/*	Hash on block ^ nodeid and look for match.
	 */

	if (dbd->dbd_type == DBD_SWAP) {
		devid = effdev(swaptab[dbd->dbd_swpi].st_vp);
		blkno = dbd->dbd_blkno;
		nodeid = dbd->dbd_swpi;
	} else {
		register struct  vnode	*vp;
		struct vattr		vattr;

		ASSERT(rp != NULL);
		vp = rp->r_vptr;
		ASSERT(vp != NULL);
		devid = effdev(vp);
		nodeid = rp->nodeid;
		blkno = dbd->dbd_blkno;
	}

	hashindx = blkno ^ nodeid;
	pfd = phash[hashindx&phashmask];
	for (; pfd != NULL ; pfd = pfd->pf_hchain) {
		if ((pfd->pf_blkno == blkno) && (pfd->pf_devid == devid)
			&& (pfd->pf_nodeid == nodeid)) {
			if (pfd->pf_flags & P_BAD)
				continue;
			return(pfd);
		}
	}
	return(NULL);
}


/*
 *	pmatch -- test if pfdat matches specified rp and dbd
 *
 *	returns 1 if matches, 0 if not
 */

pmatch(rp, dbd, pfd)
register reg_t	*rp;
register dbd_t	*dbd;
register pfd_t	*pfd;
{
	register ulong		devid;
	register daddr_t	blkno;
	register ulong		nodeid;
	register struct  vnode	*vp;
#ifndef RISCOS
	struct vattr		vattr;
#endif

	if (dbd->dbd_type == DBD_SWAP) {
		devid = effdev(swaptab[dbd->dbd_swpi].st_vp);
		blkno = dbd->dbd_blkno;
		nodeid = dbd->dbd_swpi;
	} else {
		ASSERT(rp != NULL);
		vp = rp->r_vptr;
		ASSERT(vp != NULL);
		devid = effdev(vp);
#ifdef RISCOS
		nodeid = rp->nodeid;
#else
		if (VOP_GETATTR(vp, &vattr, u.u_cred) != 0)
			panic("pmatch: getattr failed");
		nodeid = vattr.va_nodeid;
#endif
		blkno = dbd->dbd_blkno;
	}

	if ((pfd->pf_blkno == blkno) && 
	    (pfd->pf_devid == devid) &&
	    (pfd->pf_nodeid == nodeid))
		return(1);

	return(0);
}


/*
 * Insert page on hash chain
 *	dbd	-> ptr to disk block descriptor.
 *	pfd	-> ptr to pfdat entry.
 * returns:
 *	none
 */

pinsert(rp, dbd, pfd)
register reg_t	*rp;
register dbd_t	*dbd;
register pfd_t	*pfd;
{
	register ulong	devid;
	register int	blkno;
	register ulong	nodeid;
	register long	hashindx;
	register struct vnode	*vp;
#ifdef RISCOS
	struct vattr		vattr;
#endif

	/* Check page range, see if already on chain
	 */

	if (dbd->dbd_type == DBD_SWAP) {
		devid = effdev(swaptab[dbd->dbd_swpi].st_vp);
		blkno = dbd->dbd_blkno;
		nodeid = dbd->dbd_swpi;
		ASSERT(swpuse(dbd) > 0);
	} else {
		/*	For pages on a file (rather than swap),
		 *	we use the block number xor'd with the
		 *	nodeid as the value to hash.
		 */
		ASSERT((dbd->dbd_type == DBD_FILE || \
			dbd->dbd_type == DBD_LSTFILE) && \
		       rp != NULL);
		vp = rp->r_vptr;
		ASSERT(vp != NULL);
		devid = effdev(vp);
#ifdef RISCOS
		nodeid = rp->nodeid;
#else
		if (VOP_GETATTR(vp, &vattr, u.u_cred) != 0)
			panic("pinsert: getattr failed");
		nodeid = vattr.va_nodeid;
#endif
		blkno = dbd->dbd_blkno;
	}

	hashindx = blkno ^ nodeid;

	ASSERT(pfd->pf_hchain == NULL);
	ASSERT(pfd->pf_use > 0);

	/*
	 * insert newcomers at tail of bucket
	 */

	{
		register struct pfdat *pfd1, *p;

		p = phash[hashindx&phashmask];
		for (pfd1 = p; pfd1 ; p=pfd1, pfd1 = pfd1->pf_hchain) {
			if ((pfd1->pf_blkno == blkno) &&
			   (pfd1->pf_devid == devid) &&
			   (pfd1->pf_nodeid == nodeid)) {
#ifdef DEBUG
				cmn_err(CE_CONT,
					"swapdev %x %x %x\n", swapdev,
					pfd1->pf_devid,devid);
				cmn_err(CE_CONT,
					"blkno %x %x\n",
					blkno, pfd1->pf_blkno);
				cmn_err(CE_CONT,
					"swpi %x %x\n", pfd1->pf_swpi,
					pfd->pf_swpi);
				cmn_err(CE_CONT,
					"pfd %x %x\n", pfd,pfd1);
				cmn_err(CE_CONT,
					"use %x %x\n",
					pfd->pf_use, pfd1->pf_use);
				cmn_err(CE_CONT,
					"flags %x %x\n", pfd->pf_flags,
					pfd1->pf_flags);
#endif DEBUG
				cmn_err(CE_PANIC,
					"pinsert - pinsert dup");
			}
		}
		if (p == NULL)
			phash[hashindx&phashmask] = pfd;
		else
			p->pf_hchain = pfd;
		pfd->pf_hchain = pfd1;
	}

	if (&(pfd->pf_dbd) != dbd) 
		if (pfd->pf_dbd.dbd_type == DBD_NONE)
		  	pfd->pf_dbd = *dbd;
		else {
			ASSERT(pfd->pf_dbd.dbd_type == dbd->dbd_type);
			ASSERT(pfd->pf_dbd.dbd_swpi == dbd->dbd_swpi);
			ASSERT(pfd->pf_dbd.dbd_blkno == dbd->dbd_blkno);
		};
	pfd->pf_devid = devid;
	pfd->pf_nodeid = nodeid;
	if (dbd->dbd_type == DBD_SWAP) {
		pfd->pf_flags |= P_SWAP;
		pfd->pf_flags &= ~P_CLEARED;
	} else {
		pfd->pf_flags &= ~P_SWAP;
	}
	pfd->pf_flags |= P_HASH;
}


/*
 * remove page from hash chain
 *	pfd	-> page frame pointer
 * returns:
 *	0	Entry not found.
 *	1	Entry found and removed.
 */
premove(pfd)
register struct pfdat *pfd;
{
	register struct pfdat *pfd1, *p;
	int	rval;
	register long hashindx;


	rval = 0;
	hashindx = pfd->pf_blkno ^ pfd->pf_nodeid;
	p = phash[hashindx&phashmask];
	for (pfd1 = p; pfd1 ; p=pfd1, pfd1 = pfd1->pf_hchain) {
		if (pfd1 == pfd) {
			if (p == pfd1)
				phash[hashindx&phashmask] = pfd->pf_hchain;
			else
				p->pf_hchain = pfd->pf_hchain;
			rval = 1;
			break;
		}
	}
	/*
	 * Disassociate page from disk and
	 * remove from hash table
	 */
	pfd->pf_dbd = null_dbd;
	pfd->pf_hchain = NULL;
	pfd->pf_flags &= ~(P_HASH | P_SWAP);
	pfd->pf_devid = 0;
	pfd->pf_nodeid = 0;
	return(rval);
}

/*
 * Allocate system virtual address space and
 * allocate or link  pages.
 */
sptalloc(size, mode, base, flag)
register int size, mode, base;
int flag;
{
	register i, sp;
	int s;

	/*
	 * Allocate system virtual address space
	 */
	s = splhi();
	if ((sp = malloc(sptmap, size))  ==  0) {

#ifdef DEBUG
		cmn_err(CE_WARN, "No kernel virtual space.");
		cmn_err(CE_CONT, "\tsize=%d, mode=%d, base=%d\n",
			size, mode, base);
#endif
		splx(s);
		return(NULL);
	}
	splx(s);
	/*
	 * Allocate and fill in pages
	 */
	if (base  ==  0) {
		memlock();
		if (ptmemall(&sysreg, kvtokptbl(ctob(sp)),
			size, REGION_NOT_LOCKED, flag & NOSLEEP, sp)) {
			memunlock();
			s = splhi();
			mfree(sptmap,size,sp);
			splx(s);
			return(NULL);
		}
		memunlock();
	}

	/*
	 * Setup page table entries
	 */
	for (i = 0; i < size; i++) {
		pde_t	*pde;

		pde = kvtokptbl(ctob(sp + i));
		if (base > 0)
			pde->pgi.pg_pde =
				mkpde(mode, base++);
		else
			pde->pgi.pg_pde |= mode;
		pde->pgi.dbd.dbd_type = DBD_SYS;
		pde->pgi.dbd.dbd_swpi = 0;
		pde->pgi.dbd.dbd_blkno = BLKNULL;
		pde_link(pde);
	}

	return(ctob(sp));
}

#ifdef PERFECT_COLORING
/*
 * Allocate system virtual address space and
 * allocate or link  pages.  Color VPN and PPN to secondary cache as
 * indicated by vpn.
 */
sptalloc_scc(size, mode, base, flag, vpn)
register int size, mode, base;
unsigned int flag, vpn;
{
	register i, sp;
	int s;

	/*
	 * Allocate system virtual address space
	 */
	s = splhi();
	if ((sp = malloc_scc(sptmap, size, vpn))  ==  0) {

#ifdef DEBUG
		cmn_err(CE_WARN, "No kernel virtual space.");
		cmn_err(CE_CONT, "\tsize=%d, mode=%d, base=%d\n",
			size, mode, base);
#endif
		splx(s);
		return(NULL);
	}
	splx(s);
	/*
	 * Allocate and fill in pages
	 */
	if (base  ==  0) {
		memlock();
		if (ptmemall(&sysreg, kvtokptbl(ctob(sp)),
			size, REGION_NOT_LOCKED, flag & NOSLEEP, sp)) {
			memunlock();
			s = splhi();
			mfree(sptmap,size,sp);
			splx(s);
			return(NULL);
		}
		memunlock();
	}

	/*
	 * Setup page table entries
	 */
	for (i = 0; i < size; i++) {
		pde_t	*pde;

		pde = kvtokptbl(ctob(sp + i));
		if (base > 0)
			pde->pgi.pg_pde =
				mkpde(mode, base++);
		else
			pde->pgi.pg_pde |= mode;
		pde->pgi.dbd.dbd_type = DBD_SYS;
		pde->pgi.dbd.dbd_swpi = 0;
		pde->pgi.dbd.dbd_blkno = BLKNULL;
		pde_link(pde);
	}

	return(ctob(sp));
}
#endif PERFECT_COLORING

sptfree(vaddr, size, flag)
register int size;
{
	register i, sp;
	int s;

	sp = btoct(vaddr);
	kvirt_inval(sp, size);
	if (flag) {
		memlock();
		pfree(&sysreg, kvtokptbl(vaddr), NULL, size);
		memunlock();
	}
	for (i = 0; i < size; i++) {
		pde_t	*pde;

		pde = kvtokptbl(ctob(sp + i));
		if (pg_isvalid(pde)) 
		  	pde_unlink(pde);
		pde->pgi.pg_pde = 0;
		pde->pgi.dbd = null_dbd;
		
		vaddr += NBPC;
	}
	s = splhi();
	mfree(sptmap, size, sp);
	splx(s);
}

/*
 * Initialize memory map
 *	first	-> first free page #
 *	last	-> last free page #
 * returns:
 *	none
 */

meminit(first, last)
register int first;
{
	register struct pfdat *pfd;
	register int i;
	struct pfdat *pfd_kplow;
	struct pfdat *pfd_kphigh;
	struct region **rpp;

	freemem = (last - first);
	maxmem = freemem;
	availrmem = freemem;
	availsmem = freemem;

	/*
	 * Initialize pfdat entries for kernel tables
	 */
	bzero((caddr_t) pfdat,syssegsz * sizeof(struct pfdat));
	pfd_kplow = kvtopfdat(kptbl);
	pfd_kphigh = kvtopfdat(&kptbl[syssegsz]);
#ifdef FRAG_PTS
	/* we allocated extra space for the pf_regions after kptbl */
	rpp = (reg_t **)(&kptbl[syssegsz]);
#endif
	for (pfd = pfntopfdat(first - 1); pfd >= pfdat; pfd--) {
		pfd->pf_flags = (P_LOCKED | P_DONE);
		pfd->pf_rawcnt = 1;
		pfd->pf_use = 1;
		if (pfd >= pfd_kplow &&
		    pfd <= pfd_kphigh) {
			pfd->pf_dbd.dbd_type = DBD_PDE;
#ifdef FRAG_PTS
			pfd->pf_regions = rpp;
			rpp += (NBPC/PFRAG);
			for (i = 0; i < (NBPC/PFRAG); i++)
				pfd->pf_regions[i] = &sysreg;
#else
			pfd->pf_region = &sysreg;
#endif
		} else {
			pfd->pf_dbd.dbd_type = DBD_SYS;
		};
	};			

	/*
	 * Setup queue of pfdat structures.
	 * One for each page of available memory.
	 */

	pfd = pfntopfdat(first);
	phead.pf_next = &phead;
	phead.pf_prev = &phead;

	/*
	 * Add pages to queue, high memory at end of queue
	 * Pages added to queue FIFO
	 */

	for (i = freemem; --i >= 0; pfd++) {
		pfd->pf_next = &phead;
		pfd->pf_prev = phead.pf_prev;
		phead.pf_prev->pf_next = pfd;
		phead.pf_prev = pfd;
		pfd->pf_flags = P_QUEUE;
		if (pfdattopfn(pfd) < btoc(MEM16M))
			freemem16m++;
	}
	pclearp = &phead;
	maxmem16m = ((last>btoc(MEM16M)) ? btoc(MEM16M) : last) - btoc(kpbase);
}

/*
 * flush all pages associated with a mount device
 *	vfsp	-> vfs structure pointer
 * returns:
 *	none
 */

punmount(vfsp)
register struct vfs *vfsp;
{
	register pfd_t		*pfd;
	register pfd_t		*pfdlim;

	memlock();
	pfd = pfntopfdat(btoc(kpbase));
	pfdlim = pfntopfdat(maxclick);
	for (;  pfd < pfdlim  ;  pfd++) {
		if ((ulong)vfsp == pfd->pf_devid)
			if ((pfd->pf_flags & (P_HASH | P_SWAP)) == P_HASH)
				premove(pfd);
	}
	memunlock();
}

/*
 * Find page by looking on hash chain and remove it.
 *	dbd	Ptr to disk block descriptor for block to remove.
 * returns:
 *	0	-> can't find it.
 *	1	-> page found and removed.
 */
long pbrmcnt;

pbremove(rp, dbd)
reg_t	*rp;
dbd_t	*dbd;
{
	register struct vnode	*vp;
#ifndef RISCOS
	struct vattr		vattr;
#endif
	register ulong		nodeid;
	int ret;

	if (dbd->dbd_type == DBD_SWAP) {
		nodeid = dbd->dbd_swpi;
	} else {
		pbrmcnt++;
		ASSERT(rp != NULL);
		vp = rp->r_vptr;
		ASSERT(vp != NULL);
#ifdef RISCOS
		nodeid = rp->nodeid;
#else
		if (VOP_GETATTR(vp, &vattr, u.u_cred) != 0)
			panic("pbremove: getattr failed");
		nodeid = vattr.va_nodeid;
#endif
	}

	ret = pbremove_com (vp, dbd, nodeid);
	return ret;
}

/*
 * Common supporting procedure used by the pbremove and flushpgch procedures.
 * The real work of finding a page by looking on hash chain and removing it
 * is done here. This procedure requires that the nodeid be passed to the
 * procedure so a get attributes call is NOT made.
 * We return:
 *	0	-> can't find it.
 *	1	-> page found and removed.
 */
pbremove_com(vp, dbd, nodeid)
struct vnode *vp;
dbd_t	*dbd;
register long nodeid;
{
	register struct pfdat	*pfd;
	register struct pfdat	*p;
	register int		blkno;
	register ulong		devid;
	register long		hashindx;

	/*
	 * Hash on block and look for match
	 */
	if (dbd->dbd_type == DBD_SWAP) {
		devid = effdev(swaptab[dbd->dbd_swpi].st_vp);
	} else {
		devid = effdev(vp);
	}

	blkno = dbd->dbd_blkno;
	hashindx = blkno ^ nodeid;
	p = phash[hashindx&phashmask];

	for (pfd=p; pfd ;p=pfd, pfd = pfd->pf_hchain) {
		if ((pfd->pf_blkno == blkno) && (pfd->pf_devid == devid)
			&& (pfd->pf_nodeid == nodeid)) {
			if (p!= pfd)
				p->pf_hchain = pfd->pf_hchain;
			else
				phash[hashindx&phashmask] = pfd->pf_hchain;
			pfd->pf_dbd = null_dbd;
			pfd->pf_hchain = NULL;
			pfd->pf_flags &= ~(P_HASH | P_SWAP);
			pfd->pf_devid = 0;
			pfd->pf_nodeid = 0;
			return(1);
		}
	}

	return(0);
}

/*
 * Reserve size memory pages.  Returns with freemem
 * decremented by size.  Return values:
 *	0 - Memory available immediately
 *	1 - Had to sleep to get memory
 *     -1 - not enough memory and nosleep flag is set
 *	    or size request <= 0
 */

memreserve(rp, size, nosleep,not_locked)
register reg_t *rp;
{
	register struct proc *p;

	ASSERT((rp == NULL) || (rp->r_flags & RG_LOCK) || not_locked);
	ASSERT(memlocked());

	if (size <= 0) {
		cmn_err(CE_WARN, "!memreserve - negative or 0 memory request");
		return(-1);
	}

	if (freemem >= size) {
		freemem -= size;
		return(0);
	}
	if (nosleep)
		return(-1);
	p = u.u_procp;
	while (freemem < size) {
	  	memunlock();
		if (rp != NULL &&
		    ! not_locked)
			regrele(rp);
		p->p_stat = SXBRK;
		++sxbrkcnt;
		if (runout) {
			runout = 0;
			wakeup(&runout);
		}
		swtch();
		if (rp != NULL &&
		    ! not_locked)
			reglock(rp);
		memlock();
	}
	freemem -= size;
	return(1);
}

/*
 * Flush the page cache of pages associated with the file from which this
 * region was initialized. We do it here, rather than iput or some other
 * file system  routine, because file system writers shouldn't have to
 * bother with it.
 */
flushpgch(vp, nodeid, size)
register struct vnode *vp;
register long nodeid;
register u_long size;
{
	register int nblks, blkspp, i;
	register int bsize, ret;
	struct dbd	dbd;

	dbd.dbd_type = DBD_FILE;
	dbd.dbd_swpi = 0;
	bsize = NBPSCTR;
	nblks = (size + bsize - 1)/bsize;
	blkspp = NBPP/bsize;
	nblks = ((nblks + blkspp - 1) / blkspp) * blkspp;
	memlock();
	for (i = 0  ;  i < nblks  ;  i += blkspp) {
		dbd.dbd_blkno = i;
		ret = pbremove_com(vp, &dbd, nodeid);
	}
	memunlock();
}

/*
 * This routine is provided for compatibility with the mfind/munhash
 * routines found in the NFS4.0 code.  The two routines have been
 * combined into one, since there is no need for the separate
 * functionality provided by mfind in our kernel.
 *
 * This routine removes all entries from the page cache which match
 * <*, nodeid, blkno>, where nodeid is the nodeid corresponding to the "vp".
 */
punhash(vp, blkno, nodeid)
	register struct vnode *vp;
	register int blkno;
	register u_long nodeid;
{
	register struct pfdat	*pfd;
	register struct pfdat	*p;
	register long	hashindx;
	register u_long devid;
	int error;

	/*
	 * Hash on block and look for match
	 */
	hashindx = (blkno ^ nodeid) & phashmask;
	p = phash[hashindx];

	ASSERT(vp != NULL);
	devid = effdev(vp);	  

	for (pfd=p; pfd ;p=pfd, pfd = pfd->pf_hchain) {

		if ((pfd->pf_blkno == blkno) && (pfd->pf_nodeid == nodeid) &&
		    (devid == pfd->pf_devid)) {

			ASSERT(pfd->pf_dbd.dbd_type != DBD_SWAP);
			if (p!= pfd)
				p->pf_hchain = pfd->pf_hchain;
			else
				phash[hashindx] = pfd->pf_hchain;
			pfd->pf_dbd = null_dbd;
			pfd->pf_hchain = NULL;
			pfd->pf_flags &= ~(P_HASH | P_SWAP);
			pfd->pf_devid = 0;
			pfd->pf_nodeid = 0;
			return(0);
		}
	}
	return(0);
}
