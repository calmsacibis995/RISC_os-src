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
#ident	"$Header: getpages.c,v 1.29.1.8.1.4 90/08/09 17:04:09 hawkes Exp $"

/*	Copyright (c) 1984 AT&T	*/
/*	  All Rights Reserved  	*/

/*	THIS IS UNPUBLISHED PROPRIETARY SOURCE CODE OF AT&T	*/
/*	The copyright notice above does not evidence any   	*/
/*	actual or intended publication of such source code.	*/


#include "sys/types.h"
#include "sys/param.h"
#include "sys/sysmacros.h"
#include "sys/immu.h"
#include "sys/region.h"
#include "sys/pfdat.h"
#include "sys/sysinfo.h"
#include "sys/var.h"
#include "sys/tuneable.h"
#include "sys/signal.h"
#include "sys/pcb.h"
#include "sys/user.h"
#include "sys/buf.h"
#include "sys/sbd.h"
#include "sys/proc.h"
#include "sys/swap.h"
#include "sys/getpages.h"
#include "sys/debug.h"
#include "sys/cmn_err.h"
#include "bsd/sys/resource.h"
#include "bsd43/sys/vmmeter.h"
#include "sys/errno.h"
#include "sys/cpu_board.h"
#include "sys/systm.h"

#ifndef imin
#define	imin(a,b)		MIN((a),(b))
#endif imin

extern dbd_t null_dbd;

int	clock16m;			/* set if a separate clock for 
					 * memory under 16Mbytes. */
int	lotsfree16m;

/*
 *	In order to accomodate for the absence of reference bits in the
 *	MIPS tlb, the valid bit, and one software bit in the pde are used
 *	to simulate reference bit maintainance through
 *	valid faults. This table describes the bit settings and what they
 *	mean:
 *			vr - valid and referenced bit
 *			sv - software valid
 *
 *	 vr sv	meaning
 *	 --------------
 *	 1  1	A valid page in the process working set.
 *	 0  1	Page is in the working set, but has been set invalid
 *		in order to detect a reference. The page frame is still
 *		owned by the process.
 *	 0  0	The page is truly invalid. Full vfault processing is 
 *		required for this pte.
 */

/* 
 * request_tlb_flush tells swtch() to flush tlb before switching: saves many 
 * extra flushes.
 * This flag is set whenever page table entries are modified by this code.
 * swtch() will flush the tlb and reset the flag before allowing any other 
 * process to run. This assures tlb consistency without extra flushes.
 */

int		request_tlb_flush = 0;

/*
 * Find the last page in specific segment for specified region.
 */
#define LAST_PAGE(rp, seg) \
	min( NPGPT, (rp)->r_pgoff + (rp)->r_pgsz - stoc(seg) )


/*
 * Setup the paging constants for the clock algorithm.
 * Called after the system is initialized and the amount of memory
 * and number of paging devices is known.
 *
 * Threshold constants are defined in <sys/immu.h>
 */
vminit()
{
	/*
	 * If we are on M/120 or 3230 with more than 16M memory, we perform
	 * the clock alogorithm on two parts: under 16M and over
	 * 16M.
	 */

	if ((IS_R2400||IS_R3030) && maxclick > btoc(MEM16M))
		clock16m = 1;
	else
		clock16m = 0;
	/*
	 * Lotsfree is threshold where paging daemon turns on.
	 */
	if (lotsfree == 0) {
		lotsfree = LOTSFREE / NBPP;
		if (lotsfree > LOOPPAGES / LOTSFREEFRACT)
			lotsfree = LOOPPAGES / LOTSFREEFRACT;
	}

	/*
	 * Desfree is amount of memory desired free.
	 * If less than this for extended period, do swapping.
	 */
	if (desfree == 0) {
		desfree = DESFREE / NBPP;
		if (desfree > LOOPPAGES / DESFREEFRACT)
			desfree = LOOPPAGES / DESFREEFRACT;
	}

	/*
	 * Minfree is minimal amount of free memory which is tolerable.
	 */
	if (minfree == 0) {
		minfree = MINFREE / NBPP;
		if (minfree > desfree / MINFREEFRACT)
			minfree = desfree / MINFREEFRACT;
	}

	/*
	 * Maxpgio thresholds how much paging is acceptable.
	 * This figures that 2/3 busy on an arm is all that is
	 * tolerable for paging.  We assume one operation per disk rev.
	 */
	if (maxpgio == 0)
		maxpgio = (DISKRPM * 2) / 3;

	/*
	 * Clock to scan using max of ~~10% of processor time for sampling,
	 *     this estimated to allow maximum of 2000 samples per second.
	 * This yields a ``fastscan'' of roughly:
	 *	<=1m	2m	3m	4m	8m
	 * 	5s	10s	15s	20s	40s
	 */
	if (fastscan == 0)
		fastscan = FASTSCAN;
	if (fastscan > LOOPPAGES / SCANSECS)
		fastscan = LOOPPAGES / SCANSECS;

	/*
	 * Set slow scan time to 1/2 the fast scan time.
	 */
	if (slowscan == 0)
		slowscan = fastscan / 2;

	if (clock16m) {
		if (lotsfree16m == 0)
			lotsfree16m = maxmem16m / 32;	/* 3.3% of mem < 16M */
	}

	bclnlist = NULL;
}

#ifndef ave
/* Average new into old with aging factor time */
#define	ave(smooth, cnt, time) \
	smooth = ((time - 1) * (smooth) + (cnt)) / (time)
#endif ave

vmmeter()
{
	register unsigned *cp, *rp, *sp;

	
	/* copy selected values from sysinfo */
	cnt.v_swtch = sysinfo.pswitch - sum.v_swtch;
	cnt.bsd43_v_syscall = sysinfo.syscall - sum.bsd43_v_syscall;
	cnt.v_zeros = pclearstat.p_cnt - sum.v_zeros;
	cnt.v_zero_hits = pclearstat.p_hit - sum.v_zero_hits;

#ifdef notdef
	deficit -= imin(deficit,
	    imax((deficit / 10), (maxpgio / 4));
#endif notdef
	ave(avefree, freemem, 5);
	ave(avefree30, freemem, 30);
	/* v_pgin is maintained by clock.c */
	cp = &cnt.bsd43_v_first; rp = &rate.bsd43_v_first; sp = &sum.bsd43_v_first;
	while (cp <= &cnt.bsd43_v_last) {
		ave(*rp, *cp, 5);
		*sp += *cp;
		*cp = 0;
		rp++, cp++, sp++;
	}
	if (time.tv_sec % 5 == 0) {
		vmtotal();
		rate.v_swpin = cnt.v_swpin;
		sum.v_swpin += cnt.v_swpin;
		cnt.v_swpin = 0;
		rate.v_swpout = cnt.v_swpout;
		sum.v_swpout += cnt.v_swpout;
		cnt.v_swpout = 0;
	}
	if (avefree < minfree && runout || proc[0].p_slptime > maxslp/2) {
		if (runout != 0) {
			runout = 0;
			setrun(&proc[0]);
		};
	}
}

/*
 * Schedule rate for paging.
 * Rate is linear interpolation between
 * slowscan with lotsfree and fastscan when out of memory.
 */

/* insure non-zero */
#define	nz(x)	(x != 0 ? x : 1)

vhand();

schedpaging()
{
	register int vavail;

	/* Set up desired scan rate for memory over 16M */
	vavail = freemem - deficit;
	if (vavail < 0)
	    vavail = 0;
	if (freemem < lotsfree)
	    desscan = (slowscan * vavail + fastscan * (lotsfree - vavail)) /
			nz(lotsfree) / RATETOSCHEDPAGING;

	/* Set up desired scan rate for memory under 16M */
	vavail = freemem16m - deficit;
	if (vavail < 0)
	    vavail = 0;
	if (freemem16m < lotsfree16m)
	    desscan16m = (slowscan * vavail + fastscan * (lotsfree16m-vavail)) /
			nz(lotsfree) / RATETOSCHEDPAGING;

	if (freemem < lotsfree || freemem16m < lotsfree16m)
		wakeup((caddr_t) vhand);

	timeout(schedpaging, (caddr_t)0, HZ / RATETOSCHEDPAGING);
}

int	pushes;

#define	FRONT	1
#define	BACK	2

/*
 * vhand -- page out daemon
 *
 * As long as there are at least lotsfree pages,
 * this process is not run.  When the number of free
 * pages stays in the range desfree to lotsfree,
 * this daemon runs through the pages in the loop
 * at a rate determined in vmsched().  Pageout manages
 * two hands on the clock.  The front hand moves through
 * memory, clearing the valid bit (simulating a reference bit),
 * and stealing pages from procs that are over maxrss.
 * The back hand travels a distance behind the front hand,
 * freeing the pages that have not been referenced in the time
 * since the front hand passed.  If modified, they are pushed to
 * swap before being freed.
 */

int handspread = HANDSPREAD;
int handspread16m = 0;		/* not used but read */
#define FRACTOFMEM	3	/* 1/3 of mem = max handspread */
int newhandspread = 0;		/* set these to change on the fly */
int newhandspread16m = 0;	/* set these to change on the fly */
int reset_count = 0;		/* count resets of handspread */

vhand(once_only)
unsigned long once_only;
{
	register int count;
	register int fronthand, backhand;
	register int fronthand16m = 0, backhand16m = 0;
	int	s;

	if (once_only)
		return;	/* backward compatibility */

	spl0();

	handspread = btoc(handspread);	/* convert to clicks */
	/*
	 * Set the two clock hands to be separated by a reasonable amount,
	 * but no more than 360 degrees apart.
	 */
	if (handspread > maxmem / FRACTOFMEM) {
		handspread = maxmem / FRACTOFMEM;
	}
	backhand = 0;
	fronthand = handspread;

	if (clock16m) {
	    /* We'll be running a separate clock for pruning memory
	     * under 16M.  Adjust fronthand and backhand to point 
	     * beyond 16M memory. Set 16m hands...
	     */
	     backhand = maxmem16m;
	     fronthand += backhand;

	     backhand16m = 0;
	     fronthand16m = maxmem16m / FRACTOFMEM;
	}

	if (fronthand >= maxmem)
		fronthand = maxmem - 1;

	/* record values so user can see them in MB's */
	handspread = ctob(fronthand - backhand);
	handspread16m = ctob(fronthand16m - backhand16m);

loop:
	if (newhandspread || newhandspread16m) {
		reset_count++;				/* should be small */
		newhandspread = btoc(newhandspread);	/* convert to clicks */
		newhandspread16m = btoc(newhandspread16m);
		if (newhandspread) {
			handspread = newhandspread;
			backhand = 0;
			if (clock16m)
				backhand = maxmem16m;
			fronthand = backhand + handspread;
			if (fronthand >= maxmem) {
				fronthand = maxmem - 1;
			}
		}
		if (clock16m && newhandspread16m) {
			handspread16m = newhandspread16m;
			backhand16m = 0;
			fronthand16m = backhand16m + handspread16m;
			if (fronthand16m > maxmem16m)
				fronthand16m = maxmem16m - 1;
		}
		/* record values so user can see them in MB's */
		handspread = ctob(fronthand - backhand);
		handspread16m = ctob(fronthand16m - backhand16m);
		newhandspread = 0;	/* turn off changes */
		newhandspread16m = 0;	/* turn off changes */
	}
	/*
	 *	Shake the disk buffer cache for memory.
	 */
	if (freemem < desfree) {
		bio_freemem(desfree - freemem);
	}

	/*
	 * Before sleeping, look to see if there are any swap I/O headers
	 * in the ``cleaned'' list that correspond to dirty
	 * pages that have been pushed asynchronously. If so,
	 * empty the list by calling cleanup().
	 *
	 * N.B.: We guarantee never to block while the cleaned list is nonempty.
	 */
	s = splbio();
  	if (bclnlist != NULL) {
 		splx(s);
  		cleanup();
 		goto loop;
  	}
  	sleep((caddr_t)vhand, PSWP+1);
	splx(s);
	pushes = 0;

	if (!clock16m || maxclick > btoc(MEM16M)) {
	    /* Scan memory (in case of clock16m, memory over 16M) */
	    nscan = 0;
	    count = 0;
	    while (nscan < desscan && freemem < lotsfree) {
		/*
		 * If checkpage manages to add a page to the free list,
		 * we give ourselves another couple of trips around the loop.
		 */
		if (checkpage(fronthand, FRONT,0))
			count = 0;
		if (checkpage(backhand, BACK,0))
			count = 0;
		cnt.v_scan++;
		nscan++;
		if (++fronthand >= maxmem) {
			fronthand = (clock16m) ? maxmem16m : 0;
			cnt.v_rev++;
			if (count > 2) {
				/*
				 * Extremely unlikely, but we went around
				 * the loop twice and didn't get anywhere.
				 * Don't cycle, stop till the next clock tick.
				 */
				break;
			}
			count++;
		}
		if (++backhand >= maxmem) {
		    backhand = (clock16m) ? maxmem16m: 0;
		    if (freemem < lotsfree)
			kmem_recover(0);
		}
	    }
	}

	if (clock16m) {
	    /* Scan memory under 16M */
	    nscan = 0;
	    count = 0;
	    while (nscan < desscan16m && freemem16m < lotsfree16m) {
		/*
		 * If checkpage manages to add a page to the free list,
		 * we give ourselves another couple of trips around the loop.
		 */
		if (checkpage(fronthand16m, FRONT,0))
			count = 0;
		if (checkpage(backhand16m, BACK,0))
			count = 0;
		cnt.v_scan++;
		nscan++;
		if (++fronthand16m >= maxmem16m) {
			fronthand16m = 0;
			cnt.v_rev++;
			if (count > 2) {
				/*
				 * Extremely unlikely, but we went around
				 * the loop twice and didn't get anywhere.
				 * Don't cycle, stop till the next clock tick.
				 */
				break;
			}
			count++;
		}
		if (++backhand16m >= maxmem16m) {
			backhand16m = 0;
		}
	    }
	}
	if (freemem < desfree)
		kmem_recover(1);
	goto loop;
}

/*
 * An iteration of the clock pointer (hand) around the loop.
 * Look at the page at hand.  If it is a
 * locked (for physical i/o e.g.), system (u., page table)
 * or free, then leave it alone.
 * Otherwise, if we are running the front hand,
 * invalidate the page for simulation of the reference bit.
 * If the proc is over maxrss, we take it.
 * If running the back hand, check whether the page
 * has been reclaimed.  If not, free the page,
 * pushing it to disk first if necessary.
 */
checkpage(hand, whichhand,swapout)
	int hand, whichhand, swapout;
{
	register struct proc *p;
	register reg_t *rp;
	register ureg_t *ur;
	register struct pfdat *c;
	register struct pde *pde;
	unsigned v;
	int klsize;
	pglst_t	kllist;
	pde_t	pde_v;
	int	proc_count;
	unsigned proc_flags;
	unsigned proc_below_maxrss;
	unsigned proc_below_safesize;
	unsigned region_noswapcnt;
	int	s;
	int	did_lock;
	int	pswitch;

top:
	pswitch = sysinfo.pswitch;
	c = &pfdat[hand];
	XPRINTF(XPR_VM, "pageout scanning, freemem=%d hand=%d locked %d free %d",
		freemem, hand, ((c->pf_flags & P_LOCKED) ? 1 : 0),
			((c->pf_flags & P_QUEUE) ? 1 : 0));
	/*
	 * acquire the lock, if failed return
	 */
	if (! MLOCK_NOWAIT(c))
		return(0);

	if ((c->pf_flags & P_QUEUE) ||
	    ((c->pf_flags & P_DONE) == 0) ||
	    (c->pf_type == DBD_SYS) ||
	    (c->pf_type == DBD_PDE) ||
	    (c->pf_type == DBD_BUF)) {
		MUNLOCK(c);
		return (0);
	}
	
	pde_v.pgi.pg_pde = 0;
	pde_v.pgi.dbd = c->pf_dbd;
	proc_count = 0;
	proc_flags = 0;
	proc_below_maxrss = 0;
	proc_below_safesize = 0;
	region_noswapcnt = 0;
	for (pde = pgusetonextpde(&c->pf_use_link);
	     pde != NULL;
	     pde = pdetonextpde(pde)) {
		ASSERT(pdetopfdat(pde) == c);
		pde_v.pgi.pg_pde |= pde->pgi.pg_pde;
		rp = pdetoregion(pde);
		if (rp != NULL) {
		    region_noswapcnt += rp->r_noswapcnt;
		    if (rp->r_usesz > 0) {
			for (ur = rp->r_use;
			     ur < rp->r_use + rp->r_usesz ;
			     ur++) {
				if (ur->ur_proc_index == UREGION_PROC_NULL)
					continue;
				p = proc + ur->ur_proc_index;
				if (proc_count)
					proc_flags &= p->p_flag;
				else
					proc_flags = p->p_flag;
				proc_count++;
				if ((p->p_flag & (SSEQL|SUANOM) == 0) &&
				    (p->p_flag & SLOAD) &&
				    (p->p_rssize <= p->p_maxrss))
					proc_below_maxrss++;
				if (rp->r_type == RT_PRIVATE &&
				    p->p_flag & SLOAD &&
				    p->p_rssize < 
				        ((saferss < p->p_slptime) ? 0 
					    : (saferss - p->p_slptime)))
					proc_below_safesize++;
			};
		    };
		};
	};
	if (region_noswapcnt) {
		MUNLOCK(c);
		return(0);
	}

	XPR3(XPR_VM, "pageout checking pfdatx=%d pte=0x%x",
	    hand, pde_v.pgi.pg_pde);
	/*
	 * If page is valid; make invalid but reclaimable.
	 * If this pte is not valid, then it must be reclaimable
	 * and we can add it to the free list.
	 */
	if (pde_v.pgm.pg_vr) {
		if (whichhand == BACK) {
			MUNLOCK(c);
			return(0);
		}
		pfdat_newptes(c);
		pfdat_modpte(c, /* clear */ PG_VR,
				/* set */ (pde_v.pgi.pg_pde & PG_M));
		if (proc_below_maxrss) {
			MUNLOCK(c);
			return (0);
		}
	}
	/*
	 * Guarantee a minimal investment in data
	 * space for jobs in balance set.
	 */
	if (proc_below_safesize ||
	    pg_islocked(&pde_v)) {
		MUNLOCK(c);
		return (0);
	}

	/*
	 * Check for overhead work for this page
	 */
	switch (c->pf_type) {
	case DBD_NONE: 
		break;

	case DBD_SWAP:
		
		/*
		 *	A DBD_SWAP page should never be modified, because
		 *	pfault breaks the swap connection when DBD_SWAP 
		 *	pages are modified.
		 */
		ASSERT( !pg_ismod(&pde_v) );

		/*
		 *	We aren't deleting the swap file, so just use the
		 *	copy of the page already there.
		 */
		if ( (swaptab[c->pf_dbd.dbd_swpi].st_flags & ST_INDEL) == 0) {
			minfo.unmodsw++;
			break;
		}

		/*
		 *	We want to delete the swap file this block is on.
		 *	Release this swap file copy and write a new one.
		 */
		release_swap_for_pfd(c,NULL);

		break;

	case DBD_FILE:
	case DBD_LSTFILE:
		/*
		 *	This page cannot have been modified
		 *	since if it had been, then it would
		 *	be marked DBD_NONE, not DBD_FILE.
		 *	Either the page is text and so the
		 *	segment table entry is RO or it is
		 *	data in which case it is copy-on-
		 *	write and also RO.
		 */
		ASSERT(!pg_ismod(&pde_v));
		minfo.unmodfl++;
		break;
	}

	/*
	 * If the page is currently dirty, we
	 * have to arrange to have it cleaned before it
	 * can be freed.  We mark it clean immediately.
	 * If it is reclaimed while being pushed, then modified
	 * again, we are assured of the correct order of 
	 * writes because we lock the page during the write.  
	 * This guarantees that a swap() of this process (and
	 * thus this page), initiated in parallel, will,
	 * in fact, push the page after us.
	 *
	 * The most general worst case here would be for
	 * a reclaim, a modify and a swapout to occur
	 * all before the single page transfer completes.
	 *
	 * If the page is not dirty, but has no swap copy,
	 * we also need to allocate a swap copy and write it out.
	 */
	if (pg_ismod(&pde_v) ||
	    c->pf_type == DBD_NONE) {
#ifdef later
		/*
		 * If the process 
		 * about to exit, do not bother with its
		 * dirty pages
		 */
		if (! swapout && 
		    proc_flags & (SLOCK|SWEXIT) &&
		    pg_ismod(&pde_v)) {
			MUNLOCK(c);
			return (0);
		}
#endif later
		/*
		 * Limit pushes to avoid saturating
		 * pageout device.
		 */
		if (pushes > maxpgio / RATETOSCHEDPAGING) {
			MUNLOCK(c);
			return ((swapout ? -1 : 0));
		}
		pushes++;

		/*
		 * Now carefully make sure that there will
		 * be a header available for the push so that
		 * we will not block waiting for a header in
		 * swap().  The reason this is important is
		 * that we (vhand) are the one who cleans
		 * dirty swap headers and we could otherwise
		 * deadlock waiting for ourselves to clean
		 * swap headers.  The sleep here on vhand
		 * is actually (effectively) a sleep on both
		 * ourselves and &pfreelist, and this is known
		 * to swdone and swap in physio.c.  That is,
		 * vhand will be awakened both when dirty
		 * headers show up and also to get the pageout
		 * daemon moving.
		 */
loop2:
		s = splbio();
		if (! swapout &&
		    bclnlist != NULL) {
			splx(s);
			cleanup();
			goto loop2;
		}
		if (pfreelist.av_forw == NULL) {
			MUNLOCK(c);
			if (swapout) {
				splx(s);
				return((swapout ? -1 : 0));
			};
			pfreelist.b_flags |= B_WANTED;
			sleep((caddr_t)vhand, PSWP+2);
			splx(s);
			/*
			 * Page disposition may have changed
			 * since process may have exec'ed,
			 * forked, exited or just about
			 * anything else... try this page
			 * frame again, from the top.
			 */
			goto top;
		}
		splx(s);

		ASSERT(c->pf_use > 0);
		kllist.pl_pfdat = c;
		kllist.pl_pde = NULL;
		/*
	 	 * Allocate swap space if we need it
		 */
		if (c->pf_type != DBD_SWAP) {
			if (c->pf_type != DBD_NONE) {
				/* page disposition changed; try again */
				MUNLOCK(c);
				goto top;
			};
			if (swalloc(&kllist) < 0) {
					/* cannot allocate swap space */
				MUNLOCK(c);
				return(-1); 
			};
		};

		/*
		 * Now committed to pushing the page...
		 */
		ASSERT(pswitch == sysinfo.pswitch);
		XPRINTF(XPR_VM, "pageout cleaning page", 0,
		    0, 0, 0);
		pfdat_newptes(c);
		pfdat_modpte(c,/* clear */ PG_M, /* set */ 0);
		(void)swap(&kllist, 1, B_WRITE | B_ASYNC);
		/*
		 * The cleaning of this page will be
		 * completed later, in cleanup() called
		 * (synchronously) by us (vhand).  In
		 * the meantime, the page frame is locked
		 * so no havoc can result.
		 */
		return (1);	/* well, it'll be free soon */
	}
	/*
	 * Decrement the resident set size of the current
	 * text object/process, and put the page in the
	 * free list. Note that we don't give memfree the
	 * pte as its argument, since we don't want to destroy
	 * the pte.  If it hasn't already been discarded
	 * it may yet have a chance to be reclaimed from
	 * the free list.
	 */
	MUNLOCK(c);
	did_lock = memlock_i(); /* hold off interrupt level callers */
	pfdat_memfree(c, 0);
	if (c->pf_use == 0) {
		cnt.v_seqfree++;
		cnt.v_dfree++;
		if (did_lock)
			memunlock();
		return (1);		/* freed a page! */
	}
	if (did_lock)
		memunlock();
	return(0);
}


/*
 *	pdetorgoff -- get page offset of pde from base of region
 */

pdetorgoff(pde,rp)
	struct	pde *pde;
	reg_t	*rp;
{
	pde_t **listp;
	int	i;

	if (rp == NULL)
		rp = pdetoregion(pde);
	if (rp == NULL)
		return(0);
	for (listp = rp->r_list, i = 0;
	     i < rp->r_listsz;
	     listp++, i++) {
		if (*listp <= pde &&
		    ((*listp) + NPGPT) > pde) 
			return((stoc(i) + (pde - *listp)) - rp->r_pgoff);
	};
	return(0);
}

/*
 *	pfdat_getptes -- inclusive-or all ptes for this page
 */

pfdat_getptes(c)
	struct	pfdat *c;
{
	register struct pde *pde;
	unsigned pte_i;

	pte_i = 0;
	for (pde = pgusetonextpde(&c->pf_use_link);
	     pde != NULL;
	     pde = pdetonextpde(pde)) {
		ASSERT(pdetopfdat(pde) == c);
		pte_i |= pde->pgi.pg_pde;
	};
	return(pte_i);
}

/*
 *	pfdat_newptes -- invalidate all TLB entries for this page
 */

pfdat_newptes(c)
	struct	pfdat *c;
{
	register struct pde *pde;
	reg_t	*rp;
	ureg_t	*ur;
	proc_t	*p;
	preg_t	*pr;
	int	i;

	for (pde = pgusetonextpde(&c->pf_use_link);
	     pde != NULL;
	     pde = pdetonextpde(pde)) {
		ASSERT(pdetopfdat(pde) == c);
		if (pg_isvalid(pde)) {
			rp = pdetoregion(pde);
			if (rp == NULL)
				continue;
			if (rp->r_usesz == 0)
				continue;
			for (ur = rp->r_use;
			     ur < rp->r_use + rp->r_usesz;
			     ur++) {
				if (ur->ur_proc_index == UREGION_PROC_NULL)
					continue;
				p = proc + ur->ur_proc_index;
				for (pr = p->p_region, i = 0;
				     pr->p_reg != NULL && i < pregpp;
				     pr++, i++) {
					if (pr->p_reg == rp)
						newptes(p,
						   pnum(pr->p_regva) +
						   rp->r_pgoff +
						   pdetorgoff(pde,rp),
						   1);
				};
			};
		};
	};
}

/*
 *	pfdat_modpte -- clear and set bits in all ptes for a page
 *
 *	Scan through the list of uses of a physical page,
 *	and reset the selected bits in all ptes.
 */

pfdat_modpte(c,pte_i_clr,pte_i_set)
	struct	pfdat	*c;
	unsigned int	pte_i_clr;
	unsigned int	pte_i_set;
{
	register struct pde *pde;

	for (pde = pgusetonextpde(&c->pf_use_link);
	     pde != NULL;
	     pde = pdetonextpde(pde)) {
		ASSERT(pdetopfdat(pde) == c);
		pde->pgi.pg_pde &= ~pte_i_clr;
		pde->pgi.pg_pde |= pte_i_set;
		request_tlb_flush = 1;
	};
}

/*
 *	pfdat_memfree -- free all uses of a physical page
 *
 *	Scan through the list of uses of a physical page,
 *	and mark them all free.  Decrement the rssize values for
 *	the regions and, if appropriate, processes affected.
 */

pfdat_memfree(c,skipmod)
	struct	pfdat *c;
	int	skipmod;
{
	pde_t	*pde;
	pde_t	*next_pde;
	reg_t	*rp;
	proc_t	*p;
	int	i;

	for (pde = pgusetonextpde(&c->pf_use_link);
	     pde != NULL; 
	     pde = next_pde) {
		next_pde = pdetonextpde(pde);
		ASSERT(pdetopfdat(pde) == c);
		pde_memfree(pde,1,skipmod);
	};
}

/*
 * Process the ``cleaned'' list.
 *
 * Scan through the linked list of swap I/O headers
 * and free the corresponding pages that have been
 * cleaned by being written back to the paging area.
 * If the page has been reclaimed during this time,
 * we do not free the page.  As they are processed,
 * the swap I/O headers are removed from the cleaned
 * list and inserted into the free list.
 */

cleanup()
{
	register struct buf *bp;
	register struct pfdat *c;
	register struct pde *pde;
	struct pde *upde;
	register int i;
	int s;
	int	did_lock;
	
	for (;;) {
		s = splbio();
		if ((bp = bclnlist) == 0)
			break;
		bclnlist = bp->av_forw;
		splx(s);
		for (i = 0; i < bp->b_bcount; i += NBPP) {
			c = kvtopfdat(bp->b_un.b_addr + i);
			did_lock = memlock_i(); /* lock out interrupt level */
			ASSERT(c->pf_flags & P_LOCKED);

			MUNLOCK(c);
			XPR4(XPR_VM, "cleanup swap done page=0x%x pf_use=%d pfdatx=%d",
			    pnum(paddr(bp)),
			    c->pf_use,
			    c - pfdat);
			pfdat_memfree(c,1);
			if (c->pf_use == 0)
				cnt.v_dfree++;
			if (did_lock)
				memunlock();
		}
		free_swap_buf(bp);
	}
	splx(s);
}


/*
 * Check for valid program size
 * NB - Check data and data growth separately as they may overflow 
 * when summed together.
 */
chksize(ts, ids, uds, ss)
	unsigned ts, ids, uds, ss;
{
	extern unsigned maxtsize;

	if (ctob(ids) > u.u_rlimit[RLIMIT_DATA].rlim_cur ||
	    ctob(uds) > u.u_rlimit[RLIMIT_DATA].rlim_cur ||
	    ctob(ids + uds) > u.u_rlimit[RLIMIT_DATA].rlim_cur ||
	    ctob(ss) > u.u_rlimit[RLIMIT_STACK].rlim_cur) {
		u.u_error = ENOMEM;
		return (1);
	}
	return (0);
}



check_memoryuse(rp)
register reg_t	*rp;
{
	return; /* backward compatibility */
}


/*
 *	pde_memfree -- free a use of a physical page list
 */

pde_memfree(pde,klsize,skipmod)
	pde_t	*pde;
	int	klsize;
	int	skipmod;
{
	int	ki;
	register pfd_t		*pfd;
	register dbd_t		*dbd;
	register int		j;
	reg_t			*rgptr;
	ureg_t			*ur;
	int			do_flush = 0;
	int			did_lock;

	ASSERT(klsize > 0);

	did_lock = memlock_i(); /* hold off interrupt level allocators */
	for (ki = 0; ki < klsize; ki++) {
		ASSERT(skipmod || !(pg_islocked((pde + ki))));
		if (skipmod && 
		    (pg_ismod((pde + ki)) || pg_islocked((pde + ki))))
			continue;
#ifdef DEBUG
		dbd = pdetodbd(pde);
		ASSERT(dbd->dbd_type == DBD_SWAP || \
		       dbd->dbd_type == DBD_FILE  || \
		       dbd->dbd_type == DBD_LSTFILE  );
#endif
		if (pg_isvalid((pde + ki))) {
#ifdef DEBUG
			/*
			 *	Assure that any modified page has not
			 * 	been accessed since the swapout was started.
			 */
			ASSERT(! (pg_ismod((pde + ki)) && \
			          pdetodbd((pde + ki))->dbd_type == DBD_SWAP));
#endif DEBUG
			pg_clrvalid((pde + ki));
			rgptr = pdetoregion(pde);
			reg_change_nvalid(rgptr,-1);
			do_flush++;
		}
	}

	for (j = 0  ;  j < klsize  ;  j++, pde++) {

		/*
		 *	If we are supposed to skip modifed
		 *	pages and this page has been modified,
		 *	then skip it.  Don't forget to decrement
		 *	the use count in the region list.
		 */

		if (skipmod && 
		    (pg_ismod((pde)) || pg_islocked((pde))))
			continue;

		/*
		 *	Disassociate pte from physical page.
		 */

		dbd = pdetodbd(pde);
		rgptr = pdetoregion(pde);
		pfd = pdetopfdat(pde);
		pg_clrpfn(pde);

		/*
		 *	See if the page is in the hash list and
		 *	if not insert it there now.
		 */
		
		if (!(pfd->pf_flags & P_HASH)) {
			ASSERT( pfd->pf_flags&P_DONE );
			pinsert(rgptr, dbd, pfd);
		}
		ASSERT(dbd->dbd_type != DBD_SWAP || \
		       swpuse(&(pfd->pf_dbd)) > 0);

		/*
		 *	free unused pages.
		 *
		 *	Put pages at end of queue since they
		 *	represent disk blocks and we hope they
		 *	will be used again soon.
		 */
		pfree_pfd(pfd,&(pfd->pf_dbd),PF_TAIL);
		if (pfd->pf_use == 0) 
			minfo.freedpgs++;
	}

	/* page table changed */
	if(do_flush)
		request_tlb_flush = 1;
	if (did_lock)
		memunlock();
}



/*
 *	swapout_region
 *
 *	Mark the pages of region for swapout enmasse.
 *
 *	if (flg == 0) only take unreferenced pages 
 *	if (flg == 1) take all valid pages 
 *
 *	Called with rp locked.  This routine does not sleep.
 */

swapout_region(rp,flg)
	reg_t	*rp;
	int	flg;
{
	int	li;
	pde_t	*pde;
	pde_t	*pde_max;
	int	pn;
	int	pcnt;

	if (rp->r_nvalid == 0)
		return;

	for (li = 0, pcnt = 0; li < rp->r_listsz; li++) {
		pde = rp->r_list[li];
		pn = NPGPT;
		if (li == 0) {
			pde += rp->r_pgoff;
			pn -= rp->r_pgoff;
		};
		pcnt += pn;
		if (pcnt > rp->r_pgsz) {
			pn -= (pcnt - rp->r_pgsz);
			pcnt = rp->r_pgsz;
		};
		pde_max = pde + pn;
		for (; pde < pde_max; pde++) 
			if (pg_isvalid(pde)) {
				int	cc;

				cc = checkpage(
					  pde->pgm.pg_pfn - btoc(kpbase),
					  BACK,1);
				if (cc == -1)
					return;
				if (cc || (! flg))
					continue;
				if (checkpage(
					  pde->pgm.pg_pfn - btoc(kpbase),
					  FRONT,1) == -1)
					return;
			}
	}
}



vmtotal()
{
	register struct proc *p;
	register reg_t *rp;
	register int i;
	int nrun = 0;

	total.t_vmtxt = 0;
	total.t_avmtxt = 0;
	total.t_rmtxt = 0;
	total.t_armtxt = 0;
	total.t_vm = 0;
	total.t_avm = 0;
	total.t_rm = 0;
	total.t_arm = 0;
	total.t_rq = 0;
	total.t_dw = 0;
	total.t_pw = 0;
	total.t_sl = 0;
	total.t_sw = 0;
	for (rp = region; rp < &region[v.v_region]; rp++) {
		switch (rp->r_type) {
		case RT_STEXT:
			total.t_vmtxt += rp->r_pgsz;
			total.t_rmtxt += rp->r_nvalid;
			break;
		case RT_PRIVATE:
		case RT_SHMEM:
			total.t_vm += rp->r_pgsz;
			total.t_rm += rp->r_nvalid;
			break;
		};
		if (rp->r_usesz == 0 ||
		    rp->r_use == NULL)
			continue;
		for (i = 0; i < rp->r_usesz; i++) {
			if (rp->r_use[i].ur_proc_index == 
					UREGION_PROC_NULL)
				continue;
			p = proc + rp->r_use[i].ur_proc_index;
			switch (p->p_stat) {

			case SSTOP:
			case SSLEEP:
				if (p->p_slptime >= maxslp)
					continue;
				/* fall into... */

			case SRUN:
			case SIDL:
			case SONPROC:
				switch (rp->r_type) {
				case RT_STEXT:
					total.t_avmtxt += rp->r_pgsz;
					total.t_armtxt += rp->r_nvalid;
					break;
				case RT_PRIVATE:
				case RT_SHMEM:
					total.t_avm += rp->r_pgsz;
					total.t_arm += rp->r_nvalid;
					break;
				};
				goto next;
			};
		}
next:
		;
	};
	for (p = proc; p < (struct proc *)v.ve_proc; p++) {
		if (p->p_flag & SSYS)
			continue;
		if (p->p_stat) {
			switch (p->p_stat) {

			case SSLEEP:
			case SSTOP:
				if (p->p_pri <= PZERO && p->p_stat == SSLEEP)
					nrun++;
#ifndef RISCOS
				if (p->p_flag & SPAGE)
					total.t_pw++;
				else 
#endif RISCOS
				     if (p->p_flag & SLOAD) {
					if (p->p_pri <= PZERO)
						total.t_dw++;
					else if (p->p_slptime < maxslp)
						total.t_sl++;
				} else if (p->p_slptime < maxslp)
					total.t_sw++;
				break;

			case SRUN:
			case SIDL:
				nrun++;
				if (p->p_flag & SLOAD)
					total.t_rq++;
				else
					total.t_sw++;
				break;
			}
		}
	}
	total.t_vm += total.t_vmtxt;
	total.t_avm += total.t_avmtxt;
	total.t_rm += total.t_rmtxt;
	total.t_arm += total.t_armtxt;
	total.t_free = avefree;
}

