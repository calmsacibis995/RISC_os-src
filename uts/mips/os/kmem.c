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
#ident	"$Header: kmem.c,v 1.15.1.7.1.1.1.2 90/10/16 10:03:11 beacker Exp $"

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
 *	@(#)kern_malloc.c	7.10 (Berkeley) 6/29/88
 */

#include "sys/types.h"
#include "sys/param.h"
#include "sys/sysmacros.h"
#include "sys/tuneable.h"
#include "sys/systm.h"
#include "sys/map.h"
#include "sys/cmn_err.h"
#include "sys/immu.h"
#include "sys/region.h"
#include "sys/cpu_board.h"
#include "sys/sbd.h"
#include "sys/proc.h"
#include "sys/kmem.h"
#include "sys/pfdat.h"
#include "sys/debug.h"

#ifndef	MAX
#define	MAX(a,b)	(((a) > (b)) ? (a) : (b))
#endif
#ifndef	MIN
#define	MIN(a,b)	(((a) <= (b)) ? (a) : (b))
#endif

#if DEBUG
#define	KMEM_DEBUG	1		/* Enable address checking */
int	kmem_debug = 0;			/* 1 for simple debugging */
#endif

int	kmem_chkusage_interval = 10*HZ;	/* checking memory usage (10 seconds) */
ulong	kmem_srecov_tstamp;		/* time stamp for last soft recovery */
ulong	kmem_hrecov_tstamp;		/* time stamp for last hard recovery */

caddr_t	kmem_free_list = NULL;		/* deferred free list of pages to be returned to the system */
extern	void kmem_freepages();
int	kmem_freepages_id;		/* timeout id for kmem_freepages() */
kmeminfo_t	kmeminfo;		/* Various info */


#define	SZTOINDX
#ifdef SZTOINDX
char	sztoindx[4097];		/* maps size to bucket index */
#endif

#ifdef KMEMSTATS
struct kmemstats kmemstats[M_LAST];
#endif

int	nbuckets;		/* number of buckets in use */
int	malloc_base;		/* base of kenel memory virtual space */
kmemusage_t *kmemusage;		/* pointer to usage array */
long wantkmemmap;

extern int malloc_limit;		/* lboot-able			*/
extern struct map kmemmap[];
extern int	kmemmapsz;

/* Variables for kernel memory usage profiling */
int	*kmemlog;			/* points to kernel memory log buffer */
int	kmemlogsz;			/* #entries in kmemlog */
int	logkmem;			/* enable/disable kernel memory log */
int	kmemlogmin;			/* minimum size req to be logged */
int	kmemlogmax;			/* maximum size req to be logged */
int	kmemlogincr;			/* log bucket size */

kmempool_t	*kmempoollist;		/* linked list of local pools */

#if LOG_KMEMREQ

/* kmem_logreq(type, addr, arg1, arg2)
 *
 * Log this kmem request in the circular buffer.
 */

#define	NKMEMREQ	1024		/* space for 1024 entries */
int		kmem_nreqs = NKMEMREQ;	/* Boolean indicating logging state */
int		kmem_seqnum;		/* kmem request sequence number */
struct kmemreq	kmem_reqbuf[NKMEMREQ];	/* log buffer itself */
struct kmemreq	*kmem_reqtail = kmem_reqbuf;	/* points to the beginning */
struct kmemreq	*kmem_reqhead = kmem_reqbuf;	/* points to the tail of log buffer */

void
log_kmemreq(reqtype, addr, arg1, arg2)
int	reqtype, arg1, arg2;
caddr_t	addr;
{
	register int	s;
	kmemreq_t	*kreqp;

	s = splhi();
	if (logkmem && kmem_reqbuf) {
		kreqp = kmem_reqtail;
		kreqp->kreq_reqtype = reqtype;
		kreqp->kreq_seqnum = kmem_seqnum++;
		kreqp->kreq_addr = addr;
		kreqp->kreq_args[0] = arg1;
		kreqp->kreq_args[1] = arg2;
		if (++kreqp >= &kmem_reqbuf[NKMEMREQ])
			kreqp = kmem_reqbuf;
		kmem_reqtail = kreqp;
	}
	splx(s);
}
#endif /* LOG_KMEMREQ */


/* alloc_kmemlog_buf(min, incr, cnt) 
 *
 * This routine allocates a new buffer to log the kernel memory usage
 * data, and enables logging.
 */

alloc_kmemlog_buf(min, incr, cnt) 
int	min, incr, cnt;
{
	int	*logbuf, s;

	if (incr == 0 || cnt == 0) {
		/* Turn off logging and deallocate log buffer */
		logkmem = 0;
		s = splall();
		if (kmemlogsz > 0)
		    kmemfree(kmemlog, 0, 0);
		kmemlog = NULL;
		kmemlogsz = 0;
		kmemlogmin = 0;
		kmemlogincr = 0;
		kmemlogmax = 0;
		splx(s);
		return(0);
	}

	/* increment must be >= 1 and #entries >= 2 */
	if (incr < 1 || cnt < 2)
		return(-1);

	/* Allocate a new buffer */
	if ((logbuf = (int *) kmemzalloc(cnt*sizeof(int), 0, 0)) == NULL)
	    return(-1);
    
	/* set up new buffer */
	logkmem = 0;
	s = splall();
	if (kmemlogsz > 0)
	    kmemfree(kmemlog, 0, 0);
	kmemlogsz = cnt;
	kmemlogmin = min;
	kmemlogincr = incr;
	kmemlogmax = min + (cnt-2)*incr;
	kmemlog = logbuf;
	splx(s);
	logkmem = 1;
	return(0);
}  

/* alloc_newbucket(size) 
 *
 * Allocate a new bucket for the specified size.  If no more space left,
 * then return maxbucket.
 */
 
alloc_newbucket(size)
register unsigned size;
{
	register struct kmembuckets *kbp;
	register int	i;
	int	s;

	if (nbuckets >= maxbucket)
		return(maxbucket);

	s = splhi();
	size = (size + (1<<BUCKETSHFT) -1) & ~((1<< BUCKETSHFT)-1);

	/* Find appropriate place for this new size bucket */
	for (i = 0; i < nbuckets; i++)
		if (kmembsz[i].kb_size >= size)
			break;
	if (i < nbuckets) {
	    register int	j;

	    if (kmembsz[i].kb_size == size) {
		j = kmembsz[i].kb_index;
		splx(s);
		return(j);
	    }

	    /* move stuff down */
	    for (j = nbuckets; j > i; --j)
		    kmembsz[j] = kmembsz[j-1];
	}
	
	kmembsz[i].kb_size = size;
	kmembsz[i].kb_index = nbuckets;

      	kbp = &bucket[nbuckets];
	kbp->kb_next = NULL;
	kbp->kb_calls = 0;
	kbp->kb_total = 0;
	kbp->kb_totalfree = 0;
	kbp->kb_elmsz = size;
	kbp->kb_elmpercl = (size > ctob(1)) ? 1 : ctob(1)/size;
	kbp->kb_highwat = 5 * kbp->kb_elmpercl;
	kbp->kb_inuse = 0;
	kbp->kb_maxused = 0;
#ifdef SZTOINDX
	if ((i = (size-1) >> BUCKETSHFT) < sizeof(sztoindx))
		sztoindx[i] = (char)nbuckets;
#endif
	i = nbuckets++;
	splx(s);
	return(i);
}

#ifdef	DEBUG_BUCKETINDX

/* bucketindx(size, newbucket)
 *
 *  Return bucket index for the rquested size.  If newbucket is set, then
 *  try to create a new bucket entry, if necessary.  A new bucket entry 
 *  is created if we have enough buckets, and we can't find another bucket
 *  without wasting at least 25% of memory.
 */

bucketindx(size, newbucket)
register uint size;
{
	register int	stat, indx;

	if (size >= maxallocsave)
		indx = 0;
	else 
#ifdef SZTOINDX
	if ( (stat = (size-1)>>BUCKETSHFT) >= sizeof(sztoindx)
			    || (indx = sztoindx[stat]) == maxbucket) 
#endif
	{
	    /* Find a bucket with size closest to the requested size
	     * (but bigger).
	     */

	    register int	first, last;
	    register int	s;

	    s = splhi();
	    first = 0;
	    last = nbuckets-1;
	    while (first <= last) {
		indx = (first + last) >> 1;
		stat = size - kmembsz[indx].kb_size; 
		if (stat < 0)
			last = indx - 1;
		else if (stat > 0)
			first = indx + 1;
		else
			break;
	    }
		    
	    if (stat > 0  && ++indx >= nbuckets) {
		/* We did not find any bucket with element size greater
		 * than the requested size.  Print a warning, and use the
		 * first reserved bucket (must be the largest one).
		 */
		cmn_err(CE_NOTE, "Could not find bucket for size: %d\n",size);
		indx = 0;
	    } else if (newbucket && (size+(size>>3)) < kmembsz[indx].kb_size) {
		/* We'll be wasting lots of space by allocating this
		 * big size block.  See if we can create a new one.
		 * If we can't create a new one, then no better size
		 * may appear in future.
		 */
		if ((stat = alloc_newbucket(size)) != maxbucket)
		    indx = stat;
		else
		    indx = kmembsz[indx].kb_index;
	    } else
		    indx = kmembsz[indx].kb_index;
	    splx(s);
	}
	return(indx);
}
#endif /* DEBUG_BUCKETINDX */

/*
 * kmemalloc(size, type, flags) -
 *     Allocate a block of memory
 */
caddr_t
kmemalloc(size, type, flags)
	unsigned long size;
	int type, flags;
{
	register struct kmembuckets *kbp;
	register struct kmemusage *kup;
	register freehdr_t	*freehdrp;
	uint indx, npg, alloc, allocsize;
	int s, ptmemflag;
	caddr_t va, cp;
	pfd_t	*pfd;
	register int	stat;
#ifdef KMEMSTATS
	register struct kmemstats *ksp = &kmemstats[type];

	ASSERT(type <= M_LAST);
#endif

	if (size == 0)
		return(0);

	/* log kernel memory usage (if enabled) */
	if (logkmem) {
	    if (size < kmemlogmin)
		kmemlog[0]++;
	    else if (size >= kmemlogmax)
		kmemlog[kmemlogsz-1]++;
	    else
		kmemlog[(size-kmemlogmin+kmemlogincr-1)/kmemlogincr]++;
	}

	/* Set default flags to be used to call ptmemall() */
	ptmemflag = (IS_R2400) ? PAGE_UNDER_16M : 0;

	/* Calculate bucket index */
#ifdef DEBUG_BUCKETINDX
	indx = bucketindx(size, flags&M_FREQUENT);
#else /* !DEBUG_BUCKETINDX */

#define	newbucket	(flags&M_FREQUENT)

	if (size >= maxallocsave)
		indx = 0;
	else 
#ifdef SZTOINDX
	if ( (stat = (size-1)>>BUCKETSHFT) >= sizeof(sztoindx)
			    || (indx = sztoindx[stat]) == maxbucket) 
#endif
	{
	    /* Find a bucket with size closest to the requested size
	     * (but bigger).
	     */

	    register int	first, last;
	    register int	s;

	    s = splhi();
	    first = 0;
	    last = nbuckets-1;
	    while (first <= last) {
		indx = (first + last) >> 1;
		stat = size - kmembsz[indx].kb_size; 
		if (stat < 0)
			last = indx - 1;
		else if (stat > 0)
			first = indx + 1;
		else
			break;
	    }
		    
	    if (stat > 0  && ++indx >= nbuckets) {
		/* We did not find any bucket with element size greater
		 * than the requested size.  Print a warning, and use the
		 * first reserved bucket (must be the largest one).
		 */
		cmn_err(CE_NOTE, "Could not find bucket for size: %d\n",size);
		indx = 0;
	    } else if (newbucket && (size+(size>>3)) < kmembsz[indx].kb_size) {
		/* We'll be wasting lots of space by allocating this
		 * big size block.  See if we can create a new one.
		 * If we can't create a new one, then no better size
		 * may appear in future.
		 */
		if ((stat = alloc_newbucket(size)) != maxbucket)
		    indx = stat;
		else
		    indx = kmembsz[indx].kb_index;
	    } else
		    indx = kmembsz[indx].kb_index;
	    splx(s);
	}
#undef	newbucket

#endif /* !DEBUG_BUCKETINDX */

	if ((flags & M_CONTIGUOUS) && size > NBPC ) {
	    /* For requests bigger than a page size, we can't allocate
	     * from our bucket pool as the memory may not be contiguous.
	     * Therefore, round up the request size.
	     */
	    ptmemflag |= CONTIGUOUS_PAGES;
	    indx = 0;
	}

	ASSERT (indx == 0 || size <= bucket[indx].kb_elmsz);
	kbp = &bucket[indx];
	s = splhi();
again:
#ifdef KMEMSTATS
	while (ksp->ks_memuse >= ksp->ks_limit) {
		if (flags & M_NOWAIT) {
			splx(s);
			return (0);
		}
		if (ksp->ks_limblocks < 65535)
			ksp->ks_limblocks++;
		sleep((caddr_t)ksp, PSWP+2);
	}
#endif
	if (kbp->kb_next == NULL) {
		if (size > maxallocsave || indx == 0)
			allocsize = roundup(size, NBPC);
		else
			allocsize = kbp->kb_elmsz;
		npg = btoc(allocsize);

		/* The following call to malloc must not sleep. */
		if ((alloc = malloc(kmemmap, npg)) == 0) {
			if (flags & M_NOWAIT) {
				splx(s);
				return (0);
			}
#ifdef KMEMSTATS
			if (ksp->ks_mapblocks < 65535)
				ksp->ks_mapblocks++;
#endif
			wantkmemmap++;
			sleep((caddr_t)&wantkmemmap, PSWP+2);
			goto again;
		}
		va = (caddr_t) ctob(alloc); 
#if KMEM_DEBUG
		if (kmem_debug)
		    kmem_chkaddr(va);
#endif
		if (flags&M_NOWAIT) {
			if (!memlock_i()) {
				mfree(kmemmap, npg, alloc);
				splx(s);
				return(0);
			}
		} else {
		    memlock();
		}
		/* avail[rs]mem can be modified at interrupt level.
		 * Note that interrupts are already disabled earlier.
		 */
		if ((availrmem - npg < tune.t_minarmem) ||
		    (availsmem -  npg < tune.t_minasmem)) {
			memunlock();
			mfree(kmemmap, npg, alloc);
			if (flags & M_NOWAIT) {
			    splx(s);
			    return (0);
			} else {
			    /* Memory reservation is done all over the
			     * kernel.  It is possible to set a flag
			     * indicating need to reserve some memory
			     * and be woken up as soon as some memory
			     * is released.  However, it is desirable
			     * to introduce some damping on kernel 
			     * memory usage.  For now, I just suspend
			     * the calling process for few clock ticks.
			     */

			    cmn_err(CE_WARN, 
				"Can't reserve memory, size:%d  npg:%d\n",
					 size, npg);
			    delay(HZ/20);
			    goto again;
			}
		}
		availsmem -= npg;
		availrmem -= npg;

		if (ptmemall(&sysreg, kvtokptbl(va), npg, 
		    ptmemflag | REGION_NOT_LOCKED, flags&M_NOWAIT, alloc) < 0) {
			availsmem += npg;
			availrmem += npg;
			memunlock();
			mfree(kmemmap, npg, alloc);
			if (flags & M_NOWAIT) {
			    splx(s);
			    return (0);
			} else {
			    /* This warning message should never occur
			     * unless the caller is requesting more 
			     * memory than we can ever allocate.
			     */
			    cmn_err(CE_WARN, 
				"Can't allocate physical memory, size:%d  npg:%d\n",
				 size, npg);
			    /* Wait for few clock ticks and try again */
			    delay(HZ/4);
			    goto again;
			}
		}

		memunlock();

		/* put some good bits into the pde */
		{
		    register int	i;
		    register pde_t	*pde;
		    for (i = 0; i < npg; i++) {
			pde = kvtokptbl(va+i*NBPP);
			pde->pgi.pg_pde |= PG_G | PG_VR | PG_M | PG_SV;
			pde_link(pde);
			pfd = pdetopfdat(pde);
			pfd->pf_dbd.dbd_type = DBD_SYS;
		    }
		}
#ifdef KMEMSTATS
		kbp->kb_total += kbp->kb_elmpercl;
#endif
		kup = btokup(va);
		if (allocsize > maxallocsave || indx == 0) {
			ASSERT(npg <= 65535);
			kup->ku_indx = indx;
			kup->ku_pagecnt = npg;
			kup->ku_flags = 0;
#ifdef KMEMSTATS
			ksp->ks_memuse += allocsize;
#endif
			goto out;
		}
#ifdef KMEMSTATS
		kbp->kb_totalfree += kbp->kb_elmpercl;
#endif
		freehdrp = (freehdr_t *)(va + ((npg*NBPC) - allocsize));
		if ((freehdrp->kf_freecnt = kbp->kb_elmpercl) > 1) {
		    cp = (caddr_t)freehdrp - allocsize;
		    freehdrp->kf_queue = cp;
		    for (; (cp-allocsize) >= va; cp -= allocsize)
			*(caddr_t *)cp = cp - allocsize;
		    *(caddr_t *)cp = NULL;
		}
		else
		    freehdrp->kf_queue = NULL;
		freehdrp->kf_pagecnt = npg;

		/* put this memory on appropriate bucket */
		freehdrp->kf_next = kbp->kb_next;
		kbp->kb_next = freehdrp;

		/* Mark usage pointer */
		kup->ku_flags = KU_QUEUE;
		kup->ku_indx = indx;
		kup->ku_freehdr = (unsigned)freehdrp & POFFMASK;
	}
	else {
	    freehdrp = kbp->kb_next;
	    kup = btokup((caddr_t)freehdrp);
	}

#if KMEM_DEBUG
	if (kmem_debug)
	    kmem_chkaddr(freehdrp);
#endif
	ASSERT(kup->ku_indx == indx);
	if ((va = freehdrp->kf_queue) == NULL) {
		/* Remove freehdr from bucket linked list */
		ASSERT(freehdrp->kf_freecnt == 1);
#if KMEM_DEBUG
		if (kmem_debug && freehdrp->kf_next)
		    kmem_chkaddr(freehdrp->kf_next);
#endif
		kbp->kb_next = freehdrp->kf_next;
		va = (caddr_t) freehdrp;
		kup->ku_flags &= ~KU_QUEUE;
		kup->ku_pagecnt = freehdrp->kf_pagecnt;
	} else {
#if KMEM_DEBUG
		if (kmem_debug) {
		    register unsigned	pgaddr;

		    kmem_chkaddr(va);
		    pgaddr = (unsigned)va & ~POFFMASK;
		    ASSERT(((pgaddr+NBPC-(unsigned)va) % kbp->kb_elmsz) == 0);
		}
		ASSERT (((unsigned)freehdrp & POFFMASK) == kup->ku_freehdr);
#endif
		ASSERT (freehdrp->kf_freecnt > 1);
		freehdrp->kf_queue = *(caddr_t *)va;
		freehdrp->kf_freecnt--;
	}
#ifdef KMEMSTATS
	kbp->kb_totalfree--;
	ksp->ks_memuse += kbp->kb_elmsz;
out:
	kbp->kb_calls++;
	ksp->ks_inuse++;
	ksp->ks_calls++;
	if (ksp->ks_memuse > ksp->ks_maxused)
		ksp->ks_maxused = ksp->ks_memuse;
#else
out:
#endif
	kbp->kb_inuse++;
	if (kbp->kb_inuse > kbp->kb_maxused)
		kbp->kb_maxused = kbp->kb_inuse;
	splx(s);
#if LOG_KMEMREQ
	log_kmemreq(KREQ_ALLOC, va, size, flags);
#endif /* LOG_KMEMREQ */
	return ((caddr_t)va);
}

/*
 * kmemfree(addr, type) -
 *     Free a block of memory allocated by kmemalloc.
 */
void
kmemfree(addr, type, flags)
	caddr_t addr;
	int type;
{
	register struct kmembuckets *kbp;
	register struct kmemusage *kup;
	register freehdr_t	*freehdrp;
	long alloc, size;
	int s;
#ifdef KMEMSTATS
	register struct kmemstats *ksp = &kmemstats[type];

	ASSERT(type <= M_LAST);
#endif
	if (addr == NULL)
		return;

#if KMEM_DEBUG
	if (kmem_debug)
	    kmem_chkaddr(addr);
#endif
	kup = btokup(addr);
	kbp = &bucket[kup->ku_indx];
	ASSERT(kup->ku_indx <= nbuckets);
	ASSERT(!(kup->ku_flags & KU_DEALLOC));
	s = splhi();
	size = kbp->kb_elmsz;
#if LOG_KMEMREQ
	log_kmemreq(KREQ_FREE, addr, size, flags);
#endif /* LOG_KMEMREQ */
#if KMEM_DEBUG
	if (size > NBPC || kup->ku_indx == 0)
		ASSERT(((unsigned)addr & POFFMASK) == 0);
#endif
	if (size > maxallocsave || kup->ku_indx == 0) {
		register pde_t	*pde;
		register pfd_t *pfd;

#if KMEM_DEBUG
		if (kmem_debug) {
			register int	i;

			ASSERT (kup->ku_pagecnt > 0);
			for (i = 1; i < kup->ku_pagecnt; i++) {
				ASSERT(kup[i].ku_flags == 0);
				ASSERT(kup[i].ku_indx == 0);
				ASSERT(kup[i].ku_pagecnt == 0);
			}
		}
#endif
		if (!(flags & M_NOWAIT))
		    memlock();
		else if (!memlock_i()) { 
		    /* place this address on a deferred list of pages
		     * to be deallocated.
		     */
#if KMEM_DEBUG
		    if (kmem_debug && kmem_free_list)
			kmem_chkaddr(kmem_free_list);
#endif
		     *(caddr_t *)addr = kmem_free_list;
		     kmem_free_list = addr;
		     kup->ku_flags |= KU_DEALLOC;
#ifdef KMEMSTATS
		    size = kup->ku_pagecnt << BPCSHIFT;
		    ksp->ks_memuse -= size;
		    if (ksp->ks_memuse + size >= ksp->ks_limit &&
				    ksp->ks_memuse < ksp->ks_limit)
			wakeup((caddr_t)ksp);
		    ksp->ks_inuse--;
		    kbp->kb_total -= 1;
		    kbp->kb_totalfree -= 1;
#endif
		    kbp->kb_inuse--;

		    /* Post a timeout routine (if not already posted) 
		     * to free up this memory.
		     */
		    if (!kmem_freepages_id)
			kmem_freepages_id = timeout(kmem_freepages, 0, 1);
		    splx(s);
		    return;
		}

		/* Invalidate virtual addresses being deallocated */
		kvirt_inval(pnum(addr), kup->ku_pagecnt);
		pde = kvtokptbl(addr);
		pfd = pdetopfdat(pde);
		ASSERT(pfd->pf_dbd.dbd_type == DBD_SYS);
		pfd->pf_dbd.dbd_type = DBD_NONE;
		pfree(&sysreg, pde, 0, kup->ku_pagecnt);
		/* avail[rs]mem can be modified at interrupt level */
		availsmem += kup->ku_pagecnt;
		availrmem += kup->ku_pagecnt;
		memunlock();

		kmeminfo.ki_unmapped += kup->ku_pagecnt;
		mfree(kmemmap, (long)kup->ku_pagecnt, btoc(addr));
		if (wantkmemmap) {
		    wakeup((caddr_t)&wantkmemmap);
		    wantkmemmap = 0;
		}
#ifdef KMEMSTATS
		size = kup->ku_pagecnt << BPCSHIFT;
		ksp->ks_memuse -= size;
		if (ksp->ks_memuse + size >= ksp->ks_limit &&
		    ksp->ks_memuse < ksp->ks_limit)
			wakeup((caddr_t)ksp);
		ksp->ks_inuse--;
		kbp->kb_total -= 1;
#endif
		kup->ku_indx = 0;
		kup->ku_pagecnt = 0;
		kup->ku_flags = 0;

		kbp->kb_inuse--;
		splx(s);
		return;
	}

	if (kup->ku_flags & KU_QUEUE) {
		/* some chunks already on the bucket free list */
		ASSERT (kup->ku_freehdr < NBPC);
		freehdrp = (freehdr_t *)
			(((unsigned)addr & ~POFFMASK) | kup->ku_freehdr);
#if KMEM_DEBUG
		if (kmem_debug > 1) {
		    register caddr_t	x;
		    register unsigned	pgaddr, count;
		    register int	elmsz = kbp->kb_elmsz;
		    register freehdr_t	*next;

		    /* Verify that this freehdr is on the bucket free list. */
		    for (next = kbp->kb_next; next; next = next->kf_next)
			if (next == freehdrp)
				break;
		    ASSERT (next == freehdrp);

		    /* Verify that addr is not already on the free list and
		     * all free chunks are within the same page.
		     */
		    ASSERT(addr != (caddr_t)freehdrp);
		    pgaddr = (unsigned)freehdrp & ~POFFMASK;
		    for (count=1, x = freehdrp->kf_queue; x; 
						count++, x = *(caddr_t *)x) {
			ASSERT(((unsigned)x & ~POFFMASK) == pgaddr);
			ASSERT(addr != x);
			ASSERT (((pgaddr+NBPC-(unsigned)addr) % elmsz) == 0);
		    }
		    ASSERT(freehdrp->kf_freecnt == count);
		}
#endif
		freehdrp->kf_freecnt++;
		ASSERT(freehdrp->kf_freecnt <= kbp->kb_elmpercl);
		*(caddr_t *)addr = freehdrp->kf_queue;
		freehdrp->kf_queue = addr;
	} else {
#if KMEM_DEBUG
		ASSERT(btoc(size) == kup->ku_pagecnt);
		if (kmem_debug > 1) {
		    register unsigned	pgaddr;
		    register freehdr_t	*next;

		    /* Verify this page is not already on the free list */
		    pgaddr = (unsigned)addr & ~POFFMASK;
		    for (next = kbp->kb_next; next; next = next->kf_next)
			ASSERT(((unsigned)next & ~POFFMASK) != pgaddr);
		}
#endif
		/* First item is being freed in this page */
		freehdrp = (freehdr_t *)addr;
		freehdrp->kf_freecnt = 1;
		freehdrp->kf_queue = NULL;
		freehdrp->kf_next = kbp->kb_next;
		freehdrp->kf_pagecnt = kup->ku_pagecnt;
		kbp->kb_next = freehdrp;
		kup->ku_flags = KU_QUEUE;
		kup->ku_freehdr = (unsigned)freehdrp & POFFMASK;
	}
#ifdef KMEMSTATS
	kbp->kb_totalfree++;
	ksp->ks_memuse -= size;
	if (ksp->ks_memuse + size >= ksp->ks_limit &&
	    ksp->ks_memuse < ksp->ks_limit)
		wakeup((caddr_t)ksp);
	ksp->ks_inuse--;
#endif
	kbp->kb_inuse--;
	splx(s);
}


/*
 * kmemzalloc(size, type, flags) -
 *     Allocate a block of memory and initialize it to 0's.
 */
caddr_t
kmemzalloc(size, type, flags)
	unsigned long size;
	int type, flags;
{

	caddr_t		addr;

#if LOG_KMEMREQ
	log_kmemreq(KREQ_ZALLOC, 0, size, flags);
#endif /* LOG_KMEMREQ */
	addr = kmemalloc(size, type, flags);
	if (addr != NULL)
		bzero(addr, size);
	return(addr);
}


void
kmem_freepages()
{
	register caddr_t addr;
	int		s;

	/* Free deferred pages from the kmem_free_list */

	s = splhi();

	/* Check if we can free the pages we deallocated */
	if (memlock_i()) {
	    /* We have been successful in acquiring the lock.
	     * Now free all pages from the free list.
	     */
	    register pde_t	*pde;
	    register pfd_t *pfd;
	    register kmemusage_t	*kup;

	    kmem_freepages_id = 0;
	    while ((addr = kmem_free_list) != NULL) {

#if KMEM_DEBUG
		if (kmem_debug)
		    kmem_chkaddr(addr);
#endif
		kmem_free_list = *(caddr_t *)addr;
		kup = btokup(addr);
	        ASSERT((kup->ku_flags & (KU_DEALLOC|KU_QUEUE)) == KU_DEALLOC);

		/* Invalidate virtual addresses being deallocated */
		kvirt_inval(pnum(addr), kup->ku_pagecnt);
		pde = kvtokptbl(addr);
		pfd = pdetopfdat(pde);
		ASSERT(pfd->pf_dbd.dbd_type == DBD_SYS);
		pfd->pf_dbd.dbd_type = DBD_NONE;
		pfree(&sysreg, pde, 0, kup->ku_pagecnt);
		/* avail[rs]mem can be modified at interrupt level */
		availsmem += kup->ku_pagecnt;
		availrmem += kup->ku_pagecnt;

		kmeminfo.ki_unmapped += kup->ku_pagecnt;
		kmeminfo.ki_dfree += kup->ku_pagecnt;
		mfree(kmemmap, kup->ku_pagecnt, btoc(addr));

		kup->ku_indx = 0;
		kup->ku_pagecnt = 0;
		kup->ku_flags = 0;
	    }
	    memunlock();

	    if (wantkmemmap) {
		wakeup((caddr_t)&wantkmemmap);
		wantkmemmap = 0;
	    }
	}
	else  {
	    /* Can't acquire the lock; try again  a clock tick later */
	    kmem_freepages_id = timeout(kmem_freepages, 0, 1);
	}
	splx(s);
}


kmem_chkusage()
{
	/* Calculate average memory space used on each bucket.  It
	 * is possible to keep several samples but I decided not to
	 * maintain any extra samples of current usage.  I am using
	 * a simple minded algorithm where current usage is assigned
	 * X% and the average usage (1-X)% weight.  The following
	 * table outlines different weightage choices for current
	 * usage and its contribution to the average usage along
	 * time axis:
	 *
	 *  Cur Usage	------------ Interval -------------------
	 *   Weight 	  1      2      3      4      5      6
	 *   25.00%	25.00  18.00  13.00   9.00   6.00   4.50
	 *   37.50%	37.50  23.44  14.65   9.16   5.72   3.58
	 *   50.00%	50.00  25.00  12.50   6.25   3.13   1.56
	 *   62.50%	62.50  23.44   8.79   3.30   1.23   0.46
	 *
	 * For now, I have decided to use 50% for cur usage weight.
	 */

	register int	i;
	register struct kmembuckets *kbp;
	int		s;

	s = splhi();
	for (i = 0, kbp = &bucket[0]; i < nbuckets; i++, kbp++) {
	    kbp->kb_avguse =  (kbp->kb_avguse + kbp->kb_maxused) >> 1;
	    kbp->kb_maxused = kbp->kb_inuse;
	}
	splx(s);

	/* Post another timeout for calculating memory usage */
	timeout(kmem_chkusage, 0, kmem_chkusage_interval);
}

kmem_recover(hard)
{
	/* time to recover some memory (if any) from the buckets */
	register caddr_t addr;
	register int	i;
	register struct kmembuckets *kbp;
	register kmemusage_t	*kup;
	register freehdr_t	*freehdrp, **prev;
	register int	elmpercl, freecnt;
	int		s;

	/* Don't allow calls to recover memory more often than the
	 * frequency at which we compute the average usage.
	 */
	if (hard) {
	    if ((kmem_hrecov_tstamp - (ulong)lbolt) < kmem_chkusage_interval)
		return;
	    kmem_hrecov_tstamp = (ulong)lbolt;
	    kmeminfo.ki_hrecovcalls++;		/* #calls for soft recovery */

	} else {
	    if ((kmem_srecov_tstamp - (ulong)lbolt) < kmem_chkusage_interval)
		return;
	    kmem_srecov_tstamp = (ulong)lbolt;
	    kmeminfo.ki_srecovcalls++;		/* #calls for hard recovery */
	}

	for (i = 0, kbp = &bucket[0]; i < nbuckets; i++, kbp++) {

	    elmpercl = kbp->kb_elmpercl;
	    if (kbp->kb_next == NULL || kbp->kb_totalfree < elmpercl)
		continue;

	    s = splhi();
	    freehdrp = kbp->kb_next;

	    freecnt = kbp->kb_total - 
			((hard) ? MIN(kbp->kb_maxused,kbp->kb_avguse)
				: MAX(kbp->kb_maxused,kbp->kb_avguse));

	    /* recover pages while freecnt > 0 */
	    for (prev = &kbp->kb_next; freehdrp != NULL && freecnt > 0; 
						freehdrp = *prev) {
#if KMEM_DEBUG
		if (kmem_debug)
		    kmem_chkaddr(freehdrp);
		if (kmem_debug > 1) {
		    register caddr_t	x;
		    register unsigned	pgaddr, count;

		    pgaddr = (unsigned)freehdrp & ~POFFMASK;
		    for (count = 1, x = freehdrp->kf_queue; x; 
					count++, x = *(caddr_t *)x) {
			ASSERT(((unsigned)x & ~POFFMASK) == pgaddr);
			kmem_chkaddr(x);
		    }
		    ASSERT(freehdrp->kf_freecnt == count);
		}
#endif
		if (freehdrp->kf_freecnt < elmpercl)
		    prev = &freehdrp->kf_next;
		else {
		    /* Remove this buffer from the bucket linked list and
		     * link it on the list of pages to be deallocated.
		     */
		    *prev = freehdrp->kf_next;
		    
		    addr = (caddr_t)((unsigned)freehdrp & ~POFFMASK);
		    kup = btokup(addr);
		    kup->ku_flags &= ~KU_QUEUE;
		    kup->ku_flags |= KU_DEALLOC;
		    kup->ku_pagecnt = freehdrp->kf_pagecnt;

		    *(caddr_t *)addr = kmem_free_list;
		    kmem_free_list = addr;
		    kbp->kb_total -= elmpercl;
		    kbp->kb_totalfree -= elmpercl;

		    kmeminfo.ki_recov += kup->ku_pagecnt;
		}
	    }
	    splx(s);
	}

	/* Free pages we deallocated earlier */
        if (kmem_free_list != NULL)
		kmem_freepages();

}

/*
 * Initialize the kernel memory allocator
 */
kmeminit()
{
	register long indx;
	int npg;
	register int	i;
	static char initialized = 0;

    if (!initialized) {
	initialized = 1;

	kmempoollist = NULL;
	if (maxallocsave & (maxallocsave-1)) {
	    cmn_err(CE_NOTE, "maxallocsave must be power of 2.");
	    for  (i = maxallocsave; i&(i-1);)
		i = (i | (i-1)) + 1;
	    maxallocsave = i;
	}

	if (maxallocsave < NBPC) {
	    cmn_err(CE_NOTE, "maxallocsave too small.  Setting it to 2*NBPC.");
	    maxallocsave = 2*NBPC;
	}

	if (maxbucket >= (1<<(sizeof(char)*8-1))-1)
		maxbucket = (1<<(sizeof(char)*8-1))-1;

	npg = btoc(malloc_limit);
	if ((malloc_base = malloc(sptmap, npg)) == NULL)
		cmn_err(CE_PANIC, "Not enough virtual memory for malloc.");
	mapinit(kmemmap, kmemmapsz-2);
	mfree(kmemmap, npg, malloc_base);

#ifdef SZTOINDX
	/* Initialize sztoindx[] and bucket[] arrays */
	for (indx = 0; indx < sizeof(sztoindx); indx++)
		sztoindx[indx] = maxbucket;
#endif

	/* Reserve first entry in bucket[] for requests > maxallocsave.
	 * We know that alloc_newbucket() will allocate the next empty
	 * bucket.  
	 */
	nbuckets = 0;
	alloc_newbucket(0x700000);		/* some big size allocation */

	/* Allocate all buckets from the fixed_buckets[] array. */
	for (i = 0; fixed_buckets[i] != 0; i++) {
	    register int	bucketsz = fixed_buckets[i];

	    if (bucketsz < MINALLOCSIZE || bucketsz > maxallocsave)
		cmn_err(CE_WARN,
		    "kmeminit: bucket size %d is outside the range %d - %d.\n",
			bucketsz, MINALLOCSIZE, maxallocsave);
	    else if ( alloc_newbucket(bucketsz) == maxbucket)
		cmn_err(CE_WARN, 
		    "kmeminit: can't create a bucket of size %d. Increase maxbucket.",
			bucketsz);
	}
#ifdef KMEMSTATS
	for (indx = 0; indx < M_LAST; indx++)
		kmemstats[indx].ks_limit = npg * NBPC * 8 / 10;
#endif
		
#if DEBUG
	/*  Allocate a log buffer and enable logging right in the beginning.
	 *  This code will be removed in future.  Keep track of memory requests
	 *  in multiple of 16 bytes (max size req around 64K).
	 */
	 alloc_kmemlog_buf(17, 16, 4096);
#endif

	/* Post first timeout for maintaining memory usage */
	timeout(kmem_chkusage, 0, kmem_chkusage_interval);
     }
}

#if later

/* The following routines provide support for fast memory allocation
 * via some local pool.  These routines are not currently in use and
 * have not been tested.
 */

kmempool_t *
kmempinit(size, type, flags, chunks, highwater, limit, name)
char	*name;
{
	register kmempool_t	*kmemp;

	kmemp = (kmempool_t *)kmemzalloc(sizeof(kmempool_t),type,0);

	if (kmemp != NULL) {
		register int		s;

		kmemp->km_magic = KMEMPOOLMAGIC;
		kmemp->km_size = size;
		kmemp->km_type = type;
		kmemp->km_flags = flags;

		kmemp->km_chunks = chunks;
		kmemp->km_highwater = highwater;
		kmemp->km_limit = limit;
		kmemp->km_name = name;
		kmemp->km_queue = NULL;

		/* place it on the linked list of kmempoollist */
		s = splhi();
		kmemp->km_next = kmempoollist;
		kmempoollist = kmemp;
		splx(s);
	}
	return(kmemp);
}


caddr_t
kmempalloc(kmemp, flags)
register kmempool_t	*kmemp;
int		flags;
{
	register int	s, count;
	register caddr_t addr;

	if (kmemp == NULL)
		return(NULL);

	ASSERT(kmemp->km_magic == KMEMPOOLMAGIC);
	s = splhi();
#if KMEMSTATS
	kmemp->km_calls++;
#endif
	if ((addr = kmemp->km_queue) != NULL) {
	    kmemp->km_queue = *(caddr_t *)addr;
	    kmemp->km_totalfree--;
	} else if (kmemp->km_limit > kmemp->km_inuse) {
	    /* We allocate only one buffer and return it */
	    flags &= M_NOWAIT;
	    flags |= kmemp->km_flags;
	    splx(s);
	    addr = kmemalloc(kmemp->km_size, kmemp->km_type, flags);
	    s = splhi();
	}

#if KMEMSTATS
	if (addr) {
	    if (++kmemp->km_inuse > kmemp->km_maxused)
		kmemp->km_maxused = kmemp->km_inuse;
	}
	else
	    kmemp->km_fail++;
#endif
	splx(s);
	return(addr);
}

kmempfree(kmemp, addr, flags)
register kmempool_t	*kmemp;
register caddr_t 	addr;
register int		flags;
{
	register int	s, freecnt;

	if (kmemp == NULL)
		return;

	ASSERT(kmemp->km_magic == KMEMPOOLMAGIC);
	s = splhi();

	*(caddr_t *)addr = kmemp->km_queue;
	kmemp->km_queue = addr;
	kmemp->km_totalfree++;
#if KMEMSTATS
	kmemp->km_fcalls++;
#endif

	freecnt = kmemp->km_totalfree - kmemp->km_highwater;
	if (freecnt > 0) {
	    /* First of all, remove freecnt buffers from this
	     * pool queue.
	     */
	    register int i;
	    register caddr_t p;

	    for (p=kmemp->km_queue, i=1; i < freecnt; i++, p = *(caddr_t *)p)
		if (p == NULL)
			break;

	    ASSERT(p != NULL);
	    addr = kmemp->km_queue;
	    kmemp->km_queue = *(caddr_t *)p;
	    *(caddr_t *)p = NULL;
	    kmemp->km_totalfree -= freecnt;
	    splx(s);

	    /* Now free all these chunks */
	    for (; addr != NULL; addr = p) {
		p = *(caddr_t *)addr;
		kmemfree(addr, kmemp->km_type, flags);
	    }
	}
	else
	    splx(s);
}


void
kmempdiscard(kmemp)
register kmempool_t	*kmemp;
{
	register int	s;
	register caddr_t addr, next;

	if (kmemp == NULL)
		return;
	s = splhi();

	/* Remove this pool from local pool list and  mark it
	 * invalid to catch any further usage of it.
	 */

	kmemp->km_magic = 0;

	if (kmempoollist == kmemp)
		kmempoollist = kmemp->km_next;
	else {
	    register kmempool_t	*p1, *p2;

	    ASSERT(kmempoollist != NULL);
	    for (p1 = kmempoollist; (p2 = p1->km_next); p1 = p2)
		if (p2 == kmemp) {
			/* remove it from linked list */
			p1->km_next = kmemp->km_next;
			break;
		}

	    ASSERT (p2 != NULL);
	}
	splx(s);

	/* Free all memory chunks from the free list */
	for (addr = kmemp->km_queue; addr != NULL; addr = next) {
		next = *(caddr_t *)addr;
		kmemfree(addr, kmemp->km_type, 0);
	}

	/* Finally free this data structure */
	kmemp->km_queue = NULL;
	kmemfree(kmemp, kmemp->km_type, 0);
}


#if KMEM_STRESS
kmem_stress_pool(count)
{
	/* Stress test program to test out fast version of kernel
	 * memory allocation.
	 */

	register int	i, j;
	kmempool_t	**kmempool;
	caddr_t	bufs[128];
	char	*names;

	if (count <= 0 || count >= 150)
		return(-1);

	/* create 10 different local pools */
	kmempool = (kmempool_t **)kmemzalloc(count*sizeof(kmempool_t *), 0, 0);
	if (kmempool == NULL)
		return(-1);

	names = (char *)kmemzalloc(count * 10, 0, 0);
	if (names == NULL) {
		kmemfree(kmempool);
		return(-1);
	}

	for (i = 0; i < count; i++) {
		strcpy(names+i*10, "test000");
		names[i*10+4] += (i/1000)%10;
		names[i*10+5] += (i/100)%10;
		names[i*10+6] += (i/10)%10;
		names[i*10+7] += i%10;
		kmempool[i] = kmempinit(32+4*i, 0, 0, 16, 32, 1024, 
						&names[i*10]);
	}

	/* Allocate some memory and free it up */
	for (i = 0; i < count; i++) {
	    register kmempool_t	*kmemp = kmempool[i];

	    for (j = 0; j < (sizeof(bufs)/sizeof(caddr_t)); j++)
		bufs[j] = kmempalloc(kmempool[0], (j & 0x4) ? M_NOWAIT : 0);

	    /* Deallocate alternate one */
	    for (j = 0; j < (sizeof(bufs)/sizeof(caddr_t)); j += 2)
		if (bufs[j] != NULL)
		    kmempfree(kmemp, bufs[j], (j & 0x7) ? M_NOWAIT : 0);

	    /* Allocate alternate one */
	    for (j = 0; j < (sizeof(bufs)/sizeof(caddr_t)); j += 2)
		bufs[j] = kmempalloc(kmempool[0], (j & 0x2) ? M_NOWAIT : 0);

	    /* Finally deallocate all */
	    for (j = 0; j < (sizeof(bufs)/sizeof(caddr_t)); j++)
		if (bufs[j] != NULL)
		    kmempfree(kmemp, bufs[j], (j & 0x3) ? M_NOWAIT : 0);
	}

	/* Finally discard all buckets */
	for (i = 0; i < count; i++) {
	    kmempdiscard(kmempool[i]);
	}
	kmemfree(kmempool);
	kmemfree(names);

	return(0);
}
#endif
#endif /* later */

test_kmem(count, size, flags)
{
	/* Stress kernel memory allocation */

#if KMEM_DEBUG
#define	RANDOM	(((randx = randx * 1103515245L+12345)>>16) &0x7fff)
#define	NREQ	128

	static	randx = 1;
	register int	i, j;
	int	type = 0;
	caddr_t	bufs[NREQ];
	register int	rsize;

	if (size > 0xffff)
		return(-1);

	/* Allocate some memory and free it up */
	for (i = 0; i < count; i += NREQ) {
	    int nreq = count - i;

	    if (nreq > NREQ)
		nreq = NREQ;

	    for (j = 0; j < nreq; j++) {
		rsize = size>>1 + ((size*RANDOM)>>16);
		bufs[j] = kmemalloc(rsize, type, flags|((j&4)?M_NOWAIT:0));
	    }

	    qswtch();

	    /* Deallocate alternate one */
	    for (j = 0; j < nreq; j += 2)
		if (bufs[j] != NULL)
		    kmemfree(bufs[j], type, flags|((j&4)?M_NOWAIT:0));

	    qswtch();

	    /* Allocate alternate one */
	    for (j = 0; j < nreq; j += 2) {
		rsize = size>>1 + ((size*RANDOM)>>17);
		bufs[j] = kmemalloc(rsize, type, flags|((j&4)?M_NOWAIT:0));
	    }

	    qswtch();

	    /* Finally deallocate all */
	    for (j = 0; j < nreq; j++)
		if (bufs[j] != NULL)
		    kmemfree(bufs[j], type, flags|((j&4)?M_NOWAIT:0));
	}

#endif KMEM_DEBUG
	return(-1);
}

#if KMEM_DEBUG

kmem_chkaddr(va)
unsigned	va;
{
	register int	pgnum;

	pgnum = btoc(va);
	ASSERT(pgnum>=malloc_base && pgnum <=(malloc_base+btoc(malloc_limit)));
}
#endif

#if	SGI_KMEM_MACROS

#undef	kern_malloc
#undef	kern_free
#undef	kern_calloc

#endif	SGI_KMEM_MACROS

/* 
 * The following routines provide old (SGI) memory allocator interface.
 * In future, these routines will be removed.
 */

char *
kern_malloc(amount)
int amount;
{
	return(kmemalloc(amount, 0, 0));
}

void
kern_free(ap)
char *ap;
{
	kmemfree(ap, 0, 0);
}


char *
kern_calloc(nelem, elsize)
int nelem;
register int elsize;
{
	return(kmemzalloc(elsize*nelem, 0, 0));
}


#if	NFS4_KMEM_MACROS

#undef	kmem_alloc
#undef	kmem_zalloc
#undef	kmem_free
#undef	kmem_fast_alloc
#undef	kmem_fast_zalloc
#undef	kmem_fast_free

#endif	NFS4_KMEM_MACROS

/* 
 * The following routines provide NFS4.0 memory allocator interface.
 * In future, these routines will either be turned into macros or
 * completely removed.
 */


/*
 * kmem_alloc:
 * NFS 4.0 interface supporting allocation. This simply front end's domalloc.
 */
caddr_t
kmem_alloc(amount)
	int amount;
{
	register char *p;
	p = kmemalloc(amount, M_MISC, M_WAITOK);
	ASSERT(p != NULL);
	return (p);
}

/*
 * kmem_zalloc:
 * NFS 4.0 interface supporting allocation and zeroing of the allocated block.
 * This simply front end's domalloc.
 */
char *
kmem_zalloc(amount)
	int amount;
{
	register char *p;
	p = kmemzalloc(amount, M_MISC, M_WAITOK);
	ASSERT(p != NULL);
	return(p);
}

/*
 * kmem_free:
 * NFS 4.0 compatability interface. Call the real workhorse procedure.
 */
void
kmem_free(ap, nbytes)
	char *ap;
        int nbytes;
{
	kmemfree(ap, M_MISC, M_WAITOK);
}


/*
 * NOTICE: All of the 'fast' procedures simply call the normal allocator
 * as the normal allocator is pretty fast.  In future, if a need arised,
 * we can maintain a small pool of memory here.
 */
 
/* These routines are for quickly allocating and freeing memory in
 * some commonly used size.  The chunks argument is used to reduce the
 * number of calls to kmem_alloc, and to reduce memory fragmentation.
 * The base argument is a caller allocated caddr_t * which is the base
 * of the free list of pieces of memory.  None of this memory is ever
 * freed, so these routines should be used only for structures that
 * will be reused often.  These routines can only be called at process level.
 *
 */
caddr_t
kmem_fast_alloc(base, size, chunks)
	caddr_t *base;	/* holds linked list of freed items */
	int	size;	/* size of each item - constant for given base */
	int	chunks;	/* number of items to alloc when needed */
{
	caddr_t	p;

	p = (caddr_t) kmemalloc(size, M_MISC, M_WAITOK);
	ASSERT(p != NULL);
	return (p);
}

void
kmem_fast_free(base, p)
	caddr_t *base, p;
{
	kmemfree(p, M_MISC, M_WAITOK);
}

/*
 * Like kmem_fast_alloc, but use kmem_zalloc instead of
 * kmem_alloc to bzero the memory upon allocation.
 */
caddr_t
kmem_fast_zalloc(base, size, chunks)
	caddr_t *base;	/* holds linked list of freed items */
	int	size;	/* size of each item - constant for given base */
	int	chunks;	/* number of items to alloc when needed */
{
	caddr_t	p;
	p = kmemzalloc(size, M_MISC, M_WAITOK);
	ASSERT(p != NULL);
	return (p);
}
