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
#ident	"$Header: cache.c,v 1.5.1.2.1.1.1.2 91/01/04 17:57:39 beacker Exp $"

/*
 * cache.c -- MIPS cache control system calls
 */

#include "sys/types.h"
#include "sys/sysmacros.h"
#include "sys/signal.h"		/* needed by user.h */
#include "sys/pcb.h"		/* needed by user.h */
#include "sys/user.h"
#include "sys/immu.h"		/* needed by proc.h */
#include "sys/sbd.h"		/* needed by proc.h */
#include "sys/region.h"		/* needed by proc.h */
#include "sys/proc.h"
#include "sys/errno.h"
#include <sys/cachectl.h>
#include <sys/buf.h>

int dcache_size,
    icache_size;

/*
 * cacheflush() -- cacheflush(addr, bcnt, cache) system call routine
 */
cacheflush()
{
	register struct a {
		uint	addr;
		uint	bcnt;
		uint	cache;
	} *uap;
	uap = (struct a *)u.u_ap;

	user_flush(uap->addr, uap->bcnt, uap->cache);
}


/*
 * cachectl() -- cachectl(addr, bcnt, op) system call routine
 */
cachectl()
{
	uint addr, bcnt, op;
	register struct a {
		uint	addr;
		uint	bcnt;
		uint	op;
	} *uap;
	uint rem;
	int was_uncached;
	register struct pde *pde;
	register unsigned k0addr;
	int num_bytes_modified;

	uap = (struct a *)u.u_ap;
	addr = uap->addr;
	bcnt = uap->bcnt;
	op   = uap->op;

	/*
	 * Addr must be page aligned, and bcnt must be a pagesize
	 * multiple.
	 */
	if ( poff(addr) 
	||   poff(bcnt) 
	||   (op != CACHEABLE && op != UNCACHEABLE)) {
		u.u_error = EINVAL;
		return;
	}

	if (!useracc(addr, bcnt, B_WRITE) && !grow(addr)) {
		u.u_error = EFAULT;
		return;
	}

	num_bytes_modified = 0;
	for (; bcnt; bcnt -= NBPC, addr += NBPC) {
		pde = svtopde(addr);
		was_uncached = pg_isnoncache(pde);
		if (op == UNCACHEABLE)
			pg_setnoncache(pde);
		else
			pg_clrnoncache(pde);

		/*
		 * If pde not currently associated with phys mem, don't
		 * worry about flushing caches or tlb
		 */
		if (!pg_isvalid(pde)) {
			continue;
		}

		if (u.u_procp->p_tlbpid != -1) {
			int s = splall();
			unmaptlb(u.u_procp->p_tlbpid, pnum(addr));
			splx(s);
		}

		k0addr = ptosv(pde->pgm.pg_pfn);
		switch (op) {
		case UNCACHEABLE:
			if (!was_uncached) {
				/*
				 *  Going from Cached to Uncached requires
				 *  data to be written back to main memory,
				 *  and all virtual tags to be invalidated.
				 */
				if (num_bytes_modified < icache_size) {
					/*  avoid redundant work  */
					invalidate_virt_icache( addr, NBPC );
				}
				if (num_bytes_modified < dcache_size) {
					/*  avoid redundant work  */
					invalidate_virt_dcache( addr, NBPC );
				}
				/*  do writeback and invalidate of s-cache  */
				invalidate_scache( k0addr, NBPC );
			}
			break;
		case CACHEABLE:
			if (was_uncached) {
				/*
				 *  Going from Uncached to Cached requires
				 *  that stale data be invalidated.
				 */
				page_iflush( k0addr );
				page_dflush( k0addr );
				if (num_bytes_modified < icache_size) {
					/*  avoid redundant work  */
					invalidate_virt_icache( addr, NBPC );
				}
				if (num_bytes_modified < dcache_size) {
					/*  avoid redundant work  */
					invalidate_virt_dcache( addr, NBPC );
				}
				invalidate_scache( k0addr, NBPC );
			}
			break;
		} /* switch */
	num_bytes_modified += NBPC;
	}
}


/*
 * user_flush(addr, bcnt, cache) -- flush caches indicated by cache for
 * user addresses addr .. addr+bcnt-1
 */
user_flush(addr, bcnt, cache)
register unsigned addr, bcnt;
{
	register struct pde *pde;
	register unsigned rem;
	register unsigned k0addr;

	if (!useracc((caddr_t)addr, bcnt, B_WRITE) && !grow(addr)) {
		u.u_error = EFAULT;
		return;
	}

	for (; bcnt; bcnt -= rem, addr += rem) {
		/*
		 * calculate bytes to flush in this page
		 */
		rem = NBPC - poff(addr);
		if (rem > bcnt)
			rem = bcnt;

		/*
		 * calculate appropriate physical address
		 */
		pde = svtopde(addr);
		if (pg_isnoncache(pde) || !pg_isvalid(pde))
			continue;

		k0addr = ptosv(pde->pgm.pg_pfn) | poff(addr);

		switch (cache) {
		case ICACHE:
			invalidate_virt_icache(addr, rem);
			invalidate_scache(k0addr, rem);
			clean_icache(k0addr, rem);
			break;
		case DCACHE:
			invalidate_virt_dcache(addr, rem);
			invalidate_scache(k0addr, rem);
			clean_dcache(k0addr, rem);
			break;
		case BCACHE:
			invalidate_virt_icache(addr, rem);
			invalidate_virt_dcache(addr, rem);
			invalidate_scache(k0addr, rem);
			clean_icache(k0addr, rem);
			clean_dcache(k0addr, rem);
			break;
		default:
			u.u_error = EINVAL;
			return;
		}
	}
	return(0);
}

