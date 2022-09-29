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
#ident	"$Header: probe.c,v 1.13.1.3.1.2.1.4 90/11/15 13:26:34 beacker Exp $"

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
#include "sys/tuneable.h"
#include "sys/errno.h"
#include "sys/cmn_err.h"
#include "sys/debug.h"
#include "sys/buf.h"
#include "sys/pfdat.h"
#include "sys/cpu_board.h"

/*
 * Calculate number of pages transfer touchs
 */
#define len(base, count)	\
	btoc(base + count) - btoct(base)

/*
 * Calulate starting user PTE address for transfer
 */
#define upt(base)	\
	(pde_t *) svtopde(base)

/*
 * Check user read/write access
 * If rw has B_PHYS set, the user PTEs will be faulted
 * in and locked down.  Returns 0 on failure, 1 on success
 */

useracc(base, count, rw)
register base, count;
{
	register int	i;
	register int	npgs;
	register int	x;
	register pde_t	*pt;
	register reg_t  *rp;
	register preg_t  *prp;


	/*
	 * Check page permissions and existence
	 */

	npgs = len(base, count);

	prp = findreg(u.u_procp, base);
	if (prp == NULL)
		return(0);
	rp = prp->p_reg;
	regrele(rp);

	for (i = npgs; --i >= 0; base += (ctob(1) - poff(base))) {
		x = fubyte(base);
		if (x == -1)
			goto out;
		
		/*	In the following test we check for a copy-
		**	on-write page and if we find one, we break
		**	the copy-on-write even if we are not going
		**	to write to the page.  This is necessary
		**	in order to make the lock accounting
		**	work correctly.  The problem is that the
		**	lock bit is in the pde but the count of
		**	the number of lockers is in the pfdat.
		**	Therefore, if more than one pde refers
		**	to the same pfdat, the accounting gets
		**	wrong and we could fail to unlock a
		**	page when we should.  Note that this is
		**	not very likely to happen since it means
		**	we did a fork, no exec, and then started
		**	doing raw I/O.  Still, it could happen.
		*/

		if (rw & B_READ)
			if (subyte(base, x) == -1)
				goto out;
		if (rw & B_PHYS) {
			pt = upt(base);
			if (pg_iscw(pt) &&  !(rw & B_READ))
				if (subyte(base, x) == -1)
					goto out;
			pg_setlock(pt);
			if (pfntopfdat(pt->pgm.pg_pfn)->pf_rawcnt++ == 0) {
				register int s;

				/* avail[rs]mem can be modified at intr level */
				s = splhi();
				if (availrmem - 1 < tune.t_minarmem) {
					splx(s);
					cmn_err(CE_NOTE,
						"useracc - couldn't lock page");
					pg_clrlock(pt);
					--pfntopfdat(pt->pgm.pg_pfn)->pf_rawcnt;
					u.u_error = EAGAIN;
					goto out;
				}
				availrmem -= 1;
				splx(s);
			}
		}
	}
	return(1);
out:

	if (rw & B_PHYS) {
		for (i++, base -= ctob(1)  ;  i < npgs  ;
		    i++, base -= ctob(1)) {
			pt = upt(base);
			if (--pfntopfdat(pt->pgm.pg_pfn)->pf_rawcnt == 0) {
				register int s;
				/* avail[rs]mem can be modified at intr level */
				pg_clrlock(pt);
				s = splhi();
				availrmem += 1;
				splx(s);
			}
		}
	}
	return(0);
}

/*
 * Setup user physio
 */

userdma(base, count, rw)
{
	register int i;

	if(useracc(base, count, rw | B_PHYS)) {
		if (!IS_R6300) {
			/*
			 * By now, all user pages are locked in core,
			 * so clean the cache if we are going to modify the
			 * pages with dma
			 */
			if (rw & B_READ) {
				/* XXX - need to fix this clean cache needs a
				 * physical address not a virtual address.  For
				 * just flush the entire cache
				flush_cache();
				 */
				register pde_t *pt;
				register unsigned k0addr;
				register int size;

				ASSERT(IS_KUSEG(base));
				if (poff(base)) {
					pt = upt(base);
					k0addr = PHYS_TO_K0(ctob(pt->pgm.pg_pfn));
					k0addr += poff(base);
					size = NBPC - poff(base);
					if (size > count)
						size = count;
					clean_cache(k0addr, size);

					base += size;
					count -= size;
				}
				while (count > 0) {
					pt = upt(base);
					k0addr = PHYS_TO_K0(ctob(pt->pgm.pg_pfn));
					if (count > NBPC)
						size = NBPC;
					else
						size = count;
					clean_cache(k0addr, size);

					base += ctob(1);
					count -= size;
				}
			}
		}
		return 1;
	} else 
		return 0;
		

}

/*
 * Terminate user physio
 */

undma(base, count, rw, procp)
	int base;
	int count;
	int rw;
	struct proc *procp;
{
	register pde_t	*pt;
	register int	npgs;
	register reg_t	*rp;
	register pfd_t	*pfd;
	register preg_t	*prp;

	if (procp == NULL) {
		procp = u.u_procp;
	}

	/*
	 * Unlock PTEs, set the reference bit.
	 * Set the modify bit if B_READ
	 */

	prp = findreg(procp, base);
	ASSERT(prp != NULL);
	rp = prp->p_reg;
	ASSERT(rp != NULL);
	usereg (rp, 1);
	regrele(rp);
	
	for (npgs = len(base, count) ; --npgs >= 0 ; base += ctob(1)) {
		pt = upt(base);
		pfd = pfntopfdat(pt->pgm.pg_pfn);
		ASSERT(pt->pgm.pg_lock);
		ASSERT(pfd->pf_rawcnt > 0);
		if (--pfd->pf_rawcnt == 0) {
			register int s;

			/* avail[rs]mem can be modified at intr level */
			pg_clrlock(pt);
			s = splhi();
			availrmem += 1;
			splx(s);
		}
		if ( !(pt->pgm.pg_vr) ||
		    (rw == B_READ && !pg_ismod(pt))) {
			pg_setref(pt);
			if (rw == B_READ)
				pg_setmod(pt);
			if (u.u_procp->p_tlbpid >= 0)
				tlbdropin( u.u_procp->p_tlbpid, base
					  ,pt->pgi.pg_pde );
		}
	}

	reglock (rp);
	unusereg (rp, 0);
}

#ifdef R6000
/*
 * Finding a pde of the 6000 is a bit more complicated,
 * so it becomes a function.
 */
pde_t *
svtopde(addr)
	int addr;
{
	int *ste;
	pde_t *pde;

	ste = ((int **)KSTEBASE)[snum(addr)];
	if (ste == NULL)
		panic("svtopde");
	pde = (pde_t *)ste;
	pde += pnum(soff(addr));
	return (pde);
}
#endif
