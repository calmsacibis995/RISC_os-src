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
#ident	"$Header: grow.c,v 1.19.1.4.1.2.1.2 90/10/16 12:17:11 beacker Exp $"

/*	Copyright (c) 1984 AT&T	*/
/*	  All Rights Reserved  	*/

/*	THIS IS UNPUBLISHED PROPRIETARY SOURCE CODE OF AT&T	*/
/*	The copyright notice above does not evidence any   	*/
/*	actual or intended publication of such source code.	*/

#include "sys/types.h"
#include "sys/bitmasks.h"
#include "sys/param.h"
#include "sys/psw.h"
#include "sys/sysmacros.h"
#include "sys/immu.h"
#include "sys/systm.h"
#include "sys/fs/s5dir.h"
#include "sys/signal.h"
#include "sys/pcb.h"
#include "bsd/sys/time.h"
#include "sys/user.h"
#include "sys/errno.h"
#include "sys/var.h"
#include "sys/pfdat.h"
#include "sys/region.h"
#include "sys/sbd.h"
#include "sys/proc.h"
#include "sys/tuneable.h"
#include "sys/debug.h"
#include "sys/cmn_err.h"
#include "sys/kmem.h"

extern int userstack[];

/* brk and sbrk system calls
*/
#define NO_LOCK_REG     0
#define LOCK_REG        1


sbreak()
{
	struct a {
		uint nva;
	};
	register preg_t	*prp;
	register uint	nva;

	/*	Find the processes data region.
	 */

	prp = findpreg(u.u_procp, PT_DATA);

	nva = ((struct a *)u.u_ap)->nva;
        real_sbreak( prp, nva, LOCK_REG );
}

/*
 * will grow any region. (that contains vaddr)
 */
mips_sbreak( chng, vaddr )
int     chng;
caddr_t vaddr;
{
        preg_t  *prp;
        uint    nva;    /* new virtual address */

        prp = findreg( u.u_procp,vaddr );
	if ( prp )
	    nva = (uint)(prp->p_regva + ctob(prp->p_reg->r_pgsz) + chng);
        real_sbreak( prp,nva,NO_LOCK_REG );
}


real_sbreak( prp, nva, lock )
register preg_t *prp;
register uint   nva;
register int    lock;
{
        register int    change;
        register reg_t  *rp;


	if (prp == NULL)
		goto sbrk_err;

	rp = prp->p_reg;
	if ( lock == LOCK_REG )
	    reglock(rp);

	/* What should the upper limit be? */
	if ((nva > USERSTACK) || (nva < u.u_exdata.ux_datorg)) {
		regrele(rp);
		goto sbrk_err;
	}

	change = btoc(nva) - btoct(prp->p_regva) - rp->r_pgsz;
	if (change > 0  &&  chkpgrowth(change, BSD43_RLIMIT_DATA) < 0) {
		regrele(rp);
		goto sbrk_err;
	}

	if (growreg(prp, change, DBD_DZERO) < 0) {
		regrele(rp);
 		/* The growreg failure will have set u.u_error appropriately. */
		return;
	}

	/* flush TLB of changed entries if shrinking */
	if(change < 0 ) {
		newptes(u.u_procp, btoc(nva), -change);
		setup_wired_tlb(u.u_procp);
	}

	regrele(rp);

	return;

sbrk_err:
	u.u_error = ENOMEM;
	return;
}

/*
 * grow the stack to include the SP (Stacks grow down!)
 * true return if successful.
*/

grow(sp)
int *sp;
{
	register preg_t	*prp;
	register reg_t	*rp;
	register	si;

	/*	Find the processes stack region.
	 */

	prp = findpreg(u.u_procp, PT_STACK);
	if (prp == NULL)
		return(0);
	rp = prp->p_reg;
	reglock(rp);

	si = btoc((unsigned)userstack - (unsigned)sp) - rp->r_pgsz + SINCR;
	/* XXX - check lower bound of stackpointer? */
	if (si <= 0) {
		regrele(rp);
		return(0);
	}

	if (chkpgrowth(si, BSD43_RLIMIT_STACK) < 0) {
		regrele(rp);
		return(0);
	}

	if (growreg(prp, si, DBD_DZERO) < 0) {
		regrele(rp);
		return(0);
	}

	u.u_ssize = rp->r_pgsz;

	regrele(rp);

	return(1);
}

/*
 * pt_alloc(flag)  --  allocate a page to hold page table entries.
 *
 * rp                     -> region page table belongs to
 * flag = 0 		  -> can sleep
 * flag & NOSLEEP > 0	  -> return immediately if no memory
 */

pde_t *
pt_alloc(rp, flag)
	reg_t	*rp;
	int	flag;
{
	register uint	pt;
	register pfd_t	*pf;
	register pdeinfo_t *pip;
	register int s;

#ifdef FRAG_PTS
	/* try to allocate a page frag */
	pt = pt_pfalloc(rp);
	if (pt)
		return ((pde_t *)pt);
#endif
	/* avail[rs]mem can be modified at interrupt level */
	s = splhi();
	if (availrmem - 1 < tune.t_minarmem  ||
	    availsmem - 1 < tune.t_minasmem) {
		splx(s);
		nomemmsg("pt_alloc", 1, 0, 0);
		return(NULL);
	}
	availrmem--;
	availsmem--;
	splx(s);

	pt = getcpages(1, flag & NOSLEEP);
	if (pt == 0) {
		/* avail[rs]mem can be modified at interrupt level */
		s = splhi();
		availrmem++;
		availsmem++;
		splx(s);
		/*	A message has been printed by getcpages.
		*/
		return(NULL);
	}

	pf = kvtopfdat(pt);
	pip = pfdtopdeinfo(pf);
#ifdef FRAG_PTS
	pf->pf_regions = (reg_t **)kmemzalloc(NBPC/PFRAG * sizeof (rp), M_MISC,
				    flag & NOSLEEP ? M_NOWAIT : M_WAITOK);
	if (pf->pf_regions == NULL) {
		freepage(svtopfn(pt));
		/* avail[rs]mem can be modified at interrupt level */
		s = splhi();
		availrmem++;
		availsmem++;
		splx(s);
		return (NULL);
	}
	pip->pde_blkmask = pde_bits(NBPC) & ~1;
	pt_pfadd(pf);
	pf->pf_regions[0] = rp;
#else
	pip->pde_blkmask = 0;
	pf->pf_region = rp;
#endif
	pip->pde_type = DBD_PDE;

	/* Zero out the page table we have just allocated.
	 */
	bzero((caddr_t)pt, NBPC);

	return((pde_t *)pt);
}

#ifdef FRAG_PTS

/*
 * Try to allocate shared pages.
 * Returns 1 if successful, 0 if not.
 */
pt_pfalloc(rp)
	reg_t *rp;
{
	pfd_t *pfd;
	pdeinfo_t *pip;
	u_int want, have, pt;
	int i;

	for (pfd = pt_pflist; pfd; pfd = pfd->pf_next) {
		pip = pfdtopdeinfo(pfd);
		ASSERT(pip->pde_blkmask != 0);
		want = pde_bits(PFRAG);
		have = pip->pde_blkmask;
		for (i = 0; i < NBPC/PFRAG; i++) {
			if ((want & have) == want) {
				pip->pde_blkmask ^= want;
				if (pip->pde_blkmask == 0)
					pt_pfdel(pfd);
				pt = ptosv(pfdattopfn(pfd)) + pfragtob(i);
				pfd->pf_regions[i] = rp;
				return (pt);
			}
			want <<= 1;
		}
	}
	return (0);
}

#endif

/*
 *  pt_free  --  deallocate a page of page table entries
 */
pt_free(pt)
	register int	pt;
{
	register pfd_t	*pf;
	int		pfn;

#ifdef FRAG_PTS
	/* return if page frag is not completely freed */
	if (pt_pffree(pt))
		return;
#endif
	pfn = svtopfn(pt);
	pf = kvtopfdat(pt);
	pf->pf_dbd.dbd_type = DBD_SYS;

#ifdef FRAG_PTS
	kmemfree(pf->pf_regions, M_MISC, M_WAITOK);
	pf->pf_regions = NULL;
#else
	pf->pf_region = NULL;
#endif

	ASSERT((pf->pf_flags & (P_QUEUE | P_HASH)) == 0);

	/* Free the pages
	 * Update the count of allocated and free pages.
	 */

	freepage(pfn);
	/* avail[rs]mem can be modified at interrupt level */
	{   register int s;
	    s = splhi();
	    availrmem += 1;
	    availsmem += 1;
	    splx(s);
	}
}

#ifdef FRAG_PTS

/*
 * Try to free a chunk.
 * Returns 0 if page is completely free, 1 if not.
 */
pt_pffree(pt)
	register int pt;
{
	pfd_t *pfd;
	pdeinfo_t *pip;
	u_int bits;
	int on_pffree;

	pfd = kvtopfdat(pt);
	pip = pfdtopdeinfo(pfd);
	ASSERT(pip->pde_type == DBD_PDE);
	on_pffree = pip->pde_blkmask != 0;
	bits = pde_bits(PFRAG) << btopfrag(poff(pt));
	ASSERT((pip->pde_blkmask & bits) == 0);
	pip->pde_blkmask |= bits;
	if (pip->pde_blkmask == pde_bits(NBPC)) {
		ASSERT(on_pffree);
		pt_pfdel(pfd);
		return (0);
	}
	if (!on_pffree)
		pt_pfadd(pfd);
	return (1);
}

#endif

/*	Check that a process is not trying to expand
**	beyond the maximum allowed virtual address
**	space size.
*/

chkpgrowth(size, rlimtype)
register int	size;	/* Increment being added (in pages).	*/
register int	rlimtype;
{
	register preg_t	*prp;
	register reg_t	*rp;
	register int	cursz = size;

	prp = u.u_procp->p_region;

	while (rp = prp->p_reg) {
		size += rp->r_pgsz;
		switch (rlimtype)  {
		  case BSD43_RLIMIT_DATA:
			if (prp->p_type == PT_DATA || prp->p_type == PT_LIBDAT)
				cursz += rp->r_pgsz;
			break;
		  case BSD43_RLIMIT_STACK:
			if (prp->p_type == PT_STACK)
				cursz += rp->r_pgsz;
			break;
		  default:
			/* Either we have an invalid rlimtype or
			 * we don't enforce any limit.  In any case,
			 * let's set it to -1 so that we won't do
			 * any limit check.
			 */
			rlimtype = -1;
		}
		prp++;
	}

	if (size > tune.t_maxumem)
		return(-1);
	
	/* The following code assumes that rlimtype will be non-zero
	 * whenever we want to check the rlimit.
	 */
	if (rlimtype >= 0 &&
	    cursz > btoc(u.u_rlimit[rlimtype].rlim_cur))
	  	return(-1);

	return(0);
}
