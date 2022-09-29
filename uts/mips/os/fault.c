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
#ident	"$Header: fault.c,v 1.36.1.6.1.3.1.3 90/11/15 13:24:31 beacker Exp $"

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
#include "sys/uio.h"
#include "sys/sysinfo.h"
#include "sys/pfdat.h"
#include "sys/reg.h"
#include "sys/swap.h"
#include "sys/getpages.h"
#include "sys/errno.h"
#include "sys/cmn_err.h"
#include "sys/debug.h"
#include "sys/buf.h"
#include "sys/vnode.h"
#include "bsd43/sys/vmmeter.h"

extern dbd_t null_dbd;

/*
 * tfault handles tlb misses to k2seg:
 *	User/kernel double tlb misses for accesses to kuseg.
 *	Kernel tlb misses for direct k2seg accesses.
 */


tfault(ep, code, vaddr, cause, utlb)
int *ep, code, cause, utlb;
uint vaddr;
{
	register proc_t *p;
	register pde_t *pd;
	register uint tmp;
	register reg_t *rp;
	register preg_t *prp;
	caddr_t uvaddr;

	p = u.u_procp;

	minfo.tfault++;

	/* 
	 * vaddr is the address in kpteseg that caused the fault.
	 * Calculate the corresponding user virtual address.
	 */

	uvaddr = (caddr_t)( ((vaddr - KPTEBASE) >> PNUMSHFT) * NBPS
			  + ((vaddr & POFFMASK) >> PTESIZESHFT) * NBPC );

	if(utlb) {
		/* miss from utlbmiss handler */

		/* set up EPC and SR to values from the first (user) tlbmiss */
		ep[EF_EPC] = ep[EF_K1];
		ep[EF_SR] &= ~(SR_KUP|SR_IEP);
		ep[EF_SR] |= (ep[EF_SR] & (SR_KUO|SR_IEO)) >> 2;

		/* At this point, we know virtual page address, but not the
		 * the virtual byte address of the first (user) tlbmiss.
		 * Set up an invalid pte for that page, so we will cause
		 * another fault and regenerate a BADVADDR that can be
		 * used by the signal handler.
		 */
		if((prp = findreg(p, uvaddr)) == NULL) {
			tlbdropin(p->p_tlbpid, uvaddr, 0); 
			return(0);
		}
		
		rp = prp->p_reg;

		/*	The pde could have been changed while we waited for 
		 *	the lock, but it makes no difference here.
		 */

		/* TODO: do we do enough checking on range here? */

		/*
		 * Find physical address of page table.
		 * Set modify bit on tlb entry for kpteseg so direct
		 * modifications by kernel don't tlbmod fault.
		 * If a wired tlb entry is available, use it;
		 * otherwise, drop in tlb entry to point to it in kpteseg.
		 * Get entry for page user wanted.
		 * Drop entry for it into tlb.
		 * Adjust saved registers to return to original
		 * fault location.
		 */

		tmp = btotp(uvaddr - prp->p_regva);
		pd = (pde_t *) (rp->r_list[tmp/NPGPT]);
		ASSERT(pd != NULL);

		if(p->p_nexttlb < NWIREDENTRIES) {
			p->p_ubptbl[p->p_nexttlb].pgi.pg_pde =
				mkpde(PG_VR|PG_M, btoc(K0_TO_PHYS(pd)));
			p->p_tlbhi_tbl[p->p_nexttlb] =
				(caddr_t)(vaddr & ~(NBPP-1));
			tlbwired(TLBWIREDBASE + p->p_nexttlb, p->p_tlbpid,
				p->p_tlbhi_tbl[p->p_nexttlb],
				p->p_ubptbl[p->p_nexttlb].pgi.pg_pde);
			p->p_nexttlb++;
		} else {
			tlbdropin(p->p_tlbpid, vaddr, 
				mkpde(PG_VR|PG_M, btoc(K0_TO_PHYS(pd))));
		}
		tlbdropin(p->p_tlbpid, uvaddr, pd[tmp % NPGPT].pgi.pg_pde);

		regrele(rp);

		return(0);
		
	} else {
		/* miss from kernel */

		if((prp = findreg(p, uvaddr)) == NULL) {
			return(SEXC_SEGV);
		}
		
		rp = prp->p_reg;

		/*	The pde could have been changed while we waited for 
		 *	the lock, but it makes no difference here.
		 */

		/* TODO: do we do enough checking on range here? */

		/*
		 * Find physical address of page table.
		 * Set modify bit on tlb entry for kpteseg so direct
		 * modifications by kernel don't tlbmod fault.
		 * Drop in tlb entry to point to it in kpteseg.
		 */

		tmp = btotp(uvaddr - prp->p_regva);
		pd = (pde_t *) (rp->r_list[tmp/NPGPT]);
		ASSERT(pd != NULL);
		tlbdropin(p->p_tlbpid, vaddr, 
			mkpde(PG_VR|PG_M, btoc(K0_TO_PHYS(pd))));

		regrele(rp);

		return(0);
	}
}
/*	Protection fault handler
 *
 *	pfault is called when you try to write a "protected page"
 *	a) modify bit emulation
 *	b) copy on write
 *	c) out of range (error)
 *	d) really read only region (error)
 *	e) swap association that needs breaking
 */

pfault(vaddr)
{
	register dbd_t	*dbd;
	register proc_t	*p;
	register pfd_t	*pfd;
	register reg_t	*rp;
	register preg_t	*prp;
	register unsigned tmp;
	register pde_t	*pd;	/* address of faulting pde.	*/
	register pfd_t	*tpfd;


	/*	Get a pointer to the region which the faulting
	 *	virtual address is in.
	 */

	p = u.u_procp;
retry:
	if ((prp = findreg(p, vaddr))==NULL)
		return(SEXC_SEGV);
	rp = prp->p_reg;

	/*
	 *	pd is the virtual address of the faulting pde
	 *	in the k2seg CONTEXT map of the user's page tables.
	 *	We have to find the physical address of the faulting
	 *	pde since the dbd's are contiguous to the physical page
	 *	tables, not the virtual page tables.
	 *	The physical address is found through the region r_list.
	 *	The dereference of the r_list pointer below can't fail
	 *	because we already know the pde is valid.
	 */

	tmp = btotp(vaddr - (int)prp->p_regva);
	pd = (pde_t *)&(rp->r_list[tmp/NPGPT][tmp%NPGPT]);

	/*	Check to see that the pde hasn't been modified
	 *	while waiting for the lock
	 */

	if (!pd->pgm.pg_vr) {
		regrele(rp);
		return(0);
	}

	/*	Now check for:
	 *	A real protection error vs.
	 *	Mod bit emulation	vs.
	 *	Copy on write.
	 *	since pfault is only called for tlbmod exceptions,
	 *	we have to check the pregion attach type for permissions.
	 *	There are no protection bits in the pde's.
	 */

	minfo.pfault++;
	/* Real protection error */
	if(prp->p_flags & PF_RDONLY) {
		regrele(rp);
		return(SEXC_BUS);
	}

	dbd = pdetodbd(pd);

	if (dbd->dbd_type == DBD_MMAP) {
		regrele(rp);
		return(SEXC_BUS); /* read-only mmap'ed pages stay that way */
	};

	pfd = pdetopfdat(pd);
	if (! MLOCK_NOWAIT(pfd)) {
		regrele(rp);
		MLOCK(pfd);

		/*
		 * recheck that pde is still consistent
		 */
		if ((prp = findreg(p, vaddr))==NULL) {
			MUNLOCK(pfd);
			return(SEXC_SEGV);
		};
		rp = prp->p_reg;

		tmp = btotp(vaddr - (int)prp->p_regva);
		pd = (pde_t *)&(rp->r_list[tmp/NPGPT][tmp%NPGPT]);

		if (! pg_isvalid(pd) ||
		    ! pd->pgm.pg_vr ||
		    pg_ismod(pd) ||
		    pfd != pdetopfdat(pd)) {
			MUNLOCK(pfd);
			regrele(rp);
			new_tlbpid(p);	/* resync pte with tlb/virttags */
			return(0);
		}
		dbd = pdetodbd(pd);
	};

	/*
	 * In all cases below, the virtual tags that say
	 * "not writable" need to be invalidated.
	 */
	invalidate_virt_scache(ptosv(pd->pgm.pg_pfn), NBPC);

	/* Mod bit emulation */
	if (! pg_iscw(pd)) {
		pg_setmod(pd);
		tlbdropin(p->p_tlbpid, vaddr, pd->pgi.pg_pde);
		regrele(rp);

		/* break disk association to swap, if any */
		break_pfd_from_disk(pfd,pd);

		MUNLOCK(pfd);
		return(0);
	}

	/* Copy on write page */
	ASSERT(rp->r_type == RT_PRIVATE);
	ASSERT(!pg_ismod(pd));

	/*	Copy on write
	 *	If use is 1, and page is not from a file,
	 *	steal it, otherwise copy it
	 */

	if (pfd->pf_use > 1  ||
	    dbd->dbd_type == DBD_FILE || dbd->dbd_type == DBD_LSTFILE) {
		MUNLOCK(pfd);
		if (! allocpage(rp,vaddr)) {
			regrele(rp);
			return(SEXC_SEGV);
		};

		ASSERT(rp->r_flags & RG_LOCK);

		if (! MLOCK_NOWAIT(pfd)) {
			regrele(rp);
			if (pfd == pdetopfdat(&p->p_tpde)) {
				/* 
				 * someone freed the virtual page while we were 
				 * waiting, and we reallocated it; try again.
				 */
			  	killpage();
				goto retry;
			};
			mlock2(pfd,pdetopfdat(&p->p_tpde));

			/*
			 * recheck that pde is still consistent
			 */
			if ((prp = findreg(p, vaddr))==NULL) {
				MUNLOCK(pfd);
				killpage();
				return(SEXC_SEGV);
			};
			rp = prp->p_reg;

			tmp = btotp(vaddr - (int)prp->p_regva);
		};

		/*
		 *	recompute and check page descriptor, in case
		 *	region changed while we were waiting
		 */
		if (tmp < rp->r_pgoff ||
		    tmp > rp->r_pgoff + rp->r_pgsz) {
release_and_retry:
			killpage();
			MUNLOCK(pfd);
			regrele(rp);
			goto retry;
		};
		pd = (pde_t *)&(rp->r_list[tmp/NPGPT][tmp%NPGPT]);
		dbd = pdetodbd(pd); /* recalculate after possible wait */
		if (! pg_isvalid(pd) ||
		    ! pd->pgm.pg_vr ||
		    ! pg_iscw(pd) ||
		    pg_ismod(pd) ||
		    pfd != pdetopfdat(pd))
			goto release_and_retry;

		/*
		 *	Copy the data from the old page to the new
		 */
		tpfd = pdetopfdat(&p->p_tpde);
		tpfd->pf_flags &= ~P_CLEARED;
		bcopy(ptosv(pd->pgm.pg_pfn),
			ptosv(p->p_tpde.pgm.pg_pfn), NBPP);

		/*
		 *	Release the reference to the old page
		 */
		memlock();
		MUNLOCK(pfd);
		pfree(rp, pd, dbd, 1);
		memunlock();

		/*
		 *	Assign the new page to the region
		 */
		minfo.cw++;
		assignpage(rp,pd);
		ASSERT(pdetodbd(pd)->dbd_type == DBD_NONE);
		pg_setvalid(pd);
		reg_change_nvalid(rp,1);

		MDONE(tpfd);
	} else {

		/*	We are modifiying the page so
		 *	break the disk association to swap.
		 *
		 *	Note that we need not worry about having multiple
		 *	uses of the dbd, since we are here only if
		 *	pfd->pf_use == 1.
		 */
		
		break_pfd_from_disk(pfd,pd);

		minfo.steal++;
		MUNLOCK(pfd);
	}

	/*	Set the modify bit here before the region is unlocked
	 *	so that vhand will write the page to swap if necessary.
	 *	pg_clrcw() also sets the modify bit so
	 *	the following is sufficient. Also don't need to change
	 *	protection bits in pde's.
	 */

	pg_clrcw(pd);

	tlbdropin(p->p_tlbpid, vaddr, pd->pgi.pg_pde);

	regrele(rp);

	return(0);
}

/*	Translation fault handler
 *	vfault is called when the v bit is not set
 *	a) reference detection
 *	b) demand zero 
 *	c) demand from file or swap
 *	c) illegal reference
 *
 *	Note that this routine now tries to keep the region unlocked
 *	across any sleep(), to avoid contention with other processes
 *	which may be sharing the region, and to facilitate paging.
 *	Also, it assumes that vhand() may steal pages across any sleep().
 */

int check_mem_at_vfault = 1;	/* set if hogs free mem before getting more */
		/* Obsolete: retained for compatibility, but ignored 	*/

vfault(vaddr)
{
	register pde_t	*pd;	/* address of faulting pde.	*/
	register proc_t	*p;
	register dbd_t	*dbd;
	register pfd_t	*pfd;
	register reg_t	*rp;
	register struct vnode *vp;
	register preg_t	*prp;
	register unsigned tmp;
	int	page_was_free;

	ASSERT(u.u_procp->p_flag & SLOAD);
	ASSERT((unsigned)vaddr < (unsigned)K0SEG);

	p = u.u_procp;

	/*	Lock the region containing the page that faulted.
	 */

	/* In some cases, we need to lock other resources.  To avoid
	 * holding the region lock too long, we release it first.
	 * We may then find that the initial conditions for this
	 * routine no longer hold.  If so, we start over from the top.
	 */
retry:
	if ((prp = findreg(p, vaddr))==NULL) {
		return(SEXC_SEGV);
	}
	rp = prp->p_reg;

	/*
	 *	Find the physical address of the faulting pde.
	 * 	The dbd's are contiguous to the physical pde's,
	 *	found through the region's r_list.
	 *	Reference bit faults can be optimized by allowing
	 *	access to the virtual map of pde's in k2seg,
	 *	and this works fine for legal addresses, but
	 *	currently panics the kernel when the user gives
	 *	an address that tfault can't deal with. Some
	 *	kind of nofault routine might help.
	 */

	tmp = btotp(vaddr - (int)prp->p_regva);
	pd = (pde_t *)&(rp->r_list[tmp/NPGPT][tmp%NPGPT]);
	dbd = pdetodbd(pd);

	/*
	 * Check for reference bit fault.
	 */

	if (pg_isvalid(pd)) {
		minfo.rfault++;
		pd->pgi.pg_pde |= (PG_VR | PG_NDREF);
		tlbdropin(p->p_tlbpid, vaddr, pd->pgi.pg_pde);
		regrele(rp);
		return(0);
	}
	/*	Check for an unassigned page.  This is a real
	 *	error.
	 */

	if (dbd->dbd_type == DBD_NONE) {
		regrele(rp);
		return(SEXC_SEGV);
	}

	/*	Check that the page has not been read in by
	 *	another process while we were waiting for
	 *	it on the reglock above.
	 */

	if (pd->pgm.pg_vr) {
		regrele(rp);
		return(0);
	}

	/*	Allocate a page in case we need it.  We must
	 *	do it now because it is not convenient to
	 *	wait later if no memory is available.  If
	 *	ptmemall does a wait and some other process
	 *	allocates the page first, then we have
	 *	nothing to do.
	 */
	
	if (! allocpage(rp,vaddr)) {
		regrele(rp);
		return(SEXC_SEGV);
	}
	pfd = pdetopfdat((&p->p_tpde));

	/*
	 *	See if previous conditions still hold
	 */

	if (tmp < rp->r_pgoff ||
	    tmp > rp->r_pgoff + rp->r_pgsz) {
release_and_retry:
		killpage();
		regrele(rp);
		goto retry;
	};
	pd = (pde_t *)&(rp->r_list[tmp/NPGPT][tmp%NPGPT]);
	dbd = pdetodbd(pd); /* recalculate after possible wait */
	if (pg_isvalid(pd) ||
	    dbd->dbd_type == DBD_NONE) 
		goto release_and_retry;

	minfo.vfault++;
	cnt.v_faults++;

	/*	See what state the page is in.
	 */

	page_was_free = 0;

	switch (dbd->dbd_type) {
	case DBD_DFILL:
	case DBD_DZERO:{

		/* 	Demand zero or demand fill page.
		 */

		minfo.demand++;
		if (dbd->dbd_type == DBD_DZERO) {
			cnt.v_zfod++;
			if ((pfd->pf_flags & P_CLEARED) == 0) {
				bzero(ptosv(p->p_tpde.pgm.pg_pfn), NBPP);
				pclearstat.p_miss++;
			} else {
				pfd->pf_flags &= ~P_CLEARED;
				pclearstat.p_hit++;
			}
			MDONE(pfd);
		} else
			cnt.v_exfod++;
		/*
		 * Assign the new page to the region
		 */
		assignpage(rp,pd);
		*dbd = null_dbd;

		u.u_ru.ru_majflt++;
		break;
	}
	case DBD_SWAP:
	case DBD_FILE:
	case DBD_LSTFILE:{
		dbd_t	dbd_save;

		/*	Page is on swap or in a file.  See if a
		 *	copy is in the hash table.
		 */

		if (pfd = pfind(rp, dbd)) {

			/*	Page is in cache.
			 *	If it is also on the free list,
			 *	remove it.
			 */

			if (pfd->pf_flags&P_QUEUE) {
				ASSERT(pfd->pf_use == 0);
				ASSERT(freemem > 0);
				freemem--;
				pfd->pf_flags &= ~P_QUEUE;
				pfd->pf_prev->pf_next = pfd->pf_next;
				pfd->pf_next->pf_prev = pfd->pf_prev;
				pfd->pf_next = NULL;
				pfd->pf_prev = NULL;
				if (pfdattopfn(pfd) < btoc(MEM16M))
					freemem16m--;
				page_was_free = 1;
			}

			/*
			 *	Record this use of the page
			 */

			pfd->pf_use++;

			/*	If the page has not yet been read
			 *	in from swap or file, then wait for
			 *	the I/O to complete.
			 */

			while (! MISDONE(pfd)) {
				dbd_save = *(pdetodbd(pd));
				regrele(rp);
				
				cnt.v_intrans++;
				MWAIT(pfd);

				/* 
				 * check if we still want the page
				 */
				if ((prp = findreg(p, vaddr))==NULL) {
					MUNLOCK_WAIT(pfd);
					pfree_pfd(pfd,&(pfd->pf_dbd),
						  PF_EITHER);
					killpage();
					return(SEXC_SEGV);
				}
				rp = prp->p_reg;
				tmp = btotp(vaddr - (int)prp->p_regva);
				pd = (pde_t *)&(rp->r_list[tmp/NPGPT][tmp%NPGPT]);
				dbd = pdetodbd(pd);
	
				if (pg_isvalid(pd) ||
				    ! (dbd->dbd_type == dbd_save.dbd_type &&
				       dbd->dbd_swpi == dbd_save.dbd_swpi &&
				       dbd->dbd_blkno == dbd_save.dbd_blkno) ||
				    ! pmatch(rp,dbd,pfd)) {
					regrele(rp);
					MUNLOCK_WAIT(pfd);
					pfree_pfd(pfd,&(pfd->pf_dbd),
						  PF_EITHER);
					killpage();
					return(0); /* retry the operation */
				};
			};

			if (pfd->pf_flags & P_BAD) {
				MUNLOCK_WAIT(pfd);
				pfree_pfd(pfd,&(pfd->pf_dbd),
					  PF_EITHER);
				killpage();
				return(SEXC_KILL);
			}

			pd->pgm.pg_pfn = pfdattopfn(pfd);

			minfo.cache++;
			if (page_was_free) {
				if (dbd->dbd_type == DBD_SWAP)
					cnt.v_xsfrec++;
				else
					cnt.v_xifrec++;
				cnt.v_pgfrec++;
			};
			cnt.v_pgrec++;
			u.u_ru.ru_minflt++;

			/*	Free the page we allocated above
			 *	since we don't need it.
			 *	(Do it here to avoid a possible race
			 *	with other processes for the virtual page.)
			 */

			killpage();
		} else {

			/*	Must read from swap or a file.
			 *	Get the pfdat for the newly allocated
			 *	page and insert it in the hash table.
			 */			

			dbd_save = *dbd;
			pfd = pdetopfdat(&p->p_tpde);
			ASSERT((pfd->pf_flags & P_HASH) == 0);

			/*
			 *  Insert now, so noone else tries to bring
			 *  in this page
			 */
			p->p_tpde.pgi.dbd = *dbd;
			pfd->pf_dbd = *dbd;
			pinsert(rp, dbd, pfd);
					
			if (dbd->dbd_type == DBD_SWAP) {
				pglst_t		pglist;

				
				/*	Read from swap.
				 */

				regrele(rp);
				minfo.swap++;
				pglist.pl_pfdat = pfd;
				pglist.pl_pde = &p->p_tpde;
				swap(&pglist, 1, B_READ);

				/* 
				 * check if we still want the page
				 */
				if ((prp = findreg(p, vaddr))==NULL) {
					killpage();
					return(SEXC_SEGV);
				}
				rp = prp->p_reg;
				tmp = btotp(vaddr - (int)prp->p_regva);
				pd = (pde_t *)&(rp->r_list[tmp/NPGPT][tmp%NPGPT]);
				dbd = pdetodbd(pd);
	
				if (pg_isvalid(pd) ||
				    ! (dbd->dbd_type == dbd_save.dbd_type &&
				       dbd->dbd_swpi == dbd_save.dbd_swpi &&
				       dbd->dbd_blkno == dbd_save.dbd_blkno)) {
					killpage();
					regrele(rp);
					return(0); /* retry the operation */
				};

				ASSERT(pmatch(rp,dbd,pfd));

				/* 
				 *  If someone is trying to remove the swap
				 *  space, make the in-core copy the only one.
				 */
				if (swaptab[dbd->dbd_swpi].st_flags
						& ST_INDEL)
					release_swap_for_pfd(pfd,pd);

				/*
				 * 	Check if page is good 
				 */
				if (pfd->pf_flags & P_BAD) {
					killpage();
					regrele(rp);
					return(SEXC_SEGV);
				};

			} else {
				/* Read from file */
				int kvaddr;
				int offset;
				int retval;
				int i;

				kvaddr = ptosv(p->p_tpde.pgm.pg_pfn);
				vp = rp->r_vptr;
				if (vp == (struct vnode *)0) {
				  /*
				   * Special case where region was invalidated
				   * by server. So it's a kill error case.
				   */
				  killpage();
				  regrele(rp);
				  return(SEXC_KILL);
				}

				VN_HOLD(vp);
				offset = dbd->dbd_blkno*NBPSCTR;
				/*
				 * Read the page from the file.
				 * BUG XXX ~~~
				 * This is not quite correct--error cases are
				 * not handled properly.
				 */
				regrele(rp);
				u.u_error =
				    vn_rdwr(UIO_READ, vp, (caddr_t)kvaddr, NBPP,
					offset, UIO_SYSSPACE, IO_UNIT, &i);
				VN_RELE(vp);
				if (u.u_error)
					retval = -1;
				else
					retval = NBPP - i;

				minfo.file++;
				cnt.v_exfod++;

				/* 
				 * check if we still want the page
				 */
				if ((prp = findreg(p, vaddr))==NULL) {
					killpage();
					return(SEXC_SEGV);
				}
				rp = prp->p_reg;
				tmp = btotp(vaddr - (int)prp->p_regva);
				pd = (pde_t *)&(rp->r_list[tmp/NPGPT][tmp%NPGPT]);
				dbd = pdetodbd(pd);
	
				if (pg_isvalid(pd) ||
				    dbd->dbd_type != dbd_save.dbd_type ||
				    dbd->dbd_swpi != dbd_save.dbd_swpi ||
				    dbd->dbd_blkno != dbd_save.dbd_blkno ||
				    pd->pgm.pg_vr) {
					killpage();
					regrele(rp);
					return(0); /* retry the operation */
				};

				/*
				 *	Check for error on the read
				 */
				if (retval <= 0) {
					killpage();
					regrele(rp);
					return(SEXC_KILL);
				}

				/*
				 * clear the last bytes of a partial page
				 */
				if (dbd->dbd_type == DBD_LSTFILE) {
					i = rp->r_filesz - offset;
					if (i < NBPP)
						bzeroba(kvaddr+i, NBPP-i);
				}
			}

			/*
			 * 	Copy the page descriptor, but leave the 
			 *	dbd alone 
			 */
			if (pg_iscw(pd)) {
				pg_setcw(&(p->p_tpde));
			}
			if (pg_isnoncache(pd))
				pg_setnoncache(&(p->p_tpde));
			ASSERT(! pg_islocked(pd) && \
			       ! pg_ismod(pd) && \
			       ! pd->pgm.pg_g && \
			       ! pg_isvalid(pd));

			assignpage(rp,pd);

			/*	Mark the I/O done in the pfdat and
			 *	awaken anyone who is waiting for it.
			 */

			MDONE(pfd);

			u.u_ru.ru_majflt++;
		};
		break;
	}
	default:
		cmn_err(CE_PANIC, "vfault - bad dbd_type");
	}

	pg_setvalid(pd);
	reg_change_nvalid(rp,1);
	pg_clrmod(pd);
	tlbdropin(p->p_tlbpid, vaddr, pd->pgi.pg_pde);
	clean_cache(ptosv(pd->pgm.pg_pfn), NBPC);
	regrele(rp);

	return(0);
}


/*
 *	Allocate a page for pfault() or vfault()
 *
 *	The page is allocated into p->p_tpde and is left locked.
 *
 *	returns 0 if allocation failed, and 1 otherwise.
 */

allocpage(rp,vaddr)
	register reg_t *rp;
	int	vaddr;
{
	register proc_t *p = u.u_procp;
	register pfd_t *pfd;

	p->p_tpde.pgi.pg_pde = 0;
	p->p_tpde.pgi.dbd = null_dbd;
	memlock();
	if (ptmemall(rp, &p->p_tpde, 1, PAGE_COLOR | NO_CHANGE_NVALID, 
		     0, btoct(vaddr))) {
		memunlock();
		return(0);
	}
	memunlock();
	pfd = pdetopfdat((&p->p_tpde));
	MLOCK(pfd);
	return(1);
}


/*
 * Clean up after a read error during vfault processing.
 * This code frees the previously allocated page, and marks
 * the pfdat as bad.
 */
killpage()
{
	register proc_t *p = u.u_procp;
	register pfd_t *pfd;

	ASSERT(! pg_isvalid((&p->p_tpde)));
	pg_setvalid((&p->p_tpde));
	/* need tlb_flush here ? */
	memlock();
	pfd = pdetopfdat(&(p->p_tpde));
	if (pfd->pf_flags & P_HASH)
		premove(pfd);
	pfd->pf_flags |= P_BAD;
	pfd->pf_dbd = null_dbd;
	pfd->pf_flags &= ~P_SWAP;
	MDONE(pfd);
	MUNLOCK(pfd);
	pfree(NULL, &p->p_tpde, NULL, 1);
	memunlock();
	p->p_tpde.pgi.pg_pde = 0;
}


/*
 * Assign temporary page to permanent page table entry in vfault.
 */

assignpage(rp,pd)
	register reg_t *rp;
	register pde_t *pd;
{
	register proc_t *p = u.u_procp;
	register pfd_t *pfd;

	ASSERT((! pg_isvalid((&p->p_tpde))) && \
	       (! pg_isvalid(pd)));
	pd->pgi.pg_pde = p->p_tpde.pgi.pg_pde;
	p->p_tpde.pgi.pg_pde = 0;
	p->p_tpde.pgi.dbd = null_dbd;
	pfd = pdetopfdat(pd);
	MUNLOCK(pfd);
}


/*
 * pg_setsftval(pde)
 *
 *	Set PG_SV in a pde.  If appropriate, move the
 * 	dbd to the pfdat, and link the pde to the pfdat usage list
 */

pg_setsftval(pde)
	pde_t *pde;
{
	if (pg_isvalid(pde))
		return(pde->pgi.pg_pde);
	
	pde->pgi.pg_pde |= PG_SV;
	pde_link(pde);
	return(pde->pgi.pg_pde);
}


/*
 * pg_clrsftval(pde)
 *
 *  	This procedure clears PG_SV.  If this changes its state,
 *	and the dbd is in the corresponding pfdat, this procedure
 *	unlinks the pde from the pfdat, and copies the dbd back
 *	to the pde.
 */

pg_clrsftval(pde)
	pde_t	*pde;
{
	if (! pg_isvalid(pde))
		return(pde->pgi.pg_pde);
	
	pde_unlink(pde);
	pde->pgi.pg_pde &= ~PG_SV;
	return(pde->pgi.pg_pde);
}


/*
 * pde_link(pde)
 *	Link a pde onto a pfdat use list, if appropriate
 */

pde_link(pde)
	pde_t	*pde;
{
	pfd_t	*pfd;
	dbd_t	*dbd;

	if (! pg_isvalid(pde))
		return;

	dbd = pdetorealdbd(pde);
	if (dbd->dbd_type == DBD_MMAP ||
	    dbd->dbd_type == DBD_PDE)
		return;
	if (dbd->dbd_type == DBD_PGUSE) {
		cmn_err(CE_WARN,"pde_link:  multiple link");
		pde_unlink(pde);
	};
	pfd = pdetopfdat(pde);
	if (!(dbd->dbd_type == pfd->pf_dbd.dbd_type &&
	      dbd->dbd_swpi == pfd->pf_dbd.dbd_swpi &&
	      dbd->dbd_blkno == pfd->pf_dbd.dbd_blkno)) {

		if (pgusetonextpde((&pfd->pf_use_link)) == NULL &&
		    pfd->pf_dbd.dbd_type == DBD_NONE) {
			pfd->pf_dbd = *dbd;
			if (pfd->pf_dbd.dbd_type == DBD_SWAP) {
				pfd->pf_flags |= P_SWAP;
				ASSERT(swpuse(&(pfd->pf_dbd)) > 0);
			} else
				pfd->pf_flags &= ~P_SWAP;
		} else
			cmn_err(CE_WARN,"pde_link:  dbd mismatch");
	};

	if (pfd->pf_use_link.pguse_type != DBD_PGUSE) {
	  	pfd->pf_use_link.pguse_type = DBD_PGUSE;
		pfd->pf_use_link.pguse_next = 0;
	};

	*pdetopguselink(pde) = pfd->pf_use_link;
	pgusesetnextpde(&(pfd->pf_use_link),pde);
}

/*
 * pde_unlink(pde)
 *	Unlink a pde from a pfdat use list, if on one.
 */

pde_unlink(pde)
	pde_t	*pde;
{
	pguselink_t *last_pguse;
	pde_t	*next_pde;
	pfd_t	*pfd;
	dbd_t	*dbd;

	dbd = pdetorealdbd(pde);
	if (dbd->dbd_type != DBD_PGUSE) {
		return;
	};
	pfd = pdetopfdat(pde);
	if (pfd->pf_dbd.dbd_type == DBD_PDE) {
	  	cmn_err(CE_WARN,"pde_unlink:  pfdat is for DBD_PDE");
		goto zap_and_return;
	};
	last_pguse = &(pfd->pf_use_link);
	for (;;) {
		next_pde = pgusetonextpde(last_pguse);
		if (next_pde == NULL) {
			cmn_err(CE_WARN,"pde_unlink:  pde not on pfdat chain");
			goto zap_and_return;
		};
		if (next_pde == pde)
			break;
		last_pguse = pdetopguselink(next_pde);
	};
	*last_pguse = *pdetopguselink(pde);
zap_and_return:
	*pdetorealdbd(pde) = pfd->pf_dbd;
}

/*
 * pde_move(to,from)
 *	Move a pde's contents from "from" to "to".  If the dbd is
 * 	in the pfdat, unlink "from" and link "to" on the pfdat's
 *	use list.
 */

pde_move(to,from)
	pde_t	*to;
	pde_t	*from;
{
	pde_unlink(from);
	*to = *from;
	pde_link(to);
	from->pgi.pg_pde = 0;
	from->pgi.dbd = null_dbd;
}


/*
 * pde_copy(to,from)
 *	Copy a pde's contents from "from" to "to".  If the dbd is
 *	in the pfdat, link "to" on the pfdat's use list.
 */

pde_copy(to,from)
	pde_t	*to;
	pde_t	*from;
{
	*to = *from;
	if (pdetorealdbd(from)->dbd_type == DBD_PGUSE) {
	  	*pdetorealdbd(to) = *pdetodbd(from);
		pde_link(to);
	};
}


/*
 * mlock2(c1,c2)
 *	Lock pfdat entry c1 when c2 is already held.  May release
 *	c2 if c2 is higher-numbered than c1 and this routine must
 *	wait for the lock on c1.  Returns 0 if it did not have to
 *	wait, 1 if it waited without releasing c2, and 2 if it waited
 *	and released c2.
 */

mlock2(c1,c2)
	pfd_t	*c1;
	pfd_t	*c2;
{
	ASSERT(c1 != c2);
	if (MLOCK_NOWAIT(c1))
		return(0);
	if (c1 < c2) {
		MUNLOCK(c2);
		MLOCK(c1);
		MLOCK(c2);
		return(2);
	} else {
		MLOCK(c1);
		return(1);
	};
}


/*
 *	Break association of page to disk
 */

break_pfd_from_disk(pfd,pd)
	register pfd_t *pfd;
	register pde_t *pd;
{
	if (pdetodbd(pd)->dbd_type == DBD_SWAP) {
		release_swap_for_pfd(pfd,pd);
	} else {
		if (pfd->pf_flags & P_HASH)
			premove(pfd);
		*pdetodbd(pd) = null_dbd;
	};
}
