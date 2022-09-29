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
#ident	"$Header: region.c,v 1.37.1.13.1.7.1.2 90/10/16 10:05:58 beacker Exp $"

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
#include "sys/var.h"
#include "sys/tuneable.h"
#include "sys/pfdat.h"
#include "sys/errno.h"
#include "sys/cmn_err.h"
#include "sys/debug.h"
#include "sys/vnode.h"
#include "sys/kmem.h"
#include "bsd43/sys/vmmeter.h"

/*
 *  The r_list[] grows in increments of N elements, where each element
 *  is a pointer to a page of page table entries (spanning a segment).
 */
#define PTLIST_INCR	8	/* incr r_list by N elements -- power of 2! */
#define listsz_to_incr(x) ( ((x) + PTLIST_INCR - 1) & ~(PTLIST_INCR-1) )

#define INITIAL_USESZ	4	/* initial number of r_use[] array elements */

extern dbd_t null_dbd;

reg_t	nullregion;
preg_t	nullpregion;
int	rlist_lock;
int	flush_2ndtlb;

void
reginit()
{
	register reg_t		*rp;
	extern struct shminfo	shminfo;

	pregpp = MINPREGPP + shmseg() + 2*shlbinfo.shlbs;

	rfree.r_forw = &rfree;
	rfree.r_back = &rfree;

	ractive.r_forw = &ractive;
	ractive.r_back = &ractive;
	ractive_count  = 0;

	for (rp = region; rp < &region[v.v_region]; rp++) {
		rp->r_back = rfree.r_back;
		rp->r_forw = &rfree;
		rfree.r_back->r_forw = rp;
		rfree.r_back = rp;
	}
}

reglock(rp)
register reg_t *rp;
{
	register int s = splhi(); 

	while (rp->r_flags & RG_LOCK) {
		rp->r_flags |= RG_WANTED;
		sleep((caddr_t)rp, PZERO);
	}
	rp->r_flags |= RG_LOCK;
	splx(s);
}

/* lock a region without sleeping */
int					/* 0=failed 1=region locked */
reglock_i(rp)
register reg_t *rp;
{
	register int s = splhi(); 

	if (rp->r_flags & RG_LOCK) {
		splx(s);
		return 0;
	}
	rp->r_flags |= RG_LOCK;
	splx(s);
	return 1;
}

regrele(rp)
register reg_t *rp;
{
	register int s = splhi(); 

	ASSERT(rp->r_flags & RG_LOCK);
	rp->r_flags &= ~RG_LOCK;
	if (rp->r_flags & RG_WANTED) {
		rp->r_flags &= ~RG_WANTED;
		wakeup((caddr_t)rp);
	}
	splx(s);
}

rlstlock()
{
	while (rlist_lock)
		sleep(&rlist_lock, PZERO);
	rlist_lock++;
}

rlstunlock()
{
	ASSERT(rlist_lock);
	rlist_lock = 0;
	wakeup(&rlist_lock);
}

regwait(rp)
register reg_t	*rp;
{
	while (!(rp->r_flags & RG_DONE))
	{	rp->r_flags |= RG_WAITING;
		sleep((caddr_t)&(rp->r_flags), PZERO);
	}
}


/*
 * Allocate a new region.
 * Returns a locked region pointer or NULL on failure
 * The region is linked into the active list.
 */

reg_t *
allocreg(vp, type, flag)
register struct vnode	*vp;
short type;
short flag;
{
	register reg_t *rp;
	struct vattr 	vattr;
	int	error;

	if (vp != NULL) {
		error = VOP_GETATTR(vp, &vattr, u.u_cred);
		if (error != 0) {
			u.u_error = error;
			return(NULL);
		};
	};

	rlstlock();

	if ((rp = rfree.r_forw) == &rfree) {
		rlstunlock();
		cmn_err(CE_WARN, "Region table overflow");
		u.u_error = EAGAIN;
		return(NULL);
	}
	/*
	 * Remove from free list
	 */
	rp->r_back->r_forw = rp->r_forw;
	rp->r_forw->r_back = rp->r_back;

	/* Initialize region fields and bump vnode reference
	 * count.  
	 */

	rp->r_type = type;
	rp->r_vptr = vp;
	rp->nodeid = (vp != NULL ? vattr.va_nodeid : 0);
	rp->r_flags |= flag;
	reglock(rp);

	if (vp != NULL) {
		VN_HOLD(vp);
	}

	/*
	 * Link onto active list
	 */
	rp->r_forw = &ractive;
	rp->r_back = ractive.r_back;
	ractive.r_back->r_forw = rp;
	ractive.r_back = rp;
	ractive_count++;

	rlstunlock();
	return(rp);
}

/*
 * Free an unused region table entry.
 */
void
freereg(rp)
register reg_t *rp;	/* pointer to a locked region */
{
	register struct vnode	*vp;
	register int		i;
	register int		lim;
	register int		size;
	register pde_t		*pt;
	register int		tsize;
	register int		pgoff;
	register int		j;

	ASSERT(rp->r_flags & RG_LOCK);
	ASSERT(rp->r_refcnt == 0);

	/* If the region is still in use, then don't free it. */
	vp = rp->r_vptr;

	if (rp->r_flags & RG_WANTED) {
		regrele(rp);
		return;
	}

	/*
	 * remove from the active list
	 */

	rlstlock();

	rp->r_back->r_forw = rp->r_forw;
	rp->r_forw->r_back = rp->r_back;
	ractive_count--;

	rlstunlock();

	/*
	 * Decrement use count on associated vnode
	 */

	if (vp) {
		VN_RELE(vp);
		rp->r_vptr = NULL;
	}

	/*	Free the memory pages and the page tables and
	 *	disk use maps.  These latter are always allocated
	 *	together in pairs in a contiguous 128 word piece
	 *	of kernel virtual address space.  Note that the
	 *	pfree for the first page table is special because
	 *	pages may not have been allocated from the beginning
	 *	of the segment.  The last page table is also special
	 *	since it may not have been fully allocated.
	 */
	
	pgoff = rp->r_pgoff;
	tsize = (rp->r_pgsz  != 0) ? (rp->r_pgsz + pgoff) : 0;
	lim = ctos(tsize);

	for (i = 0  ;  i < lim  ;  i++) {
		register pfd_t *pfd;

		pt = rp->r_list[i] + pgoff;
		size = tsize - stoc(i);
		if (size > NPGPT)
			size = NPGPT;
		size -= pgoff;
		pgoff = 0;
		memlock();
		for (j = 0; j < size; j++, pt++) {
			if (pg_isvalid(pt)) {
				pfd = pdetopfdat(pt);
				/*
				 * wait for any swapout to complete 
				 */
				if (MISLOCKED(pfd)) {
					memunlock();
					regrele(rp);
					MLOCK(pfd);
					reglock(rp);
					memlock();
					MUNLOCK(pfd);
				};
			}
			pfree(rp, pt, pdetodbd(pt), 1);
		};
		memunlock();

		pt_free(rp->r_list[i]);
		rp->r_list[i] = NULL;
	}

	/* avail[rs]mem can be modified at interrupt level */
	{   register int s;
	    s = splhi();
	    availsmem += rp->r_pgsz;

	    ASSERT(rp->r_noswapcnt >= 0);
	    if (rp->r_noswapcnt)
		availrmem += rp->r_pgsz;
	    splx(s);
	}

	/*
	 * Free the list.
	 */
	
	rp->r_listsz = 0;
	if (rp->r_list) {
		kmemfree(rp->r_list, M_PT_LIST, M_WAITOK);
		rp->r_list = NULL;
	}

	/*
	 * Free the usage list
	 */

	if (rp->r_usesz > 0) {
		rp->r_usesz = 0;
		kmemfree((caddr_t) rp->r_use, M_MISC, M_WAITOK);
	};
	rp->r_use = NULL;		

	/*
	 * clean up region fields.
	 */

	rp->r_flags = 0;
	rp->r_pgsz = 0;
	rp->r_pgoff = 0;
	ASSERT(rp->r_nvalid == 0);
	rp->r_type = 0;
	rp->r_filesz = 0;
	rp->r_noswapcnt = 0;

	/*
	 * Link into free list
	 */
	rlstlock();

	rp->r_forw = rfree.r_forw;
	rp->r_back = &rfree;
	rfree.r_forw->r_back = rp;
	rfree.r_forw = rp;

	rlstunlock();
}

/*
 * Attach a region to a process' address space
 */
preg_t *
attachreg(rp, up, vaddr, type, prot)
register reg_t *rp;	/* pointer to region to be attached */
struct user *up;	/* pointer to u-block (needed by fork) */
caddr_t	vaddr;		/* virtual address to attach at */
int	type;		/* Type to make the pregion. */
int	prot;		/* permissions for segment table entries. */
{
	register preg_t *prp1, *first;
	register preg_t *prp;
	register ureg_t *newur, *ur;
	ureg_t	*oldur;
	int	new_usesz;
	int	i;
	preg_t  tmp;
	int	seg;
	pde_t	**stebase;

	ASSERT(rp->r_flags & RG_LOCK);

	/*	Check attach address.
	 *	It must be segment aligned.
	 */

	 if ((int)vaddr & SOFFMASK) {
		u.u_error = EINVAL;
		return(NULL);
	 }

	/* 
	 *	Allocate a uregion slot
	 */

	if (rp->r_usesz == 0) {
		/* allocate an INITIAL_USIZE-element array */
		newur = (ureg_t *)kmemalloc( INITIAL_USESZ * sizeof(ureg_t)
					    ,M_MISC, M_WAITOK);
		if (newur == NULL) {
			u.u_error = EMFILE;
			return(NULL);
		};
		/* initialize contents first, then update region */
		for (ur = newur; ur < newur + INITIAL_USESZ; ur++)
		  	ur->ur_proc_index = UREGION_PROC_NULL;
		rp->r_use = newur;
		rp->r_usesz = INITIAL_USESZ;	/* now r_use is good */
		ur = newur;
	} else {
		for (ur = rp->r_use;
		     ur < rp->r_use + rp->r_usesz;
		     ur++) {
			if (ur->ur_proc_index == UREGION_PROC_NULL)
				break;
		};
		if (ur >= rp->r_use + rp->r_usesz) {
			/* allocate space for twice as many */
			new_usesz = 2 * rp->r_usesz;
			newur = (ureg_t *)kmemalloc( sizeof(ureg_t) * new_usesz
						    ,M_MISC, M_WAITOK);
			if (newur == NULL) {
				u.u_error = EMFILE;
				return(NULL);
			};
			bcopy((caddr_t) rp->r_use,
			      (caddr_t) newur,
			      sizeof(ureg_t) * rp->r_usesz);
			for (i = rp->r_usesz; i < new_usesz; i++)
				newur[i].ur_proc_index = UREGION_PROC_NULL;
			oldur = rp->r_use;
			rp->r_use = newur;	/* reset r_use before r_usesz */
			ur = newur + rp->r_usesz;
			rp->r_usesz = new_usesz;
			kmemfree((caddr_t) oldur, M_MISC, M_WAITOK);
		};			
	};

	/*	Allocate a pregion.  We should always find a
	 *	free pregion because of the way the system
	 *	is configured.
	 */

	prp = findpreg(up->u_procp, PT_UNUSED);
	if (prp == NULL) {
		u.u_error = EMFILE;
		return(NULL);
	}

	/*	init pregion
	 */

	prp1 = &tmp;
	prp1->p_reg = rp;
	prp1->p_regva = vaddr;
	prp1->p_type = type;
	prp1->p_flags = 0;
	if (prot == (SEG_RO))
		prp1->p_flags |= PF_RDONLY;

	/*	Check that region does not go beyond end of virtual
	 *	address space.
	 */

	 if (chkgrowth(up, prp1, 0, rp->r_pgsz, prp1->p_type != PT_STACK)) {
		u.u_error = EINVAL;
		return(NULL);
	}

	/* Insert tmp in the pregion list
	*/

	first = (up->u_procp)->p_region;
	for (prp1=prp; prp1!=first; --prp1)
		if ((prp1-1)->p_regva > vaddr)
			*prp1 = *(prp1-1);
		else break;
	*prp1 = tmp;

	++rp->r_refcnt;
	up->u_procp->p_size += rp->r_pgsz;
	if (rp->r_type == RT_PRIVATE) {
		up->u_procp->p_rssize += rp->r_nvalid;
	};
	ur->ur_proc_index = up->u_procp - proc;

#ifdef R6000
	/*
	 *	Setup segment table
	 *	When text regions from exec or dup'd regions from fork
	 *	are attached, they already have page tables. Copy
	 *	those to our segment table.
	 */
	stebase = utostbl(up);
	seg = snum(prp1->p_regva);
	for (i = 0; i < rp->r_listsz; i++)
		stebase[seg++] = rp->r_list[i];
#endif

	return(prp1);
}

/*	Detach a region from a process' address space.
**/

void
detachreg(prp, up, invtlb)
register preg_t *prp;
register user_t	*up;
int	invtlb;
{
	register reg_t	*rp;
	register int	i;
	register ureg_t *ur;
	int		seg;
	pde_t		**stebase;

	rp = prp->p_reg;

	ASSERT(rp);
	ASSERT(rp->r_flags & RG_LOCK);

	/*
	 * Invalidate TLB entries pointing at the region
	 */
	if (rp->r_pgsz > 0 && invtlb) {
		int	sdeloc;
		newptes(up->u_procp,pnum(prp->p_regva)+rp->r_pgoff,rp->r_pgsz);
#ifndef	R6000
		sdeloc = (pnum(prp->p_regva)+rp->r_pgoff)<<PTESIZESHFT + 
					KPTEBASE;
		newptes(up->u_procp, pnum(sdeloc),ctos(rp->r_pgoff+rp->r_pgsz));
		setup_wired_tlb(u.u_procp); /* blast the 2nd-level tlbs */
#endif !R6000
	}
	
	/*	Decrement process size by size of region.
	*/

	ASSERT((int)up->u_procp->p_size >= rp->r_pgsz);
	up->u_procp->p_size -= rp->r_pgsz;
	if (rp->r_type == RT_PRIVATE) {
		ASSERT((int)up->u_procp->p_rssize >= rp->r_nvalid);
		up->u_procp->p_rssize -= rp->r_nvalid;
	};

	/*
	 * Remove the reference item from the use chain
	 */
	if (rp->r_usesz > 0) {
		for (ur = rp->r_use;
		     ur < rp->r_use + rp->r_usesz;
		     ur++) 
			if (ur->ur_proc_index == (up->u_procp - proc)) {
				ur->ur_proc_index = UREGION_PROC_NULL;
				break;
			};
	};

#ifdef R6000
	/*
	 *	Blow away this region's page tables
	 *	from this process's segment table.
	 */
	stebase = utostbl(up);
	seg = snum(prp->p_regva);
	for (i = 0; i < rp->r_listsz; i++)
		stebase[seg++] = 0;
#endif
	
	/*
	 *	Clear the proc region we just detached.
	 *	We need to do this before calling freereg, so that
	 *	/proc cannot reach under and try to lock the region
	 *	if we ever go to sleep in freereg.
	 */
	
	for (i = prp - up->u_procp->p_region; i < pregpp-1; i++, prp++) {
		*prp = *(prp+1);
	}
	*prp = nullpregion;

	/*
	 * Decrement use count and free region if zero
	 * and RG_NOFREE is not set, otherwise unlock.
	 */
	if (--rp->r_refcnt == 0 && !(rp->r_flags & RG_NOFREE)) {
		freereg(rp);
	} else {
		regrele(rp);
	}
}

/*
 * Duplicate a region
 */

reg_t *
dupreg(rp, slpflg, force)
register reg_t *rp;
int slpflg, force;
{
	register int	i;
	register int	j;
	register int	size;
	register int	pgsz;		/* # pgs remaining to copy */
	register pde_t	*ppte;
	register pde_t	*cpte;
	register pde_t	**plp;
	register pde_t	**clp;
	register reg_t	*rp2;
	register int	pgoff;
	ureg_t		*ur;
	pfd_t		*pfdc;
	int		kmem_flag;
	register int s;

	ASSERT(rp->r_flags & RG_LOCK);

	/* If region is shared, and we're not forcing a duplicate,
	 * there is no work to do.
	 * Just return the passed region.  The region reference
	 * counts are incremented by attachreg
	 */

	if (rp->r_type != RT_PRIVATE && force == 0)
		return(rp);
	
	/*	Make sure we have enough space to duplicate
	**	this region without potential deadlock.
	*/

	/* avail[rs]mem can be modified at interrupt level */
	s = splhi();
	if (availsmem - rp->r_pgsz < tune.t_minasmem) {
		splx(s);
		nomemmsg("dupreg", rp->r_pgsz, 0, 0);
		u.u_error = EAGAIN;
		return(NULL);
	}
	availsmem -= rp->r_pgsz;
	splx(s);

	kmem_flag = (slpflg & NOSLEEP) ? M_NOWAIT : M_WAITOK;

	/*
	 * Need to copy the region.
	 * Allocate a region descriptor
	 */

	if ((rp2 = allocreg(NULL, force ? RT_PRIVATE : rp->r_type,
		0)) == NULL) {
		/* avail[rs]mem can be modified at interrupt level */
		s = splhi();
		availsmem += rp->r_pgsz;
		splx(s);
		u.u_error = EAGAIN;
		return(NULL);
	}
	
	/*
	 * Mark the new region unused
	 */
	rp2->r_usesz = 0;
	rp2->r_use = NULL;

	/*	Allocate a list for the new region.
	 */

	rp2->r_listsz = rp->r_listsz;
	rp2->r_list = (pde_t **)kmemzalloc( (u_int)listsz_to_incr(rp2->r_listsz)
						 * sizeof(char *)
					   ,M_PT_LIST, kmem_flag );
	rp2->r_flags = rp->r_flags;
	if (rp2->r_list == NULL) {
		rp2->r_listsz = 0;
		freereg(rp2);
		/* avail[rs]mem can be modified at interrupt level */
		s = splhi();
		availsmem += rp->r_pgsz;
		splx(s);
		u.u_error = EAGAIN;
		return(NULL);
	}


	/*
	 * Copy pertinent data to new region
	 */

	rp2->r_pgsz = rp->r_pgsz;
	pgoff = rp2->r_pgoff = rp->r_pgoff;
	rp2->r_filesz = rp->r_filesz;
	if (rp->r_vptr) {
		rp2->r_vptr = rp->r_vptr;
		VN_HOLD(rp2->r_vptr);
		rp2->nodeid = rp->nodeid;
	};

	/* Scan the parents page table  list and fix up each page table.
	 * Allocate a page table and map table for the child and
	 * copy it from the parent.
	 */

	pgsz = rp->r_pgsz;
	for (i = 0  ;  i < ctos(rp->r_pgsz + rp->r_pgoff)  ;  i++) {
		plp = &rp->r_list[i];
		clp = &rp2->r_list[i];

		/* Allocate a page descriptor (table) and
		 * map table for the child.
		 */

		if ((cpte = (pde_t *)pt_alloc(rp2, slpflg)) == 0) {
			rp2->r_pgsz = (i == 0) ? 0 : (stoc(i) - rp2->r_pgoff);
			/* avail[rs]mem can be modified at interrupt level */
			s = splhi();
			availsmem += rp->r_pgsz - rp2->r_pgsz;
			splx(s);
			freereg(rp2);
			u.u_error = EAGAIN;
			return(NULL);
		}

		/* Set pointer to the newly allocated page descriptor (table)
		 * and dbd into the child's list.  Then get a
		 * pointer to the parents page descriptor (table) and dbd.
		 */

		*clp = cpte;
		ppte = *plp;

		/* Move page table pointers up to offset.  Note that this
		 * only happens in first loop.  pgoff will be reset below.
		 */

		ppte += pgoff;
		cpte += pgoff;

		/* Get the total number of unmapped pages remaining.
		 * This is the total size of the region minus the
		 * number of segments for which we have allocated
		 * page tables already.
		 */


		if (pgsz <= NPGPT)
			size = pgsz;
		else {
		/* If the remaining pages to copy is greater than a segment,
		 * then we will only process a segment.  Take care of
		 * the case when there is a pgoff.
		 */
			size = NPGPT - pgoff;
			pgsz -= size;
		}

		/* Check each parents page and then copy it to
		 * the childs pte.  Also check the map table
		 * entries.
		 */

		for (j = 0, pgoff = 0 ; j < size  ;  j++, ppte++, cpte++) {
			dbd_t	map;

			map = *pdetodbd(ppte);

			/* Set copy-on-write bit
		 	 */
			if (map.dbd_type != DBD_MMAP) {
				pg_setcw(ppte);
			}

			/*	Copy parents page to child.
			 */

			pde_copy(cpte,ppte);

			/*	If the page is in core, then
			 *	increment the page use count.
			 */

			if (pg_isvalid(ppte)) {
				if (map.dbd_type != DBD_MMAP) {
					pfdc = pdetopfdat(ppte);
					ASSERT(pfdc->pf_use > 0); 
					pfdc->pf_use++;
				};
				reg_change_nvalid(rp2,1);
			}

			/* Increment the swap use count for pages which
			 * are on swap.
			 */

			if (map.dbd_type == DBD_SWAP) {
				ASSERT( !pg_ismod(ppte) );
				ASSERT(swpuse(&map) != 0);
				if (!swpinc(&map, "dupreg")) {

					/* The swap use count overflowed.
					 * Free the region and return
					 * an error.
					 */

					if (pg_isvalid(cpte)) {
						pfdc = pdetopfdat(cpte);
						pg_clrvalid(cpte);
						pg_clrpfn(cpte);
						reg_change_nvalid(rp2,-1);
						pfree_pfd(pfdc,
							  &(pfdc->pf_dbd),
							  PF_EITHER);
					};
					*pdetodbd(cpte) = null_dbd;
					rp2->r_pgsz = ((i == 0) ? 0 :(stoc(i) - rp2->r_pgoff))
					  		+ j + 1;
					/* avail[rs]mem can be modified at interrupt level */
					s = splhi();
					availsmem += rp->r_pgsz - rp2->r_pgsz;
					splx(s);
					freereg(rp2);
					u.u_error = EAGAIN;
					return(NULL);
				}
			} else if (map.dbd_type == DBD_MMAP) {
				pde_unlink(cpte);
				pdetodbd(cpte)->dbd_type = DBD_DZERO;
				pdetodbd(cpte)->dbd_swpi = 0;
				pdetodbd(cpte)->dbd_blkno = BLKNULL;
				cpte->pgi.pg_pde = 0;
			} 
		}
	}

	return(rp2);
}

/*
 * Change the size of a region
 *  change == 0  -> no-op
 *  change  < 0  -> shrink
 *  change  > 0  -> expand
 * For expansion, you get demand zero pages
 * For shrink, the caller must flush the ATB
 * Returns 0 on no-op, -1 on failure, and 1 on success.
 */

growreg(prp, change, type)
register preg_t *prp;
{
	register pde_t	*pt;
	register int	i;
	register reg_t	*rp;
	register int	size;
	register int	osize;
	pde_t		pte;
	int		lotohi;		/* for regions that grow down (stack) */
	int		start, end;
	int		npgoff;
	int		j;
	register pfd_t	*pfd;
	register int s;

	rp = prp->p_reg;
	ASSERT(rp->r_flags & RG_LOCK);

	if (change == 0)
		return(0);
	
	lotohi = prp->p_type != PT_STACK;

	osize = rp->r_pgsz + rp->r_pgoff;

	ASSERT(rp->r_noswapcnt >=0);

	if (change < 0) {

		/*	The region is being shrunk.  Compute the new
		 *	size and free up the unneeded space.
		 */

		/* avail[rs]mem can be modified at interrupt level */
		s = splhi();
		availsmem -= change;	/* change < 0.	*/

		if (rp->r_noswapcnt)
			availrmem -= change;
		splx(s);

		if (lotohi) {
			start = osize + change;
			end = osize;
			npgoff = rp->r_pgoff;
		} else {
			start = rp->r_pgoff;
			end = rp->r_pgoff - change;
			npgoff = end % NPGPT;
		}

		i = ctost(start);

		for (;  i < ctos(end)  ;  i++) {
			/*	Free up the allocated pages for
			 *	this segment.  Need to check if the
			 *	freed area crosses a segment boundary. 
			 */

			pt = rp->r_list[i];
			size = end - stoc(i);
			if (size > NPGPT)
				size = NPGPT;
			if (start > stoc(i)) {
				size -= start - stoc(i);
				pt += start - stoc(i);
			}
			memlock();
			for (j = 0; j < size; j++, pt++) {
				if (pg_isvalid(pt)) {
					pfd = pdetopfdat(pt);
					/*
					 * wait for any swapout to complete 
					 */
					if (MISLOCKED(pfd)) {
						memunlock();
						regrele(rp);
						MLOCK(pfd);
						reglock(rp);
						memlock();
						MUNLOCK(pfd);
					};
				};
				pfree(rp, pt, pdetodbd(pt), 1);
			};
			memunlock();
		}

		/*	Free up the page tables which we no
		 *	longer need.
		 */

		(void) ptexpand(prp, rp->r_pgsz + change, npgoff, lotohi);
	} else {
		/*	We are expanding the region.  Make sure that
		 *	the new size is legal and then allocate new
		 *	page tables if necessary.
		 */

		/* avail[rs]mem can be modified at interrupt level */
		s = splhi();
		if (availsmem - change < tune.t_minasmem) {
			splx(s);
			nomemmsg("growreg", change, 0, 0);
			u.u_error = EAGAIN;
			return(-1);
		}
		if (rp->r_noswapcnt) {
		    if (availrmem - change < tune.t_minarmem) {
			splx(s);
			nomemmsg("growreg", change, 0, 1);
			u.u_error = EAGAIN;
			return(-1);
		    } 
		    availrmem -= change;
		}
		availsmem -= change;
		splx(s);

		if (lotohi) {
			end = osize + change;
			start = osize;
			npgoff = rp->r_pgoff;
		} else {
			end = rp->r_pgoff;
			if (end < change)
				end += roundup(change - end, NPGPT);
			start = end - change;
			npgoff = start;
		}
		if (chkgrowth(&u, prp, osize, change, lotohi) ||
		   ptexpand(prp, rp->r_pgsz + change, npgoff, lotohi)) {
			/* avail[rs]mem can be modified at interrupt level */
			s = splhi();
			availsmem += change;
			if (rp->r_noswapcnt)
			    availrmem += change;
			splx(s);
			u.u_error = ENOMEM;
			return(-1);
		}

		/*	Initialize the new page tables and allocate
		 *	pages if required.
		 */

		pte.pgi.pg_pde = 0;
		/* XXX protection used to be set here */

		i = ctost(start);
		for (; i < ctos(end) ; i++) {
			pt = rp->r_list[i];
			size = end - stoc(i);
			if (size > NPGPT)
				size = NPGPT;
			if (start > stoc(i)) {
				size -= start - stoc(i);
				pt += start - stoc(i);
			}

			while (--size >= 0) {
				pt->pgi.pg_pde = pte.pgi.pg_pde;
				pdetodbd(pt)->dbd_type = type;
				pt++;
			}
		}
		switch (type) {
		case DBD_DFILL:
		case DBD_FILE:
		case DBD_LSTFILE:
			cnt.v_nexfod += change;
			break;
		case DBD_DZERO:
			cnt.v_nzfod += change;
			break;
		default:
			break;
		};
	}

	rp->r_pgsz += change;
	rp->r_pgoff = npgoff;
	if (change < 0) {
		ASSERT((int)up->u_procp->p_size >= -change);
	}
	u.u_procp->p_size += change;
	return(1);
}

/*
 * Check that grow of a pregion is legal
 * returns 0 if legal, -1 if illegal.
 * This code assumes only one hitolo (ie stack) region and that there is
 * one lotohi region below it.  Also note that the p_regva for a stack region
 * is the lowest address that the stack can grow to in it's currently lowest
 * segment.
 */

chkgrowth(up, prp, osize, change, lotohi)
register user_t	*up;
register preg_t	*prp;
register int	osize;	/* Old size in pages. */
register int	change;	/* size change in pages. */
register int	lotohi;	/* direction of growth */
{
	register size;
	register preg_t *prp1;
	register caddr_t start, end, vaddr;

	if (osize!= 0) { /* prp is in pregion list */
		if (change <= 0)
			return(0);
		if (lotohi) {
			prp1= prp + 1;
			if (prp1->p_reg != NULL) {
				if (prp->p_regva + ctob(osize + change) - 1 >=
						prp1->p_regva)
					return(-1);
			}
		} else {
			prp1 = prp - 1;
			if( prp1->p_reg != NULL ) {
				if(prp->p_regva +
					ctob(prp->p_reg->r_pgoff - change) <=
					prp1->p_regva +
					ctob(prp1->p_reg->r_pgoff + 
						prp1->p_reg->r_pgsz) -1)
					return(-1);
			}
		}
		return(0);
	}

	/* prp is a new region */

	prp1 = (up->u_procp)->p_region;
	start = prp->p_regva;
	size = ctob((prp->p_reg)->r_pgsz);
	end = (size>0)? start+size-1:start;

	for (;prp1->p_reg; ++prp1) {
		if (prp1==prp) continue;
		vaddr = prp1->p_regva;
		if (end<vaddr)
			break;
		if (start == vaddr || (start< vaddr && end>=vaddr))
			return(-1);
		if (start>vaddr) {
			size = ctob((prp1->p_reg)->r_pgsz);
			if (size != 0 && start<vaddr+size)
				return(-1);
		}
	}

	return(0);
}


/*
 * Expand user page tables for a region 
 * If a stack region grows to require a new segment, prp->p_regva is 
 * changed so region is attached at a lower region.
 */
 
ptexpand(prp, npgsz, npgoff, lotohi)
register preg_t *prp;
{
	register reg_t *rp = prp->p_reg;
	register pde_t	**lp;
	register int	oldpts;
	register int	newpts;
	register int	i;
	register int	ptdiff;
	register int	nlist_incr;
	register int	nlist_incr_bytes;
	register pde_t	**lp2;
	pde_t		**tmppts, *space[5];
	int		alloc_tmp;
	int		seg;
	pde_t		**stebase;

	/* Calculate the new size in pages.
	 */

#ifdef R6000
	stebase = utostbl(&u);
#endif
	oldpts = rp->r_listsz;
	newpts = ctos(npgoff + npgsz);
	nlist_incr = listsz_to_incr(newpts);
	nlist_incr_bytes = (u_int)nlist_incr * sizeof(char *);

	/*	If we are shrinking the region, then free up
	 *	the page tables and map tables.  Use a smaller
	 *	list if possible.
	 */

	if (newpts < oldpts) {
		/* stack segments don't shrink */
		ASSERT(lotohi);
		lp = &rp->r_list[newpts];
		seg = snum(prp->p_regva) + newpts;
		for (; lp < &rp->r_list[oldpts]; lp++) {
			pt_free(*lp);
			*lp = 0;
#ifdef R6000
			stebase[seg++] = 0;
#endif
		}
		if (nlist_incr < listsz_to_incr(oldpts)) {
			if (newpts > 0) {
				lp2 = (pde_t **)kmemzalloc( nlist_incr_bytes
							   ,M_PT_LIST
							   ,M_WAITOK );
				if (lp2 == NULL) {
					/* we are shrinking and can't get space
					 * for a smaller list; just keep the
					 * bigger one.  Return OK
					 */
					rp->r_listsz = newpts;
					return(0);
				}
				bcopy(rp->r_list, lp2, nlist_incr_bytes);
			}else{
				lp2 = 0;
			}
			kmemfree( rp->r_list, M_PT_LIST, M_WAITOK );
			rp->r_list = lp2;
		}
		rp->r_listsz = newpts;
#ifndef	R6000
		{ 
		    int	sdeloc;

		    sdeloc = (pnum(prp->p_regva)+npgoff+npgsz)
					<<PTESIZESHFT + KPTEBASE;
		    newptes(up->u_procp, pnum(sdeloc), oldpts - newpts);
		}
		setup_wired_tlb(u.u_procp); /* blast the 2nd-level tlbs */
#endif !R6000
	}
		
	/*	If the region shrunk or didn't grow by enough to
	 *	require any more page tables, then we are done.
	 */

	if (oldpts == newpts) {
		return(0);
	}
	
	/*	If the region grew into the next segment,  then we 
	 *	must allocate one or more new page and map tables.
	 *	See if we have enough space in the list for the
	 *	new segments.  If not, allocate a new list and
	 *	copy over the old data.
	 */
	
	if (nlist_incr > listsz_to_incr(oldpts)) {
		lp2 = (pde_t **)kmemzalloc( nlist_incr_bytes
					   ,M_PT_LIST, M_WAITOK );
		if (lp2 == NULL)
			return(-1);
		if (rp->r_list) {
			bcopy(rp->r_list, lp2, oldpts * sizeof(pde_t *));
			kmemfree(rp->r_list, M_PT_LIST, M_WAITOK);
		}
		rp->r_list = lp2;
	}

	/*
	 *	Allocate the new page tables before moving things
	 *	around so we can back out easier if we fail.
	 */
	ptdiff = newpts - oldpts;
	if (ptdiff > 5) {
		tmppts = (pde_t **)kmemzalloc(ptdiff * sizeof(pde_t *),
				    M_PT_LIST, M_WAITOK);
		if (tmppts == NULL)
			return (-1);
		alloc_tmp = 1;
	} else {
		tmppts = space;
		alloc_tmp = 0;
	}
	lp = tmppts;
	for (i = ptdiff; i > 0; lp++, i--) {
		*lp = (pde_t *)pt_alloc(rp, 0);
		if (*lp == NULL) {
			while (--lp >= tmppts)
				pt_free(*lp);
			if (alloc_tmp)
				kmemfree(tmppts, M_PT_LIST, M_WAITOK);
			return (-1);
		}
	}

	if(lotohi) {
		lp = &rp->r_list[oldpts];
		lp2 = &rp->r_list[newpts];
		seg = snum(prp->p_regva) + oldpts;
	} else {
		if(oldpts != 0) {
			/* If not creating a new stack segment: 
			 * Move pointer list up to make room for new stack 
			 * segment(s).
			 * Adjust prp->p_regva to reflect new segment(s).
			 */
			i = oldpts;
			lp = &rp->r_list[oldpts];
			lp2 = &rp->r_list[newpts];
			while(i-- > 0) {
				*(--lp2) = *(--lp);
				*lp = NULL; /* clean up behind us */
			};
			prp->p_regva -= stob(ptdiff);
		}
		lp = &rp->r_list[0];
		lp2 = &rp->r_list[ptdiff];
		seg = snum(prp->p_regva);
	}

	for (i = 0; lp < lp2; lp++, i++) {
		ASSERT(*lp==0);
		*lp = tmppts[i];
#ifdef R6000
		stebase[seg++] = *lp;
#endif
	}
	if (alloc_tmp)
		kmemfree(tmppts, M_PT_LIST, M_WAITOK);
	rp->r_listsz = newpts;

	return(0);
}

loadreg(prp, vaddr, vp, off, count)
register preg_t		*prp;
caddr_t			vaddr;
register struct vnode	*vp;
{
	register reg_t	*rp;
	register int	gap;
	int		resid;

	/*	Grow the region to the proper size to load the file.
	 */

	rp = prp->p_reg;
	ASSERT(rp->r_flags & RG_LOCK);
	gap = vaddr - prp->p_regva;

	if (growreg(prp, btoct(gap), DBD_NONE) < 0) 
		return(-1);
	if (growreg(prp, btoc(count+gap) - btoct(gap), DBD_DZERO) < 0) 
		return(-1);

	/*	Set up to do the I/O.
	 */


	/*	We must unlock the region here because we are going
	 *	to fault in the pages as we read them.  No one else
	 *	will try to use the region before we finish because
	 *	the RG_DONE flag is not set yet.
	 */

	regrele(rp);

	u.u_error =
	    vn_rdwr(UIO_READ, vp, (caddr_t)vaddr, count, off,
		    UIO_USERSPACE, IO_UNIT, &resid);

	if (u.u_error) {
		reglock(rp);
		return(-1);
	}

	/*	Clear the last (unused)  part of the last page.
	 */
	
	vaddr += count;
	count = ctob(1) - poff(vaddr);
	if (count > 0  &&  count < ctob(1))
		bzeroba(vaddr, count);

	reglock(rp);
	rp->r_flags |= RG_DONE;
	if (rp->r_flags & RG_WAITING) {
		rp->r_flags &= ~RG_WAITING;
		wakeup((caddr_t) &(rp->r_flags));
	}
	if (resid)
		return(-1);
	return(0);
}

mapreg(prp, vaddr, vp, off, count)
preg_t	*prp;
caddr_t		vaddr;
struct vnode	*vp;
int		off;
register int	count;
{
	register int	i;
	register int	j;
	register int	blkspp;
	register reg_t	*rp;
	int		gap;
	int		seglim;
	dbd_t		*dbd;
	register pde_t	*pt;
	struct vattr 	vattr;

	/* Get region pointer and effective device number */
	rp = prp->p_reg;
	ASSERT(rp->r_flags & RG_LOCK);

	/*	Make sure that we are not trying to map
	 *	beyond the end of the file.  This can
	 *	happen for a bad a.out where the header
	 *	lies and says the file is bigger than
	 *	it actually is.
	 */

	ASSERT(vp != NULL && rp->r_vptr == vp);
	if ((VOP_GETATTR(vp, &vattr, u.u_cred) != 0)
	    || (off + count > vattr.va_size))
		return(-1);

	/*
	 * The intent here is to just do the mapping during the call to 
	 * READMAP.  This flag is required for more efficient FREEMAP
	 * calls, and for some error cases.
	 */
	vp->v_flag |= VHASMAP;

	/*	Compute the number of file system blocks in a page.
	 *	This depends on the file system block size.
	 */

	blkspp = NBPP/NBPSCTR;

	/*	Allocate invalid pages for the gap at the start of
	 *	the region and demand-fill pages for the actual
	 *	text.
	 */
	
	gap = vaddr - prp->p_regva;
	if (growreg(prp, btoct(gap), DBD_NONE) < 0)
		return(-1);
	if (growreg(prp, btoc(count+gap) - btoct(gap), DBD_DFILL) < 0)
		return(-1);
	
	ASSERT(rp->nodeid == vattr.va_nodeid);
	rp->r_filesz = count + off;
	
	/*	Build block list pointing to map table.
	 */

	gap = btoct(gap);  /* Gap in pages. */
	off = off >>SCTRSHFT;  /* File offset in blocks. */
	i = ctost(gap);
	seglim = ctos(rp->r_pgsz);

	for (;  i < seglim  ;  i++) {
		register int	lim;

		if (gap > stoc(i))
			j = gap - stoc(i);
		else
			j = 0;

		lim = rp->r_pgsz - stoc(i);
		if (lim > NPGPT)
			lim = NPGPT;

		pt = (pde_t *)rp->r_list[i] + j;

		for (;  j < lim  ;  j++, pt++) {
			dbd = pdetodbd(pt);

			/*	If these are private pages, then make
			 *	them copy-on-write since they will
			 *	be put in the hash table.
			 */

			if (rp->r_type == RT_PRIVATE) {
				pg_setcw(pt);
			}
			dbd->dbd_type  = DBD_FILE;
			dbd->dbd_blkno = off;
			off += blkspp;
		}
	}

	/*	Mark the last page for special handling
	 */
	
	(*pdetodbd(&pt[-1])).dbd_type = DBD_LSTFILE;

	rp->r_flags |= RG_DONE;
	if (rp->r_flags & RG_WAITING)
	{	rp->r_flags &= ~RG_WAITING;
		wakeup(&rp->r_flags);
	}
	return(0);

}

/*	Find the pregion corresponding to a virtual address.
 */

preg_t	*
findreg(p, vaddr)
register struct proc	*p;
register caddr_t	vaddr;
{
	register preg_t	*prp;
	register preg_t	*oprp;

	oprp = p->p_region;
	for (prp = &p->p_region[1] ; prp->p_reg ; prp++)
		if (vaddr >= prp->p_regva  &&
		   prp->p_regva > oprp->p_regva)
			oprp = prp;
	if (oprp->p_reg  &&
	   vaddr >= oprp->p_regva + ctob(oprp->p_reg->r_pgoff) && 
	   vaddr < (oprp->p_regva +
	   ctob(oprp->p_reg->r_pgsz + oprp->p_reg->r_pgoff))) {
		reglock(oprp->p_reg);
		return(oprp);
	}
	return(NULL);
}

/*	Find the pregion of a particular type.
 */

preg_t *
findpreg(pp, type)
register proc_t	*pp;
register int	type;
{
	register preg_t	*prp;

	for (prp = pp->p_region ; prp->p_reg ; prp++) {
		if (prp->p_type == type)
			return(prp);
	}

	/*	We stopped on an unused region.  If this is what
	 *	was called for, then return it unless it is the
	 *	last region for the process.  We leave the last
	 *	region unused as a marker.
	 */

	if ((type == PT_UNUSED)  &&  (prp < &pp->p_region[pregpp - 1]))
		return(prp);
	return(NULL);
}

/* Load region's ptes with protection specified by region flags */
void
chgpteprot(prp)
preg_t	*prp;
{
	register int	i, j;
	register int	size;
	register pde_t	**lp;
	register pde_t	*pte;
	register int	pgoff;
	register reg_t	*rp;

	rp = prp->p_reg;
	ASSERT(rp->r_flags & RG_LOCK);

	/*
	 * If their are no valid ptes in this region then don't go through
	 * the motions.
	 */
	if (rp->r_nvalid == 0)
		return;

	/* Go through each of the r_list sections */
	for (i = 0; i < ctos(rp->r_pgsz); i++) {
		lp = &rp->r_list[i];
		pte = *lp;
		pgoff = rp->r_pgoff;
		/* Get the total number of unmapped pages remaining.
		 * This is the total size of the region minus the
		 * number of segments for which we have allocated
		 * page tables already.
		 */
		size = rp->r_pgsz - stoc(i);

		/* If this size is greater than a segment, then
		 * we will only process a segment.  Take care of
		 * the case when there is a pgoff.
		 */
		if (size > NPGPT)
			size = NPGPT - pgoff;

		/* Go through the individual pte entries */
		for (j = 0; j < size  ;  j++, pte++) {
			if (prp->p_flags & PF_RDONLY) {
				pg_clrmod(pte);
			}
			else {
				pg_setmod(pte);
			}
		}
	}
	new_tlbpid(u.u_procp);
}

/* Locate process region for a given virtual address. */
preg_t *
vtopreg(p, vaddr)
register struct proc *p;
register caddr_t vaddr;
{
	register preg_t *prp;
	register reg_t *rp;

	for (prp = p->p_region; rp = prp->p_reg; prp++) {
		caddr_t lo = prp->p_regva + ctob(rp->r_pgoff);
		caddr_t hi = lo + ctob(rp->r_pgsz);

		if ((unsigned long)vaddr >= (unsigned long)lo
		  && (unsigned long)vaddr < (unsigned long)hi)
			return(prp);
	}
	return(NULL);
}

/* Locate process region for a given virtual address, if any,  or next
 * possible virtual address following the specified one.
 */
preg_t *
vtonextpreg(p, vaddr)
register struct proc *p;
register caddr_t vaddr;
{
	register preg_t *prp, *nextprp;
	register reg_t *rp;
	caddr_t	nextvaddr;

	nextvaddr = (caddr_t)0xffffffff;
	nextprp = NULL;
	for (prp = p->p_region; rp = prp->p_reg; prp++) {
		caddr_t lo = prp->p_regva + ctob(rp->r_pgoff);

		if ((unsigned long)vaddr < (unsigned long)lo
		  && (unsigned long)lo < (unsigned long)nextvaddr) {
			nextvaddr = lo;
			nextprp = prp;
		}
	}
	return(nextprp);
}

/* 
 * Increment the usage count for region, to prevent deallocation 
 * while it is unlocked, and leave the region locked.
 * The region is assumed to be locked when
 * this routine is called, if the is_locked argument is true.
 */

usereg(rp,is_locked)
	register reg_t *rp;
	int	is_locked;
{
	if (! is_locked)
		reglock(rp);
	ASSERT(rp->r_flags & RG_LOCK);
	rp->r_refcnt++;
	ASSERT(rp->r_refcnt > 0);
}

/* 
 * Decrement the usage count for region, and unlock the region.
 * The region is assumed to be locked when
 * this routine is called.
 * This routine may free the region if appropriate.
 * This routine leaves the region locked, if leave_locked is set,
 * unless the reference count goes to zero, in which case the 
 * region is freed.  This routine returns 0 if the region is returned
 * locked, and 1 if it is freed.
 */

unusereg(rp,leave_locked)
	register reg_t *rp;
{
	ASSERT(rp->r_flags & RG_LOCK);
	ASSERT(rp->r_refcnt > 0);
	rp->r_refcnt--;
	if (rp->r_refcnt == 0 && !(rp->r_flags & RG_NOFREE)) {
		freereg(rp);
		return(1);
	} else {
		if (! leave_locked)
			regrele(rp);
		return(0);
	}
}


/*
 *	Change region valid count, and adjust p_rssize where needed
 */

reg_change_nvalid(rp,npages)
	reg_t	*rp;
	int	npages;
{
	register ureg_t *ur;

	if (rp == NULL)
		return;
	rp->r_nvalid += npages;
	if (rp->r_usesz == 0 ||
	    rp->r_type != RT_PRIVATE)
		return;
	for (ur = rp->r_use; ur < rp->r_use + rp->r_usesz; ur++)
		if (ur->ur_proc_index != UREGION_PROC_NULL) {
			if (npages < 0) {
			    ASSERT((int)proc[ur->ur_proc_index].p_rssize >= -npages);
			}
			proc[ur->ur_proc_index].p_rssize += npages;
		};
}

