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
 * |         928 Arques Avenue                                 |
 * |         Sunnyvale, CA 94086                               |
 * |-----------------------------------------------------------|
 */
#ident	"$Header: swapalloc.c,v 1.17.1.8 90/05/10 05:55:28 wje Exp $"

/*	Copyright (c) 1984 AT&T	*/
/*	  All Rights Reserved  	*/

/*	THIS IS UNPUBLISHED PROPRIETARY SOURCE CODE OF AT&T	*/
/*	The copyright notice above does not evidence any   	*/
/*	actual or intended publication of such source code.	*/

#include "sys/types.h"
#include "sys/sysmacros.h"
#include "sys/signal.h"
#include "sys/sbd.h"
#include "sys/immu.h"
#include "sys/pcb.h"
#include "sys/user.h"
#include "sys/errno.h"
#include "sys/conf.h"
#include "sys/buf.h"
#include "sys/pfdat.h"
#include "sys/region.h"
#include "sys/proc.h"
#include "sys/getpages.h"
#include "sys/debug.h"
#include "sys/cmn_err.h"
#include "sys/swap.h"
#include "sys/tuneable.h"
#include "sys/file.h"
#include "sys/open.h"
#include "sys/sysinfo.h"
#include "sys/vnode.h"
#include "sys/vfs.h"
#include "sys/kmem.h"
#include "sys/fs/nfs.h"
#include "sys/fs/nfs_rnode.h"
#include "sys/var.h"

extern dbd_t null_dbd;

proc_t		*swapwold;	/* Head of "waiting for swap" list. */
proc_t		*swapwnew;	/* Tail of "waiting for swap" list. */


/*	Allocate swap file space.
 */

swalloc(pglist)
register pglst_t	*pglist;/* Ptr to a list of pointers to page  */
				/* table entries for which swap is to */
				/* be allocated.		      */
{
	register int	smi;
	register use_t	*cntptr;
	register int	swappg;
	register dbd_t	*dbd;
	register proc_t	*pp;
	register pde_t	*pde;
	register pfd_t	*pfd;

	swappg = 0;

	/*	Search all of the swap files, starting with the one
	 *	following the one which we allocated on last, looking
	 *	for a file with enough space to satisfy the current
	 *	request.
	 */

	smi = nextswap;

	/*	There can be holes in the swap file table
	 *	(swaptab) due to deletions.
	 */

	do{
		/*	If the current swaptab entry is not
		 *	in use or is in the process of being
		 *	deleted, go on to the next one.
		 */

		if ((swaptab[smi].st_ucnt == NULL)  ||
		   (swaptab[smi].st_flags & ST_INDEL))
			continue;
		swappg = swapfind(&swaptab[smi], 1);
		if (swappg >= 0)
			goto found;
	} while ((smi = (smi + 1) % MSFILES) != nextswap);

	return(-1);
found:

	/*	Set up for main processing loop.
	*/

	cntptr = &swaptab[smi].st_ucnt[swappg];
	swappg = swaptab[smi].st_swplo + (swappg << DPPSHFT);
	swaptab[smi].st_nfpgs -= 1;
	nextswap = (smi + 1) % MSFILES;


	/*	Initialize the swap use counts for each page
	 *	and set up the disk block descriptors (dbd's).
	 */

	pfd = pglist->pl_pfdat;
	for (pde = pgusetonextpde(&(pfd->pf_use_link)), *cntptr = 0;
	     pde != NULL;
	     pde = pdetonextpde(pde)) {
		if (*cntptr == MAXSUSE) {
			cmn_err(CE_NOTE, "swalloc - swpuse count overflow.\n");
			*cntptr = 0;
			break;
		};
		(*cntptr)++;
	};
	if (*cntptr == 0)
		return (-1);
	pfd->pf_dbd.dbd_type = DBD_SWAP;
	pfd->pf_dbd.dbd_swpi = smi;
	pfd->pf_dbd.dbd_blkno = swappg;
	pfd->pf_flags |= P_SWAP;
	pinsert(NULL,&(pfd->pf_dbd),pfd);
	
	return(swappg);
}


/*	Free one page of swap and return the resulting use count.
 */

swfree1(dbd)
register dbd_t	*dbd;	/* Ptr to disk block descriptor for	*/
			/* block to be removed.			*/
{
	register use_t	*cntptr;
	register int	pgnbr;
	register swpt_t	*st;
	register int	retval;


	st = &swaptab[dbd->dbd_swpi];
	pgnbr = (dbd->dbd_blkno - st->st_swplo) >> DPPSHFT;
	cntptr = &st->st_ucnt[pgnbr];

	ASSERT(*cntptr != 0);

	/*	Decrement the use count for this page.  If it goes
	 *	to zero, then free the page.  If anyone is waiting
	 *	for swap space, wake them up.
	 */

	retval = (*cntptr -= 1);

	if (retval == 0) {
		st->st_nfpgs += 1;

		/*	Wake up the first process waiting for swap
		 *	if we have freed up enough space.  Since we
		 *	are only freeing one page, we cannot
		 *	satisfy more than one process's request.
		 */

		if (swapwold  &&  swapwold->p_mpgneed <= st->st_nfpgs) {
			setrun(swapwold);
			swapwold = swapwold->p_mlink;
		}
	}

	return(retval);
}


/*	Find the use count for a block of swap.
 */

swpuse(dbd)
register dbd_t	*dbd;
{
	register swpt_t	*st;
	register int	pg;

	st = &swaptab[dbd->dbd_swpi];
	pg = (dbd->dbd_blkno - st->st_swplo) >> DPPSHFT;

	return(st->st_ucnt[pg]);
}


/*	Increment the use count for a block of swap.
 */

swpinc(dbd, nm)
register dbd_t	*dbd;
char		*nm;
{
	register swpt_t	*st;
	register int	pg;

	st = &swaptab[dbd->dbd_swpi];
	pg = (dbd->dbd_blkno - st->st_swplo) >> DPPSHFT;

	if (st->st_ucnt[pg] >= MAXSUSE) {
		cmn_err(CE_NOTE, "%s - swpuse count overflow.\n", nm);
		return(0);
	}
	st->st_ucnt[pg]++;
	return(1);
}

/*	Add a new swap file.
 */

swapadd(vp, name, namelen, lowblk, nblks)
struct vnode	*vp;		/* The device code.		*/
char		*name;		/* The device name.		*/
int		namelen;	/* The device name length.	*/
int		lowblk;		/* First block on device to use.*/
int		nblks;		/* Nbr of blocks to use.	*/
{
	register int	smi;
	register swpt_t	*st;
	register int	i;
	register enum vtype vtype;
	int	hiblk;
	extern struct vnodeops nfs_vnodeops;

	/*	Find a free entry in the swap file table.
	 *	Check to see if the new entry duplicates an
	 *	existing entry.  If so, this is an error unless
	 *	the existing entry is being deleted.  In this
	 *	case, just clear the INDEL flag and the swap
	 *	file will be used again.
	 */

	switch (vtype = vp->v_type) {
	    case VBLK: 
		break;

	    case VREG:
		if (vp->v_op != &nfs_vnodeops)
			u.u_error = EINVAL;
		else if (vp->v_vfsp->vfs_flag & VFS_RDONLY) {
			u.u_error = EROFS;
		} else 
			u.u_error = VOP_ACCESS(vp, VREAD|VWRITE, u.u_cred);
		break;

	    case VDIR:
		u.u_error = EISDIR;
		break;

	    default:
		u.u_error = EINVAL;
		break;
	}

	if (nblks < (1<<DPPSHFT))
		u.u_error = EINVAL;

	if (u.u_error)
	    return(-1);

	smi = -1;
	for (hiblk = lowblk+nblks, i = 0  ;  i < MSFILES  ;  i++) {
		st = &swaptab[i];
		if (st->st_ucnt == NULL) {
			if (smi == -1)
				smi = i;
		} else if ((vtype == VREG && st->st_vp == vp) || 
			(vtype == VBLK && st->st_vp->v_rdev == vp->v_rdev)) {
		    /* Check for overlap */
		    if (st->st_swplo == lowblk && 
			    (st->st_flags & ST_INDEL)  &&
			       (st->st_npgs == (nblks >> DPPSHFT))) {

				st->st_flags &= ~ST_INDEL;
				/* avail[rs]mem can be modified at intr level */
				{   register int s;
				    s = splhi();
				    availsmem += st->st_npgs;
				    splx(s);
				}
				return(smi);
		    } else if (st->st_swplo < hiblk && 
			    (st->st_swplo+(st->st_npgs<<DPPSHFT)) > lowblk) {
			u.u_error = EEXIST;
			return(-1);
		    }
		}
	}

	/*	If no free entry is available, give an error
	 *	return.
	 */

	if (smi < 0) {
		u.u_error = ENOSPC;
		return(-1);
	}
	st = &swaptab[smi];

	/*	Open the swap file.
	 */

	u.u_error = 0;
	/* XXX  check error returns? */
	if (vtype == VBLK) {
		(*bdevsw[major(vp->v_rdev)].d_open)(vp->v_rdev, FREAD|FWRITE, OTYP_SWP);
	} else {
		u.u_error = VOP_OPEN(&vp, FREAD|FWRITE, u.u_cred);

		if (!u.u_error && vp->v_op == &nfs_vnodeops)  {
		    /*
		     * Need credentials in the rtvp so do_bio can find them.
		     */
		    crhold(u.u_cred);
		    vtor(vp)->r_cred = u.u_cred;
		}
	}

	if (u.u_error) {
		st->st_ucnt = NULL;
		st->st_flags = 0;
		return(-1);
	}

	/*	Initialize the new entry.
	 */

	st->st_vp = vp;
	st->st_swplo = lowblk;
	st->st_npgs = nblks >> DPPSHFT;
	st->st_nfpgs = st->st_npgs;


	/*
	 *	Allocate space for the name.
	 */

	if (namelen > 0) {
		st->st_name = (char *)kmemzalloc (namelen, M_MISC, M_WAITOK );
		if (st->st_name == NULL)
			goto out;
		bcopy (name, st->st_name, namelen);
	}
	st->st_len = namelen;

	/*	Allocate space for the use count array.  One counter
	 *	for each page of swap.
	 */

	st->st_ucnt = (use_t *)kmemzalloc( sizeof(use_t) * st->st_npgs
					  ,M_MISC, M_WAITOK );
	if (st->st_ucnt == NULL) 
		goto out;
	st->st_next = st->st_ucnt;
	{
	    /* avail[rs]mem can be modified at interrupt level */
	    register int s;
	    s = splhi();
	    availsmem += st->st_npgs;
	    splx(s);
	}

	/*	Clearing the flags allows swalloc to find it
	 */
	st->st_flags = 0;
	while (swapwold) {
		setrun(swapwold);
		swapwold = swapwold->p_mlink;
	}
	VN_HOLD(vp);
	return(smi);

out:
	if (st->st_name) {
		kmemfree(st->st_name, M_MISC, M_WAITOK);
		st->st_name = NULL;
	}
	VOP_CLOSE(vp, FREAD|FWRITE, 1, u.u_cred);
	return(-1);
}


/*	Delete a swap file.
 */

swapdel(vp, lowblk)
register struct vnode *vp;	/* Device to delete.			*/
register int	lowblk;	/* Low block number of area to delete.	*/
{
	register swpt_t	*st;
	register int	smi;
	register int	i;
	register int	ok;

	/*	Find the swap file table entry for the file to
	 *	be deleted.  Also, make sure that we don't
	 *	delete the last swap file.
	 */

	ok = 0;
	smi = -1;
	for (i = 0  ;  i < MSFILES  ;  i++) {
		st = &swaptab[i];
		if (st->st_ucnt == NULL)
			continue;
		if (st->st_swplo == lowblk 
		    && st->st_vp->v_type == vp->v_type
		    && ((vp->v_type == VBLK && st->st_vp->v_rdev == vp->v_rdev)
		        || (vp->v_type == VREG && st->st_vp == vp)))
			smi = i;
		else if ((st->st_flags & ST_INDEL) == 0)
			ok++;
	};
	
	/*	If the file was not found, then give an error
	 *	return.
	 */

	if (smi < 0) {
		u.u_error = EINVAL;
		return(-1);
	}

	/*	If we are trying to delete the last swap file,
	 *	then give an error return.
	 */
	
	if (!ok) {
		u.u_error = ENOMEM;
		return(-1);
	}
	
	st = &swaptab[smi];

	/*	Set the delete flag.  Clean up its pages.
	 *	The file will be removed by swfree1 when
	 *	all of the pages are freed.
	 */

	if (!(st->st_flags & ST_INDEL)) {
		/* avail[rs]mem can be modified at interrupt level */
		register int s;
		s = splhi();
		if (availsmem - st->st_npgs < tune.t_minasmem) {
			splx(s);
			cmn_err(CE_NOTE,
				"swapdel - too few free pages");
			u.u_error = ENOMEM;
			return(-1);
		}
		availsmem -= st->st_npgs;
		splx(s);
		st->st_flags |= ST_INDEL;
	}
	if (st->st_nfpgs < st->st_npgs) {
		getswap(smi);
	}

	if (st->st_nfpgs == st->st_npgs)
		swaprem(st);

	return(smi);
}


/*	Remove a swap file from swaptab.
 *	Called with swaplock set.
 */

swaprem(st)
swpt_t	*st;
{
	register struct vnode *vp;

	/*	Release the space used by the use count array.
	 */

	if (st->st_name) {
		kmemfree(st->st_name, M_MISC, M_WAITOK);
		st->st_name = NULL;
	}
	vp = st->st_vp;
	if (st->st_ucnt)
	    kmemfree(st->st_ucnt,M_MISC,M_WAITOK);

	/*	Mark the swaptab entry as unused.
	 */

	st->st_ucnt = NULL;
	/* XXX  check errors? */
	if (vp->v_type == VBLK) {
		(*bdevsw[major(vp->v_rdev)].d_close)(vp->v_rdev, 1, OTYP_SWP);
	} else
		VOP_CLOSE(vp, FREAD|FWRITE, 1, u.u_cred);
	VN_RELE(st->st_vp);
}

/*	Try to free up swap space on the swap device being deleted.

/*	Try to free up swap space on the swap device being deleted.
 *	Look at every region for pages which are swapped to the
 *	device we want to delete.  Read in these pages and delete
 *	the swap.
 */

getswap(smi)
int	smi;
{
	register reg_t	*rp;
	register pde_t	*pt;
	register dbd_t	*dbd;
	register pde_t	*pglim;
	register int	i;
	unsigned int seg;

	for (rp = region; rp < &region[v.v_region]; rp++) {

		usereg(rp,/* is_locked = */ 0);

		/*	Loop through all the segments of the region.
		*/

		for (i = rp->r_pgoff; i < rp->r_pgoff + rp->r_pgsz; i++) {

			/*	Look at all of the pages of the segment.
			 */

			/* This takes care of shrinking stacks. */
			if (i < rp->r_pgoff) {
				i = rp->r_pgoff - 1;
				continue;
			}

			seg = ctost (i);
			pt = rp->r_list[seg] + i - stoc (seg);
			dbd = pdetodbd (pt);
			if (dbd->dbd_type == DBD_SWAP  &&
			   dbd->dbd_swpi ==  smi) {
				unswap(rp, pt, seg, pt - rp->r_list[seg]);
			}
		}

		unusereg(rp,/* leave_locked = */ 0);
	}
}

/*	Free up the swap block being used by the indicated page.
 *	The region is locked when we are called.
 */

unswap(rp, pt, segno, segindex)
register reg_t	*rp;
register pde_t	*pt;
int	segno;			/* segment number in region */
int	segindex;		/* index of page in segment page table */
{
	register pfd_t	*pfd;
	pfd_t	*tpfd;
	pglst_t		pglist;
	dbd_t	*dbd;
	dbd_t	dbd_save;
	register pde_t	*pde;

	ASSERT(rp->r_flags & RG_LOCK);

	/*	If a copy of the page is in core, then just
	 *	release the copy on swap.
	 */

retry:
	dbd = pdetodbd(pt);
	if (dbd->dbd_type != DBD_SWAP)
		return(0);

	dbd_save = *dbd;
	pfd = pfind(rp,dbd);
	if (pfd) {
release_swap:
		ASSERT((pg_isvalid(pt) ? pfd == pdetopfdat(pt) : 1));

		/* 
		 * If page is in transit, wait for it to finish 
		 */
		if (! MISDONE(pfd)) {
			regrele(rp);
			MWAIT(pfd);
			/*
			 * check if this is still the same page.  
			 */
			reglock(rp);
			if (! (segno < ctos(rp->r_pgsz) &&
			       pt == (rp->r_list[segno] + segindex)))
				return;
			dbd = pdetodbd(pt);
			if (dbd->dbd_type != dbd_save.dbd_type ||
			    dbd->dbd_swpi != dbd_save.dbd_swpi ||
			    dbd->dbd_blkno != dbd_save.dbd_blkno)
				return;
			if (pg_isvalid(pt)) {
				if (pfd != pdetopfdat(pt))
					goto retry;
			} else {
				if (pfd->pf_dbd.dbd_type != dbd_save.dbd_type ||
				    pfd->pf_dbd.dbd_swpi != dbd_save.dbd_swpi ||
				    pfd->pf_dbd.dbd_blkno != dbd_save.dbd_blkno)
					goto retry;
			};
		};
			
		release_swap_for_pfd(pfd,pt);

		return;
	}

	/*
	 *	The page is definitely not in physical memory.
	 *	Try to bring it in.
	 */

	/*	Allocate a page of physical memory for the page.
	 */
	if (! allocpage(rp,ctob(stoc(segno) + segindex)))
		return;
	pfd = pdetopfdat((&u.u_procp->p_tpde));
	ASSERT(! MISDONE(pfd));

	/*
	 *	See if previous conditions still hold
	 */
	if (! (segno < ctos(rp->r_pgsz) &&
	       pt == (rp->r_list[segno] + segindex))) {
		killpage();
		return;
	};
	dbd = pdetodbd(pt);
	if (dbd->dbd_type != dbd_save.dbd_type ||
	    dbd->dbd_swpi != dbd_save.dbd_swpi ||
	    dbd->dbd_blkno != dbd_save.dbd_blkno) {
		killpage();
		return;
	};

	if (pg_isvalid(pt)) {
		killpage();
		goto retry;
	};
	if (tpfd = pfind(rp, dbd)) {
		pfd = tpfd;
		killpage();
		goto release_swap;
	};

	/*
	 *  Insert now, so noone else tries to bring
	 *  in this page
	 */
	pfd->pf_dbd = dbd_save;
	pinsert(rp, dbd, pfd);
					
	/*	Read in the page from swap and then free up the swap.
	 */
	regrele(rp);
	minfo.swap++;
	pglist.pl_pfdat = pfd;
	pglist.pl_pde = pt;
	swap(&pglist, 1, B_READ);
	reglock(rp);
	MDONE(pfd);
	
	/*
	 * Check if we are still interested
	 */
	if (! (segno < ctos(rp->r_pgsz) &&
	       pt == (rp->r_list[segno] + segindex))) {
		killpage();		
		return;
	};

	dbd = pdetodbd(pt);
	if (dbd->dbd_type != dbd_save.dbd_type ||
	    dbd->dbd_swpi != dbd_save.dbd_swpi ||
	    dbd->dbd_blkno != dbd_save.dbd_blkno) {
		killpage();
		return;
	};

	if (pg_isvalid(pt)) {
		killpage();
		goto retry;
	};

	assignpage(rp,pt);
	reg_change_nvalid(rp,1);
	pg_setvalid(pt);
	pg_clrmod(pt);
	/* 
	 * No valid translation can be in the tlb for this page,
	 * so no need to flush even though clearing mod bit.
	 */

	release_swap_for_pfd(pfd,pt);
}

/*	Search swap use counters looking for size contiguous free pages.
 *	Returns the page number found + 1 on sucess, 0 on failure.
 */

swapfind(st, size)
register swpt_t	*st;
register int size;
{
	register use_t *p, *e;
	register int i;
	use_t *b;

	e = &st->st_ucnt[st->st_npgs - size];
	for (p = st->st_next; p <= e; p++) {
		if (*p == 0) {
			b = p;
			p++;
			for (i = 1; i < size; i++, p++)
				if (*p != 0) goto Cont;
			st->st_next = p;
			return(b - st->st_ucnt);
		}
	  Cont:;
	}
	e = st->st_next - size;
	for (p = st->st_ucnt; p <= e; p++) {
		if (*p == 0) {
			b = p;
			p++;
			for (i = 1; i < size; i++, p++)
				if (*p != 0) goto Cont2;
			st->st_next = p;
			return(b - st->st_ucnt);
		}
	  Cont2:;
	}

	st->st_next = st->st_ucnt;
	return(-1);
}


/* 
 * release all references to this page of swap with 
 * respect to this physical page frame
 */

release_swap_for_pfd(pfd,pt)
	register pfd_t *pfd;
	register pde_t *pt;
{
	register pde_t *pde;
	register dbd_t *dbd;

	ASSERT(pfd->pf_dbd.dbd_type == DBD_SWAP);
	for (pde = pgusetonextpde(&pfd->pf_use_link);
	     pde != NULL;
	     pde = pdetonextpde(pde)) {
		ASSERT(pg_isvalid(pde));
		if (swfree1(pdetodbd(pde)) == 0)
			premove(pfd);
	};
	if (pfd->pf_dbd.dbd_type == DBD_SWAP)
		premove(pfd);

	if (pt != NULL)
		if (! pg_isvalid(pt)) {
			dbd = pdetodbd(pt);
			if (dbd->dbd_type == DBD_SWAP) {
				swfree1(dbd);
				*dbd = null_dbd;
			};
		} else {
			ASSERT(pdetopfdat(pt) == pfd);
		};
}
