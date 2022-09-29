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
#ident	"$Header: text.c,v 1.18.1.4.1.3.1.2 90/10/16 12:17:51 beacker Exp $"

/*	Copyright (c) 1984 AT&T	*/
/*	  All Rights Reserved  	*/

/*	THIS IS UNPUBLISHED PROPRIETARY SOURCE CODE OF AT&T	*/
/*	The copyright notice above does not evidence any   	*/
/*	actual or intended publication of such source code.	*/

#include "sys/types.h"
#include "sys/param.h"
#include "sys/sbd.h"
#include "sys/immu.h"
#include "sys/systm.h"
#include "sys/sysinfo.h"
#include "sys/signal.h"
#include "sys/pcb.h"
#include "sys/user.h"
#include "sys/errno.h"
#include "sys/vnode.h"
#include "sys/pfdat.h"
#include "sys/region.h"
#include "sys/proc.h"
#include "sys/debug.h"
#include "sys/cmn_err.h"

/*	Allocate text region for a process
 */
struct vnode *
xalloc(exec_data, tpregtyp)
register struct exdata *exec_data;
{
	register struct vnode   *vp = exec_data->vp;
	register reg_t		*rp;
	register preg_t		*prp;
	register int		size = exec_data->ux_tsize;
	register caddr_t	org = (caddr_t)(exec_data->ux_toffset);
	register int		base = exec_data->ux_txtorg;
	register int		regva = base & ~SOFFMASK;
#ifdef RFS
	extern short dufstyp;
#endif

	if (size == 0)
		return(vp);

#ifdef RFS
	/* kind a KLUDGY but reduces number of DU hooks
	 * if the ip if remote overwrite magic number to 410
	 */
	if (ip->i_fstyp == dufstyp) 
		exec_data->ux_mag = 0410;
#endif

	/*	Search the region table for the text we are
	 *	looking for.
	 */

loop:
	rlstlock();

	for (rp = ractive.r_forw ; rp != &ractive ; rp = rp->r_forw) {
		if (rp->r_type == RT_STEXT  &&
		   rp->r_vptr == vp  &&
		   (rp->r_flags & RG_NOSHARE) == 0) {
			rlstunlock();
			reglock(rp);
			if (rp->r_type != RT_STEXT || rp->r_vptr != vp) {
				regrele(rp);
				goto loop;
			}
			/*	Artificially bump the reference count
			 *	to make sure the region doesn't go away
			 *	while we are waiting.
			 *	Since we have the inode locked, we should
			 *	never get into xalloc with RG_DONE not set.
			 */
			if ((rp->r_flags & RG_DONE) == 0) {
				rp->r_refcnt++;
				rp->r_waitcnt++;
				regrele(rp);
				regwait(rp);
				reglock(rp);
				rp->r_refcnt--;
			}
			prp = attachreg(rp, &u, regva, tpregtyp, SEG_RO);
			regrele(rp);
			if (prp == NULL) {
				VN_RELE(vp);
				return(NULL);
			}
			return(vp);
		}
	}
	rlstunlock();
	
	/*	Text not currently being executed.  Must allocate
	 *	a new region for it.
	 *	vnode is held by getxfile;  it expects us to
	 *	do an VN_RELE on every failure.  Since allocreg holds
	 *	the vnode, we must decrement here.  Don't set
	 *	RG_NOFREE until the end also.
	 */

	if ((rp = allocreg(vp, RT_STEXT, 0)) == NULL) {
		VN_RELE(vp);
		return(NULL);
	}

	/*	Attach the region to our process.
	 */
	
	if ((prp = attachreg(rp, &u, regva, tpregtyp, SEG_RW)) == NULL) {
		VN_RELE(vp);
		freereg(rp);
		return(NULL);
	}
	
	/*	Load the region or map it for demand load.
	 */

	switch (exec_data->ux_mag) {
	case 0413:
	case 0443:
		/*
		 * mapreg gets confused if the first logical block
		 * is negative. This only happens on r6000 machines
		 * when the sections are only 4k aligned (r6000
		 * prefers 16k congruency, which is its pagesize)
		 */
		if (poff(base) <= (int)org) {
			if (mapreg(prp, base, vp, org, size) < 0) {
				VN_RELE(vp);
				detachreg(prp, &u, 0 /* don't invalidate TLBs */);
				return(NULL);
			}
			break;
		}
		/* fall into ... */


		/* fall thru case if REMOTE */
	case 0410:
		if (loadreg(prp, base, vp, org, size) < 0) {
			VN_RELE(vp);
			detachreg(prp, &u, 0 /* don't invalidate TLBs */);
			return(NULL);
		}
		break;
	default:
		cmn_err(CE_PANIC, "xalloc - bad magic");
	}


	{struct vattr vattr;
	 if (VOP_GETATTR(vp, &vattr, u.u_cred) != 0 || (vattr.va_mode & VSVTX))
		rp->r_flags |= RG_NOFREE;
	}

	ASSERT(prp->p_reg->r_listsz != 0);
	/* Change the region back to read only */
	prp->p_flags |= PF_RDONLY;
	chgpteprot(prp);
	regrele(rp);
	return(vp);
}


/*	Free the swap image of all unused shared text regions
 *	which are from device dev (used by umount system call).
 */

xumount(vfsp)
register struct vfs *vfsp;
{
	register reg_t		*rp;
	register reg_t		*nrp;
	register int		count;
	register struct vnode	*vp;

	count = 0;
loop:
	rlstlock();

	for (rp = ractive.r_forw ; rp != &ractive ; rp = nrp) {
		if (rp->r_type != RT_STEXT)
			nrp = rp->r_forw;
		else {
			rlstunlock();
			vp = rp->r_vptr;
			reglock(rp);
			if (rp->r_type != RT_STEXT) {
				regrele(rp);
				goto loop;
			}
			rlstlock();
			nrp = rp->r_forw;
			if (vfsp == vp->v_vfsp && rp->r_refcnt == 0) { /* } */
				rlstunlock();
				freereg(rp);
				count++;
				goto loop;
			} else {
				regrele(rp);
			}
		}
	}
	rlstunlock();
	return(count);
}
/*
 * Invalidate all text regions associated with the vnode vp from region table,
 * purge the in memory pages associated with all regions used by this vp and
 * issue a kill signal to all active processes associated with those regions.
 */
xinval(vp)
register struct vnode *vp;
{
	register reg_t	*rp;
	register reg_t	*nrp;
	register ureg_t *ur;
	register int	inuse = 0;
	register int    pswitch;

	if ((vp->v_flag&VTEXT) == 0) return;
	
loop:
	rlstlock();

	for (rp = ractive.r_forw ; rp != &ractive ; rp = nrp) {
		if (vp != rp->r_vptr) {
			nrp = rp->r_forw;
			continue;
		}

		rlstunlock();

		/* optimization to avoid excessive restarts */
		pswitch = sysinfo.pswitch;

		reglock(rp);
		if (vp != rp->r_vptr) {
			regrele(rp);
			if (pswitch != sysinfo.pswitch) goto loop;
		}

		for (ur = rp->r_use; ur < rp->r_use + rp->r_usesz; ur++) {
		    if (ur->ur_proc_index == UREGION_PROC_NULL) continue;

		    printf("pid %d killed due to text modification\n",
			   proc[ur->ur_proc_index].p_pid);

		    psignal(&proc[ur->ur_proc_index], SIGKILL);
                }

		pswitch = sysinfo.pswitch;
		rlstlock();
		nrp = rp->r_forw;

		if (rp->r_refcnt == 0) {
			rlstunlock();
			/*
			 * NOTE: The clients MUST increment the reference
			 * count via HOLD in order to keep it around.
			 */
			rp->r_vptr = (struct vnode *)0;
			freereg(rp);
			if (pswitch != sysinfo.pswitch) goto loop;
		} else {
			regrele(rp);
		}
	}

	if (!inuse)
		vp->v_flag &= ~VTEXT;
	rlstunlock();
}

/*
 * Remove a shared text region associated with vnode vp from the region
 * table, if possible.
 */
xrele(vp)
register struct vnode *vp;
{
	register reg_t	*rp;
	register reg_t	*nrp;
	register int	inuse;

	if ((vp->v_flag&VTEXT) == 0) return;
	
loop:
	inuse = 0;
	rlstlock();

	for (rp = ractive.r_forw ; rp != &ractive ; rp = nrp) {
		if (vp == rp->r_vptr) {
			if (rp->r_refcnt == 0) {
				rlstunlock();
				reglock(rp);
				if ((vp != rp->r_vptr) || (rp->r_refcnt != 0)) {
					regrele(rp);
					goto loop;
				}
				freereg(rp);
				goto loop;
			} else {
				inuse++;
			}
		}
		nrp = rp->r_forw;
	}
	if (!inuse)
		vp->v_flag &= ~VTEXT;
	rlstunlock();
}


/*	Try to remove unused sticky regions in order to free up swap
 *	space.
 */

swapclup()
{
	register reg_t		*rp;
	register reg_t		*nrp;
	register int		rval;
	register struct vnode	*vp;

	rval = 0;

loop:
	rlstlock();

	for (rp = ractive.r_forw ; rp != &ractive ; rp = nrp) {
		nrp = rp->r_forw;
		if (rp->r_type == RT_SHMEM)
			continue;
		if (rp->r_flags & RG_LOCK)
			continue;
		reglock(rp);
		if (rp->r_type == RT_UNUSED) {
			regrele(rp);
			continue;
		}
		if (rp->r_refcnt == 0) {
			rlstunlock();
			freereg(rp);
			rval = 1;
			goto loop;
		} else {
			regrele(rp);
		}
	}
	rlstunlock();
	return(rval);
}

/*
 * Prepare object process text region for writing.  If there is currently
 * only one reference to it, and it's not sticky text (RG_NOFREE set on the
 * region), remove it from the active region list so that it will no longer
 * be found by xalloc() and change the region type (not the pregion type)
 * to RT_PRIVATE.  If more than one process is using it, make a private
 * duplicate, detach the original region from the object process, and attach
 * the duplicate in its place.  In either case return the pregion pointer
 * of the (new or old, and now private) text region.
 */
preg_t *
xdup(p, prp)
register struct proc *p;
register preg_t *prp;
{
	register reg_t *rp = prp->p_reg, *nrp;
	register preg_t *nprp;
	register user_t *uservad;
	caddr_t va;
	register struct vnode *vp;

	ASSERT(rp->r_flags & RG_LOCK);
	ASSERT(rp->r_type == RT_STEXT);
	va = prp->p_regva;
	/* Pull funny business to make this region copy on write */
	if ((nrp = dupreg(rp, SEG_RO, 1)) == NULL) {
		regrele(rp);
		return(NULL);
	}
	vp = nrp->r_vptr;
	winublock();
	uservad = (struct user *)win_ublk;
	/* Should RG_DONE be set on the new region? */
	uaccess(p, uservad);

	/* detachreg() unlocks vp */
	detachreg(prp, uservad, 1 /* invalidate TLBs */);
	setup_wired_tlb(p);

	nprp = attachreg(nrp, uservad, va, PT_TEXT, SEG_RO);

	udeaccess(p);
	winubunlock();
	return(nprp);
}

