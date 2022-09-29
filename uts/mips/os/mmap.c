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
#ident	"$Header: mmap.c,v 1.10.1.3.1.1.1.2 90/10/16 10:04:45 beacker Exp $"

#include "sys/types.h"
#include "sys/sysmacros.h"
#include "sys/param.h"
#include "sys/signal.h"
#include "sys/errno.h"
#include "sys/sbd.h"
#include "sys/immu.h"
#include "sys/pcb.h"
#include "sys/user.h"
#include "sys/file.h"
#include "sys/vnode.h"
#include "sys/var.h"
#include "sys/sysinfo.h"
#include "sys/region.h"
#include "sys/proc.h"
#include "sys/conf.h"
#include "sys/pfdat.h"
#include "sys/debug.h"
#include "sys/mman.h"

pde_t *vtop();

mmap()
{
	register struct file	*fp;
	register struct vnode	*vp;
	register struct a {
		caddr_t	vaddr;
		int	len;
		int	prot;
		int	share;
		int	fd;
 		off_t	pos
	}			*uap;
	register int		off;
	int			type;
	dev_t			dev;
	int (*mapfun)();
	register int		prot;
	register pde_t		*pdep;
	register dbd_t		*dbd;
	register reg_t		*rp;
	register preg_t		*prp;

	uap = (struct a *)u.u_ap;

	if (uap->fd > MAX_MMAP_FD) {
		u.u_error = EINVAL;
		return;
	}
	GETF(fp, uap->fd);

	vp = (struct vnode *)fp->f_data;
	if (vp->v_type != VCHR) {
		u.u_error = EINVAL;
		return;
	}
	dev = vp->v_rdev;
	mapfun = cdevsw[major(dev)].d_mmap;
	if (mapfun == NULL) {
		u.u_error = EINVAL;
		return;
	}

	prot = uap->prot;
	
	/* check if vaddr, len and pos are on page boundaries */
	if ((int)uap->vaddr & (ctob(1)-1) || uap->len & (ctob(1)-1) ||
						uap->pos & (ctob(1)-1)) {
		u.u_error = EINVAL;
		return;
	}

	/* MAP_PRIVATE yet to be implemented (just like in BSD) */
	if (uap->share != MAP_SHARED) {
		u.u_error = EINVAL;
		return;
	}

	/* check for if vaddr is mapped in one of this proc's pregions */
	/* NOTE: findreg (on success) locks the region, need regrele's below */
	if ((prp = findreg(u.u_procp, uap->vaddr))==NULL) {
		u.u_error = EINVAL;
		return;
	}
	rp = prp->p_reg;
	if (prp->p_type != PT_DATA && prp->p_type != PT_SHMEM) {
		u.u_error = EINVAL;
		goto rprelease;
	}

	switch(prot & (PROT_WRITE | PROT_READ)) {
	case (PROT_READ | PROT_WRITE):
	case PROT_WRITE:
		if (prp->p_flags & PF_RDONLY) {
			/* can't write a read only region */
			u.u_error = EINVAL;
			goto rprelease;
		}
		break;
	case PROT_READ:
		break;
	default:
		u.u_error = EINVAL;
		goto rprelease;
	}

	/* check if len of mmap block fits into vaddr's region */
	if ((uap->len <= 0) || ((uap->vaddr + uap->len) >
					(prp->p_regva + ctob(rp->r_pgsz)))) {
		u.u_error = EINVAL;
		goto rprelease;
	}
	/* allow mmap() only in private regions */
	if (rp->r_type != RT_PRIVATE) {
		u.u_error = EACCES;
		goto rprelease;
	}
	/* check for legal memory to map */
	for (off = uap->pos; off < (uap->pos+uap->len); off += ctob(1)) {
		if ((*mapfun)(dev, off, prot) == -1) {
			u.u_error = EINVAL;
			goto rprelease;
		}
	}

	/* do the remapping */
	rp->r_flags |= RG_MMAP;	/* this pregion has mmapped pages */
	u.u_pofile[uap->fd] |= UF_MAPPED;

	for (off = uap->pos; off < uap->pos+uap->len;
				uap->vaddr += ctob(1), off += ctob(1)) {
		/* get pde address for user virtual page */
		regrele(rp);		/* vtop() will try to lock rp again */
		pdep = vtop(uap->vaddr, u.u_procp);
		reglock(rp);
		dbd = pdetodbd(pdep);

		/* free user virtual page and associate dbd */
		/* usually pfree will only have to clear the pde because
		 * the page will not have been used yet.
		 */
		if (pg_isvalid(pdep)) {
			register pfd_t *pfd;

			pfd = pdetopfdat(pdep);
			MUNLOCK_WAIT(pfd);
		};
		memlock();		/* pfree() expects mem to be locked */
		pfree(rp, pdep, dbd, 1);
#ifdef notdef
		/*
		 * We would like to unreserve the pages replaced by the
		 * mapped pages, but we cannot do that and still guarantee
		 * that munmap() will be possible.  Therefore, for the time
		 * being, we need to leave the pages reserved.  Later,
		 * we will want to improve region management, to allow
		 * for "unallocated and zero" pages.
		 */
		if (0) {
			register int s;

			/* avail[rs]mem can be modified at interrupt level */
			s = splhi();
			availsmem++;
			if (rp->r_noswapcnt)
				availrmem++;
			splx(s);
		};
#endif notdef

		/* remap user virtual page: non-cacheable, modified, valid
		 * & referenced, locked, and software valid
		 */
		pde_unlink(pdep);
		dbd = pdetodbd(pdep);
		pdep->pgi.pg_pde = (uint)mkpde((PG_N|PG_M|PG_VR|PG_LOCK|PG_SV),
						(*mapfun)(dev, off, prot));
		((struct mmapinfo *)dbd)->mmap_type = DBD_MMAP;
		((struct mmapinfo *)dbd)->mmap_fileno = uap->fd;

		/* Normally protection only exists at the region level, but
		 * since we are mmapping into a data or shared-memory
		 * region where there may be non-mmapped data as well, we
		 * will protect the mmap pages at the page level.
		 */
		if ((prot & PROT_WRITE) == 0) {
			pg_clrmod(pdep);
		}
		memunlock();
	}
	/* invalidate tlb */
	newptes(u.u_procp, pnum(uap->vaddr), btoc(uap->len));

rprelease:
	regrele(rp);
	return;
}

mremap()
{

}

munmap()
{
	register struct a {
		caddr_t	vaddr;
		int	len;
	}			*uap;
	register pde_t		*pdep;
	register dbd_t		*dbd;
	register preg_t *prp;
	register reg_t *rp;
	register int		off;
	register caddr_t	cur_vaddr;

	uap = (struct a *)u.u_ap;

	/* check if vaddr, len and pos are on page boundaries */
	if ((int)uap->vaddr & (ctob(1)-1) || uap->len & (ctob(1)-1)) {
		u.u_error = EINVAL;
		return;
	}

	/* check for if vaddr is mapped in one of this proc's pregions */
	/* NOTE: findreg (on success) locks the region, need regrele's below */
	if ((prp = findreg(u.u_procp, uap->vaddr))==NULL) {
		u.u_error = EINVAL;
		return;
	}
	if (! (prp->p_type == PT_DATA || prp->p_type == PT_SHMEM)) {
		u.u_error = EINVAL;
		return;
	};

	rp = prp->p_reg;
	ASSERT(rp->r_refcnt > 0);
	/* check if len of mmap block fits into vaddr's region */
	if (uap->len <= 0 || uap->vaddr + uap->len >
					prp->p_regva + ctob(rp->r_pgsz)) {
		u.u_error = EINVAL;
		goto rprelease;
	}

	
	for (off = 0, cur_vaddr = uap->vaddr;
	     off < uap->len; cur_vaddr += ctob(1), off += ctob(1)) {
		/* get pde address for user virtual page */
		regrele(rp);		/* vtop() will try to lock rp again */
		pdep = vtop(cur_vaddr, u.u_procp);
		reglock(rp);

		if (pdetodbd(pdep)->dbd_type != DBD_MMAP) {
			u.u_error = EINVAL;
			goto rprelease;
		}
	};

	/* do the remapping */
	for (off = 0, cur_vaddr = uap->vaddr;
	     off < uap->len; cur_vaddr += ctob(1), off += ctob(1)) {
		/* get pde address for user virtual page */
		regrele(rp);		/* vtop() will try to lock rp again */
		pdep = vtop(cur_vaddr, u.u_procp);
		reglock(rp);

		if (pdetodbd(pdep)->dbd_type != DBD_MMAP) {
			continue; /* just in case */
		}

		/* Invalidate pte */
		pde_unlink(pdep);
		pdep->pgi.pg_pde = 0;
		pdetodbd(pdep)->dbd_type = DBD_DZERO;
	}
	/* invalidate tlb */
	newptes(u.u_procp, pnum(uap->vaddr), btoc(uap->len));

rprelease:
	regrele(rp);
	return;
}

munmapfd(fd)
int fd;
{
	register reg_t		*rp;
	register preg_t		*prp;
	register pde_t		*pt;
	register dbd_t		*dbd;
	register caddr_t	vaddr;
	register int i,j,k;
	register int pgsz;
	register int cur_pgsz;
	int need_inval = 0;

	/* for every pregion that has mmapped pages ... */
	prp = up->u_procp->p_region;
	pregpp = MINPREGPP + shmseg() + 2*shlbinfo.shlbs;
	for (i = prp - up->u_procp->p_region; i < pregpp-1; i++, prp++) {
		rp = prp->p_reg;
		if (rp && rp->r_flags & RG_MMAP) {
		    ASSERT(prp->p_type==PT_DATA || prp->p_type==PT_SHMEM);

			    /* free any pages that are mmapped */
		    pgsz = rp->r_pgsz;
		    for (j = 0; j < rp->r_listsz; j++) {
			pt = &rp->r_list[j][0];
			cur_pgsz = ((pgsz < ((j + 1) * NPGPT))
			  		? (pgsz % NPGPT)
					: NPGPT);
			vaddr = prp->p_regva + ctob((j * NPGPT));
			for (k = 0;
			     k < cur_pgsz;
			     pt++, k++, (vaddr += ctob(1)) ) {
			    dbd = pdetodbd(pt);
			    if (dbd->dbd_type==DBD_MMAP &&
				((struct mmapinfo *)dbd)->mmap_fileno == fd) {
				/* Invalidate pte */
				pde_unlink(pt);
				pt->pgi.pg_pde = 0;
				pdetodbd(pt)->dbd_type=DBD_DZERO;
				need_inval = 1;
			    }
			}
		    }
		}
	}
	u.u_pofile[fd] &= ~UF_MAPPED;
	if (need_inval)
		new_tlbpid(u.u_procp);
}


mprotect()
{

}

madvise() {}

mincore()
{

}

getpagesize()
{
	u.u_rval1 = NBPC;
}
