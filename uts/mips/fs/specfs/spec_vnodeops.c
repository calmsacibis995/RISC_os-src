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
#ident	"$Header: spec_vnodeops.c,v 1.4.1.8.1.2.1.2 90/10/16 12:04:31 beacker Exp $"
/* @(#)spec_vnodeops.c 2.4 88/08/05 4.0NFSSRC; SMI */

/* Originally included param.h systm.h user.h buf.h kernel.h vfs.h
*  vnode.h uio.h conf.h file.h cmap.h ../specfs/snode.h ../krpc/lockmgr.h
*/
#include "sys/types.h"
#include "sys/signal.h"		/* needed by user.h */
#include "sys/pcb.h"		/* needed by user.h */
#include "sys/user.h"
#include "sys/immu.h"		/* needed by proc.h */
#include "sys/sbd.h"		/* needed by proc.h */
#include "sys/region.h"		/* needed by proc.h */
#include "sys/proc.h"
#include "sys/systm.h"
#include "sys/param.h"
#include "sys/buf.h"
#include "sys/errno.h"
#include "sys/vfs.h"
#include "sys/vnode.h"
#include "sys/uio.h"
#include "sys/conf.h"
#include "sys/file.h"
#include "sys/open.h"
#include "sys/cmap.h"
#include "sys/sysmacros.h"
#include "sys/numips.h"
#include "sys/fs/snode.h"
#include "sys/stream.h"
#include "../klm/lockmgr.h"
#include "sys/kmem.h"
#include "sys/debug.h"
#include "sys/cmn_err.h"

static int spec_open();
static int spec_close();
static int spec_rdwr();
static int spec_ioctl();
static int spec_select();
static int spec_inactive();
static int spec_strategy();
static int spec_noop();
static int spec_dump();
static int spec_cmp();
/*
 * Used directly in fifo_vnodeops
 */
int spec_setattr();
int spec_getattr();
int spec_access();
int spec_link();
int spec_lockctl();
int spec_fsync();
int spec_fid();
int spec_realvp();
/*
 * Declared in spec_vfsops.c
 */
int spec_badop();

struct vnodeops spec_vnodeops = {
	spec_open,
	spec_close,
	spec_rdwr,
	spec_ioctl,
	spec_select,
	spec_getattr,
	spec_setattr,
	spec_access,
	spec_noop,	/* lookup */
	spec_noop,	/* create */
	spec_noop,	/* remove */
	spec_link,
	spec_noop,	/* rename */
	spec_noop,	/* mkdir */
	spec_noop,	/* rmdir */
	spec_noop,	/* readdir */
	spec_noop,	/* symlink */
	spec_noop,	/* readlink */
	spec_fsync,
	spec_inactive,
	spec_badop,	/* bmap */
	spec_strategy,	/* strategy */
	spec_badop,	/* bread */
	spec_badop,	/* brelse */
	spec_lockctl,
	spec_fid,
	spec_dump,
	spec_cmp,
	spec_realvp,
};

#define	MIN(a,b) (((a)<(b))?(a):(b))

#ifdef RISCOS
#ifdef MAXBSIZE
#undef MAXBSIZE
#endif
#define MAXBSIZE FSMAXBSIZE
#define selwait pollwait		/* combine streams & select wait */
struct vnode *clone_open_vp;		/* points to new vnode if device driver did a clone open */
#endif /* RISCOS */

/*
 * open a special file (device)
 * Some weird stuff here having to do with clone and indirect devices:
 * When a file lookup operation happens (e.g. ufs_lookup) and the vnode has
 * type VDEV specvp() is used to return a spec vnode instead.  Then when
 * the VOP_OPEN routine is called, we get control here.  When we do the
 * device open routine there are several possible strange results:
 * 1) An indirect device will return the error EAGAIN on open and return
 *    a new dev number.  We have to make that into a spec vnode and call
 *    open on it again.
 * 2) The clone device driver will return the error EEXIST and return a
 *    new dev number.  As above, we build a new vnode and call open again,
 *    explicitly asking the open routine to do a clone open.
 * 3) A clone device will return a new dev number on open but no error.
 *    In this case we just make a new spec vnode out of the new dev number
 *    and return that.
 * The last two cases differ in that the decision to clone arises outside
 * of the target device in 2) and from within in 3).
 *
 * TODO: extend case 2) to apply to all character devices, not just streams
 * devices.
 */
/*ARGSUSED*/
static int
spec_open(vpp, flag, cred)
	struct vnode **vpp;
	int flag;
	struct ucred *cred;
{
	register struct snode *sp;
	dev_t dev;
	dev_t newdev;
	register int error;

	sp = VTOS(*vpp);

	/*
	 * Do open protocol for special type.
	 */
	dev = sp->s_dev;

	switch ((*vpp)->v_type) {

	case VCHR:
		newdev = dev;
		error = 0;
		for (;;) {
			register struct vnode *nvp;

			dev = newdev;
#ifdef RISCOS
			if ((u_int)major(dev) >= cdevcnt)
#else
			if ((u_int)major(dev) >= nchrdev)
#endif
				return (ENXIO);

			while (isclosing(dev, (*vpp)->v_type))
				(void) sleep((caddr_t)sp, PSLEP);

#ifdef RISCOS
			/* If this open was successful, then we
			 * check to see if the vnode changed
			 * (ie, if we just opened a clone device).
			 *
			 * In the streams case we check the stdata
			 * structure to see if it is pointing at
			 * a different vnode.
			 *
			 * In the non-streams case, we look for a 
			 * non-zero value in the global variable
			 * clone_open_vp.
			 *
			 * Of course, the right way would be to do
			 * the cloning here (since we do the snode
			 * allocation anyway), but that requires
			 * changing the interface to all open routines.
			 */
			clone_open_vp = (struct vnode *) 0;
			if (cdevsw[major(dev)].d_str != NULL) {
				sp->s_count++;
				stropen(*vpp, flag);
				sp->s_count--;
				if (!u.u_error && (*vpp)->v_stream &&
				 (nvp = (*vpp)->v_stream->sd_vnode) != *vpp) {
					(*vpp)->v_stream = NULL;
					VN_HOLD(nvp);
					sp = VTOS(nvp);
					sp->s_count++;
					VN_RELE(*vpp);
					*vpp = nvp;
					sp->s_count--;
				} else if (sp->s_count == 0 &&
					   u.u_error &&
					   (*vpp)->v_stream != NULL &&
					   ! stillopen(sp->s_dev,(*vpp)->v_type)) {
					int	old_error = u.u_error;

					strclose(*vpp,flag | FNDELAY);
					u.u_error = old_error;
				};
			} else {
				(*cdevsw[major(dev)].d_open)(dev, flag,
					OTYP_CHR);
				if (clone_open_vp) {
					VN_RELE(*vpp);
					sp = VTOS(clone_open_vp);
					*vpp = clone_open_vp;
					clone_open_vp = (struct vnode *) 0;
				}
			}
			error = u.u_error;
#else
			error = (*cdevsw[major(dev)].d_open)(dev, flag,
				     &newdev);
#endif
			/*
			 * If this is an indirect device or a forced clone,
			 * we need to do the open again.  In both cases,
			 * we insist that newdev differ from dev, to help
			 * avoid infinite regress.
			 */
			if (newdev == dev ||
			    (error != 0 && error != EAGAIN && error != EEXIST))
				break;

			/*
			 * Allocate new snode with new device.  Release old
			 * snode. Set vpp to point to new one.  This snode will
			 * go away when the last reference to it goes away.
			 * Warning: if you stat this, and try to match it with
			 * a name in the filesystem you will fail, unless you
			 * had previously put names in that match.
			 */
			nvp = specvp(*vpp, newdev, VCHR);
			sp = VTOS(nvp);
			VN_RELE(*vpp);
			*vpp = nvp;

		}
		break;

	case VFIFO:
		printf("spec_open: got a VFIFO???\n");
		/* fall through to... */

	case VSOCK:
		error = EOPNOTSUPP;
		break;

	case VBLK:
		/*
		 * The block device sizing was already done in specvp().
		 * However, we still need to verify that we can open the
		 * block device here (since specvp was called as part of a
		 * "lookup", not an "open", and e.g. "stat"ing a block special
		 * file with an illegal major device number should be legal).
		 */
#ifdef RISCOS
		if ((u_int)major(dev) >= bdevcnt)
#else
		if ((u_int)major(dev) >= nblkdev)
#endif
			error = ENXIO;
		else
#ifdef RISCOS
		{
			(void) (*bdevsw[major(dev)].d_open)(dev, flag,
				OTYP_BLK);
			error = u.u_error;
		}
#else
			error = (*bdevsw[major(dev)].d_open)(dev, flag);
#endif
		break;

	default:
		error = 0;
		break;
	}
	if (error == 0)
		sp->s_count++;		/* one more open reference */
	return (error);
}

/*ARGSUSED*/
static int
spec_close(vp, flag, count, cred)
	struct vnode *vp;
	int flag;
	int count;
	struct ucred *cred;
{
	register struct snode *sp;
	dev_t dev;

#if RISCOS
	if (vp->v_stream && vp->v_type != VDIR)
		strclean(vp);
#endif
	if (count > 1)
		return (0);

	/*
	 * setjmp in case close is interrupted
	 */
#ifdef RISCOS
	if (setjmp(u.u_qsav)) {
#else
	if (setjmp(&u.u_qsave)) {
#endif
		sp = VTOS(vp);	/* recompute - I don't trust setjmp/longjmp */
		sp->s_flag &= ~SCLOSING;
		wakeup((caddr_t)sp);
		return (EINTR);
	}

	sp = VTOS(vp);
	sp->s_count--;		/* one fewer open reference */

	/*
	 * Only call the close routine when the last open
	 * reference through any [s,v]node goes away.
	 */
	if (stillopen(sp->s_dev, vp->v_type))
		return (0);
	ASSERT(sp->s_count == 0);

	sp->s_flag |= SCLOSING;
	dev = sp->s_dev;

	switch (vp->v_type) {

	case VCHR:
#ifdef RISCOS
		if (cdevsw[major(dev)].d_str != NULL)
			strclose(vp, flag);
		else
			(*cdevsw[major(dev)].d_close)(dev, flag, OTYP_CHR);
#else
		(void) (*cdevsw[major(dev)].d_close)(dev, flag);
#endif
		break;

	case VBLK:
		/*
		 * On last close of a block device (that isn't mounted)
		 * we must invalidate any in core blocks so that
		 * we can, for instance, change floppy disks.
		 */
#ifdef RISCOS
		bflush(sp->s_bdevvp);
#else
		bflush(vp);
#endif
#if RISCOS
		bdevwait(vp->v_rdev);
#endif
#ifdef RISCOS
		binval(sp->s_bdevvp);
#else
		binval(vp);
#endif
#ifdef RISCOS
		(void) (*bdevsw[major(dev)].d_close)(dev, flag, OTYP_BLK);
#else
		(void) (*bdevsw[major(dev)].d_close)(dev, flag);
#endif
		break;

	case VFIFO:
		printf("spec_close: got a VFIFO???\n");
		break;
	}

	/* The following wakeup can be optimized if we set a flag to
	 * indicate that someone is waiting for the close to finish.
	 */
	sp->s_flag &= ~SCLOSING;
	wakeup((caddr_t)sp);

	return (0);
}

/*
 * read or write a spec vnode
 */
/*ARGSUSED*/
static int
spec_rdwr(vp, uiop, rw, ioflag, cred)
	struct vnode *vp;
	register struct uio *uiop;
	enum uio_rw rw;
	int ioflag;
	struct ucred *cred;
{
	register struct snode *sp;
	struct vnode *blkvp;
	dev_t dev;
	struct buf *bp;
	daddr_t lbn, bn;
	register int n, on;
	int size;
	u_int bdevsize;
	int error;
	register struct vattr va;
	register u_long nodeid;

#ifndef RISCOS /* All filesystems support bit offsets, mem is not different */
	extern int mem_no;
#endif

	sp = VTOS(vp);
	dev = (dev_t)sp->s_dev;
	if (rw != UIO_READ && rw != UIO_WRITE)
		panic("spec_rdwr");
	if (rw == UIO_READ && uiop->uio_resid == 0) 
		return (0);
#ifndef RISCOS    /* We do this test in the filesystem-independent code */
	if ((uiop->uio_offset < 0 ||
	    (uiop->uio_offset + uiop->uio_resid) < 0) &&
	    !(vp->v_type == VCHR && mem_no == major(dev))) {
		return (EINVAL);
	}
#endif /* !RISCOS */

	if (rw == UIO_READ)
		smark(sp, SACC);

	if (vp->v_type == VCHR) {
		if (rw == UIO_READ) {
#ifdef RISCOS
			error = uio_iteration(UIO_READ, vp, uiop);
#else
			error = (*cdevsw[major(dev)].d_read)(dev, uiop);
#endif
		} else {
			smark(sp, SUPD|SCHG);
#ifdef RISCOS
			error = uio_iteration(UIO_WRITE, vp, uiop);
#else
			error = (*cdevsw[major(dev)].d_write)(dev, uiop);
#endif
		}
		return (error);
	}

	if (vp->v_type != VBLK)
		return (EOPNOTSUPP);

	if (uiop->uio_resid == 0)
		return (0);

	error = 0;
	blkvp = sp->s_bdevvp;
	bdevsize = MAXBSIZE;
	do {
		lbn = uiop->uio_offset / bdevsize;
		on = uiop->uio_offset % bdevsize;
		n = MIN((MAXBSIZE - on), uiop->uio_resid);
		bn = lbn * (bdevsize/DEV_BSIZE);
		rablock = bn + (bdevsize/DEV_BSIZE);
		rasize = size = bdevsize;
		if (rw == UIO_READ) {
			if ((long)bn<0) {
				bp = geteblk(size);
				clrbuf(bp);
			} else if (sp->s_lastr + 1 == lbn) 
				bp = breada(blkvp, bn, size, rablock,
					rasize);
			else 
				bp = bread(blkvp, bn, size);
			sp->s_lastr = lbn;
		} else {
			int i, count;
#if RISCOS
			error = spec_getattr(vp, &va, cred);
			if (error) {
				goto bad;
			}
			nodeid = va.va_nodeid;
			/* use 'n' since 'size' would invalidate too many */
			count = howmany(n, DEV_BSIZE);
			for (i = 0; i < count; i += NBPC_I/DEV_BSIZE)
				/* The block number needs to be given in
				 * terms of 512 bytes, not 'size' or 'bsize'
				 * NOTE: uiop->uio_offset != lbn*bsize
				 * for uiop->uio_offset < bsize.
				 */
				punhash(STOV(sp),
					(btodb(uiop->uio_offset) & 
					     ~((NBPC_I/DEV_BSIZE) - 1))+i,
					 nodeid);
#else
			extern struct cmap *mfind();

			count = howmany(size, DEV_BSIZE);
			for (i = 0; i < count; i += CLBYTES/DEV_BSIZE)
				if (mfind(blkvp, (daddr_t)(bn + i)))
					munhash(blkvp, (daddr_t)(bn + i));
#endif
			if (n == bdevsize) 
				bp = getblk(blkvp, bn, size);
			else
				bp = bread(blkvp, bn, size);
		}
		n = MIN(n, bp->b_bcount - bp->b_resid);
		if (bp->b_flags & B_ERROR) {
			error = EIO;
			brelse(bp);
			goto bad;
		}
		error = uiomove(bp->b_un.b_addr+on, n, rw, uiop);
		if (rw == UIO_READ) {
			if (n + on == bdevsize)
				bp->b_flags |= B_AGE;
			brelse(bp);
		} else {
			if (ioflag & IO_SYNC)
				bwrite(bp);
			else if (n + on == bdevsize) {
				bp->b_flags |= B_AGE;
				bawrite(bp);
			} else
				bdwrite(bp);
			smark(sp, SUPD|SCHG);
		}
	} while (error == 0 && uiop->uio_resid > 0 && n != 0);
bad:
	return (error);
}

/*ARGSUSED*/
static int
spec_ioctl(vp, com, data, flag, cred)
	struct vnode *vp;
	int com;
	caddr_t data;
	int flag;
	struct ucred *cred;
{
	register struct snode *sp;

	sp = VTOS(vp);
	if (vp->v_type != VCHR)
		panic("spec_ioctl");
#if RISCOS
	if (cdevsw[major(sp->s_dev)].d_str != NULL)
		strioctl(vp, com, data, flag);
	else
		(*cdevsw[major(sp->s_dev)].d_ioctl)(sp->s_dev, com, data, flag);
	return (u.u_error);
#else
	return ((*cdevsw[major(sp->s_dev)].d_ioctl)
	    (sp->s_dev, com, data, flag));
#endif
}

/*ARGSUSED*/
static int
spec_select(vp, which, cred)
	struct vnode *vp;
	int which;
	struct ucred *cred;
{
	register struct snode *sp;

	sp = VTOS(vp);
	if (vp->v_type != VCHR)
		panic("spec_select");
#if RISCOS
	(*cdevsw[major(sp->s_dev)].d_select)(sp->s_dev, which);
	return (u.u_error);
#else
	return ((*cdevsw[major(sp->s_dev)].d_select)(sp->s_dev, which));
#endif
}

static int
spec_inactive(vp, cred)
	struct vnode *vp;
	struct ucred *cred;
{
	struct snode *sp;

	sp = VTOS(vp);
	if (sp->s_count)
	    cmn_err(CE_WARN, 
		"spec_inactive: non-zero s_count:%d sp:%x s_dev:%x\n",
			    sp->s_count, sp, sp->s_dev);
        /* must sunsave() first to prevent a race when spec_fsync() sleeps */
	sunsave(sp);
 
	if (sp->s_realvp)
		(void) spec_fsync(vp, cred);

	/* now free the realvp (no longer done by sunsave()) */
	if (sp->s_realvp) {
		VN_RELE(sp->s_realvp);
		sp->s_realvp = NULL;
		if (sp->s_bdevvp)
			VN_RELE(sp->s_bdevvp);
	}

	kmem_free((caddr_t)sp, sizeof (*sp));
	return (0);
}

static int
spec_getattr(vp, vap, cred)
	struct vnode *vp;
	register struct vattr *vap;
	struct ucred *cred;
{
	int error;
	register struct snode *sp;
	register struct vnode *realvp;
	enum vtype type;

	sp = VTOS(vp);
	if ((realvp = sp->s_realvp) == NULL) {
		/*
		 * No real vnode behind this one.
		 * Set the device size from snode.
		 * Set times to the present.
		 * Set blocksize based on type in the unreal vnode.
		 */
		bzero((caddr_t)vap, sizeof (*vap));
#if RISCOS
		vap->va_fmode = -1;	/* set to uninitialized state */
		vap->va_type = vp->v_type;
		vap->va_rdev = sp->s_dev;
#endif /* RISCOS */
		vap->va_size = sp->s_size;
		vap->va_atime = time;
		vap->va_mtime = time;
		vap->va_ctime = time;
		type = vp->v_type;
	} else {
		error = VOP_GETATTR(realvp, vap, cred);
		if (error != 0)
			return (error);
		/* set current times from snode, even if older than vnode */
		vap->va_atime = sp->s_atime;
		vap->va_mtime = sp->s_mtime;
		vap->va_ctime = sp->s_ctime;
		type = vap->va_type;
	}

	/* set device-dependent blocksizes */
	switch (type) {
	case VBLK:			/* was BLKDEV_IOSIZE	*/
	case VCHR:
		vap->va_blocksize = MAXBSIZE;
		break;
	}
	return (0);
}

int
spec_setattr(vp, vap, cred)
	struct vnode *vp;
	register struct vattr *vap;
	struct ucred *cred;
{
	register struct snode *sp;
	register struct vnode *realvp;
	int error;
	register int chtime = 0;

	sp = VTOS(vp);
	if ((realvp = sp->s_realvp) == NULL)
		error = 0;			/* no real vnode to update */
	else
		error = VOP_SETATTR(realvp, vap, cred);
	if (error == 0) {
		/* if times were changed, update snode */
		if (vap->va_mtime.tv_sec != -1) {
			sp->s_mtime = vap->va_mtime;
			chtime++;
		}
		if (vap->va_atime.tv_sec != -1) {
			sp->s_atime = vap->va_atime;
			chtime++;
		}
		if (chtime)
			sp->s_ctime = time;
	}
	return (error);
}

int
spec_access(vp, mode, cred)
	struct vnode *vp;
	int mode;
	struct ucred *cred;
{
	register struct vnode *realvp;

	if ((realvp = VTOS(vp)->s_realvp) != NULL)
		return (VOP_ACCESS(realvp, mode, cred));
	else
		return (0);	/* allow all access */
}

int
spec_link(vp, tdvp, tnm, cred)
	struct vnode *vp;
	struct vnode *tdvp;
	char *tnm;
	struct ucred *cred;
{
	register struct vnode *realvp;

	if ((realvp = VTOS(vp)->s_realvp) != NULL)
		return (VOP_LINK(realvp, tdvp, tnm, cred));
	else
		return (ENOENT);	/* can't link to something non-existent */
}

/*
 * In order to sync out the snode times without multi-client problems,
 * make sure the times written out are never earlier than the times
 * already set in the vnode.
 */
int
spec_fsync(vp, cred)
	struct vnode *vp;
	struct ucred *cred;
{
	register int error;
	register struct snode *sp;
	register struct vnode *realvp;
	struct vattr *vap;
	struct vattr *vatmp;

	sp = VTOS(vp);
	/*
	 * If times didn't change on a non-block
	 * special file, don't flush anything.
	 */
	if ((sp->s_flag & (SACC|SUPD|SCHG)) == 0 && vp->v_type != VBLK)
		return (0);

	/*
	 * If no real vnode to update, don't flush anything
	 */
	if ((realvp = sp->s_realvp) == NULL)
		return (0);

	vatmp = (struct vattr *)kmem_alloc(sizeof (*vatmp));
	error = VOP_GETATTR(sp->s_realvp, vatmp, cred);
	if (error == 0) {
		vap = (struct vattr *)kmem_alloc(sizeof (*vap));
		vattr_null(vap);
		vap->va_atime = timercmp(&vatmp->va_atime, &sp->s_atime, >) ?
		    vatmp->va_atime : sp->s_atime;
		vap->va_mtime = timercmp(&vatmp->va_mtime, &sp->s_mtime, >) ?
		    vatmp->va_mtime : sp->s_mtime;
		VOP_SETATTR(realvp, vap, cred);
		kmem_free((caddr_t)vap, sizeof (*vap));
	}
	kmem_free((caddr_t)vatmp, sizeof (*vatmp));
	(void) VOP_FSYNC(realvp, cred);
	return (0);
}

static int
spec_dump(vp, addr, bn, count)
	struct vnode *vp;
	caddr_t addr;
	int bn;
	int count;
{

	return ((*bdevsw[major(vp->v_rdev)].d_dump)
	    (vp->v_rdev, addr, bn, count));
}

static int
spec_noop()
{

	return (EINVAL);
}

/*
 * Record-locking requests are passed back to the real vnode handler.
 */
int
spec_lockctl(vp, ld, cmd, cred, clid)
	struct vnode *vp;
	struct flock *ld;
	int cmd;
	struct ucred *cred;
	int clid;
{
	register struct vnode *realvp;

	if ((realvp = VTOS(vp)->s_realvp) != NULL)
		return (VOP_LOCKCTL(realvp, ld, cmd, cred, clid));
	else
		return (EINVAL);	/* can't lock this, it doesn't exist */
}

int
spec_fid(vp, fidpp)
	struct vnode *vp;
	struct fid **fidpp;
{
	register struct vnode *realvp;

	if ((realvp = VTOS(vp)->s_realvp) != NULL)
		return (VOP_FID(realvp, fidpp));
	else
		return (EINVAL);	/* you lose */
}

static int
spec_cmp(vp1, vp2)
	struct vnode *vp1, *vp2;
{

	return (vp1 == vp2);
}

int
spec_realvp(vp, vpp)
	struct vnode *vp;
	struct vnode **vpp;
{
	extern struct vnodeops fifo_vnodeops;
	struct vnode *rvp;

	if (vp &&
	    (vp->v_op == &spec_vnodeops || vp->v_op == &fifo_vnodeops)) {
		vp = VTOS(vp)->s_realvp;
	}
	if (vp && VOP_REALVP(vp, &rvp) == 0) {
		vp = rvp;
	}
	*vpp = vp;
	return (0);
}

static int
spec_strategy(bp)
	struct buf *bp;
{
	(*bdevsw[major(bp->b_vp->v_rdev)].d_strategy)(bp);
	return (0);
}

/*
* Generic procedure which handles iterating over the number of io operations
* defined in the uio structure vector.
*/
int uio_iteration (op, vp, uiop)
int op;
struct vnode *vp;
struct uio *uiop;
{
	register struct iovec *iov;
	int i, dev, mdev, use_uio;
	caddr_t u_base;
	label_t	old_qsav;

	dev = vp->v_rdev;
	mdev = major(dev);

	/*
	 * Set up error handling.  We intercept all longjmps here in order
	 * to do any post-processing required to make all the bookkeeping
	 * correct.
	 */
	bcopy((caddr_t) u.u_qsav,(caddr_t) old_qsav,sizeof(old_qsav));
	if (setjmp(u.u_qsav)) {
		if (use_uio) {
			u.u_uio = NULL;
		} else {
			uiop->uio_resid -= (u.u_base - u_base);
			uiop->uio_offset += (u.u_base - u_base);
		}
		if (BSD_SYSCALL &&
		    (u.u_sigintr & bsd43_sigmask(u.u_procp->p_cursig)) == 0) {
			u.u_error = 0;
			u.u_eosys = RESTARTSYS;
		} else
			u.u_error = EINTR;
		bcopy((caddr_t) old_qsav,(caddr_t) u.u_qsav,sizeof(old_qsav));
		longjmp(u.u_qsav);  /* Now go wherever we were supposed to */
	};

	/*
	 * Streams know how to handle uio structs, but use a funny parm
	 * passing scheme:  store the uiop in u.u_uio.  Yuk.
	 */
	if ((vp->v_type == VCHR) && (mdev <= cdevcnt) && cdevsw[mdev].d_str) {
		use_uio = 1;
		u.u_uio = uiop;
		/*
		 * Those routines which use the uio structure are
		 * responsible for resetting u.u_count if uio_resid
		 * is artificially reduced (to fit in a signed int).
		 * This is so that if we are interrupted, we can tell 
		 * if uio_resid was reduced artificially or data
		 * was actually transferred.
		 */
		u.u_count = uiop->uio_resid;
		u.u_fmode = uiop->uio_fmode;
		if (op == UIO_READ)
			strread(vp);
		else
			strwrite(vp);
		u.u_uio = NULL;
		return (u.u_error);
	}
	
	/*
	 * Rest of the world is pretty clueless about uio (boo, hiss!),
	 * so we take all the interesting info out of the uio struct,
	 * and put it in the well-known global fields in the user struct.
	 */
	use_uio = 0;
	u.u_uio = NULL;
	u.u_offset = uiop->uio_offset; /* offset into file for io */
	u.u_segflg = uiop->uio_segflg;
	u.u_fmode = uiop->uio_fmode;

	for (i = 0, iov = uiop->uio_iov ; i < uiop->uio_iovcnt ; i++, iov++) {
		u_base = u.u_base = iov->iov_base;   /* base address of io */
	        u.u_count = iov->iov_len;   /* number bytes remaining for io */

		if (op == UIO_READ)
		        (*cdevsw[major(dev)].d_read)(dev);
		else
		        (*cdevsw[major(dev)].d_write)(dev);
		uiop->uio_resid -= (u.u_base - u_base);
		uiop->uio_offset += (u.u_base - u_base);
		if (u.u_error || u.u_count)
			break;
	} /* for */
	return (u.u_error);
}
