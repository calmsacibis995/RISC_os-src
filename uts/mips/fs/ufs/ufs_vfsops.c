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
#ident	"$Header: ufs_vfsops.c,v 1.6.1.8 90/06/05 16:51:35 wje Exp $"
#ifndef lint
#endif

/*
* Original includes param.h systm.h user.h proc.h buf.h pathname.h vfs.h
* vfs_stat.h vnode.h file.h uio.h conf.h ../ufs/fs.h ../ufs/mount.h
* ../ufs/inode.h" undef NFS "../../bsd43/sys/mount.h reboot.h"
*/

#include "sys/types.h"
#include "sys/signal.h"		/* needed by user.h */
#include "sys/pcb.h"		/* needed by user.h */
#include "sys/user.h"
#include "sys/systm.h"
#include "sys/buf.h"
#include "sys/uio.h"
#include "sys/dnlc.h"
#include "sys/errno.h"
#include "sys/file.h"
#include "sys/vfs.h"
#include "sys/vfs_stat.h"
#include "sys/vnode.h"
#include "sys/pathname.h"
#include "sys/conf.h"
#include "sys/sysmacros.h"
#include "sys/open.h"
#undef itoo  /* needed to avoid conflicts in fs.h */
#undef itod  /* needed to avoid conflicts in fs.h */
#include "sys/fs/ufs_fs.h"
#include "sys/fs/ufs_inode.h"
#include "sys/fs/ufs_mount.h"
#undef NFS
#include "sys/mount.h"
#include "sys/kmem.h"
#include "sys/cmn_err.h"	/* so I can call cmn_err() */

#if MIPS_LOCAL
int	lock_cgs_incore = 0;	/* set it to lock cylinder groups incore */
#endif MIPS_LOCAL

/*
 * ufs vfs operations.
 */
static int ufs_mount();
static int ufs_unmount();
static int ufs_root();
static int ufs_statfs();
static int ufs_sync();
static int ufs_vget();
static int ufs_mountroot();
static int ufs_badvfsop();

struct vfsops ufs_vfsops = {
	ufs_mount,
	ufs_unmount,
	ufs_root,
	ufs_statfs,
	ufs_sync,
	ufs_vget,
	ufs_mountroot,
	ufs_badvfsop,		/* XXX - swapvp */
};

/*
 * Mount table.
 */
struct mount	*mounttab = NULL;

/*
 * ufs_mount system call
 */
static int
ufs_mount(vfsp, path, data)
	struct vfs *vfsp;
	char *path;
	caddr_t data;
{
	int error;
	dev_t dev;
	struct vnode *devvp;
	struct ufs_args args;
#if RISCOS
	struct vnode *vp = vfsp->vfs_vnodecovered;
#endif
	/*
	 * Get arguments
	 */
	error = copyin(data, (caddr_t)&args, sizeof (struct ufs_args));
#ifdef RISCOS
	if (error)
		error = EFAULT;
#endif /* RISCOS */
	if (error) {
		return (error);
	}
	if ((error = getmdev(args.fspec, &dev)) != 0)
		return (error);

#if RISCOS
	/*
	 * The NFS4.0 code violates the layering scheme and locks the
	 * inode in vfs.c:smount().  They do not completely close the
	 * timing window, and they introduce a possible kernel hang
	 * if the user gave the same [iv]node as the fspec and the
	 * directory.  For RISCOS, we lock later (here), thus enlarging
	 * the timing window, but removing the kernel hang.
	 * Also see "vfs.c":smount().
	 */
	if (vp->v_op == &ufs_vnodeops)
		ILOCK(VTOI(vp));
#endif /* RISCOS */
	/*
	 * make a special (device) vnode for the filesystem
	 */
	devvp = bdevvp(dev);

#ifndef RISCOS
	/*
	 * If the device is a tape, mount it read only
	 */
	if ((bdevsw[major(dev)].d_flags & B_TAPE) == B_TAPE)
		vfsp->vfs_flag |= VFS_RDONLY;
#endif
	/*
	 * Mount the filesystem.
	 */
	error = mountfs(&devvp, path, vfsp);
#if RISCOS
	/* See comment near "ILOCK" above */
	if (vp->v_op == &ufs_vnodeops)
		IUNLOCK(VTOI(vp));
#endif /* RISCOS */
	if (error) {
		VN_RELE(devvp);
	}
	return (error);
}

/*
 * Called by vfs_mountroot when ufs is going to be mounted as root
 */
static int
#if RISCOS
ufs_mountroot(vfsp, vpp, name, op)
	struct vfs *vfsp;
	struct vnode **vpp;
	char *name;
	mountrootop_t op;
#else
ufs_mountroot(vfsp, vpp, name)
	struct vfs *vfsp;
	struct vnode **vpp;
	char *name;
#endif
{
	register struct fs *fsp;
	register int error;
	static int ufsrootdone = 0;
	extern dev_t getblockdev();

#if RISCOS
	if (op == ROOT_MOUNT) {
		if (ufsrootdone++)
			return (EBUSY);
		if (rootdev == NODEV)
			return (ENODEV);
		*vpp = bdevvp(rootdev);
	} else if (op == ROOT_REMOUNT) {
		struct mount *mp = (struct mount *)vfsp->vfs_data;
#ifdef QUOTA
		iflush((*vpp)->v_rdev, mp->m_qinod);
#else /* !QUOTA */
		iflush((*vpp)->v_rdev);
#endif /* QUOTA */
		binval(*vpp);
		if (mp->m_bufp->b_un.b_fs)
			kmem_free(mp->m_bufp->b_un.b_fs->fs_csp[0],
			    (int)mp->m_bufp->b_un.b_fs->fs_cssize);
		vfsp->vfs_flag |= VFS_REMOUNT;
	} else if (op == ROOT_UNMOUNT) {
		if (error = vfs_lock(vfsp))
			return (error);
		dnlc_purge();
		xumount(vfsp);
		VFS_SYNC(vfsp);
		IUPDAT(VTOI(*vpp),1);
		error = unmount1(vfsp, 1);
		return (error);
	} else {
		panic("ufs_mountroot: bad op");
	}
	error = vfs_lock(vfsp);
	if (error)
		return error;
	error = mountfs(vpp, "/", vfsp);
	if (error) {
		vfs_unlock(vfsp);
		VN_RELE(*vpp);
		*vpp = (struct vnode *)0;
		return (error);
	}
	if (op == ROOT_MOUNT)
		error = vfs_add((struct vnode *)0, vfsp,
			(vfsp->vfs_flag & VFS_RDONLY) ? M_RDONLY : 0);
	vfs_unlock(vfsp);
	fsp = ((struct mount *)(vfsp->vfs_data))->m_bufp->b_un.b_fs;
	if (op == ROOT_MOUNT) {
		extern int	rootfs_dblks;
		rootfs_dblks =  fsbtodb(fsp, fsp->fs_size);
	}
#else /* !RISCOS */
	if (ufsrootdone++) {
		return (EBUSY);
	}
	*vpp = bdevvp(rootdev);
	error = mountfs(vpp, "/", vfsp);
	if (error) {
		VN_RELE(*vpp);
		*vpp = (struct vnode *)0;
		return (error);
	}
	error = vfs_add((struct vnode *)0, vfsp,
			(vfsp->vfs_flag & VFS_RDONLY) ? M_RDONLY : 0);
	if (error) {
		(void) unmount1(vfsp, 0);
		VN_RELE(*vpp);
		*vpp = (struct vnode *)0;
		return (error);
	}
	vfs_unlock(vfsp);
	fsp = ((struct mount *)(vfsp->vfs_data))->m_bufp->b_un.b_fs;
	inittodr(fsp->fs_time);
#endif /* RISCOS */
	return (0);
}

static int
mountfs(devvpp, path, vfsp)
	struct vnode **devvpp;
	char *path;
	struct vfs *vfsp;
{
	register struct fs *fsp;
	register struct mount *mp = 0;
	register struct buf *bp = 0;
	struct buf *tp = 0;
	int error;
	int blks;
	caddr_t space;
	int i;
	int size;
	u_int len;
	static int initdone = 0;
	int needclose = 0;

	if (!initdone) {
		ihinit();
		initdone = 1;
	}
	/*
	 * Open block device mounted on.
	 * When bio is fixed for vnodes this can all be vnode operations
	 */
	error = VOP_OPEN(devvpp,
	    (vfsp->vfs_flag & VFS_RDONLY) ? FREAD : FREAD|FWRITE, u.u_cred);
	if (error) {
		return (error);
	}
	needclose = 1;
	/*
	 * read in superblock
	 */
	tp = bread(*devvpp, SBLOCK, SBSIZE);
	if (tp->b_flags & B_ERROR) {
		goto out;
	}
	/*
	 * check for dev already mounted on
	 */
	for (mp = mounttab; mp != NULL; mp = mp->m_nxt) {
		if (mp->m_bufp != 0 && (*devvpp)->v_rdev == mp->m_dev) {
			if (vfsp->vfs_flag & VFS_REMOUNT) {
				bp = mp->m_bufp;
#if RISCOS
				/*
				 * The NFS4.0 code does not support
				 * remounting, in general.  It only
				 * permits remounting from readonly to
				 * read/write, and therefore it assumes
				 * that the data in the incore struct fs
				 * matches what is on disk.
				 *
				 * We must support remounting root, in
				 * which case the disk contents may have
				 * changed (if, for example, we did an
				 * fsck on /dev/root).  To get things
				 * right, we must update the incore buf
				 * with the latest disk data.
				 */
				if (vfsp == rootvfs)
					bcopy((caddr_t)tp->b_un.b_addr,
					      (caddr_t)bp->b_un.b_addr,
					      (u_int)tp->b_un.b_fs->fs_sbsize);
#endif /* RISCOS */
				goto modify_now;
			} else {
				mp = 0;
				error = EBUSY;
				needclose = 0;
				goto out;
			}
		}
	}

	/*
	 * SUN: If this is a remount request and we didn't spot the mounted
	 * device, then we're in error here....
	 * MIPS: I'm not sure if this will ever happen.  I think SUN just
	 * wanted to close the bug.  What's a few more bytes?
	 */
	if (vfsp->vfs_flag & VFS_REMOUNT) {
		vfsp->vfs_flag &= ~VFS_REMOUNT;
		cmn_err(CE_WARN, "mountfs: illegal remount request");
		error = EINVAL;
		goto out;
	}

	/*
	 * find empty mount table entry
	 */
	for (mp = mounttab; mp != NULL; mp = mp->m_nxt) {
		if (mp->m_bufp == 0)
			goto found;
	}

	/*
	 * get new mount table entry
	 */
	mp = (struct mount *)kmem_zalloc(sizeof (struct mount));
	if (mp == 0) {
		error = EMFILE;		/* needs translation */
		goto out;
	}
	mp->m_nxt = mounttab;
	mounttab = mp;
found:
	vfsp->vfs_data = (caddr_t)mp;
	mp->m_vfsp = vfsp;
	mp->m_bufp = tp;	/* just to reserve this slot */
	mp->m_dev = NODEV;
	mp->m_devvp = *devvpp;
	fsp = tp->b_un.b_fs;
	if (fsp->fs_magic != FS_MAGIC ||
#ifdef RISCOS
	    fsp->fs_bsize > FSMAXBSIZE ||
#else
	    fsp->fs_bsize > MAXBSIZE ||
#endif
	    fsp->fs_bsize < sizeof (struct fs)) {
		error = EINVAL;	/* also needs translation */
		goto out;
	}
	/*
	 * Copy the super block into a buffer in it's native size.
	 */
	bp = geteblk((int)fsp->fs_sbsize);
	mp->m_bufp = bp;
	bcopy((caddr_t)tp->b_un.b_addr, (caddr_t)bp->b_un.b_addr,
	   (u_int)fsp->fs_sbsize);
	brelse(tp);
	tp = 0;
modify_now:
	fsp = bp->b_un.b_fs;

#ifdef RISCOS
	/*
	 * Check for dirty file system.
	 *
	 * File systems get marked MOUNT when mounted and !MOUNT when 
	 * unmounted.  If the FS being mounted is already marked MOUNT, 
	 * then the system must have crashed.  So mark it !CLEAN.
	 */
	if ( (fsp->fs_clean&UFS_MOUNT) && !(vfsp->vfs_flag & VFS_REMOUNT) )
		fsp->fs_clean &= ~UFS_CLEAN;

	/*
	 * Only root and readonly file systems can be mounted when !CLEAN.
	 */
	if ( ((fsp->fs_clean&UFS_CLEAN) != UFS_CLEAN) &&
	     ((vfsp->vfs_flag&VFS_RDONLY) != VFS_RDONLY) &&
	     (vfsp != rootvfs) ) {
		u.u_error = error = ENOSPC;
		goto out;
	}
	fsp->fs_clean |= UFS_MOUNT;
#endif /* RISCOS */

	/*
	 * Curently we only allow a remount to change from
	 * read-only to read-write.
	 */
	if (vfsp->vfs_flag & VFS_RDONLY) {
		if (vfsp->vfs_flag & VFS_REMOUNT) {
			printf ("mountfs: can't remount ro\n");
			error = EINVAL;
			goto out;
		}
		fsp->fs_ronly = 1;
	} else {
		fsp->fs_fmod = 1;
		fsp->fs_ronly = 0;
		if (vfsp->vfs_flag & VFS_REMOUNT)
		{
			brelse(tp);
			vfsp->vfs_flag &= ~VFS_REMOUNT;
#if RISCOS
			/* We want to reinitialize fs_csp, so keep going! */
			tp = 0;
#else
			return (0);
#endif /* RISCOS */
		}
	}
	vfsp->vfs_bsize = fsp->fs_bsize;
#if MIPS_LOCAL
        /* Bring all cylinders groups in-core and lock them */
        if (lock_cgs_incore) {
            register int i;
            register struct buf *bp1;
            register struct cg *cgp;
            dev_t       dev = mp->m_devvp->v_rdev;

            for (i = 0; i < fsp->fs_ncg; i++) {
                bp1 = bread(mp->m_devvp,
                        fsbtodb(fsp, cgtod(fsp, i)), (int)fsp->fs_cgsize);
                bp1->b_flags |= B_LOCKED;
                cgp = bp1->b_un.b_cg;
                brelse(bp1);
            }
        }
#endif MIPS_LOCAL

	/*
	 * Read in cyl group info
	 */
	blks = howmany(fsp->fs_cssize, fsp->fs_fsize);
#ifdef RISCOS
	space = kmem_alloc ((int)fsp->fs_cssize);
#else
	space = wmemall(vmemall, (int)fsp->fs_cssize);
#endif
	if (space == 0) {
		error = ENOMEM;
		goto out;
	}
	for (i = 0; i < blks; i += fsp->fs_frag) {
		size = fsp->fs_bsize;
		if (i + fsp->fs_frag > blks)
			size = (blks - i) * fsp->fs_fsize;
		tp = bread(mp->m_devvp, fsbtodb(fsp, fsp->fs_csaddr+i), size);
		if (tp->b_flags & B_ERROR) {
#ifdef RISCOS
			kmem_free(space, (int)fsp->fs_cssize);
#else
			wmemfree(space, (int)fsp->fs_cssize);
#endif
			goto out;
		}
		bcopy((caddr_t)tp->b_un.b_addr, space, (u_int)size);
		fsp->fs_csp[fragstoblks(fsp, i)] = (struct csum *)space;
		space += size;
		brelse(tp);
		tp = 0;
	}
	mp->m_dev = mp->m_devvp->v_rdev;
	vfsp->vfs_fsid.val[0] = (long)mp->m_dev;
	vfsp->vfs_fsid.val[1] = MOUNT_UFS;
	(void) copystr(path, fsp->fs_fsmnt, sizeof (fsp->fs_fsmnt) - 1, &len);
	bzero(fsp->fs_fsmnt + len, sizeof (fsp->fs_fsmnt) - len);
#if RISCOS
	/* We must set the clock after reading the disk, and before the sync */
	if (vfsp == rootvfs)
		clkset(fsp->fs_time);
	if (fsp->fs_fmod) {
		fsp->fs_fmod = 0;
		fsp->fs_time = time.tv_sec;
		sbupdate(mp);	/* Flush updated MOUNT/CLEAN bits to disk */
	}
#endif /* RISCOS */
	return (0);
out:
	if (error == 0)
		error = EIO;
	if (mp)
		mp->m_bufp = 0;
	if (bp)
		brelse(bp);
	if (tp)
		brelse(tp);
	if (needclose) {
		(void) VOP_CLOSE(*devvpp, (vfsp->vfs_flag & VFS_RDONLY) ?
		    FREAD : FREAD|FWRITE, 1, u.u_cred);
		binval(*devvpp);
	}
	return (error);
}

/*
 * The System V Interface Definition requires a "umount" operation
 * which takes a device pathname as an argument.  This requires this
 * to be a system call.
 */
umount(uap)
	struct a {
		char	*fspec;
	} *uap;
{
	register struct mount *mp;
	dev_t dev;

	if (!suser())
		return;

	if ((u.u_error = getmdev(uap->fspec, &dev)) != 0)
		return;

	if ((mp = getmp(dev)) == NULL) {
		u.u_error = EINVAL;
		return;
	}

	dounmount(mp->m_vfsp);
}

/*
 * vfs operations
 */
static int
ufs_unmount(vfsp)
	struct vfs *vfsp;
{

	return (unmount1(vfsp, 0));
}

static int
unmount1(vfsp, forcibly)
	register struct vfs *vfsp;
	int forcibly;
{
	dev_t dev;
	register struct mount *mp;
	register struct fs *fs;
	register int stillopen;
	int flag;

	mp = (struct mount *)vfsp->vfs_data;
	dev = mp->m_dev;
#ifdef QUOTA
	if ((stillopen = iflush(dev, mp->m_qinod)) < 0 && !forcibly)
#else
	if ((stillopen = iflush(dev)) < 0 && !forcibly)
#endif
		return (EBUSY);
#if RISCOS
	/* Why do they ignore forcibly? */
	if (forcibly)
		stillopen = 0;
#endif /* RISCOS */
	if (stillopen < 0)
		return (EBUSY);			/* XXX */
#ifdef QUOTA
	(void) closedq(mp);
	/*
	 * Here we have to iflush again to get rid of the quota inode.
	 * A drag, but it would be ugly to cheat, & this doesn't happen often
	 */
	(void) iflush(dev, (struct inode *)NULL);
#endif
	fs = mp->m_bufp->b_un.b_fs;
#ifdef RISCOS
	if (!fs->fs_ronly) {
		bflush(mp->m_devvp);
		fs->fs_clean &= ~UFS_MOUNT;
		fs->fs_fmod = 0;
		sbupdate(mp);
		bdevwait(dev);
	}
	kmem_free((caddr_t)fs->fs_csp[0], (int)fs->fs_cssize);
#else
	wmemfree((caddr_t)fs->fs_csp[0], (int)fs->fs_cssize);
#endif
	flag = !fs->fs_ronly;
	brelse(mp->m_bufp);
	mp->m_bufp = 0;
	mp->m_dev = 0;
	if (!stillopen) {
		register struct mount *mmp;

		(void) VOP_CLOSE(mp->m_devvp, flag, 1, u.u_cred);
#if RISCOS
		punmount(vfsp);
#endif /* RISCOS */
		binval(mp->m_devvp);
		VN_RELE(mp->m_devvp);
		mp->m_devvp = (struct vnode *)0;
		if (mp == mounttab) {
			mounttab = mp->m_nxt;
		} else {
			for (mmp = mounttab; mmp != NULL; mmp = mmp->m_nxt) {
				if (mmp->m_nxt == mp) {
					mmp->m_nxt = mp->m_nxt;
				}
			}
		}
		kmem_free((caddr_t)mp, sizeof (struct mount));
	}
#if RISCOS
	if (vfsp == rootvfs)
		bdevwait(NODEV);
#endif
	return (0);
}

/*
 * find root of ufs
 */
static int
ufs_root(vfsp, vpp)
	struct vfs *vfsp;
	struct vnode **vpp;
{
	register struct mount *mp;
	struct inode *ip;

	VFS_RECORD(vfsp, VS_ROOT, VS_CALL);

	mp = (struct mount *)vfsp->vfs_data;
	ip = iget(mp->m_dev, mp->m_bufp->b_un.b_fs, (ino_t)ROOTINO);
	if (ip == (struct inode *)0) {
		return (u.u_error);
	}
	IUNLOCK(ip);
	*vpp = ITOV(ip);
	return (0);
}

/*
 * Get file system statistics.
 */
static int
#if RISCOS
ufs_statfs(vfsp, sbp, vp)
	register struct vfs *vfsp;
	struct bsd43_statfs *sbp;
	struct vnode *vp;
#else
ufs_statfs(vfsp, sbp)
	register struct vfs *vfsp;
	struct bsd43_statfs *sbp;
#endif /* RISCOS */
{
	register struct fs *fsp;
#if RISCOS
	register struct vnode *devvp;
	register struct buf *bp;
	register int error;
	dev_t dev;
#endif /* RISCOS */

#if RISCOS
	if (vfsp == (struct vfs *)0) {
		/*
		 * File system not mounted.  The vnode pointer
		 * refers to a device from which the superblock
		 * must be read.
		 */
		if (vp == (struct vnode *)0 || vp->v_type != VBLK)
			return (EINVAL);
		dev = vp->v_rdev;
		devvp = bdevvp(dev);
		(void) (*bdevsw[bmajor(dev)].d_open)(dev, 0, OTYP_LYR);
		if (u.u_error)
			return (u.u_error);
		/*
		 * Read a BFS super block.
		 */
		bp = bread(devvp, SBLOCK, SBSIZE);
		if (bp->b_flags & B_ERROR)
			goto out;
		fsp = bp->b_un.b_fs;

		/*
		 * Check superblock validity.
		 */
		if (fsp->fs_magic != FS_MAGIC 
		||  fsp->fs_bsize > FSMAXBSIZE
		||  fsp->fs_bsize < sizeof (struct fs)) {
			error = EINVAL;
			goto out;
		}
	} else {
		VFS_RECORD(vfsp, VS_STATFS, VS_CALL);
		fsp = ((struct mount *)vfsp->vfs_data)->m_bufp->b_un.b_fs;
	}
#else
	VFS_RECORD(vfsp, VS_STATFS, VS_CALL);
	fsp = ((struct mount *)vfsp->vfs_data)->m_bufp->b_un.b_fs;
#endif /* RISCOS */
	if (fsp->fs_magic != FS_MAGIC)
		panic("ufs_statfs");
	sbp->f_bsize = fsp->fs_fsize;
	sbp->f_blocks = fsp->fs_dsize;
	sbp->f_bfree = fsp->fs_cstotal.cs_nbfree * fsp->fs_frag +
	    fsp->fs_cstotal.cs_nffree;
	/*
	 * avail = MAX(max_avail - used, 0)
	 */
	sbp->f_bavail = (fsp->fs_dsize * (100 - fsp->fs_minfree) / 100) -
	    (fsp->fs_dsize - sbp->f_bfree);
	/*
	 * inodes
	 */
	sbp->f_files =  fsp->fs_ncg * fsp->fs_ipg;
	sbp->f_ffree = fsp->fs_cstotal.cs_nifree;
	bcopy((caddr_t)fsp->fs_id, (caddr_t)&sbp->f_fsid, sizeof (fsid_t));
#if RISCOS
	sbp->f_frsize = fsp->fs_fsize;	/* f_frsize defined only in kernel */
	bcopy("bfs_fs", sbp->f_fname, sizeof(sbp->f_fname));
	bcopy("bfs_pack", sbp->f_fpack, sizeof(sbp->f_fpack));
out:
	if (vfsp == (struct vfs *)0) {
		(void) (*bdevsw[bmajor(dev)].d_close)(dev, 0, OTYP_LYR);
		brelse(bp);
		VN_RELE(devvp);
	}
#endif /* RISCOS */
	return (0);
}

/*
 * Flush any pending I/O to file system vfsp.
 * The update() routine will only flush *all* ufs files.
 */
/*ARGSUSED*/
static int
ufs_sync(vfsp)
	struct vfs *vfsp;
{

	update();
	return (0);
}

sbupdate(mp)
	struct mount *mp;
{
	register struct fs *fs = mp->m_bufp->b_un.b_fs;
	register struct buf *bp;
	int blks;
	caddr_t space;
	int i, size;

	bp = getblk(mp->m_devvp, SBLOCK, (int)fs->fs_sbsize);
	bcopy((caddr_t)fs, bp->b_un.b_addr, (u_int)fs->fs_sbsize);
	bwrite(bp);
	blks = howmany(fs->fs_cssize, fs->fs_fsize);
	space = (caddr_t)fs->fs_csp[0];
	for (i = 0; i < blks; i += fs->fs_frag) {
		size = fs->fs_bsize;
		if (i + fs->fs_frag > blks)
			size = (blks - i) * fs->fs_fsize;
		bp = getblk(mp->m_devvp, (daddr_t)fsbtodb(fs, fs->fs_csaddr+i),
		    size);
		bcopy(space, bp->b_un.b_addr, (u_int)size);
		space += size;
		bwrite(bp);
	}
}

/*
 * Common code for mount and umount.
 * Check that the user's argument is a reasonable
 * thing on which to mount, and return the device number if so.
 */
static int
getmdev(fspec, pdev)
	char *fspec;
	dev_t *pdev;
{
	register int error;
	struct vnode *vp;

	/*
	 * Get the device to be mounted
	 */
	error = lookupname(fspec, UIO_USERSPACE, FOLLOW_LINK,
	    (struct vnode **)0, &vp);
	if (error) {
		if (u.u_error == ENOENT)
			return (ENODEV);	/* needs translation */
		return (error);
	}
	if (vp->v_type != VBLK) {
		VN_RELE(vp);
		return (ENOTBLK);
	}
	*pdev = vp->v_rdev;
	VN_RELE(vp);
#ifdef RISCOS
	if (major(*pdev) >= bdevcnt)
#else
	if (major(*pdev) >= nblkdev)
#endif
		return (ENXIO);
	return (0);
}

static int
ufs_vget(vfsp, vpp, fidp)
	struct vfs *vfsp;
	struct vnode **vpp;
	struct fid *fidp;
{
	register struct ufid *ufid;
	register struct inode *ip;
	register struct mount *mp;

	mp = (struct mount *)vfsp->vfs_data;
	ufid = (struct ufid *)fidp;
	ip = iget(mp->m_dev, mp->m_bufp->b_un.b_fs, ufid->ufid_ino);
	if (ip == NULL) {
		*vpp = NULL;
		return (0);
	}
	if (ip->i_gen != ufid->ufid_gen) {
		idrop(ip);
		*vpp = NULL;
		return (0);
	}
	IUNLOCK(ip);
	*vpp = ITOV(ip);
	if ((ip->i_mode & ISVTX) && !(ip->i_mode & (IEXEC | IFDIR))) {
		(*vpp)->v_flag |= VISSWAP;
	}
	return (0);
}

static int
ufs_badvfsop()
{

	return (EINVAL);
}
