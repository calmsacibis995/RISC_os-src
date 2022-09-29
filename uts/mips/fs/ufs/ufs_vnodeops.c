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
#ident	"$Header: ufs_vnodeops.c,v 1.6.1.7.1.3.1.5 90/12/20 18:02:25 beacker Exp $"
/*
 * @(#)ufs_vnodeops.c 2.6 88/08/02 4.0NFSSRC; Sun Microsystems, Inc.
 * @(#)(sys_inode.c, ufs_syscalls.c) 7.1 6/5/86 from UCB
 *
 * Original includes:
 * param.h systm.h user.h buf.h vfs.h vfs_stat.h vnode.h proc.h file.h uio.h
 * conf.h kernel.h cmap.h ../ufs/fs.h ../ufs/inode.h  ../ufs/mount.h
 * ../ufs/fsdir.h ifdef QUOTA ../ufs/quota.h endif dirent.h
 *
 * NOTE: dirent.h must be after ufs/fsdir to get the proper definition of DIRSIZ
 * ../specfs/fifo.h ../krpc/lockmgr.h 
 */

#include "sys/types.h"
#include "sys/signal.h"		/* needed by user.h */
#include "sys/pcb.h"		/* needed by user.h */
#include "sys/user.h"
#include "sys/buf.h"
#include "sys/vfs.h"
#include "sys/vfs_stat.h"
#include "sys/file.h"
#include "sys/stat.h"           /* symbols needed for enforcement-mode files */
#include "sys/vnode.h"
#include "sys/immu.h"		/* needed by proc.h */
#include "sys/sbd.h"		/* needed by proc.h */
#include "sys/region.h"		/* needed by proc.h */
#include "sys/proc.h"
#include "sys/uio.h"
/* #include "sys/stream.h"            Needed by conf.h */
#include "sys/conf.h"
#include "sys/errno.h"
#include "sys/systm.h"
#include "sys/fs/ufs_fs.h"
#include "sys/fs/ufs_inode.h"
#include "sys/fs/ufs_mount.h"
#include "sys/fs/ufs_fsdir.h"	/* XXX instead of h/dir.h ... */
#ifdef QUOTA
#include "sys/fs/ufs_quota.h"
#endif
#include "bsd43/sys/dirent.h"	/* must be AFTER <ufs/fsdir>! */
#include "sys/utsname.h"
#include "sys/flock.h"
#include "../klm/lockmgr.h"
#include "sys/kmem.h"

#define ISVDEV(t) ((t == VCHR) || (t == VBLK) || (t == VFIFO))
#define	MIN(a,b) (((a)<(b))?(a):(b))

#if RISCOS
extern int	_posix_chown_restricted;
extern int	_riscos_utime_owner;
extern int	_riscos_link_owner;
#define MANLOCK(ip) (((ip)->i_mode & (S_ENFMT|(S_IEXEC>>3))) == S_ENFMT)
#endif

/*
 * MAX_MRA should not be too large, two array of this size is declared on the
 * stack.
 */
#define MAX_MRA	32
extern int vnmra;

struct buf *breadma();

static	int ufs_open();
static	int ufs_close();
static	int ufs_rdwr();
static	int ufs_ioctl();
static	int ufs_select();
static	int ufs_getattr();
static	int ufs_setattr();
static	int ufs_access();
static	int ufs_lookup();
static	int ufs_create();
static	int ufs_remove();
static	int ufs_link();
static	int ufs_rename();
static	int ufs_mkdir();
static	int ufs_rmdir();
static	int ufs_readdir();
static	int ufs_symlink();
static	int ufs_readlink();
static	int ufs_fsync();
static	int ufs_inactive();
static	int ufs_bmap();
static	int ufs_strategy();
static	int ufs_bread();
static	int ufs_brelse();
static	int ufs_lockctl();
static	int ufs_fid();
static  int ufs_cmp();
static  int ufs_realvp();
static	int ufs_badop();

struct vnodeops ufs_vnodeops = {
	ufs_open,
	ufs_close,
	ufs_rdwr,
	ufs_ioctl,
	ufs_select,
	ufs_getattr,
	ufs_setattr,
	ufs_access,
	ufs_lookup,
	ufs_create,
	ufs_remove,
	ufs_link,
	ufs_rename,
	ufs_mkdir,
	ufs_rmdir,
	ufs_readdir,
	ufs_symlink,
	ufs_readlink,
	ufs_fsync,
	ufs_inactive,
	ufs_bmap,
	ufs_strategy,
	ufs_bread,
	ufs_brelse,
	ufs_lockctl,
	ufs_fid,
	ufs_badop,		/* dump */
	ufs_cmp,
	ufs_realvp,
};

/*ARGSUSED*/
static int
ufs_open(vpp, flag, cred)
	struct vnode **vpp;
	int flag;
	struct ucred *cred;
{
	register struct inode *ip;
        int error;

	VFS_RECORD((*vpp)->v_vfsp, VS_OPEN, VS_CALL);
	ip = VTOI(*vpp);

	error = (((*vpp)->v_filocks != NULL) && MANLOCK(ip)) ? EAGAIN : 0;
	return error;
}

/*ARGSUSED*/
static int
ufs_close(vp, flag, count, cred)
	struct vnode *vp;
	int flag;
	int count;
	struct ucred *cred;
{
	VFS_RECORD(vp->v_vfsp, VS_CLOSE, VS_CALL);
	cleanlocks(vp, USE_PID);
	return (0);
}

/*
 * read or write a vnode
 */
/*ARGSUSED*/
static int
ufs_rdwr(vp, uiop, rw, ioflag, cred)
	struct vnode *vp;
	struct uio *uiop;
	enum uio_rw rw;
	int ioflag;
	struct ucred *cred;
{
	register struct inode *ip;
	int error;
	int didlock;

	ip = VTOI(vp);
	if  ((ip->i_mode & IFMT) == IFREG) {
		if (MANLOCK(ip)) {
		  error = ufs_chklock (vp, uiop, rw, cred);
		  if (error) return error;
		}

		didlock = 1;
		ILOCK(ip);
		if ((ioflag & IO_APPEND) != 0 && (rw == UIO_WRITE)) {
			/*
			 * In append mode start at end of file after locking it.
			 */
			uiop->uio_offset = ip->i_size;
		}
	} else {
		didlock = 0;
	}

	error = rwip(ip, uiop, rw, ioflag);
	ITIMES(ip);

	if (didlock)
		IUNLOCK(ip);

	return (error);
}

/*
 * Don't cache write blocks to files with the sticky bit set.
 * Used to keep swap files from blowing the page cache on a server.
 */
int stickyhack = 1;

/*
 * rwip does the real work of read or write requests for ufs.
 */
static int
rwip(ip, uio, rw, ioflag)
	register struct inode *ip;
	register struct uio *uio;
	enum uio_rw rw;
	int ioflag;
{
	struct vnode *devvp;
	struct buf *bp;
	struct fs *fs;
	daddr_t lbn, bn;
	register int n, on, type;
	int size;
	long bsize;
#ifndef RISCOS  /* All filesystems support big offsets, mem is not different */
	extern int mem_no;
#endif
	int error = 0;
	int iupdat_flag = 0;
	u_short execmask = (IEXEC | (IEXEC >> 3) | (IEXEC >> 6));
	int nmra = ((vnmra > MAX_MRA) ? MAX_MRA : vnmra);
	daddr_t	rablock[MAX_MRA];
	int	rasize[MAX_MRA];

	if (rw != UIO_READ && rw != UIO_WRITE)
		panic("rwip");
	type = ip->i_mode&IFMT;
	if (type != IFREG && type != IFDIR && type != IFLNK)
		panic("rwip type");
	if ((uio->uio_offset < 0 || (uio->uio_offset + uio->uio_resid) < 0))
		return (EINVAL);
	if (uio->uio_resid == 0)
		return (0);

	if (rw == UIO_WRITE) {
#ifndef RISCOS /* In RISCOS, we check this in vno_rw */
		if (type == IFREG && uio->uio_offset + uio->uio_resid >
		    u.u_rlimit[RLIMIT_FSIZE].rlim_cur) {
			psignal(u.u_procp, SIGXFSZ);
			return (EFBIG);
		}
#endif /* !RISCOS */
	} else {
		ip->i_flag |= IACC;
	}
	devvp = ip->i_devvp;
	fs = ip->i_fs;
	bsize = fs->fs_bsize;
	u.u_error = 0;
	do {
		lbn = uio->uio_offset / bsize;
		on = uio->uio_offset % bsize;
		n = MIN((unsigned)(bsize - on), uio->uio_resid);
		if (rw == UIO_READ) {
			int diff = ip->i_size - uio->uio_offset;

			VFS_RECORD(ITOV(ip)->v_vfsp, VS_READ, VS_CALL);
			if (diff <= 0) {
				error = 0;
				goto out;
			}
			if (diff < n)
				n = diff;
		} else {
			VFS_RECORD(ITOV(ip)->v_vfsp, VS_WRITE, VS_CALL);
		}
		bn = fsbtodb(fs,
		    mbmap(ip, lbn, rw == UIO_WRITE ? B_WRITE: B_READ,
		      &nmra, rablock, rasize,
		      (int)(on+n), (ioflag & IO_SYNC ? &iupdat_flag : 0)));
		if (u.u_error || rw == UIO_WRITE && (long)bn<0) {
			error = u.u_error;
			goto out;
		}
		if (rw == UIO_WRITE && (uio->uio_offset + n > ip->i_size) &&
		   (type == IFDIR || type == IFREG || type == IFLNK)) {
			ip->i_size = uio->uio_offset + n;
			if (ioflag & IO_SYNC) {
				iupdat_flag = 1;
			}
		}
		size = blksize(fs, ip, lbn);
		if (rw == UIO_READ) {
			if ((long)bn<0) {
				bp = geteblk(size);
				clrbuf(bp);
			} else if (ip->i_lastr + 1 == lbn)
				bp = breadma(devvp, bn, size, nmra, rablock, rasize);
			else
				bp = bread(devvp, bn, size);
			ip->i_lastr = lbn;
		} else {
			int i, count;
#ifdef RISCOS
#if OLD_CODE
			int base_db = btodb(lbn << fs->fs_bshift);
#else
			/* the two calculations are not the same when
			 * uio->uio_offset < bsize.
			 * NBPC_I is machine independent.
			 */
			int base_db =
			  btodb(uio->uio_offset) & ~(NBPC_I/DEV_BSIZE - 1);
#endif /* OLD_CODE */
			register u_long nodeid = ip->i_number;

			/* use 'n' since 'size' would invalidate too many */
			count = howmany(n, DEV_BSIZE);
			for (i = 0; i < count; i += NBPC_I/DEV_BSIZE)
				punhash(ITOV(ip), (i + base_db), nodeid);
#else
			extern struct cmap *mfind();

			count = howmany(size, DEV_BSIZE);
			for (i = 0; i < count; i += CLBYTES/DEV_BSIZE)
				if (mfind(devvp, (daddr_t)(bn + i)))
					munhash(devvp, (daddr_t)(bn + i));
#endif /* RISCOS */
			if (n == bsize) 
				bp = getblk(devvp, bn, size);
			else
				bp = bread(devvp, bn, size);
		}
 		n = MIN(n, bp->b_bcount - bp->b_resid);
		if (bp->b_flags & B_ERROR) {
			error = EIO;
			brelse(bp);
			goto out;
		}
		u.u_error = uiomove(bp->b_un.b_addr+on, n, rw, uio);
		if ((ioflag & IO_SYNC) && (ip->i_mode & ISVTX) && stickyhack
		  && (ip->i_mode & execmask) == 0)
		      bp->b_flags |= B_NOCACHE;
		if (rw == UIO_READ) {
			if (n + on == bsize || uio->uio_offset == ip->i_size)
				bp->b_flags |= B_AGE;
			brelse(bp);
		} else {
			if ((ioflag & IO_SYNC) || (ip->i_mode&IFMT) == IFDIR)
				bwrite(bp);
			/* If we've written entire block OR if we've written
			 * a 4K segment at the end of the block to a new file
			 * then use async write.  Using bawrite on 4K seq write
			 * to existing file causes next write to stall since
			 * it must do a read first.
			 */
			else if ((n + on == bsize) && 
				 ((on == 0) ||
				  ((on == 4096) && (uio->uio_offset+n >= ip->i_size)))) {
					bp->b_flags |= B_AGE;
					bawrite(bp);
			} else
				bdwrite(bp);
			ip->i_flag |= IUPD|ICHG;
			if (u.u_ruid != 0) {
#ifdef RISCOS
/*
 * BSD turns off the set uid and set gid on writes.  This is not totally
 * correct for System V which overloads the group ID and exec bits for
 * enabling mandatory locking.  We don't turn off the set gid bit if
 * mandatory locking is enabled.
 */
				if (MANLOCK(ip)) {
					ip->i_mode &= ~ISUID;
				} else {
					ip->i_mode &= ~(ISUID|ISGID);
				}
#else
				ip->i_mode &= ~(ISUID|ISGID);
#endif /* RISCOS */
			}
		}
	} while (u.u_error == 0 && uio->uio_resid > 0 && n != 0);
	if (iupdat_flag) {
		iupdat(ip, 1);
	}
	if (error == 0)				/* XXX */
		error = u.u_error;		/* XXX */
out:
	return (error);
}

/*ARGSUSED*/
static int
ufs_ioctl(vp, com, data, flag, cred)
	struct vnode *vp;
	int com;
	caddr_t data;
	int flag;
	struct ucred *cred;
{
	VFS_RECORD(vp->v_vfsp, VS_IOCTL, VS_CALL);
	return (EINVAL);
}

/*ARGSUSED*/
static int
ufs_select(vp, which, cred)
	struct vnode *vp;
	int which;
	struct ucred *cred;
{
	VFS_RECORD(vp->v_vfsp, VS_SELECT, VS_CALL);
	return (EINVAL);
}

/*ARGSUSED*/
static int
ufs_getattr(vp, vap, cred)
	struct vnode *vp;
	register struct vattr *vap;
	struct ucred *cred;
{
	register struct inode *ip;

	VFS_RECORD(vp->v_vfsp, VS_GETATTR, VS_CALL);

	ip = VTOI(vp);
	/*
	 * Mark correct time in inode.
	 */
	ITIMES(ip);
	/*
	 * Copy from inode table.
	 */
#if RISCOS
	vap->va_fmode = -1; 		/* set to uninitialized state */
#endif
	vap->va_type = IFTOVT(ip->i_mode);
	vap->va_mode = ip->i_mode;
	vap->va_uid = ip->i_uid;
	vap->va_gid = ip->i_gid;
	vap->va_fsid = ip->i_dev;
	vap->va_nodeid = ip->i_number;
	vap->va_nlink = ip->i_nlink;
	vap->va_size = ip->i_size;
	vap->va_atime.tv_sec = ip->i_atime;
	vap->va_atime.tv_usec = 0;
	vap->va_mtime.tv_sec = ip->i_mtime;
	vap->va_mtime.tv_usec = 0;
	vap->va_ctime.tv_sec = ip->i_ctime;
	vap->va_ctime.tv_usec = 0;
	vap->va_rdev = ip->i_rdev;
	vap->va_blocks = ip->i_blocks;

	switch(ip->i_mode & IFMT) {

	case IFBLK:				/* was BLKDEV_IOSIZE */
	case IFCHR:
#ifdef RISCOS
		vap->va_blocksize = FSMAXBSIZE;
#else
		vap->va_blocksize = MAXBSIZE;
#endif
		break;

	default:
		vap->va_blocksize = vp->v_vfsp->vfs_bsize;
		break;
	}
	return (0);
}

static int
ufs_setattr(vp, vap, cred)
	register struct vnode *vp;
	register struct vattr *vap;
	struct ucred *cred;
{
	register struct inode *ip;
	int chtime = 0;
	int error = 0;

	VFS_RECORD(vp->v_vfsp, VS_SETATTR, VS_CALL);

	/*
	 * Cannot set these attributes
	 */
	if ((vap->va_nlink != -1) || (vap->va_blocksize != -1) ||
	    (vap->va_rdev != -1) || (vap->va_blocks != -1) ||
	    (vap->va_fsid != -1) || (vap->va_nodeid != -1) ||
	    ((int)vap->va_type != -1)) {
		return (EINVAL);
	}

	ip = VTOI(vp);
	ilock(ip);
	/*
	 * Change file access modes.  Must be owner or su.
	 */
	if (vap->va_mode != (u_short)-1) {
		error = OWNER(cred, ip);
		if (error)
			goto out;
		ip->i_mode &= IFMT;
		ip->i_mode |= vap->va_mode & ~IFMT;
		if (cred->cr_uid != 0) {
			if ((ip->i_mode & IFMT) != IFDIR)
				ip->i_mode &= ~ISVTX;
			if (!groupmember(ip->i_gid))
				ip->i_mode &= ~ISGID;
		}
		ip->i_flag |= ICHG;
	}
	/*
	 * To change file ownership, must be su.
	 * To change group ownership, must be su or owner and in target group.
	 * This is now enforced in chown1() below.
	 */
	if ((vap->va_uid != (uid_t)-1) || (vap->va_gid != (gid_t)-1)) {
		error = chown1(ip, vap->va_uid, vap->va_gid);
		if (error)
			goto out;
	}
	/*
	 * Truncate file.  Must have write permission and not be a directory.
	 */
	if (vap->va_size != (u_long)-1) {
		if ((ip->i_mode & IFMT) == IFDIR) {
			error = EISDIR;
			goto out;
		}
		if (vap->va_fmode != -1) {
		    /* make sure file was opened in write mode */
		    if (!(vap->va_fmode & FWRITE)) {
			error = EACCES;
			goto out;
		    }
		} else if ((error = iaccess(ip, IWRITE)) != 0) {
			goto out;
		}
		if ((error = itrunc(ip, vap->va_size)) != 0) {
			goto out;
		}
	}
	/*
	 * Change file access or modified times.
	 */
	if (vap->va_atime.tv_sec != -1) {
#if RISCOS
		/* We can do either SysV or BSD perm checking */
		error = OWNER(cred, ip);	/* First, do BSD semantics */
		if (_riscos_utime_owner != 0	/* if want SysV semantics  */
		    && error) {			/* and not owner or suser  */
			/* XXX This is gross, but it is going away in
			 * the next release, anyway.
			 */
			register struct a {
				char	*fname;
				time_t	*tptr;
			} *uap = (struct a *)u.u_ap;
			if (uap->tptr != NULL)
				error = EPERM;
			else
				error = iaccess(ip, IWRITE);
		}
#else
		error = OWNER(cred, ip);
#endif
		if (error)
			goto out;
		ip->i_atime = vap->va_atime.tv_sec;
		chtime++;
	}
	if (vap->va_mtime.tv_sec != -1) {
#if RISCOS
		/* We can do either SysV or BSD perm checking */
		error = OWNER(cred, ip);	/* First, do BSD semantics */
		if (_riscos_utime_owner != 0	/* if want SysV semantics  */
		    && error) {			/* and not owner or suser  */
			/* XXX This is gross, but it is going away in
			 * the next release, anyway.
			 */
			register struct a {
				char	*fname;
				time_t	*tptr;
			} *uap = (struct a *)u.u_ap;
			if (uap->tptr != NULL)
				error = EPERM;
			else
				error = iaccess(ip, IWRITE);
		}
#else
		error = OWNER(cred, ip);
#endif
		if (error)
			goto out;
		ip->i_mtime = vap->va_mtime.tv_sec;
		chtime++;
	}
	if (chtime) {
		ip->i_ctime = time.tv_sec;
		ip->i_flag |= IMOD;
	}
out:
	iupdat(ip, 1);			/* XXX - should be async for perf */
	iunlock(ip);
	return (error);
}

/*
 * Perform chown operation on inode ip;
 * inode must be locked prior to call.
 */
static int
chown1(ip, uid, gid)
	register struct inode *ip;
	register uid_t uid;
	register gid_t gid;
{
#ifdef QUOTA
	register long change;
#endif

	if (uid == (uid_t) -1)
		uid = ip->i_uid;
	if (gid == (gid_t) -1)
		gid = ip->i_gid;

	/*
	 * If:
	 *    1) not the owner of the file, or
	 *    2) trying to change the owner of the file, or
	 *    3) trying to change the group of the file to a group not in the
	 *	 process' group set,
	 * then must be super-user.
	 * Check super-user last, and use "suser", so that the accounting
	 * file's "used super-user privileges" flag is properly set.
	 */
#if RISCOS
	/*
	 * We allow emulation of either SysV or BSD (== POSIX) restrictions.
	 * This is controlled by "_posix_chown_restricted".  If 0, then
	 * use BSD (== POSIX) restrictions, as commented on above; else
	 * use SysV restrictions, which are:
	 *   1) if we are not the file owner, and
	 *   2) we are not the superuser
	 * then the call will fail.
	 *
	 * BSD BUG? Shouldn't we be using cred here?
	 */
	if (_posix_chown_restricted == 0) { /* BSD, POSIX semantics */
	  if ((u.u_uid != uid || uid != ip->i_uid || !groupmember(gid)) &&
	    !suser())
		return (EPERM);
	} else { /* SysV semantics */
	  if (u.u_uid != ip->i_uid && !suser())
		return (EPERM);
	}
#else
	if ((u.u_uid != uid || uid != ip->i_uid || !groupmember(gid)) &&
	    !suser())
		return (EPERM);
#endif

#ifdef QUOTA
	if (ip->i_uid == uid)		/* this just speeds things a little */
		change = 0;
	else
		change = ip->i_blocks;
	(void) chkdq(ip, -change, 1);
	(void) chkiq(VFSTOM(ip->i_vnode.v_vfsp), ip, ip->i_uid, 1);
	dqrele(ip->i_dquot);
#endif
	ip->i_uid = uid;
	ip->i_gid = gid;
	ip->i_flag |= ICHG;
	if (u.u_uid != 0)
#ifdef RISCOS
/*
 * We should not turn off the set gid bit if mandatory locking is enabled
 * (i.e. the set gid bit is on but the group exec bit is off.
 */
	  	if (MANLOCK(ip)) {
			ip->i_mode &= ~ISUID;
		} else {
			ip->i_mode &= ~(ISUID|ISGID);
		}
#else
		ip->i_mode &= ~(ISUID|ISGID);
#endif /* RISCOS */
#ifdef QUOTA
	ip->i_dquot = getinoquota(ip);
	(void) chkdq(ip, change, 1);
	(void) chkiq(VFSTOM(ip->i_vnode.v_vfsp), (struct inode *)NULL, uid, 1);
#endif
	return (0);
}

/*ARGSUSED*/
static int
ufs_access(vp, mode, cred)
	struct vnode *vp;
	int mode;
	struct ucred *cred;
{
	register struct inode *ip;
	int error;

	VFS_RECORD(vp->v_vfsp, VS_ACCESS, VS_CALL);

	ip = VTOI(vp);
	ILOCK(ip);
	error = iaccess(ip, mode);
	iunlock(ip);
	return (error);
}

/*ARGSUSED*/
static int
ufs_readlink(vp, uiop, cred)
	struct vnode *vp;
	struct uio *uiop;
	struct ucred *cred;
{
	register struct inode *ip;
	register int error;

	VFS_RECORD(vp->v_vfsp, VS_READLINK, VS_CALL);

	if (vp->v_type != VLNK)
		return (EINVAL);
	ip = VTOI(vp);
	error = rwip(ip, uiop, UIO_READ, 0);
	ITIMES(ip);
	return (error);
}

/*ARGSUSED*/
static int
ufs_fsync(vp, cred)
	struct vnode *vp;
	struct ucred *cred;
{
	register struct inode *ip;

	VFS_RECORD(vp->v_vfsp, VS_FSYNC, VS_CALL);

	ip = VTOI(vp);
	ilock(ip);
	syncip(ip);			/* do synchronous writes */
	iunlock(ip);

	return (0);
}

/*ARGSUSED*/
static int
ufs_inactive(vp, cred)
	struct vnode *vp;
	struct ucred *cred;
{

	VFS_RECORD(vp->v_vfsp, VS_INACTIVE, VS_CALL);

	iinactive(VTOI(vp));
	return (0);
}

/*
 * Unix file system operations having to do with directory manipulation.
 */
/*ARGSUSED*/
static int
ufs_lookup(dvp, nm, vpp, cred, pnp, flags)
	struct vnode *dvp;
	char *nm;
	struct vnode **vpp;
	struct ucred *cred;
	struct pathname *pnp;
	int flags;
{
	register struct inode *ip;
	struct inode *xip;
	register int error;

	VFS_RECORD(dvp->v_vfsp, VS_LOOKUP, VS_CALL);

	ip = VTOI(dvp);
	error = dirlook(ip, nm, &xip);
	ITIMES(ip);
	if (error == 0) {
		ip = xip;
		*vpp = ITOV(ip);
		if ((ip->i_mode & ISVTX) && !(ip->i_mode & (IEXEC | IFDIR)) &&
		    stickyhack) {
			(*vpp)->v_flag |= VISSWAP;
		}
		ITIMES(ip);
		iunlock(ip);
		/*
		 * If vnode is a device return special vnode instead
		 */
		if (ISVDEV((*vpp)->v_type)) {
			struct vnode *newvp;

			newvp = specvp(*vpp, (*vpp)->v_rdev, (*vpp)->v_type);
			VN_RELE(*vpp);
			*vpp = newvp;
		}
	}
	return (error);
}

static int
ufs_create(dvp, nm, vap, exclusive, mode, vpp, cred)
	struct vnode *dvp;
	char *nm;
	struct vattr *vap;
	enum vcexcl exclusive;
	int mode;
	struct vnode **vpp;
	struct ucred *cred;
{
	register int error;
	register struct inode *ip;
	struct inode *xip;

	VFS_RECORD(dvp->v_vfsp, VS_CREATE, VS_CALL);

	/*
	 * Can't create directories - use ufs_mkdir instead.
	 */
	if (vap->va_type == VDIR)
		return (EISDIR);
	xip = (struct inode *)0;
	ip = VTOI(dvp);
	if (cred->cr_uid != 0)		/* only allow root to set sticky bit */
		vap->va_mode &= ~VSVTX;
	error = direnter(ip, nm, DE_CREATE, (struct inode *)0,
	    (struct inode *)0, vap, &xip);
	ITIMES(ip);
	ip = xip;
	/*
	 * If file exists and this is a nonexclusive create,
	 * check for not directory and access permissions.
	 * If create/read-only an existing directory, allow it.
	 */
	if (error == EEXIST) {
		if (exclusive == NONEXCL) {
			if (((ip->i_mode & IFMT) == IFDIR) && (mode & IWRITE)) {
				error = EISDIR;
			} else if (mode) {
				error = iaccess(ip, mode);
			} else {
				error = 0;
			}
		}
		if (error) {
			iput(ip);
		} else if (((ip->i_mode&IFMT) == IFREG) && (vap->va_size == 0)) {
		         *vpp = ITOV(ip);
		         if (MANLOCK(ip) && ((*vpp)->v_filocks != NULL)) {
			        error = EAGAIN;
				iput(ip);
			 } else
			   /*
			   * Truncate regular files, if required
			   */
			   (void) itrunc(ip, (u_long)0);
		}
	}
	if (error) {
		return (error);
	}
	*vpp = ITOV(ip);
	ITIMES(ip);
	iunlock(ip);
	/*
	 * If vnode is a device return special vnode instead
	 */
	if (ISVDEV((*vpp)->v_type)) {
		struct vnode *newvp;

		newvp = specvp(*vpp, (*vpp)->v_rdev, (*vpp)->v_type);
		VN_RELE(*vpp);
		*vpp = newvp;
	}

	if (vap != (struct vattr *)0) {
		(void) VOP_GETATTR(*vpp, vap, cred);
	}
	return (error);
}

/*ARGSUSED*/
static int
ufs_remove(vp, nm, cred)
	struct vnode *vp;
	char *nm;
	struct ucred *cred;
{
	register int error;
	register struct inode *ip;

	VFS_RECORD(vp->v_vfsp, VS_REMOVE, VS_CALL);

	ip = VTOI(vp);
	error = dirremove(ip, nm, (struct inode *)0, 0);
	ITIMES(ip);
	return (error);
}

/*
 * Link a file or a directory.
 * If source is a directory, must be superuser.
 */
/*ARGSUSED*/
static int
ufs_link(vp, tdvp, tnm, cred)
	struct vnode *vp;
	register struct vnode *tdvp;
	char *tnm;
	struct ucred *cred;
{
	register struct inode *sip;
	register int error;
	struct vnode *realvp;

	if (VOP_REALVP(vp, &realvp) == 0) {
		vp = realvp;
	}

	VFS_RECORD(vp->v_vfsp, VS_LINK, VS_CALL);

	sip = VTOI(vp);
	if (((sip->i_mode & IFMT) == IFDIR) && !suser()) {
		return (EPERM);
	}
#if RISCOS
	if (_riscos_link_owner == 0 && (error = OWNER(cred, sip)))
		return(error);
#endif
	error = direnter(VTOI(tdvp), tnm, DE_LINK,
	    (struct inode *)0, sip, (struct vattr *)0, (struct inode **)0);
	ITIMES(sip);
	ITIMES(VTOI(tdvp));
	return (error);
}

/*
 * Rename a file or directory.
 * We are given the vnode and entry string of the source and the
 * vnode and entry string of the place we want to move the source to
 * (the target). The essential operation is:
 *	unlink(target);
 *	link(source, target);
 *	unlink(source);
 * but "atomically".  Can't do full commit without saving state in the inode
 * on disk, which isn't feasible at this time.  Best we can do is always
 * guarantee that the TARGET exists.
 */
/*ARGSUSED*/
static int
ufs_rename(sdvp, snm, tdvp, tnm, cred)
	struct vnode *sdvp;		/* old (source) parent vnode */
	char *snm;			/* old (source) entry name */
	struct vnode *tdvp;		/* new (target) parent vnode */
	char *tnm;			/* new (target) entry name */
	struct ucred *cred;
{
	struct inode *sip;		/* source inode */
	register struct inode *sdp;	/* old (source) parent inode */
	register struct inode *tdp;	/* new (target) parent inode */
	register int error;

	VFS_RECORD(sdvp->v_vfsp, VS_RENAME, VS_CALL);

	sdp = VTOI(sdvp);
	tdp = VTOI(tdvp);
	/*
	 * Make sure we can delete the source entry.
	 */
	error = iaccess(sdp, IWRITE);
	if (error) {
		return (error);
	}
	/*
	 * Look up inode of file we're supposed to rename.
	 */
	error = dirlook(sdp, snm, &sip);
	if (error) {
		return (error);
	}

	iunlock(sip);			/* unlock inode (it's held) */
	/*
	 * Check for renaming '.' or '..' or alias of '.'
	 */
	if ((strcmp(snm, ".") == 0) || (strcmp(snm, "..") == 0) ||
	    (sdp == sip)) {
		error = EINVAL;
		goto out;
	}
	/*
	 * Link source to the target.
	 */
	error = direnter(tdp, tnm, DE_RENAME,
	    sdp, sip, (struct vattr *)0, (struct inode **)0);
	if (error) {
		/*
		 * ESAME isn't really an error; it indicates that the
		 * operation should not be done because the source and target
		 * are the same file, but that no error should be reported.
		 */
		if (error == ESAME)
			error = 0;
		goto out;
	}

	/*
	 * Unlink the source.
	 * Remove the source entry.  Dirremove checks that the entry
	 * still reflects sip, and returns an error if it doesn't.
	 * If the entry has changed just forget about it.
	 * Release the source inode.
	 */
	error = dirremove(sdp, snm, sip, 0);
	if (error == ENOENT) {
		error = 0;
	} else if (error) {
		goto out;
	}

out:
	ITIMES(sdp);
	ITIMES(tdp);
	irele(sip);
	return (error);
}

/*ARGSUSED*/
static int
ufs_mkdir(dvp, nm, vap, vpp, cred)
	struct vnode *dvp;
	char *nm;
	register struct vattr *vap;
	struct vnode **vpp;
	struct ucred *cred;
{
	register struct inode *ip;
	struct inode *xip;
	register int error;

	VFS_RECORD(dvp->v_vfsp, VS_MKDIR, VS_CALL);

	ip = VTOI(dvp);
	error =
	    direnter(ip, nm, DE_CREATE,
		(struct inode *)0, (struct inode *)0, vap, &xip);
	ITIMES(ip);
	if (error == 0) {
		ip = xip;
		*vpp = ITOV(ip);
		ITIMES(ip);
		iunlock(ip);
	} else if (error == EEXIST) {
		iput(xip);
	}
	return (error);
}

/*ARGSUSED*/
static int
ufs_rmdir(vp, nm, cred)
	struct vnode *vp;
	char *nm;
	struct ucred *cred;
{
	register struct inode *ip;
	register int error;

	VFS_RECORD(vp->v_vfsp, VS_RMDIR, VS_CALL);

	ip = VTOI(vp);
	error = dirremove(ip, nm, (struct inode *)0, 1);
	ITIMES(ip);
	return (error);
}

/*ARGSUSED*/
static int
ufs_readdir(vp, uiop, cred)
	struct vnode *vp;
	struct uio *uiop;
	struct ucred *cred;
{
	register struct iovec *iovp;
	register struct inode *ip;
	register struct direct *idp;
#ifdef RISCOS
	register struct bsd43_dirent *odp;
#else
	register struct dirent *odp;
#endif
	register int incount;
	register int outcount = 0;
	register u_int offset;
	register u_int count, bytes_read;
	struct iovec t_iovec;
	struct uio t_uio;
	caddr_t outbuf;
	caddr_t inbuf;
	u_int bufsize;
	int error = 0;
	static caddr_t *dirbufp = NULL;

	VFS_RECORD(vp->v_vfsp, VS_READDIR, VS_CALL);

	ip = VTOI(vp);
	iovp = uiop->uio_iov;
	count = iovp->iov_len;

	/*
	 * Get space to change directory entries into fs independent format.
	 * Also, get space to buffer the data returned by rwip.
	 */
#ifdef RISCOS
	bufsize = count + sizeof (struct bsd43_dirent);
	outbuf = kmem_alloc(bufsize * 2);
	odp = (struct bsd43_dirent *)outbuf;
#else
	bufsize = count + sizeof (struct dirent);
	outbuf = kmem_alloc(bufsize * 2);
	odp = (struct dirent *)outbuf;
#endif
	iovp = &t_iovec;
	inbuf = (caddr_t)((u_int)outbuf + bufsize);

	/* Force offset to be valid (to guard against bogus lseek() values) */
	offset = uiop->uio_offset & ~(DIRBLKSIZ - 1);

nextblk:
	/*
	 * Setup a temporary uio structure to handle the directory information
	 * before reformatting.
	 */
	iovp->iov_len = count;
	iovp->iov_base = inbuf;
	t_uio.uio_iov = iovp;
	t_uio.uio_iovcnt = 1;
	t_uio.uio_segflg = UIO_SYSSPACE;
	t_uio.uio_offset = offset;
	t_uio.uio_fmode = uiop->uio_fmode;
	t_uio.uio_resid = uiop->uio_resid;

	if (error = rwip(ip, &t_uio, UIO_READ, 0))
		goto out;
	bytes_read = uiop->uio_resid - t_uio.uio_resid;

	incount = 0;
	idp = (struct direct *)inbuf;

	/* Transform to file-system independent format */
	while (incount < bytes_read) {
		extern char *strcpy();

		/* skip empty entries */
		if (idp->d_ino != 0 && offset >= uiop->uio_offset) {
			odp->d_fileno = idp->d_ino;
			odp->d_namlen = idp->d_namlen;
			(void) strcpy(odp->d_name, idp->d_name);
			odp->d_reclen = DIRSIZ(odp);
			odp->d_off = offset + idp->d_reclen;
			outcount += odp->d_reclen;
			/* Got as many bytes as requested, quit */
			if (outcount > count) {
				outcount -= odp->d_reclen;
				break;
			}
#ifdef RISCOS
			odp = (struct bsd43_dirent *)((int)odp + odp->d_reclen);
#else
			odp = (struct dirent *)((int)odp + odp->d_reclen);
#endif
		}
		incount += idp->d_reclen;
		offset += idp->d_reclen;
		idp = (struct direct *)((int)idp + idp->d_reclen);
	}

        /* Read whole block, but got no entries, read another if not eof */
        if (offset < ip->i_size && outcount == 0)
                goto nextblk;
 
	/* Copy out the entry data */
	if (error = uiomove(outbuf, outcount, UIO_READ, uiop))
		goto out;

	uiop->uio_offset = offset;
out:
	ITIMES(ip);
	kmem_free(outbuf, (bufsize * 2));
	return (error);
}

/*
 * Old form of the ufs_readdir op. Returns directory entries directly
 * from the disk in the 4.2 structure instead of the new sys/dirent.h
 * structure. This routine is called directly by the old getdirentries
 * system call when it discovers it is dealing with a ufs filesystem.
 * The reason for this mess is to avoid large performance penalties
 * that occur during conversion from the old format to the new and
 * back again.
 */

/*ARGSUSED*/
int
old_ufs_readdir(vp, uiop, cred)
	struct vnode *vp;
	register struct uio *uiop;
	struct ucred *cred;
{
	register struct iovec *iovp;
	register unsigned count;
	register struct inode *ip;
	int error;

	ip = VTOI(vp);
	iovp = uiop->uio_iov;
	count = iovp->iov_len;
	if ((uiop->uio_iovcnt != 1) || (count < DIRBLKSIZ) ||
	    (uiop->uio_offset & (DIRBLKSIZ -1)))
		return (EINVAL);
	count &= ~(DIRBLKSIZ - 1);
	uiop->uio_resid -= iovp->iov_len - count;
	iovp->iov_len = count;
	error = rwip(ip, uiop, UIO_READ, 0);
	ITIMES(ip);
	return (error);
}

/*ARGSUSED*/
static int
ufs_symlink(dvp, lnm, vap, tnm, cred)
	register struct vnode *dvp;
	char *lnm;
	struct vattr *vap;
	char *tnm;
	struct ucred *cred;
{
	struct inode *ip;
	register int error;

	VFS_RECORD(dvp->v_vfsp, VS_SYMLINK, VS_MISS);

	ip = (struct inode *)0;
	vap->va_type = VLNK;
	vap->va_rdev = 0;
	error = direnter(VTOI(dvp), lnm, DE_CREATE,
	    (struct inode *)0, (struct inode *)0, vap, &ip);
	if (error == 0) {
		error = rdwri(UIO_WRITE, ip, tnm, strlen(tnm),
		    (off_t)0, UIO_SYSSPACE, (int *)0);
		iput(ip);
	} else if (error == EEXIST) {
		iput(ip);
	}
	ITIMES(VTOI(dvp));
	return (error);
}

/*
 * Ufs specific routine used to do ufs io.
 */
int
rdwri(rw, ip, base, len, offset, seg, aresid)
	enum uio_rw rw;
	struct inode *ip;
	caddr_t base;
	int len;
	off_t offset;
	int seg;
	int *aresid;
{
	struct uio auio;
	struct iovec aiov;
	register int error;

	aiov.iov_base = base;
	aiov.iov_len = len;
	auio.uio_iov = &aiov;
	auio.uio_iovcnt = 1;
	auio.uio_offset = offset;
	auio.uio_seg = seg;
	auio.uio_resid = len;
	if (rw == UIO_READ)
		auio.uio_fmode = FREAD;
	else
		auio.uio_fmode = FWRITE;
	error = ufs_rdwr(ITOV(ip), &auio, rw, 0, u.u_cred);
	if (aresid) {
		*aresid = auio.uio_resid;
	} else if (auio.uio_resid) {
		error = EIO;
	}
	return (error);
}

static int
ufs_bmap(vp, lbn, vpp, bnp)
	struct vnode *vp;
	daddr_t lbn;
	struct vnode **vpp;
	daddr_t *bnp;
{
	register struct inode *ip;

	VFS_RECORD(vp->v_vfsp, VS_BMAP, VS_CALL);
	ip = VTOI(vp);
	if (vpp)
		*vpp = ip->i_devvp;
	if (bnp)
		*bnp = fsbtodb(ip->i_fs, bmap(ip, lbn, B_READ));
	return (0);
}

/*
 * read a logical block and return it in a buffer
 */
/*ARGSUSED*/
static int
ufs_bread(vp, lbn, bpp, sizep)
	struct vnode *vp;
	daddr_t lbn;
	struct buf **bpp;
	long *sizep;
{
	register struct inode *ip;
	register struct buf *bp;
	register daddr_t bn;
	register int size;
	daddr_t	rablock[MAX_MRA];
	int 	rasize[MAX_MRA];
	int	nmra = ((vnmra > MAX_MRA) ? MAX_MRA : vnmra);

	VFS_RECORD(vp->v_vfsp, VS_BREAD, VS_CALL);
	ip = VTOI(vp);
	size = blksize(ip->i_fs, ip, lbn);
	bn = fsbtodb(ip->i_fs, mbmap(ip, lbn, B_READ, &nmra, rablock, rasize));
	if ((long)bn < 0) {
		bp = geteblk(size);
		clrbuf(bp);
	} else if (ip->i_lastr + 1 == lbn) {
		bp = breadma(ip->i_devvp, bn, size, nmra, rablock, rasize);
	} else {
		bp = bread(ip->i_devvp, bn, size);
	}
	ip->i_lastr = lbn;
	ip->i_flag |= IACC;
	IMARK(ip);
	if (bp->b_flags & B_ERROR) {
		brelse(bp);
		return (EIO);
	} else {
		*bpp = bp;
		return (0);
	}
}

/*
 * release a block returned by ufs_bread
 */
/*ARGSUSED*/
static int
ufs_brelse(vp, bp)
	struct vnode *vp;
	struct buf *bp;
{
	VFS_RECORD(vp->v_vfsp, VS_BRELSE, VS_CALL);
	bp->b_flags |= B_AGE;
	bp->b_resid = 0;
	brelse(bp);
}

#ifdef RISCOS
/*
 * Enforce the record locking protocol on a regular file described by the vnode.
 */
int
ufs_chklock(vp, uiop, rw, cred)
	struct vnode *vp;
	struct uio *uiop;
        enum uio_rw rw;
	struct ucred *cred;
{
	register struct flock bf;
	register struct vattr file_vattr;
	register int retval, sleepwait;

	retval = VOP_GETATTR (vp, &file_vattr, cred);
	if (retval) return retval;

	bf.l_type = (rw == UIO_WRITE) ? F_WRLCK : F_RDLCK;
	bf.l_whence = 0;
	bf.l_start = uiop->uio_offset;
	bf.l_len = uiop->uio_resid;
	sleepwait = (uiop->uio_fmode & FNDELAY) ? 0 : 1;

	retval = reclock(vp, &bf, 0, bf.l_start, file_vattr.va_size,
	 sleepwait);
	if ((retval == 0) && (bf.l_type != F_UNLCK)) {
		retval = EAGAIN;
	}
	return retval;
}
#endif

static int
ufs_cmp(vp1, vp2)
	struct vnode *vp1, *vp2;
{
	VFS_RECORD(vp1->v_vfsp, VS_CMP, VS_CALL);
	return (vp1 == vp2);
}

/*ARGSUSED*/
static int
ufs_realvp(vp, vpp)
	struct vnode *vp;
	struct vnode **vpp;
{
	VFS_RECORD(vp->v_vfsp, VS_REALVP, VS_CALL);
	return (EINVAL);
}

static int
ufs_badop()
{
	panic("ufs_badop");
}

/*
 * Record-locking requests under SUNOS are passed to the local Lock-Manager
 * daemon BUT since risc/os implement's SVID madatory locking for local files
 * we hand it off to the procedures which implement honest-to-god locking.
 */
static int
ufs_lockctl(vp, ld, cmd, cred, clid)
	struct vnode *vp;
	struct flock *ld;
	int cmd;
	struct ucred *cred;
	int clid;
{
#ifdef RISCOS
	register struct vattr file_vattr;
	int sleepwait, retval;

	VFS_RECORD(vp->v_vfsp, VS_LOCKCTL, VS_CALL);

	retval = VOP_GETATTR (vp, &file_vattr, cred);
	if (retval) return retval;

	sleepwait = (cmd == F_SETLKW) ? 1 : 0;
	retval = reclock(vp, ld, cmd, ld->l_start, file_vattr.va_size, sleepwait);
	return retval;
#else
	lockhandle_t lh;
	struct fid *fidp;

	VFS_RECORD(vp->v_vfsp, VS_LOCKCTL, VS_CALL);

	/* Convert vnode into lockhandle-id. This is awfully like makefh() */
	if (VOP_FID(vp, &fidp) || fidp == NULL) {
		return (EINVAL);
	}
	bzero((caddr_t)&lh.lh_id, sizeof (lh.lh_id));	/* clear extra bytes */
	lh.lh_fsid.val[0] = vp->v_vfsp->vfs_fsid.val[0];
	lh.lh_fsid.val[1] = vp->v_vfsp->vfs_fsid.val[1];
	lh.lh_fid.fid_len = fidp->fid_len;
	bcopy(fidp->fid_data, lh.lh_fid.fid_data, fidp->fid_len);
	freefid(fidp);

	/* Add in vnode and server and call to common code */
	lh.lh_vp = vp;
	lh.lh_servername = hostname;
	return (klm_lockctl(&lh, ld, cmd, cred, clid));
#endif /* RISCOS */
}

static int
ufs_fid(vp, fidpp)
	struct vnode *vp;
	struct fid **fidpp;
{
	register struct ufid *ufid;

	VFS_RECORD(vp->v_vfsp, VS_FID, VS_CALL);

	ufid = (struct ufid *)kmem_zalloc(sizeof (struct ufid));
	ufid->ufid_len = sizeof(struct ufid) - (sizeof(struct fid) - MAXFIDSZ);
	ufid->ufid_ino = VTOI(vp)->i_number;
	ufid->ufid_gen = VTOI(vp)->i_gen;
	*fidpp = (struct fid *)ufid;
	return (0);
}

static int
ufs_strategy(bp)
	register struct buf *bp;
{
	int resid;

	VFS_RECORD(bp->b_vp->v_vfsp, VS_STRATEGY, VS_CALL);

	if ((bp->b_flags & B_READ) == B_READ) {
		bp->b_error = rdwri(UIO_READ, VTOI(bp->b_vp), bp->b_un.b_addr,
			(int) bp->b_bcount, (off_t) bp->b_blkno * DEV_BSIZE,
			UIO_SYSSPACE, &resid);
	} else {
		bp->b_error = rdwri(UIO_WRITE, VTOI(bp->b_vp), bp->b_un.b_addr,
			(int)bp->b_bcount, (off_t) bp->b_blkno * DEV_BSIZE,
			UIO_SYSSPACE, &resid);
	}
	bp->b_resid = resid;
	if (bp->b_error) {
		bp->b_flags |= B_ERROR;
	}
	iodone(bp);

}
