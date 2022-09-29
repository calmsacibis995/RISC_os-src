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
#ident	"$Header: vfs_syscalls.c,v 1.6.1.13.1.1.1.3 90/11/17 11:41:22 beacker Exp $"

/*@(#)vfs_syscalls.c	2.4 88/06/20 4.0NFSSRC SMI;  from SMI 2.17 87/01/17 */

/* Originally included param.h user.h file.h conf.h stat.h uio.h ioctl.h
 * tty.h vfs.h dirent.h pathname.h vnode.h ../ufs/inode.h ../ufs/fsdir.h
 */

#include "sys/types.h"
#include "sys/signal.h"		/* needed by user.h */
#include "sys/pcb.h"		/* needed by user.h */
#include "sys/user.h"
#include "sys/immu.h"		/* needed by proc.h */
#include "sys/sbd.h"		/* needed by proc.h */
#include "sys/region.h"		/* needed by proc.h */
#include "sys/proc.h"
#include "sys/uio.h"
#include "sys/var.h"
#include "sys/file.h"
#include "sys/cmn_err.h"
#include "sys/errno.h"
#include "sys/debug.h"
#include "sys/stat.h"
#include "bsd43/sys/stat.h"
#include "sys/vfs.h"
#include "sys/dirent.h"
#include "bsd43/sys/dirent.h"
#include "sys/pathname.h"
#include "sys/vnode.h"
#ifdef RISCOS
#include "sys/fs/nfs.h"
#endif
#include "sys/fs/ufs_inode.h"
#include "sys/fs/ufs_fsdir.h"
#include "sys/numips.h"
#include "sys/termio.h"
#include "sys/kmem.h"

extern	struct fileops vnodefops;

#if RISCOS
/* Disgusting hacks */
#undef r_val1
#define r_val1	r_reg.R_val1
#define fileNFILE ((struct file *)v.ve_file)

extern struct exportinfo *findexport();
#endif

/*
 * System call routines for operations on files other
 * than read, write and ioctl.  These calls manipulate
 * the per-process file table which references the
 * networkable version of normal UNIX inodes, called vnodes.
 *
 * Many operations take a pathname, which is read
 * into a kernel buffer by pn_get (see vfs_pathname.c).
 * After preparing arguments for an operation, a simple
 * operation proceeds:
 *
 *	error = lookupname(pname, seg, followlink, &dvp, &vp, &vattr)
 *
 * where pname is the pathname operated on, seg is the segment that the
 * pathname is in (UIO_USERSPACE or UIO_SYSSPACE), followlink specifies
 * whether to follow symbolic links, dvp is a pointer to the vnode that
 * represents the parent directory of vp, the pointer to the vnode
 * referenced by the pathname. vattr is a vattr structure which hold the
 * attributes of the final component. The lookupname routine fetches the
 * pathname string into an internal buffer using pn_get (vfs_pathname.c),
 * and iteratively running down each component of the path until the
 * the final vnode and/or it's parent are found. If either of the addresses
 * for dvp or vp are NULL, then it assumes that the caller is not interested
 * in that vnode. If the pointer to the vattr structure is NULL then attributes
 * are not returned. Once the vnode or its parent is found, then a vnode
 * operation (e.g. VOP_OPEN) may be applied to it.
 *
 * One important point is that the operations on vnode's are atomic, so that
 * vnode's are never locked at this level.  Vnode locking occurs
 * at lower levels either on this or a remote machine. Also permission
 * checking is generally done by the specific filesystem. The only
 * checks done by the vnode layer is checks involving file types
 * (e.g. VREG, VDIR etc.), since this is static over the life of the vnode.
 *
 */

/*
 * Change current working directory (".").
 */
chdir(uap)
	register struct a {
		char *dirnamep;
	} *uap;
{
	struct vnode *vp;

	u.u_error = chdirec(uap->dirnamep, &vp);
	if (u.u_error == 0) {
		VN_RELE(u.u_cdir);
		u.u_cdir = vp;
	}
}

/*
 * Change notion of root ("/") directory.
 */
chroot(uap)
	register struct a {
		char *dirnamep;
	} *uap;
{
	struct vnode *vp;

	if (!suser())
		return;

	u.u_error = chdirec(uap->dirnamep, &vp);
	if (u.u_error == 0) {
		if (u.u_rdir != (struct vnode *)0)
			VN_RELE(u.u_rdir);
		u.u_rdir = vp;
	}
}

/*
 * Common code for chdir and chroot.
 * Translate the pathname and insist that it
 * is a directory to which we have execute access.
 * If it is replace u.u_[cr]dir with new vnode.
 */
chdirec(dirnamep, vpp)
	char *dirnamep;
	struct vnode **vpp;
{
	struct vnode *vp;		/* new directory vnode */
	register int error;

	error =
	    lookupname(dirnamep, UIO_USERSPACE, FOLLOW_LINK,
		(struct vnode **)0, &vp);
	if (error)
		return (error);
	if (vp->v_type != VDIR) {
		error = ENOTDIR;
	} else {
		error = VOP_ACCESS(vp, VEXEC, u.u_cred);
	}
	if (error) {
		VN_RELE(vp);
	} else {
		*vpp = vp;
	}
	return (error);
}

/*
 * Open system call.
 */
open(uap)
	register struct a {
		char *fnamep;
		int fmode;
		int cmode;
	} *uap;
{

	u.u_error = copen(uap->fnamep, uap->fmode - FOPEN, uap->cmode);
}

#ifdef RISCOS
/*
 * External NFS file handle Open system call. Used to support cooperative
 * locking between kernel implmented locking and the user lock manager process.
 * This is a VERY restrictive open system call. It assumes that a standard
 * open call was performed to create the file on the server. This interface
 * only supports the lock manager to allow it to obtain a fileid given an
 * external NFS handle, so that subsequent fcntl operations can be performed
 * on the file. Typically the "open" operation would have been performed
 * via NFS.
 */
nfsfh_open(uap)
	register struct a {
		char *externalfh;
		int fmode, cmode;
	} *uap;
{
	fhandle_t fileh;
	fsid_t fsid;
	struct fid export_fid;
	register struct file *fp;
	struct vnode *vp, *fhtovp();
	struct exportinfo *exi;
	int i;
	int cmode = uap->cmode;
	int filemode = (uap->fmode - FOPEN);

	u.u_error = copyin (uap->externalfh, &fileh, sizeof(fhandle_t));
	if (u.u_error) {
		u.u_error = EFAULT;
		return;
	}

	if (! suser())
		return;
	/*
	 * allocate a user file descriptor and file table entry.
	 */
	fp = falloc();
	if (fp == NULL) { /* u.u_error is set in falloc */	  
		return;
	}

	/* save index for possible later error recovery; A weird side-effect */
	i = u.u_r.r_val1;

	/*
	 * Extract the file-system-id fsid, and the file-id fid
	 * from the nfs handle passed in.
	 */
	fsid = fileh.fh_fsid;

	/* extract the exported file number from the network handle */
	export_fid.fid_len = fileh.fh_xlen;
	bcopy (fileh.fh_xdata, export_fid.fid_data,
	       MIN(fileh.fh_xlen, NFS_FHMAXDATA));

	/* lookup the exported filesystem information */
	exi = findexport (&fsid, &export_fid);
	vp = fhtovp(&fileh, exi);

	if (vp == (struct vnode *)NULL)
	  u.u_error = ENOENT;
	else {
	  if ((vp->v_type == VREG) || (vp->v_type == VDIR)) { /* ok */
		fp->f_flag = filemode & FMASK;
		fp->f_type = DTYPE_VNODE;
		fp->f_data = (caddr_t)vp;
		fp->f_ops = &vnodefops;
	  }
	  else { /* wrong vnode type for this particular open */
		u.u_ofile[i] = NULL;
		unfalloc(fp);
		u.u_error = EINVAL;
	  }
	}
	return;
}
#endif /* RISCOS */

/*
 * Creat system call.
 */
creat(uap)
	register struct a {
		char *fnamep;
		int cmode;
	} *uap;
{

	u.u_error = copen(uap->fnamep, FWRITE|FCREAT|FTRUNC, uap->cmode);
}

/*
 * Common code for open, creat.
 */
copen(pnamep, filemode, createmode)
	char *pnamep;
	int filemode;
	int createmode;
{
	register struct file *fp;
	struct vnode *vp;
	register int error;
	register int i;

	/*
	 * allocate a user file descriptor and file table entry.
	 */
	fp = falloc();
	if (fp == NULL)
		return(u.u_error);
	i = u.u_r.r_val1;		/* this is bullshit */
	/*
	 * open the vnode.
	 */
	error =
	    vn_open(pnamep, UIO_USERSPACE,
		filemode, ((createmode & 07777) & ~u.u_cmask), &vp);

	/*
	 * If there was an error, deallocate the file descriptor.
	 * Otherwise fill in the file table entry to point to the vnode.
	 */
	if (error) {
		u.u_ofile[i] = NULL;
#if RISCOS
		unfalloc(fp);
#else /* RISCOS */
		crfree(fp->f_cred);
		fp->f_count = 0;
#endif /* RISCOS */
	} else {
		/* Mask off the FNDELAY bit in the BSD world because
		 * they really want the O_NDELAY bit to affect the open() only
		 * where other worlds let this propagate to read().  In the
		 * BSD world, a fcntl() must be used to set this bit for read().
		 */
		if (BSD_SYSCALL)
			fp->f_flag = filemode & FMASK & ~FNDELAY;
		else
			fp->f_flag = filemode & FMASK;
		fp->f_type = DTYPE_VNODE;
		fp->f_data = (caddr_t)vp;
		fp->f_ops = &vnodefops;

		/*
		 * For named pipes, the FNDELAY flag must propagate to the
		 * rdwr layer, for backward compatibility.
		 */
		if (vp->v_type == VFIFO)
			fp->f_flag |= (filemode & FNDELAY);
#if RISCOS
		/* 
		 * see if we should acquire a controlling terminal
		 *
		 * In BSD land, we should acquire it if p_pgrp == 0 or
		 * 	p_pgrp == p_pid, 
		 * in SysV land, we should acquire it if p_pgrp == p_pid.
		 * in POSIX land, we should acquire it if p_pgrp == p_pid,
		 * 	but only if FNOCTTY is not set.
		 */
		if (SYSV_SYSCALL || (POSIX_SYSCALL && !(filemode&FNOCTTY))) {
		   	if (vp->v_type == VCHR && 
			    u.u_procp->p_pid == u.u_procp->p_pgrp &&
			    u.u_procp->p_ttyvp == NULL) {
				set_controlling_tty(fp);
			}
		} else if (BSD_SYSCALL) {
			if (vp->v_type == VCHR &&
			    ((u.u_procp->p_pgrp == 0) ||
			     (u.u_procp->p_pid == u.u_procp->p_pgrp)) &&
			    u.u_procp->p_ttyvp == NULL) {
				set_controlling_tty(fp);
			}
		}
#endif /* RISCOS */
	}
	return(error);
}

#if RISCOS
/*
 * Acquire a controlling terminal for the current process.
 */
set_controlling_tty(fp)
struct file *fp;
{
	int	cur_pgrp;
	int	cur_mask;
	struct	termio termio;
	int	rval1;
	int	is_tty;
	register struct vnode *vp = (struct vnode *)fp->f_data;
       
	ASSERT(vp != NULL);
	ASSERT(vp->v_type == VCHR);
	rval1 = u.u_r.r_val1;
	cur_mask = u.u_procp->p_hold;
	u.u_procp->p_hold |= (sigmask(SIGTTIN) | sigmask(SIGTTOU));
	if (iioctl(vp, TIOCGPGRP, (char *) &cur_pgrp, FWRITE | FREAD)) {
		u.u_error = 0;
		u.u_procp->p_hold = cur_mask;
		u.u_r.r_val1 = rval1;
		return; /* does not support process groups */
	};
	if (cur_pgrp != 0 &&
	    cur_pgrp != u.u_procp->p_pid) {
		u.u_procp->p_hold = cur_mask;
		u.u_r.r_val1 = rval1;
		return; /* terminal already taken by another group */
	};
	if (iioctl(vp, TCGETA, (char *) &termio, FWRITE | FREAD)) {
		u.u_error = 0;
		u.u_procp->p_hold = cur_mask;
		u.u_r.r_val1 = rval1;
		return; /* not a terminal */
	};
	is_tty = 0;
	if (iioctl(vp, TIOC_GISTTY, &is_tty, FWRITE | FREAD)) {
		u.u_error = 0;
		is_tty = 1; 	/* if device supports TCGETA, but */
				/* TIOC_GISTTY, assume it is a 	*/
				/* terminal 			*/
	};
	if (! is_tty) {
		u.u_procp->p_hold = cur_mask;
		u.u_r.r_val1 = rval1;
		return; /* device is not a terminal */
	};
	if (u.u_procp->p_pgrp == 0) {
		/* bsd process becomes process group leader */
		u.u_procp->p_pgrp = u.u_procp->p_pid;
		u.u_procp->p_jcpgrp = u.u_procp->p_pgrp;
	};
	if (u.u_procp->p_jcpgrp == 0) 
		u.u_procp->p_jcpgrp = u.u_procp->p_pgrp;
	else if (u.u_procp->p_pgrp != u.u_procp->p_jcpgrp) {
		cmn_err(CE_WARN,
		   "set_controlling_tty: fixed process %d pgrp (%d) != jcpgrp (%d)",
			u.u_procp->p_pid,u.u_procp->p_pgrp,
			u.u_procp->p_jcpgrp);
		u.u_procp->p_jcpgrp = u.u_procp->p_pgrp;
	};
	cur_pgrp = u.u_procp->p_jcpgrp;
	if (iioctl(vp, TIOCSPGRP, (char *)&cur_pgrp, FWRITE | FREAD)) {
		u.u_error = 0;
		u.u_procp->p_hold = cur_mask;
		u.u_r.r_val1 = rval1;
		return; /* could not acquire controlling terminal */
	};
	u.u_procp->p_hold = cur_mask;
	u.u_r.r_val1 = rval1;

	/*
	 * Record the controlling terminal, retaining the inode
	 * while needed.
	 */
	ASSERT(u.u_procp->p_ttyvp == NULL);
	FP_HOLD(fp);
	ASSERT(fp->f_count != 0);
	u.u_procp->p_ttyfp = fp;
	u.u_procp->p_ttyvp = vp;
	/* 
	 * Temporarily kludge old user fields, to keep ps happy
	 * (there should be no references to these fields in the 
	 *  kernel)
	 */
	u.u_ttyvp = vp;
	u.u_ttyd = vp->v_rdev;
	u.u_ttyp = &u.u_procp->p_jcpgrp;
}

clr_controlling_tty()
{
    register struct file *fp;

    if (u.u_procp->p_ttyvp) {
	fp = u.u_procp->p_ttyfp;
	u.u_procp->p_ttyfp = NULL;
	u.u_procp->p_ttyvp = NULL;
	FP_RELE(fp);
    }
}
#endif /* RISCOS */

/*
 * Create a special (or regular) file.
 */
mknod(uap)
	register struct a {
		char		*pnamep;
		int		fmode;
		int		dev;
	} *uap;
{
	struct vnode *vp;
	struct vattr vattr;

	/* map 0 type into regular file, as other versions of UNIX do */
	if ((uap->fmode & IFMT) == 0)
		uap->fmode |= IFREG;

	/* Must be super-user unless making a FIFO node */
	if (((uap->fmode & IFMT) != IFIFO) && !suser())
		return;

	/*
	 * Setup desired attributes and vn_create the file.
	 */
	vattr_null(&vattr);
	vattr.va_type = MFTOVT(uap->fmode);
	vattr.va_mode = (uap->fmode & 07777) & ~u.u_cmask;

	switch (vattr.va_type) {
	case VDIR:
		u.u_error = EISDIR;	/* Can't mknod directories: use mkdir */
		return;

	case VBAD:
	case VCHR:
	case VBLK:
		vattr.va_rdev = uap->dev;
		break;

	case VNON:
		u.u_error = EINVAL;
		return;

	default:
		break;
	}

	u.u_error = vn_create(uap->pnamep, UIO_USERSPACE, &vattr, EXCL, 0, &vp);
	if (u.u_error == 0)
		VN_RELE(vp);
}

/*
 * Make a directory.
 */
mkdir(uap)
	struct a {
		char	*dirnamep;
		int	dmode;
	} *uap;
{
	struct vnode *vp;
	struct vattr vattr;

	vattr_null(&vattr);
	vattr.va_type = VDIR;
	vattr.va_mode = (uap->dmode & 0777) & ~u.u_cmask;

	u.u_error = vn_create(uap->dirnamep, UIO_USERSPACE, &vattr, EXCL, 0, &vp);
	if (u.u_error == 0)
		VN_RELE(vp);
}

/*
 * make a hard link
 */
link(uap)
	register struct a {
		char	*from;
		char	*to;
	} *uap;
{

	u.u_error = vn_link(uap->from, uap->to, UIO_USERSPACE);
}

/*
 * rename or move an existing file
 */
rename(uap)
	register struct a {
		char	*from;
		char	*to;
	} *uap;
{

	u.u_error = vn_rename(uap->from, uap->to, UIO_USERSPACE);
}

/*
 * Create a symbolic link.
 * Similar to link or rename except target
 * name is passed as string argument, not
 * converted to vnode reference.
 */
symlink(uap)
	register struct a {
		char	*target;
		char	*linkname;
	} *uap;
{
	struct vnode *dvp;
	struct vattr vattr;
	struct pathname tpn;
	struct pathname lpn;

	u.u_error = pn_get(uap->linkname, UIO_USERSPACE, &lpn);
	if (u.u_error)
		return;
	u.u_error = lookuppn(&lpn, NO_FOLLOW, &dvp, (struct vnode **)0);
	if (u.u_error) {
		pn_free(&lpn);
		return;
	}
	if (dvp->v_vfsp->vfs_flag & VFS_RDONLY) {
		u.u_error = EROFS;
		goto out;
	}
	u.u_error = pn_get(uap->target, UIO_USERSPACE, &tpn);
	vattr_null(&vattr);
#ifdef RISCOS
	vattr.va_mode = (SYSV_SYSCALL) ? 0 : 0777;
#else
	vattr.va_mode = 0777;
#endif
	if (u.u_error == 0) {
		u.u_error =
		   VOP_SYMLINK(dvp, lpn.pn_path, &vattr, tpn.pn_path, u.u_cred);
		pn_free(&tpn);
	}
out:
	pn_free(&lpn);
	VN_RELE(dvp);
}

/*
 * Unlink (i.e. delete) a file.
 */
unlink(uap)
	struct a {
		char	*pnamep;
	} *uap;
{

	u.u_error = vn_remove(uap->pnamep, UIO_USERSPACE, FILE);
}

/*
 * Remove a directory.
 */
rmdir(uap)
	struct a {
		char	*dnamep;
	} *uap;
{

	u.u_error = vn_remove(uap->dnamep, UIO_USERSPACE, DIRECTORY);
}


/*
 * get directory entries in a file system independent format
 * This is the old system call.  It remains in 4.0 for binary compatibility.
 * It will disappear in 5-?.0. It returns directory entries in the
 * old file-system independent directory entry format. This structure was
 * formerly defined in /usr/include/sys/dir.h. It is now no longer available to
 * user source files. It is defined only by struct direct below.
 *	This system call is superseded by the new getdents() call, which
 * returns directory entries in the new filesystem-independent format
 * given in /usr/include/sys/dirent.h and /usr/include/sys/dir.h. The
 * vn_readdir interface has also been modified to return entries in this
 * format.
 */
#if RISCOS
/*
 * Ha, and you thought that was bad....
 * In RISC/os we have to support backward compatibility with the SysV
 * struct as well as the old bsd struct.  The SysV getdents() syscall
 * has been routed through this routine, not through the getdents()
 * routine below, since we have to do translation.  Also, notice that
 * we must not call the old_getdirentries unless we really want the
 * old BSD format, not the SysV format.  Then convert to the appropriate
 * output format, and voila!
 *
 * Forgive me, Father, ....
 */
#endif /* RISCOS */

getdirentries(uap)
	register struct a {
		int	fd;
		char	*buf;
		unsigned count;
		long	*basep;
	} *uap;
{

#define DIRECTSIZ(dp)  \
    (((sizeof (struct direct) - (MAXNAMLEN+1) + ((dp)->d_namlen+1)) + 3) & ~3)


	struct file *fp;
	struct uio auio;
	struct iovec aiov;
	register caddr_t ibuf, obuf, ibufend, obufend;
#if RISCOS
	register struct bsd43_dirent *idp;
	register struct direct *odp;
	register struct dirent *sodp;
#else
	register struct dirent *idp;
	register struct direct *odp;
#endif
	int outcount;
	off_t offset;
	extern char *strcpy();
	extern struct vnodeops ufs_vnodeops;

	u.u_error = getvnodefp(uap->fd, &fp);
	if (u.u_error)
		return;
	if ((fp->f_flag & FREAD) == 0) {
		u.u_error = EBADF;
		return;
	}

#if RISCOS
	/* These next two only apply to the bsd syscall */
	if (BSD_SYSCALL) {
#endif
	if (uap->count < sizeof(struct direct)) {
		u.u_error = EINVAL;
		return;
	}

/* #ifdef UFS */
	/* Performance hack, use old dir. stuff for local ufs filesystems */
	if (((struct vnode *)fp->f_data)->v_op == &ufs_vnodeops)
	{
		old_getdirentries(uap, fp);
		return;
	}
/* #endif */
#if RISCOS
	} /* if BSD_SYSCALL */
	else /* if SYSV_SYSCALL */ {
		if (((struct vnode *)fp->f_data)->v_type != VDIR) {
			u.u_error = ENOTDIR;
			return;
		}
	}
#endif
	/* Allocate temporary space for format conversion */
	ibuf = kmem_alloc(uap->count);
#if RISCOS
	obuf = kmem_alloc(uap->count +
			  MAX(sizeof(struct direct), sizeof(struct dirent)));
#else
	obuf = kmem_alloc(uap->count + sizeof (struct direct));
#endif
	aiov.iov_base = ibuf;
	aiov.iov_len = uap->count;
	auio.uio_iov = &aiov;
	auio.uio_iovcnt = 1;
	auio.uio_offset = fp->f_offset;
	auio.uio_segflg = UIO_SYSSPACE;
#if RISCOS
	auio.uio_fmode = fp->f_flag;
#endif
	auio.uio_resid = uap->count;

	u.u_error = VOP_READDIR((struct vnode *)fp->f_data, &auio, fp->f_cred);
	if (u.u_error)
		goto out;
	offset = auio.uio_offset;

	/* Convert entries from new back to old format */

#if RISCOS
	/* We handle the BSD_SYSCALL case the same as the NFS4.0 code,
	 * except that we must reference the ibp with a bsd43_ prefix.
	 * We handle the SYSV_SYSCALL case similarly, but we fill
	 * a SysV struct dirent rather than a BSD struct direct.
	 */
	idp = (struct bsd43_dirent *) ibuf;
	odp = (struct direct *) obuf;
	sodp = (struct dirent *) obuf;
	ibufend = ibuf + (uap->count - auio.uio_resid);
	obufend = obuf + uap->count;
	if (BSD_SYSCALL) {
	  for ( ; (caddr_t)idp < ibufend;
	     idp = (struct bsd43_dirent *) ((caddr_t)idp  + idp->d_reclen),
	     odp = (struct direct *) ((caddr_t) odp  + odp->d_reclen)) {
		odp->d_ino = idp->d_fileno;
		odp->d_namlen = idp->d_namlen;
		(void) strcpy(odp->d_name, idp->d_name);
		odp->d_reclen = DIRECTSIZ(odp);
		if ((caddr_t) odp + odp->d_reclen > obufend)
			break;
	  }  /* for */
	  outcount = (caddr_t) odp - obuf;
	} else /* if SYSV_SYSCALL */ {
	  for ( ; (caddr_t)idp < ibufend;
	     idp = (struct bsd43_dirent *) ((caddr_t) idp  + idp->d_reclen),
	     sodp = (struct dirent *) ((caddr_t)sodp  + sodp->d_reclen)) {
		sodp->d_ino = idp->d_fileno;
		sodp->d_off = idp->d_off;
		sodp->d_reclen = DIRENTSIZE(idp->d_namlen);
		bcopy(idp->d_name, sodp->d_name, idp->d_namlen);
		sodp->d_name[idp->d_namlen] = '\0';
		if ((caddr_t) sodp + sodp->d_reclen > obufend) {
			/*
			 * Our output buffer is full.  If we didn't
			 * have room for even 1 direntry, error return.
			 */
			if ((caddr_t)sodp == obuf) {
				u.u_error = EINVAL;
				goto out;
			}
			break;
		}
	  }  /* for */
	  outcount = (caddr_t) sodp - obuf;
	} /* if BSD_SYSCALL */
#else /* !RISCOS */
	for (idp = (struct dirent *) ibuf, odp = (struct direct *) obuf,
	     ibufend = ibuf + (uap->count - auio.uio_resid),
	     obufend = obuf + uap->count;
	     (caddr_t)idp < ibufend;
	     idp = (struct dirent *) ((caddr_t) idp  + idp->d_reclen),
	     odp = (struct direct *) ((caddr_t) odp  + odp->d_reclen)) {
		odp->d_ino = idp->d_fileno;
		odp->d_namlen = idp->d_namlen;
		(void) strcpy(odp->d_name, idp->d_name);
		odp->d_reclen = DIRECTSIZ(odp);
		if ((caddr_t) odp + odp->d_reclen > obufend)
			break;
	}
	outcount = (caddr_t) odp - obuf;
#endif /* RISCOS */
	aiov.iov_base = uap->buf;
	aiov.iov_len = outcount;
	auio.uio_iov = &aiov;
	auio.uio_iovcnt = 1;
	auio.uio_offset = fp->f_offset;
	auio.uio_segflg = UIO_USERSPACE;
	auio.uio_resid = outcount;

	if (u.u_error = uiomove(obuf, outcount, UIO_READ, &auio))
		goto out;

#if RISCOS
	if (BSD_SYSCALL &&
	    copyout((caddr_t)&fp->f_offset, (caddr_t)uap->basep, sizeof (long)))
		u.u_error = EFAULT;
#else
	u.u_error =
	    copyout((caddr_t)&fp->f_offset, (caddr_t)uap->basep, sizeof (long));
#endif
	u.u_r.r_val1 = outcount - auio.uio_resid;
	fp->f_offset = offset;
out:
	kmem_free(ibuf, uap->count);
	kmem_free(obuf, uap->count + sizeof (struct direct));
}

/* #ifdef UFS */
/*
 * old form of the getdirentries system call. This is called by
 * getdirentries() when it discovers it is dealing with a ufs filesystem.
 * This routine in turn calls the old ufs_readdir routine directly. This
 * ugly hack is to avoid a big performance penalty that occurs with local ufs
 * filesystems because the directory entries have to be converted to new
 * format by ufs_readdir and then back to the old format by getdirentries.
 * By using this calling method no conversion takes place and no penalties
 * are incurred. Unsightly, but it will go away soon, we hope!
 */
old_getdirentries(uap, fp)
	register struct a {
		int	fd;
		char	*buf;
		unsigned count;
		long	*basep;
	} *uap;
	struct file *fp;
{
	struct uio auio;
	struct iovec aiov;
	extern int old_ufs_readdir();

	aiov.iov_base = uap->buf;
	aiov.iov_len = uap->count;
	auio.uio_iov = &aiov;
	auio.uio_iovcnt = 1;
	auio.uio_offset = fp->f_offset;
	auio.uio_segflg = UIO_USERSPACE;
#if RISCOS
	auio.uio_fmode = fp->f_flag;
#endif
	auio.uio_resid = uap->count;
	u.u_error = old_ufs_readdir((struct vnode *)fp->f_data, &auio, fp->f_cred);
	if (u.u_error)
		return;
	u.u_error =
	    copyout((caddr_t)&fp->f_offset, (caddr_t)uap->basep, sizeof (long));
#if RISCOS
	if (u.u_error)
		u.u_error = EFAULT;
#endif
	u.u_r.r_val1 = uap->count - auio.uio_resid;
	fp->f_offset = auio.uio_offset;
}
/* #endif */
/*
 * Seek on file.  Only hard operation
 * is seek relative to end which must
 * apply to vnode for current file size.
 * 
 * Note: lseek(0, 0, L_XTND) costs much more than it did before.
 */
lseek(uap)
	register struct a {
		int	fd;
		off_t	off;
		int	sbase;
	} *uap;
{
	struct file *fp;

	u.u_error = getvnodefp(uap->fd, &fp);
	if (u.u_error) {
		if (u.u_error == EINVAL)
			u.u_error = ESPIPE;	/* be compatible */
		return;
	}

	if (((struct vnode *)fp->f_data)->v_type == VFIFO) {
		u.u_error = ESPIPE;
		return;
	}

	switch (uap->sbase) {

	case L_INCR:
	  	if (POSIX_SYSCALL && (fp->f_offset + uap->off < 0)) {
		  	if (((struct vnode *)fp->f_data)->v_type == VREG ||
		  	    ((struct vnode *)fp->f_data)->v_type == VDIR ||
		  	    ((struct vnode *)fp->f_data)->v_type == VLNK) {
			  	u.u_error = EINVAL;
				return;
			}
		}
		fp->f_offset += uap->off;
		break;

	case L_XTND: {
		struct vattr vattr;

		u.u_error =
		    VOP_GETATTR((struct vnode *)fp->f_data, &vattr, u.u_cred);
		if (u.u_error)
			return;
	  	if (POSIX_SYSCALL && (uap->off + vattr.va_size < 0)) {
		  	if (((struct vnode *)fp->f_data)->v_type == VREG ||
		  	    ((struct vnode *)fp->f_data)->v_type == VDIR ||
		  	    ((struct vnode *)fp->f_data)->v_type == VLNK) {
			  	u.u_error = EINVAL;
				return;
			}
		}
		fp->f_offset = uap->off + vattr.va_size;
		break;
	}

	case L_SET:
	  	if (POSIX_SYSCALL && (uap->off < 0)) {
		  	if (((struct vnode *)fp->f_data)->v_type == VREG ||
		  	    ((struct vnode *)fp->f_data)->v_type == VDIR ||
		  	    ((struct vnode *)fp->f_data)->v_type == VLNK) {
			  	u.u_error = EINVAL;
				return;
			}
		}
		fp->f_offset = uap->off;
		break;

	default:
		u.u_error = EINVAL;
		if (SYSV_SYSCALL)
			psignal(u.u_procp, SIGSYS);
	}
	u.u_r.r_off = fp->f_offset;
}

/*
 * Determine accessibility of file, by
 * reading its attributes and then checking
 * against our protection policy.
 */
access(uap)
	register struct a {
		char	*fname;
		int	fmode;
	} *uap;
{
	struct vnode *vp;
	register u_short mode;
	register int svuid;
	register int svgid;

	/*
	 * check for legal access mode
	 */
	if ((SYSV_SYSCALL || POSIX_SYSCALL) &&
	    (uap->fmode & ~(R_OK|W_OK|X_OK|F_OK))) {
		u.u_error = EINVAL;
		return;
	}
	/*
	 * Lookup file
	 */
	u.u_error =
	    lookupname(uap->fname, UIO_USERSPACE, FOLLOW_LINK,
		(struct vnode **)0, &vp);
	if (u.u_error)
		return;

	/*
	 * Use the real uid and gid and check access
	 */
	svuid = u.u_uid;
	svgid = u.u_gid;
	u.u_uid = u.u_ruid;
	u.u_gid = u.u_rgid;
#if RISCOS
	/* I think this is useless, since neither the SysV nor the 
	 * NFS4.0 code touch u.u_ruid, but ....
	 */
	u.u_ruid = svuid;
	u.u_rgid = svgid;
#endif /* RISCOS */

	mode = 0;
	/*
	 * fmode == 0 means only check for exist
	 */
	if (uap->fmode) {
		if (uap->fmode & R_OK)
			mode |= VREAD;
		if (uap->fmode & W_OK) {
#ifdef RISCOS	/* Sun bug fix: 1006269 & 1008383 */
			if (isrofile(vp)) {
#else
			if(vp->v_vfsp->vfs_flag & VFS_RDONLY) {
#endif
				u.u_error = EROFS;
				goto out;
			}
			mode |= VWRITE;
		}
		if (uap->fmode & X_OK)
			mode |= VEXEC;
		u.u_error = VOP_ACCESS(vp, mode, u.u_cred);
	}

	/*
	 * release the vnode and restore the uid and gid
	 */
out:
	VN_RELE(vp);
#if RISCOS
	u.u_ruid = u.u_uid;
	u.u_rgid = u.u_gid;
#endif /* RISCOS */
	u.u_uid = svuid;
	u.u_gid = svgid;
}

/*
 * Get attributes from file or file descriptor.
 * Argument says whether to follow links, and is
 * passed through in flags.
 */
stat(uap)
	caddr_t uap;
{

	u.u_error = stat1(uap, FOLLOW_LINK);
}

lstat(uap)
	caddr_t uap;
{

	u.u_error = stat1(uap, NO_FOLLOW);
}

stat1(uap0, follow)
	caddr_t uap0;
	enum symfollow follow;
{
	struct vnode *vp;
#if RISCOS
	struct bsd43_stat sb;
#else
	struct stat sb;
#endif
	register int error;
	register struct a {
		char	*fname;
#if RISCOS
		struct	bsd43_stat *ub;
#else
		struct	stat *ub;
#endif
	} *uap = (struct a *)uap0;

	error =
	    lookupname(uap->fname, UIO_USERSPACE, follow,
		(struct vnode **)0, &vp);
	if (error)
		return (error);
	error = vno_stat(vp, &sb);
	VN_RELE(vp);
	if (error)
		return (error);
#if RISCOS
	return (stat2(&sb, uap->ub));
#else
	return (copyout((caddr_t)&sb, (caddr_t)uap->ub, sizeof (sb)));
#endif
}

#if RISCOS
/*
 * Send the results to the user in the form they desire
 */
stat2(bsd_statp, user_statp)
	register struct bsd43_stat *bsd_statp;
	register struct bsd43_stat *user_statp;
{
	struct stat sv_sb;

	if (BSD_SYSCALL)
		return (bsd_copyout((caddr_t)bsd_statp,	(caddr_t)user_statp,
				    sizeof (*bsd_statp)));
	else if (SYSV_SYSCALL || POSIX_SYSCALL) {
		sv_sb.st_dev	= bsd_statp->st_dev;
		sv_sb.st_ino	= bsd_statp->st_ino;
		sv_sb.st_mode	= bsd_statp->st_mode;
		sv_sb.st_nlink	= bsd_statp->st_nlink;
		sv_sb.st_uid	= bsd_statp->st_uid;
		sv_sb.st_gid	= bsd_statp->st_gid;
		sv_sb.st_rdev	= bsd_statp->st_rdev;
		sv_sb.st_size	= bsd_statp->st_size;
		sv_sb.st_atime	= bsd_statp->st_atime;
		sv_sb.st_mtime	= bsd_statp->st_mtime;
		sv_sb.st_ctime	= bsd_statp->st_ctime;
		return (bsd_copyout((caddr_t)&sv_sb, (caddr_t)user_statp,
				    sizeof (sv_sb)));
	}
}
#endif

#if RISCOS
/* 
 * This is in kern_descrip.c in the NFS4.0 release, but it is moved
 * here for simplicity for RISC/os.
 */
fstat()
{
	register struct file *fp;
	register struct a {
		int	fdes;
		struct stat *sb;
	} *uap;
	struct	bsd43_stat ub;

	uap = (struct a *)u.u_ap;
	GETF(fp, uap->fdes);
	switch (fp->f_type) {

	case DTYPE_VNODE:
		u.u_error = vno_stat((struct vnode *)fp->f_data, &ub);
		break;

	case DTYPE_SOCKET:
		u.u_error = soo_stat((struct socket *)fp->f_data, &ub);
		break;

	default:
		panic("fstat");
		/*NOTREACHED*/
	}
	if (u.u_error == 0)
		stat2(&ub, uap->sb);
}
#endif

/*
 * Read contents of symbolic link.
 */
readlink(uap)
	register struct a {
		char	*name;
		char	*buf;
		int	count;
	} *uap;
{
	struct vnode *vp;
	struct iovec aiov;
	struct uio auio;

	u.u_error =
	    lookupname(uap->name, UIO_USERSPACE, NO_FOLLOW,
		(struct vnode **)0, &vp);
	if (u.u_error)
		return;
	if (vp->v_type != VLNK) {
		u.u_error = EINVAL;
		goto out;
	}
	aiov.iov_base = uap->buf;
	aiov.iov_len = uap->count;
	auio.uio_iov = &aiov;
	auio.uio_iovcnt = 1;
	auio.uio_offset = 0;
	auio.uio_segflg = UIO_USERSPACE;
#if RISCOS
	auio.uio_fmode = 0; /* XXX */
#endif
	auio.uio_resid = uap->count;
	u.u_error = VOP_READLINK(vp, &auio, u.u_cred);
out:
	VN_RELE(vp);
	u.u_r.r_val1 = uap->count - auio.uio_resid;
}

/*
 * Change mode of file given path name.
 */
chmod(uap)
	register struct a {
		char	*fname;
		int	fmode;
	} *uap;
{
	struct vattr vattr;

	vattr_null(&vattr);
	vattr.va_mode = uap->fmode & 07777;
	u.u_error = namesetattr(uap->fname, FOLLOW_LINK, &vattr);
}

/*
 * Change mode of file given file descriptor.
 */
fchmod(uap)
	register struct a {
		int	fd;
		int	fmode;
	} *uap;
{
	struct vattr vattr;

	vattr_null(&vattr);
	vattr.va_mode = uap->fmode & 07777;
	u.u_error = fdsetattr(uap->fd, &vattr);
}

/*
 * Change ownership of file given file name.
 */
chown(uap)
	register struct a {
		char	*fname;
		int	uid;
		int	gid;
	} *uap;
{
	struct vattr vattr;

	vattr_null(&vattr);
	vattr.va_uid = uap->uid;
	vattr.va_gid = uap->gid;
	u.u_error = namesetattr(uap->fname, NO_FOLLOW,  &vattr);
}

/*
 * Change ownership of file given file descriptor.
 */
fchown(uap)
	register struct a {
		int	fd;
		int	uid;
		int	gid;
	} *uap;
{
	struct vattr vattr;

	vattr_null(&vattr);
	vattr.va_uid = uap->uid;
	vattr.va_gid = uap->gid;
	u.u_error = fdsetattr(uap->fd, &vattr);
}

/*
 * Set access/modify times on named file.
 */
utimes(uap)
	register struct a {
		char	*fname;
		struct	timeval *tptr;
	} *uap;
{
	struct timeval tv[2];
	struct vattr vattr;

	u.u_error = copyin((caddr_t)uap->tptr, (caddr_t)tv, sizeof (tv));
#if RISCOS
	if (u.u_error)
		u.u_error = EFAULT;
#endif
	if (u.u_error)
		return;
	vattr_null(&vattr);
	vattr.va_atime = tv[0];
	vattr.va_mtime = tv[1];
	u.u_error = namesetattr(uap->fname, FOLLOW_LINK, &vattr);
}

/*
 * Truncate a file given its path name.
 */
truncate(uap)
	register struct a {
		char	*fname;
		int	length;
	} *uap;
{
	struct vattr vattr;

#ifdef RISCOS		/* Sun bug fix: 1014308 */
	/*
	 * Put this in for now until we know that files > 2^31-BSIZE will
	 * work.  With large disks or virtual disks, this will need to be
	 * tested and reworked.
	 */
	if ((unsigned)uap->length > ((unsigned)up->u_limit)*BSIZE) {
		u.u_error = EINVAL;
		return;
	}
#endif		
	vattr_null(&vattr);
	vattr.va_size = uap->length;
	u.u_error = namesetattr(uap->fname, FOLLOW_LINK, &vattr);
}

/*
 * Truncate a file given a file descriptor.
 */
ftruncate(uap)
	register struct a {
		int	fd;
		int	length;
	} *uap;
{
	register struct vnode *vp;
	struct file *fp;

#ifdef RISCOS			/* Sun bug fix: 1014308 */
	/*
	 * Put this in for now until we know that files > 2^31-BSIZE will
	 * work.  With large disks or virtual disks, this will need to be
	 * tested and reworked.
	 */
	if ((unsigned)uap->length > ((unsigned)up->u_limit)*BSIZE) {
		u.u_error = EINVAL;
		return;
	}
#endif		
	u.u_error = getvnodefp(uap->fd, &fp);
	if (u.u_error)
		return;
	vp = (struct vnode *)fp->f_data;
	if ((fp->f_flag & FWRITE) == 0) {
		u.u_error = EINVAL;
	} else if (vp->v_vfsp->vfs_flag & VFS_RDONLY) {
		u.u_error = EROFS;
	} else {
		struct vattr vattr;

		vattr_null(&vattr);
		vattr.va_size = uap->length;
		vattr.va_fmode = fp->f_flag;
		u.u_error = VOP_SETATTR(vp, &vattr, fp->f_cred);
	}
}

/*
 * Common routine for modifying attributes
 * of named files.
 */
namesetattr(fnamep, followlink, vap)
	char *fnamep;
	enum symfollow followlink;
	struct vattr *vap;
{
	struct vnode *vp;
	register int error;

	error =
	    lookupname(fnamep, UIO_USERSPACE, followlink,
		 (struct vnode **)0, &vp);
	if (error)
		return(error);	
	if(vp->v_vfsp->vfs_flag & VFS_RDONLY)
		error = EROFS;
	else
		error = VOP_SETATTR(vp, vap, u.u_cred);
	VN_RELE(vp);
	return(error);
}

/*
 * Common routine for modifying attributes
 * of file referenced by descriptor.
 */
fdsetattr(fd, vap)
	int fd;
	struct vattr *vap;
{
	struct file *fp;
	register struct vnode *vp;
	register int error;

	error = getvnodefp(fd, &fp);
	if (error == 0) {
		vp = (struct vnode *)fp->f_data;
		if(vp->v_vfsp->vfs_flag & VFS_RDONLY)
			return(EROFS);
		error = VOP_SETATTR(vp, vap, fp->f_cred);
	}
	return(error);
}

/*
 * Flush output pending for file.
 */
fsync(uap)
	struct a {
		int	fd;
	} *uap;
{
	struct file *fp;

	u.u_error = getvnodefp(uap->fd, &fp);
	if (u.u_error == 0)
		u.u_error = VOP_FSYNC((struct vnode *)fp->f_data, fp->f_cred);
}

/*
 * Set file creation mask.
 */
umask(uap)
	register struct a {
		int mask;
	} *uap;
{
	u.u_r.r_val1 = u.u_cmask;
	u.u_cmask = uap->mask & 0777;
}

/*
 * Revoke access the current tty by all processes.
 * Used only by the super-user in init
 * to give ``clean'' terminals at login.
 */
vhangup()
{
  	int	cur_pgrp;
	int	null_pgrp = 0;
	int	old_mask;
	int	rval1;

	if (!suser())
		return;
	if (u.u_procp->p_ttyvp == NULL)
		return;
	rval1 = u.u_r.r_val1;
	old_mask = u.u_procp->p_sigignore;
	u.u_procp->p_hold |= (sigmask(SIGTTIN) | sigmask(SIGTTOU));
	if (iioctl(u.u_procp->p_ttyvp,TIOCGPGRP,(char *) &cur_pgrp,
			FWRITE | FREAD)) {
		cur_pgrp = 0;
		u.u_error = 0;
	};
	if (u.u_procp->p_ttyvp != NULL) 
		(void) iioctl(u.u_procp->p_ttyvp,
				TIOCSPGRP,(char *) &null_pgrp);
	u.u_procp->p_hold = old_mask;
	u.u_r.r_val1 = rval1;
	if (u.u_procp->p_ttyvp != NULL)
		forceclose(u.u_procp->p_ttyvp->v_rdev);
	if (cur_pgrp != 0)
		signal(cur_pgrp, SIGHUP);
}

forceclose(dev)
	dev_t dev;
{
	register struct file *fp;
	register struct vnode *vp;

	for (fp = file; fp < fileNFILE; fp++) {
		if (fp->f_count == 0)
			continue;
		if (fp->f_type != DTYPE_VNODE)
			continue;
		vp = (struct vnode *)fp->f_data;
		if (vp == 0)
			continue;
		if (vp->v_type != VCHR)
			continue;
		if (vp->v_rdev != dev)
			continue;
		/*
		 * Note that while this prohibits further I/O on the
		 * descriptor, it does not prohibit closing the
		 * descriptor.
		 */
		fp->f_flag &= ~(FREAD|FWRITE);
	}
#if RISCOS
	return(forceclose_pgrp(dev,0));
#endif
}

#if RISCOS
forceclose_pgrp(dev,pgrp)
	dev_t dev;
	int	pgrp;
{
	register struct vnode *vp;
	register struct proc *p;
	register struct file *fp;

	/*
	 *	disassociate all processes from the device as
	 *	a controlling terminal
	 */
	for (p = proc; p < (struct proc *) v.ve_proc ; p++) {
		if (p->p_stat == 0)
			continue;
		vp = p->p_ttyvp;
		if (vp == NULL)
			continue;
		if (vp->v_rdev != dev)
			continue;
		if (pgrp != 0 &&
		    p->p_pgrp != pgrp)
			continue;
		fp = p->p_ttyfp;
		p->p_ttyvp = NULL;
		p->p_ttyfp = NULL;
		FP_RELE(fp);
	};
}
#endif

/*
 * Get the file structure entry for the file descrpitor, but make sure
 * its a vnode.
 */
int
getvnodefp(fd, fpp)
	int fd;
	struct file **fpp;
{
	register struct file *fp;

	fp = getf(fd);
	if (fp == (struct file *)0)
		return(EBADF);
	if (fp->f_type != DTYPE_VNODE)
		return(EINVAL);
	*fpp = fp;
	return(0);
}

/*
 * get directory entries in a file system independent format.
 * This call returns directory entries in the new format specified
 * in sys/dirent.h which is also the format returned by the vn_readdir
 * interface. This call supersedes getdirentries(), which will disappear
 * in 5-?.0.
 */
getdents(uap)
register struct a {
	int	fd;
	char	*buf;
	unsigned count;
} *uap;
{
	struct file *fp;
	struct uio auio;
	struct iovec aiov;
	struct vnode *vp;

#if RISCOS
	if (uap->count < sizeof (struct bsd43_dirent)) {
#else
	if (uap->count < sizeof (struct dirent)) {
#endif
		u.u_error = EINVAL;
		return;
	}
	u.u_error = getvnodefp(uap->fd, &fp);
	if (u.u_error)
		return;
	vp = (struct vnode *)fp->f_data;
	if ((fp->f_flag & FREAD) == 0) {
		u.u_error = EBADF;
		return;
	}
	if (vp->v_type != VDIR) {
		u.u_error = ENOTDIR;
		return;
	}
	aiov.iov_base = uap->buf;
	aiov.iov_len = uap->count;
	auio.uio_iov = &aiov;
	auio.uio_iovcnt = 1;
	auio.uio_offset = fp->f_offset;
	auio.uio_segflg = UIO_USERSPACE;
#if RISCOS
	auio.uio_fmode = fp->f_flag;
#endif
	auio.uio_resid = uap->count;
	u.u_error = VOP_READDIR(vp, &auio, fp->f_cred);
	if (u.u_error)
		return;
	u.u_r.r_val1 = uap->count - auio.uio_resid;
	fp->f_offset = auio.uio_offset;
}
