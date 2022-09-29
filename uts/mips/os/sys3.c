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
#ident	"$Header: sys3.c,v 1.27.1.9.1.2.1.2 90/11/15 13:42:36 beacker Exp $"

/*	Copyright (c) 1984 AT&T	*/
/*	  All Rights Reserved  	*/

/*	THIS IS UNPUBLISHED PROPRIETARY SOURCE CODE OF AT&T	*/
/*	The copyright notice above does not evidence any   	*/
/*	actual or intended publication of such source code.	*/

#include "sys/types.h"
#include "sys/signal.h"
#include "sys/pcb.h"
#include "sys/user.h"
#include "sys/immu.h"
#include "sys/sbd.h"
#include "sys/region.h"
#include "sys/proc.h"
#include "sys/uio.h"
#include "sys/var.h"
#include "sys/errno.h"
#include "sys/cmn_err.h"
#include "sys/debug.h"
#include "sys/numips.h"
#include "sys/vfs.h"		/* contains bsd43_statfs, among other things */
#include "sys/statfs.h"
#include "sys/vnode.h"
#include "sys/mount.h"
#include "sys/file.h"
#include "bsd43/sys/file.h"
#include "sys/termio.h"
#include "sys/socketvar.h"
#include "bsd43/sys/ioctl.h"
#include "sys/fs/nfs.h"	/* needed by  nfsmount() kludge */
#include "sys/fs/prfcntl.h"

extern struct proc *bsd43_pfind();

/*
 * The basic routine for fstat, stat, and lstat:
 * get the inode and pass appropriate parts back.
 */

/*
 * the fstatfs system call.
 */
sysv_fstatfs()
{
	struct file *fp;
	register struct a {
		int	fdes;
		struct	statfs *sb;
		int	len;
		int	fstyp;
	} *uap = (struct a *)u.u_ap;

	u.u_error = getvnodefp(uap->fdes, &fp);
	if (u.u_error == 0)
		statfs1((struct vnode *)fp->f_data, uap->sb, uap->len,
			uap->fstyp);
}

/*
 * the statfs system call.
 */
sysv_statfs()
{
	struct vnode *vp;
	register struct a {
		char	*fname;
		struct	statfs *sb;
		int	len;
		int	fstyp;
	} *uap = (struct a *)u.u_ap;

	u.u_error = lookupname(uap->fname, UIO_USERSPACE, FOLLOW_LINK,
	    (struct vnode **)0, &vp);
	if (u.u_error)
		return;
	statfs1(vp, uap->sb, uap->len, uap->fstyp);
	VN_RELE(vp);
}

/*
 * Common routine for fstatfs and statfs.
 */
statfs1(vp, ub, len, fstyp)
register struct vnode *vp;
struct statfs *ub;
int len, fstyp;
{
	struct statfs ds;
	struct bsd43_statfs sb;

	if (len < 0 || len > sizeof(ds) || 
	    fstyp < 0 || fstyp >= (int)(vfsNVFS - vfssw)) {
		u.u_error = EINVAL;
		return;
	}
	if (fstyp && vp->v_type != VCHR && vp->v_type != VBLK) {
		u.u_error = EINVAL;
		return;
	}
	/*
	 * Get generic superblock from fs-dependent code.
	 */
	bzero((caddr_t)&sb, sizeof (sb));
	if (fstyp) {
		u.u_error = (*vfssw[fstyp].vsw_ops->vfs_statfs)(0, &sb, vp);
		ds.f_fstyp = fstyp;
	} else {
		int i;

		ASSERT(vp->v_vfsp);
		u.u_error = VFS_STATFS(vp->v_vfsp, &sb);
		for (i = 0; i < (int)(vfsNVFS - vfssw); i++) {
			if (vp->v_vfsp->vfs_op == vfssw[i].vsw_ops) {
				ds.f_fstyp = i;
				break;
			}
		}
	}
	if (u.u_error)
		return;

	/* BSD statfs() returns -1 for fields which were undefined
	 * but past versions of RISC/OS returned 0, so in the name of
	 * binary compatibility ...
	 */
	ds.f_bsize = (sb.f_bsize == -1 ? 0 : sb.f_bsize);
	/* f_frsize is not defined for the user's bsd43_statfs structure */
	ds.f_frsize = (sb.f_frsize == -1 ? 0 : sb.f_frsize);
	ds.f_blocks =
	  ((sb.f_blocks == -1 ? 0 : sb.f_blocks) * sb.f_bsize) >> SCTRSHFT;
	/* define f_bfree to be amount free to non-superuser */
	ds.f_bfree =
	  ((sb.f_bavail == -1 ? 0 : sb.f_bavail) * sb.f_bsize) >> SCTRSHFT;
	ds.f_files = (sb.f_files == -1 ? 0 : sb.f_files);
	ds.f_ffree = (sb.f_ffree == -1 ? 0 : sb.f_ffree);
	bcopy(sb.f_fname, ds.f_fname, sizeof(ds.f_fname));
	bcopy(sb.f_fpack, ds.f_fpack, sizeof(ds.f_fpack));
	u.u_error = bsd_copyout((caddr_t)&ds, (caddr_t)ub, sizeof (ds));
}

/*
 * the dup system call.
 */
dup()
{
	register struct file *fp;
	register i;
	register struct a {
		int	fdes;
	} *uap;

	uap = (struct a *)u.u_ap;
	GETF(fp, uap->fdes);
	if ((i = ufalloc(0)) < 0)
		return;
	u.u_ofile[i] = fp;
	fp->f_count++;
}

/*
 * the dup2 system call from BSD-land.
 */
dup2()
{
	register struct a {
		int	i, j;
	} *uap = (struct a *) u.u_ap;
	register struct file *fp;

	GETF(fp, uap->i);
	if (uap->j < 0 || uap->j >= v.v_nofiles) {
		u.u_error = EBADF;
		return;
	}
	u.u_rval1 = uap->j;
	if (uap->i == uap->j)
		return;
	if (u.u_ofile[uap->j]) {
		/* Release all System-V style record locks, if any */
		(void) vno_lockrelease(u.u_ofile[uap->j]);	/* errors? */
		if (u.u_pofile[uap->j] & UF_MAPPED)
			munmapfd(uap->j);
		closef(u.u_ofile[uap->j]);
		/*
		 * Even if an error occurred when calling the close routine
		 * for the vnode or the device, the file table entry has
		 * had its reference count decremented anyway.  As such,
		 * the descriptor is closed, so there's not much point
		 * in worrying about errors; we might as well pretend
		 * the "close" succeeded.
		 */
		u.u_error = 0;
	}
	dupit(uap->j, fp, u.u_pofile[uap->i]);
}

dupit(fd, fp, flags)
	int fd;
	register struct file *fp;
	register int flags;
{
	u.u_ofile[fd] = fp;
	u.u_pofile[fd] = flags &~ UF_EXCLOSE;
	fp->f_count++;
#ifdef NOTDEF
	if (fd > u.u_lastfile)
		u.u_lastfile = fd;
#endif
}


/*
 * the file control system call.
 */
fcntl()
{
	register struct file *fp;
	register struct a {
		int	fdes;
		int	cmd;
		int	arg;
	} *uap;
	register i;
	register char *pop;
	struct flock ld;
	register int oldwhence;
	register int newflag;
	int fioarg;

	uap = (struct a *)u.u_ap;
	GETF(fp, uap->fdes);
	pop = &u.u_pofile[uap->fdes];
	switch(uap->cmd) {
	case F_DUPFD:
		i = uap->arg;
		if (i < 0 || i >= v.v_nofiles) {
			u.u_error = EINVAL;
			return;
		}
		if ((i = ufalloc(i)) < 0)
			return;
		u.u_ofile[i] = fp;
		fp->f_count++;
		break;

	case F_GETFD:
		u.u_rval1 = (u.u_pofile[uap->fdes] & EXCLOSE); 
		break;

	case F_SETFD:
		u.u_pofile[uap->fdes] &= ~EXCLOSE;
		u.u_pofile[uap->fdes] |= (uap->arg & EXCLOSE);
		break;

	case F_GETFL:
		if (BSD_SYSCALL) {
			u.u_rval1 = fcntl_sysv_to_bsd(fp->f_flag) + BSD43_FOPEN;
		} else if (SYSV_SYSCALL || POSIX_SYSCALL) {
			u.u_rval1 = fp->f_flag+FOPEN;
		}
		break;

	case F_SETFL:
		/*
		 * XXX Actually, there should not be any connection
		 * between the "ioctl"-settable per-object FIONBIO
		 * and FASYNC flags and any per-file-descriptor flags,
		 * so this call should simply fiddle fp->f_flag.
		 * Unfortunately, 4.2BSD has such a connection, so we
		 * must support that.  Thus, we must pass the new
		 * values of the FNDELAY and FASYNC flags down to the
		 * object by doing the appropriate "ioctl"s.
		 */
		if (BSD_SYSCALL) {
			newflag = fp->f_flag;
			newflag &= BSD43_FCNTLCANT;
			newflag |= (uap->arg-BSD43_FOPEN) &~ BSD43_FCNTLCANT;
			newflag = fcntl_bsd_to_sysv(newflag);
			fioarg = (newflag & FNDELAY) != 0;
			u.u_error = fioctl(fp, FIONBIO, (caddr_t) &fioarg);
			if (u.u_error)
				break;
			fioarg = (newflag & FASYNC) != 0;
			u.u_error = fioctl(fp, FIOASYNC, (caddr_t) &fioarg);
			if (u.u_error) {
				fioarg = (fp->f_flag & FNDELAY) != 0;
				(void) fioctl(fp, FIONBIO, (caddr_t) &fioarg);
				break;
			}
			fp->f_flag = newflag;
		} else if (SYSV_SYSCALL || POSIX_SYSCALL) {
			/* Used to do a F_CHKFL call through the switch */
			/* That was always a noop, so eliminating it is ok? */
			uap->arg &= FMASK;
			fp->f_flag &= (FREAD|FWRITE|FMARK|FDEFER|FSHLOCK|FEXLOCK);
			fp->f_flag |= (uap->arg-FOPEN) & ~(FREAD|FWRITE);
		}
		break;

	case F_GETOWN:
		if (BSD_SYSCALL)
			u.u_error = fgetown(fp, &u.u_rval1);
		else if (SYSV_SYSCALL || POSIX_SYSCALL)
			u.u_error = EINVAL;
		break;

	case F_SETOWN:
		if (BSD_SYSCALL)
			u.u_error = fsetown(fp, uap->arg);
		else if (SYSV_SYSCALL || POSIX_SYSCALL)
			u.u_error = EINVAL;
		break;

	case F_CHKFL:
		/* XXX ~~~ This is functionally equivalent to RISC/os 4.0.
		 * Should BSD return EINVAL?  Probably not.
		 */
		if (POSIX_SYSCALL)
			u.u_error = EINVAL;
		break;

		/* System-V Record-locking (lockf() maps to fcntl()) */
	case F_GETLK:
	case F_SETLK:
	case F_SETLKW:
		/* First off, allow only vnodes here */
		if (fp->f_type != DTYPE_VNODE) {
			u.u_error = EBADF;
			return;
		}
		/* dis-allow character special file for NIST-PCTS */
		if (POSIX_SYSCALL &&
		    ((struct vnode *)fp->f_data)->v_type == VCHR) {
  			u.u_error = EINVAL;
			return;
		}
		/* get flock structure from user-land */
		if (u.u_error =
		    bsd_copyin((caddr_t)uap->arg, (caddr_t)&ld, sizeof (ld))) {
			return;
		}

		/*
		 * *** NOTE ***
		 * The SVID does not say what to return on file access errors!
		 * Here, EBADF is returned, which is compatible with S5R3
		 * and is less confusing than EACCES
		 */
		switch (ld.l_type) {
		case F_RDLCK:
			if ((uap->cmd != F_GETLK) && !(fp->f_flag & FREAD)) {
				u.u_error = EBADF;
				return;
			}
			break;

		case F_WRLCK:
			if ((uap->cmd != F_GETLK) && !(fp->f_flag & FWRITE)) {
				u.u_error = EBADF;
				return;
			}
			break;

		case F_UNLCK:
			break;

		default:
			u.u_error = EINVAL;
			return;
		}

		/* convert offset to start of file */
		oldwhence = ld.l_whence;	/* save to renormalize later */
		if (u.u_error = rewhence(&ld, fp, 0))
			return;

		/* convert negative lengths to positive */
		if (ld.l_len < 0) {
			ld.l_start += ld.l_len;		/* adjust start point */
			ld.l_len = -(ld.l_len);		/* absolute value */
		}

		/* check for validity */
		if (ld.l_start < 0) {
			u.u_error = EINVAL;
			return;
		}

		if ((uap->cmd != F_GETLK) && (ld.l_type != F_UNLCK)) {
			/*
			 * If any locking is attempted, mark file locked
			 * to force unlock on close.
			 * Also, since the SVID specifies that the FIRST
			 * close releases all locks, mark process to
			 * reduce the search overhead in vno_lockrelease().
			 */
			*pop |= UF_FDLOCK;
			u.u_procp->p_flag |= SLKDONE;
		}

		/*
		 * Dispatch out to vnode layer to do the actual locking.
		 * Then, translate error codes for SVID compatibility
		 */
		switch (u.u_error = VOP_LOCKCTL((struct vnode *)fp->f_data,
		    &ld, uap->cmd, fp->f_cred,u.u_procp->p_pid)) {
		case 0:
			break;		/* continue, if successful */
		case EWOULDBLOCK:
			u.u_error = EACCES;	/* EAGAIN ??? */
			return;
		default:
			return;		/* some other error code */
		}

		/* if F_GETLK, return flock structure to user-land */
		if (uap->cmd == F_GETLK) {
			/* if POSIX_SYSCALL, the value in l_sysid is really the
			 * top half of the l_pid field...make sure that it is 0
			 * (the flock structures are the same size, they 
			 * are just aligned a little differently)
			 */
			if (POSIX_SYSCALL) {
				ld.l_sysid = 0;
			}
			/* per SVID, change only 'l_type' field if unlocked */
			if (ld.l_type == F_UNLCK) {
				if (u.u_error = bsd_copyout((caddr_t)&ld.l_type,
				    (caddr_t)&((struct flock*)uap->arg)->l_type,
				    sizeof (ld.l_type))) {
					return;
				}
			} else {
			  	if (SYSV_SYSCALL || BSD_SYSCALL) {
					if (u.u_error = rewhence(&ld, fp, oldwhence))
						return;
				} else if (POSIX_SYSCALL) {
					if (u.u_error = rewhence(&ld, fp, 0))
						return;
				}
				if (u.u_error = bsd_copyout((caddr_t)&ld,
				    (caddr_t)uap->arg, sizeof (ld))) {
					return;
				}
			}
		}
		break;

	/* /proc filesystem fcntl stuff
	 * KLUDGE ALERT
	 * There is no filesystem-specific fcntl vector for vnodes,
	 * so these calls make a call directly to a specific
	 * fcntl call for /proc.
	 * That routine rejects non /proc vnodes.
	 */

	case PFCGETPR:
	case PFCOPENT:
	case PFCEXCLU:
	case PFCSTOP:
	case PFCWSTOP:
	case PFCRUN:
	case PFCSMASK:
	case PFCCSIG:
	case PFCKILL:
	case PFCSEXEC:
	case PFCREXEC:
	case PFCNICE:
	case PFCGMASK:
#if NOT_SUPPORTED
	case PFCTERM:
#endif
	case PFCSSTEP:
	case PFCGETREGS:
	case PFCPUTREGS:
	case PFCGETSEG:
	case PFCGETUSEG:
		ASSERT(fp->f_type == DTYPE_VNODE);
		/* This code is stolen mostly from vno_ioctl().
		 * We call directly here to control the call better.
		 */
		u.u_r.r_reg.r_val1 = 0;
		if (setjmp(u.u_qsav)) { 
			if ((u.u_sigintr & sigmask(u.u_procp->p_cursig)) != 0) {
				u.u_error = EINTR;
			} else {
				u.u_error = 0;
				u.u_eosys = RESTARTSYS;
			}
		} else {
			u.u_error = proc_fcntl((struct vnode *)fp->f_data, uap->cmd, uap->arg, fp->f_flag, fp->f_cred);
		}
		break;

	default:
		u.u_error = EINVAL;
	}
}

/* SAMEFLAGS is the file flags which are unchanged between SysV (internal
 * and used by SysV progs) and BSD (used only by BSD progs).
 */
#define SAMEFLAGS (FREAD|FWRITE|FNDELAY|FAPPEND|FASYNC)

/* This expects internal file flags, and returns internal file flags. */
fcntl_sysv_to_bsd(sysvflags)
	int	sysvflags;
{
	int	bsdflags = 0;

	bsdflags = sysvflags & SAMEFLAGS;
	if (sysvflags & FDEFER)
		bsdflags |= BSD43_FDEFER;
	if (sysvflags & FMARK)
		bsdflags |= BSD43_FMARK;
	if (sysvflags & FSHLOCK)
		bsdflags |= BSD43_FSHLOCK;
	if (sysvflags & FEXLOCK)
		bsdflags |= BSD43_FEXLOCK;
	if (sysvflags & FSYNC)
		bsdflags |= BSD43_FSYNC;
	return(bsdflags);
}

/* This expects internal file flags, and returns internal file flags. */
fcntl_bsd_to_sysv(bsdflags)
	int	bsdflags;
{
	int	sysvflags = 0;

	sysvflags = bsdflags & SAMEFLAGS;
	if (bsdflags & BSD43_FDEFER)
		sysvflags |= FDEFER;
	if (bsdflags & BSD43_FMARK)
		sysvflags |= FMARK;
	if (bsdflags & BSD43_FSHLOCK)
		sysvflags |= FSHLOCK;
	if (bsdflags & BSD43_FEXLOCK)
		sysvflags |= FEXLOCK;
	if (bsdflags & BSD43_FSYNC)
		sysvflags |= FSYNC;
	return(sysvflags);
}
#undef SAMEFLAGS

/*
 * Normalize SystemV-style record locks
 */
rewhence(ld, fp, newwhence)
	struct flock *ld;
	struct file *fp;
	int newwhence;
{
	struct vattr va;
	register int error;

	/* if reference to end-of-file, must get current attributes */
	if ((ld->l_whence == 2) || (newwhence == 2)) {
		if (error = VOP_GETATTR((struct vnode *)fp->f_data, &va,
		    u.u_cred))
			return(error);
	}

	/* normalize to start of file */
	switch (ld->l_whence) {
	case 0:
		break;
	case 1:
		ld->l_start += fp->f_offset;
		break;
	case 2:
		ld->l_start += va.va_size;
		break;
	default:
		return(EINVAL);
	}

	/* renormalize to given start point */
	switch (ld->l_whence = newwhence) {
	case 1:
		ld->l_start -= fp->f_offset;
		break;
	case 2:
		ld->l_start -= va.va_size;
		break;
	}
	return(0);
}

ofcntl()
{
	register struct file *fp;
	register struct a {
		int	fdes;
		int	cmd;
		int	arg;
	} *uap;
	register i;
	off_t offset;
	int flag;
	
	uap = (struct a *)u.u_ap;
	GETF(fp, uap->fdes);
	flag = fp->f_flag;
	offset = fp->f_offset;

	switch (uap->cmd) {
	case F_GETFL:
		u.u_rval1 = (fp->f_flag & ~FNDELAY_PRE4_5)+FOPEN;
		break;

	case F_SETFL:
		if (BSD_SYSCALL && 
		    ((struct vnode *)fp->f_data)->v_type == VCHR) {
			if (setjmp(u.u_qsav)) {
				if (BSD_SYSCALL &&
				   (u.u_sigintr & bsd43_sigmask(u.u_procp->p_cursig)) == 0) {
					u.u_error = 0;
					u.u_eosys = RESTARTSYS;
				} else u.u_error = EINTR;
				return;
			};
		};
		uap->arg = (uap->arg - FOPEN) & FMASK;
		if (BSD_SYSCALL) {
			i = fp->f_flag;
			if ((uap->arg ^ fp->f_flag) & FNDELAY) {
			    u.u_error = fset(fp,FNDELAY,uap->arg & FNDELAY);
			    if (u.u_error)
				break;
			};
			if ((uap->arg ^ fp->f_flag) & FASYNC) {
			    u.u_error = fset(fp, FASYNC, uap->arg & FASYNC);
			    if (u.u_error) {
				if ((uap->arg ^ i) & FNDELAY)
					(void) fset(fp, FNDELAY, i & FNDELAY);
				break;
			    };
			};			
		} 
		fp->f_flag &= (FREAD|FWRITE);
		fp->f_flag |= (uap->arg & ~(FREAD|FWRITE));
		if (SYSV_SYSCALL) {
			fp->f_flag |= FNDELAY_PRE4_5;
		}
		break;

	default:
		fcntl();
		break;
	}
}

/*
 * These come from kern_descrip.c in the NFS release.
 * Put them here for convenience.
 */
fset(fp, bit, value)
	struct file *fp;
	int bit, value;
{

	if (value)
		fp->f_flag |= bit;
	else
		fp->f_flag &= ~bit;
	return (fioctl(fp, (int)(bit == FNDELAY ? FIONBIO : FIOASYNC),
	    (caddr_t)&value));
}

fgetown(fp, valuep)
	struct file *fp;
	int *valuep;
{
	int error;

	switch (fp->f_type) {

	case DTYPE_SOCKET:
		*valuep = ((struct socket *)fp->f_data)->so_pgrp;
		return (0);

	default:
		error = fioctl(fp, (int)TIOCGPGRP, (caddr_t)valuep);
		*valuep = -*valuep;
		return (error);
	}
}

fsetown(fp, value)
	struct file *fp;
	int value;
{

	if (fp->f_type == DTYPE_SOCKET) {
		((struct socket *)fp->f_data)->so_pgrp = value;
		return (0);
	}
	if (value > 0) {
		struct proc *p = bsd43_pfind(value);
		if (p == 0)
			return (ESRCH);
		value = p->p_jcpgrp;
	} else
		value = -value;
	return (fioctl(fp, (int)TIOCSPGRP, (caddr_t)&value));
}

fioctl(fp, cmd, value)
	struct file *fp;
	int cmd;
	caddr_t value;
{
	int	old_ksegflg = u.u_ksegflg;
	int	error;

	u.u_ksegflg = U_KS_KERNEL;
	error = (*fp->f_ops->fo_ioctl)(fp, cmd, value);
	u.u_ksegflg = old_ksegflg;
	return (error);
}

/*
 * Apply an advisory lock on a file descriptor.
 */
flock()
{
	register struct a {
		int	fd;
		int	how;
	} *uap = (struct a *)u.u_ap;
	register struct file *fp;

	GETF(fp, uap->fd);
	if (fp->f_type != DTYPE_VNODE) {
		u.u_error = EOPNOTSUPP;
		return;
	}
	if (uap->how & LOCK_UN) {
		vno_bsd_unlock(fp, FSHLOCK|FEXLOCK);
		return;
	}

	if (uap->how & LOCK_EX)
		uap->how &= ~LOCK_SH;	/* can't have both types */
	else if (!(uap->how & LOCK_SH)) {
		u.u_error = EINVAL;	/* but must have one */
		return;
	}
	u.u_error = vno_bsd_lock(fp, uap->how);
}

/*
 * character special i/o control
 */
ioctl()
{
	register struct file *fp;
	register struct a {
		int	fdes;
		int	cmd;
		int	arg;
	} *uap;

	uap = (struct a *)u.u_ap;
	GETF(fp, uap->fdes);
	switch (uap->cmd) {
	/*
	 *	convert old format IOCTL's for backward compatility 
	 */
	case oTCGETA:
	  	{
			struct Otermio old_termio;
			struct termio new_termio;
			register int i;

			u.u_error = fioctl(fp,TCGETA,&new_termio);
			if (u.u_error)
				return;
			old_termio.c_iflag = new_termio.c_iflag;
			old_termio.c_oflag = new_termio.c_oflag;
			old_termio.c_cflag = new_termio.c_cflag;
			old_termio.c_lflag = new_termio.c_lflag;
			old_termio.c_line = new_termio.c_line;
			for (i = 0; i < oNCC; i++)
				old_termio.c_cc[i] = new_termio.c_cc[i];
			if (copyout((caddr_t) &old_termio,
				    (caddr_t) uap->arg,
				    sizeof(struct Otermio)))
				u.u_error = EFAULT;
			return;
		};

	case oTCSETA:
	case oTCSETAW:
	case oTCSETAF:
	  	{
			struct Otermio old_termio;
			struct termio new_termio;
			register int i;

			if (copyin((caddr_t)uap->arg,
				   (caddr_t) &old_termio,
				   sizeof(struct Otermio))) {
				u.u_error = EFAULT;
				return;
			};
			u.u_error = fioctl(fp,TCGETA,&new_termio);
			if (u.u_error)
				return;
			new_termio.c_iflag = old_termio.c_iflag;
			new_termio.c_oflag = old_termio.c_oflag;
			new_termio.c_cflag = old_termio.c_cflag;
			new_termio.c_lflag = old_termio.c_lflag;
			new_termio.c_line = old_termio.c_line;
			for (i = 0; i < oNCC; i++)
				new_termio.c_cc[i] = old_termio.c_cc[i];
			switch (uap->cmd) {
			case oTCSETA:
				i = TCSETA;
				break;

			case oTCSETAW:
				i = TCSETAW;
				break;

			case oTCSETAF:
				i = TCSETAF;
				break;

			default:
				ASSERT(0);
				break;
			};
			u.u_error = fioctl(fp,i,&new_termio);
			return;
		};


	case oFIONREAD:
		uap->cmd = FIONREAD;
		break;
	case oTIOCSTI:
		uap->cmd = TIOCSTI;
		break;
	case oTIOCPKT:
		uap->cmd = TIOCPKT;
		break;
	case oTIOCEXCL:
		uap->cmd = TIOCEXCL;
		break;
	case oTIOCNXCL:
		uap->cmd = TIOCNXCL;
		break;

	/*
	 *	Implement some operations at the this level
	 */
	case oTIOCNOTTY:
		uap->cmd = TIOCNOTTY;
	case TIOCNOTTY:
		{
			struct	vnode *vp;
		        int	cur_pgrp;
			int	null_pgrp = 0;
			int	cur_mask;
			int	rval1;

			u.u_ttyd = 0;
			u.u_ttyp = NULL;
			u.u_ttyvp = NULL;
			if (u.u_procp->p_ttyvp == NULL) 
				return;

			if (u.u_procp->p_jcpgrp == 0) {
				clr_controlling_tty();
				return;
			};

			rval1 = u.u_rval1;
			cur_mask = u.u_procp->p_hold;
			u.u_procp->p_hold |=
				(sigmask(SIGTTIN) | sigmask(SIGTTOU));
			if (iioctl(u.u_procp->p_ttyvp,TIOCGPGRP,
				      (char *) &cur_pgrp,
				      FWRITE | FREAD)) {
				u.u_error = 0;
				cur_pgrp = 0;
			};
			if (u.u_procp->p_pgrp == u.u_procp->p_pid) {
			    /* 
			     * session process group leader:
			     *   handle like exit
			     */
			    if (u.u_procp->p_jcpgrp != 0 &&
			        u.u_procp->p_pgrp != u.u_procp->p_jcpgrp) {
				cmn_err(CE_WARN,
		"ioctl: fixed process %d pgrp (%d) != jcpgrp (%d)",
					u.u_procp->p_pid,
					u.u_procp->p_pgrp,
					u.u_procp->p_jcpgrp);
				if (cur_pgrp == u.u_procp->p_jcpgrp) {
					cur_pgrp = u.u_procp->p_pgrp;
					if (u.u_procp->p_ttyvp != NULL)
						(void) iioctl(
						      u.u_procp->p_ttyvp,
						      TIOCSPGRP,
						      (char *) &cur_pgrp,
					      	      FWRITE | FREAD);
				};
				u.u_procp->p_jcpgrp = u.u_procp->p_pgrp;
			    };
			    if (u.u_procp->p_jcpgrp != 0 &&
			        cur_pgrp == u.u_procp->p_jcpgrp) {
				signal(u.u_procp->p_jcpgrp, SIGHUP);
			    } else if (cur_pgrp != 0) {
				struct proc *ap;

				/*
				 * If the terminal's foreground process group
				 * is in the
				 * same session (same p_pgrp) as the current
				 * process (session leader) which is exiting,
				 * send SIGHUP to the foreground process group.
				 */
				for (ap = proc; ap < (struct proc *)v.ve_proc;
				     ap++)
					if (ap->p_jcpgrp == cur_pgrp &&
					    ap->p_stat != SZOMB &&
					    ap->p_pgrp == u.u_procp->p_pgrp) {
						signal(ap->p_jcpgrp,SIGHUP);
						break;
					};
				if (ap >= (struct proc *)v.ve_proc)
					cur_pgrp = 0;
					/* terminal controlled by a 	*/
					/* different session 		*/
			    };
			    if (cur_pgrp != 0 &&
				u.u_procp->p_ttyvp != NULL) {
				(void) iioctl(u.u_procp->p_ttyvp,TIOCSPGRP,
					(char *) &null_pgrp, FWRITE | FREAD);
				cur_pgrp = 0;
			    };
			    if (u.u_procp->p_ttyvp != NULL) 
				forceclose_pgrp(u.u_procp->p_ttyvp->v_rdev,
						u.u_procp->p_pgrp);
			};
			u.u_procp->p_hold = cur_mask;
			u.u_rval1 = rval1;

			u.u_procp->p_pgrp = 0;
			u.u_procp->p_jcpgrp = u.u_procp->p_pgrp;
			if (u.u_procp->p_ttyvp != NULL) {
				clr_controlling_tty();
			}
		}
		return;

	case oFIOCLEX:
	case FIOCLEX:
		u.u_pofile[uap->fdes] |= EXCLOSE;
		return;

	case oFIONCLEX:
	case FIONCLEX:
		u.u_pofile[uap->fdes] &= ~EXCLOSE;
		return;

	case oTIOCGETP:
		uap->cmd = BSD43_TIOCGETP;
		bsd43_ioctl();
		return;

	case oTIOCSETP:
		uap->cmd = BSD43_TIOCSETP;
		bsd43_ioctl();
		return;

	/*
	 *	check legality of some operations
	 */
     /* case BSD43_TIOCSPGRP: */
	case TIOCSPGRP:
		{
			int pgrp;
			struct proc *p;

			if (copyin((caddr_t)uap->arg, 
				   (caddr_t) &pgrp,
				   sizeof(int))) {
				u.u_error = EFAULT;
				return;
			};
			if ((fp->f_flag & FREAD) == 0 && u.u_uid != 0) {
				u.u_error = EPERM;
				return;
			};
			if (POSIX_SYSCALL && pgrp < 0) {
				u.u_error = EINVAL;
				return;
			};
			if (pgrp != 0) {
				p = bsd43_pfind(pgrp);
				if (p != NULL) {
				    if (POSIX_SYSCALL) {
					if (p->p_pgrp != u.u_procp->p_pgrp) {
						u.u_error = EPERM;
						return;
					}
				    } else {
					if ((p->p_pgrp == pgrp || 
					     p->p_jcpgrp == pgrp) &&
				    	    p->p_uid != u.u_uid &&
				    	    u.u_uid != 0 &&
					    ! inferior(p) &&
					    p->p_pgrp != up->u_procp->p_pgrp) {
						u.u_error = EPERM;
						return;
					};
				    };
				};
			};

			u.u_error = fioctl(fp,TIOCSPGRP,&pgrp);
			return;
		};

	default:
		break;
	};
	u.u_error = (*fp->f_ops->fo_ioctl)(fp, uap->cmd, uap->arg);
	if (u.u_cttyop) {
	    int	old_error;

	    old_error = u.u_error;

	    if (u.u_cttyop & U_SETCTTY) {
		    u.u_cttyop &= ~U_SETCTTY;
		    set_controlling_tty(fp);
	    }
	    if (u.u_cttyop & U_CLRCTTY) {
		struct	vnode *vp;
	        int	cur_pgrp;
		int	null_pgrp = 0;
		int	cur_mask;
		int	rval1;

		u.u_cttyop &= ~U_CLRCTTY;

		if (fp->f_type == DTYPE_VNODE) {
			rval1 = u.u_rval1;
			cur_mask = u.u_procp->p_hold;
			u.u_procp->p_hold |=
				(sigmask(SIGTTIN) | sigmask(SIGTTOU));
			vp = (struct vnode *) fp->f_data;
			if (iioctl(vp,TIOCGPGRP,
				      (char *) &cur_pgrp,
				      FWRITE | FREAD)) {
				u.u_error = 0;
				cur_pgrp = 0;
			};
			if (cur_pgrp != 0) {
				iioctl(vp,TIOCSPGRP,
					(char *) &null_pgrp,
					FWRITE | FREAD);
				if (vp->v_type == VCHR) {
					forceclose_pgrp(vp->v_rdev,
							cur_pgrp);
				};
			};
			u.u_procp->p_hold = cur_mask;
			u.u_rval1 = rval1;
		}
	    }
	    u.u_error = old_error;
	}
}


iioctl(vp, cmd, value, flags)
	struct vnode *vp;
	int cmd;
	caddr_t value;
	int	flags;
{
	int	old_ksegflg, old_rval1;
	struct file f;
	extern	struct fileops vnodefops;

	/* Fake a file structure pointing to this vnode */
	f.f_flag = flags;
	f.f_type = DTYPE_VNODE;
	f.f_count = 1;
	f.f_msgcount = 0;
	f.f_ops = &vnodefops;
	f.f_data = (caddr_t) vp;
	f.f_offset = 0;
	crhold(u.u_cred);
	f.f_cred = u.u_cred;

	/* Save some stuff that will get smashed during the call */
	old_ksegflg = u.u_ksegflg;
	old_rval1 = u.u_rval1;
	u.u_ksegflg = U_KS_KERNEL;
	u.u_rval1 = 0;

	/* Now do what fioctl would have done */
	u.u_error = (*f.f_ops->fo_ioctl)(&f, cmd, value);

	/* And restore any state we smashed */
	u.u_ksegflg = old_ksegflg;
	u.u_rval1 = old_rval1;
	crfree(u.u_cred);
	return (u.u_error);
}

#ifdef NOTDEF
/*
 * old stty and gtty
 */
stty()
{
	register struct a {
		int	fdes;
		int	arg;
		int	narg;
	} *uap;

	uap = (struct a *)u.u_ap;
	uap->narg = uap->arg;
	uap->arg = TIOCSETP;
	u.u_syscall = DUIOCTL;
	bsd_ioctl();
}

gtty()
{
	register struct a {
		int	fdes;
		int	arg;
		int	narg;
	} *uap;

	uap = (struct a *)u.u_ap;
	uap->narg = uap->arg;
	uap->arg = TIOCGETP;
	u.u_syscall = DUIOCTL;
	bsd_ioctl();
}
#endif NOTDEF

/*
 * the mount system call.
 */
/* 
 * Here we translate the SVID arguments into the NFS4.0 arguments.
 * Conveniently, smount() takes uap as an argument, rather than pulling
 * it out of the uarea.
 *
 * We translate the old sysv call into a "new" NFS call; that is, the
 * "type" argument is a string, not an integer.  We set the flag bit
 * appropriately, and then translate the rest of the SysV flags into
 * the NFS flags.
 *
 * We use the u.u_ksegflg hack since the args are now coming from
 * kernel space instead of user space.  Of course, the right way would
 * be to pass in a UIO_ flag which told the lower layers whence to fetch.
 */

sysv_smount()
{
	/* System V Argument Structure */
	register struct a {
		char	*fspec;
		char	*freg;
		int	flags;
		int	fstyp;
	} *uap;

	/* NFS4.0 Argument Structures */
	struct a2 {
		char	*type;
		char	*dir;
		int	flags;
		caddr_t	data;
	} uap2;
	struct ufs_args ufs_arg;
	register struct vfssw	*vs;

	uap = (struct a *)u.u_ap;
	/*
	 * Backward compatibility: require the user program to
	 * supply a flag indicating a SVR4-style mount, otherwise
	 * assume the fstyp of the root file system.
	 */
	if ((uap->flags & MS_FSS) == 0)
		for (vs = vfssw; vs < vfsNVFS; vs++) {
			if (vs->vsw_ops == rootvfs->vfs_op) {
				uap2.type = vs->vsw_name;
				break;
			}
		}
	else
		uap2.type = vfssw[uap->fstyp].vsw_name;

	uap2.dir = uap->freg;
	uap2.flags = M_NEWTYPE;
	if (uap->flags & MS_RDONLY)
		uap2.flags |= M_RDONLY;
	ufs_arg.fspec = uap->fspec;
	uap2.data = (caddr_t) &ufs_arg;
	u.u_ksegflg = U_KS_KERNEL;
	smount(&uap2);
	u.u_ksegflg = 0;
}

#ifdef NFSCLIENT
/*
 * This is a system call added in some previous RISC/os which must be emulated.
 * This was the RISC/os way of mounting nfs filesystems (yes, a separate
 * system call, sigh).  As above, we mash the arguments into something the
 * NFS4.0 code wants.
 */
nfsmount()
{
	/* RISC/os Argument Structure */
	struct old_nfs_args {
		struct sockaddr_in *addr;	/* file server address */
		fhandle_t	*fh;		/* File handle to be mounted */
		int		flags;		/* flags */
		int		wsize;		/* write size in bytes */
		int		rsize;		/* read size in bytes */
		int		timeo;		/* initial timeout in .1 secs */
		int		retrans;	/* times to retry send */
		char		*hostname;	/* server's name */
	};
	struct a {
		struct old_nfs_args	*argp;
		char		*localdir;
		int		readonly;
	} *uap = (struct a *) u.u_ap;

	/* NFS4.0 Argument Structures */
	struct a2 {
		char	*type;
		char	*dir;
		int	flags;
		caddr_t	data;
	} uap2;
	struct nfs_args args;		/* nfs-specific arguments */

	uap = (struct a *)u.u_ap;
	uap2.type = vfssw[MOUNT_NFS].vsw_name;

	uap2.dir = uap->localdir;
	uap2.flags = M_NEWTYPE;

	if (uap->readonly)
		uap2.flags |= M_RDONLY;
	/*
	 * The old_nfs_args struct is a subset of the new one.  We'll just
	 * copyin the old one into the beginning of the new one, and zero
	 * out the rest.
	 */
	if (copyin((caddr_t)uap->argp, (caddr_t)&args, sizeof(*uap->argp))) {
		u.u_error = EFAULT;
		return;
	};
	bzero(((caddr_t)&args) + sizeof(*uap->argp),
	      sizeof(args) - sizeof(*uap->argp));
	uap2.data = (caddr_t) &args;
	u.u_ksegflg = U_KS_KERNEL;
	smount(&uap2);
	u.u_ksegflg = 0;
}
#endif NFSCLIENT

/*
 * the umount system call.
 */
/*
 * This is the System V umount call.  (BSD call goes right to vfs.c unmount.)
 * Here we don't know whether the argument is a special device or the mount
 * point itself.  If the former, we call the compatibility routine
 * umount() in fs/ufs/ufs_vfsops.c.  If the latter, we call to the normal
 * unmount() routine in vfs.c.
 */
sumount()
{
	struct vnode *vp;
	register struct a {
		char	*file;	/* directory or block-special */
	} *uap;

	if (!suser())
		return;
	uap = (struct a *)u.u_ap;
	u.u_error = lookupname(uap->file, UIO_USERSPACE, FOLLOW_LINK,
	    (struct vnode **)0, &vp);
	if (u.u_error)
		return;
	if (vp->v_type == VBLK) {
		VN_RELE(vp);
		umount(uap);		/* call compat routine */
	}
	else if (vp->v_type != VDIR) {
		/* File must be directory or block special. */
		u.u_error = ENOTBLK;
		VN_RELE(vp);
	}
	else {
		VN_RELE(vp);
		unmount(uap);		/* call "normal" NFS routine */
	}
}

