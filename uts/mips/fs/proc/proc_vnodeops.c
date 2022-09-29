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
#ident	"$Header: proc_vnodeops.c,v 1.2.1.7 90/06/05 16:50:57 wje Exp $"


#include "sys/types.h"
#include "sys/signal.h"		/* needed by user.h */
#include "sys/pcb.h"		/* needed by user.h */
#include "sys/user.h"
#include "sys/buf.h"
#include "sys/vfs.h"
#include "sys/vfs_stat.h"
#include "sys/file.h"
#include "sys/vnode.h"
#include "sys/immu.h"		/* needed by proc.h */
#include "sys/sbd.h"		/* needed by proc.h */
#include "sys/region.h"		/* needed by proc.h */
#include "sys/proc.h"
#include "sys/uio.h"
#include "sys/errno.h"
#include "sys/systm.h"
#include "sys/utsname.h"
#include "sys/var.h"
#include "sys/debug.h"
#include "sys/reg.h"
#include "sys/cmn_err.h"
#include "sys/fs/proc_fs.h"
#include "sys/fs/prfcntl.h"
#include "sys/ptrace.h"
#include "sys/fs/s5dir.h"
#include "sys/sysmacros.h"
#include "bsd43/sys/dirent.h"
#include "sys/kmem.h"

/* KLUDGE ALERT
 * This is necessary to set some mode bits in the attribute mode field
 * that somebody at a higher level should be setting.  See proc_getattr()
 * for the whole sordid story
 */
#include "bsd43/sys/stat.h"

/*
 * /proc filesystem vnode operations.
 *
 */

static	int proc_open();
static	int proc_close();
static	int proc_rdwr();
static	int proc_ioctl();
static	int proc_select();
static	int proc_getattr();
static	int proc_setattr();
static	int proc_access();
static	int proc_lookup();
static	int proc_create();
static	int proc_remove();
static	int proc_link();
static	int proc_rename();
static	int proc_mkdir();
static	int proc_rmdir();
static	int proc_readdir();
static	int proc_symlink();
static	int proc_readlink();
static	int proc_fsync();
static	int proc_inactive();
static	int proc_bmap();
static	int proc_strategy();
static	int proc_bread();
static	int proc_brelse();
static	int proc_lockctl();
static	int proc_fid();
static  int proc_cmp();
static  int proc_realvp();
static	int proc_badop();

struct vnodeops proc_vnodeops = {
	proc_open,
	proc_close,
	proc_rdwr,
	proc_ioctl,
	proc_select,
	proc_getattr,
	proc_setattr,
	proc_access,
	proc_lookup,
	proc_create,
	proc_remove,
	proc_link,
	proc_rename,
	proc_mkdir,
	proc_rmdir,
	proc_readdir,
	proc_symlink,
	proc_readlink,
	proc_fsync,
	proc_inactive,
	proc_bmap,
	proc_strategy,
	proc_bread,
	proc_brelse,
	proc_lockctl,
	proc_fid,
	proc_badop,		/* dump */
	proc_cmp,
	proc_realvp,
};


#ifdef NOTDEF
#ifdef VFSSTATS
#define	VFS_RECORD(vfs, op, hitmiss) \
proc_break(op); \
	((vfs)->vfs_stats \
	    ? ((struct vfsstats *)(vfs)->vfs_stats)->vs_counts[op][hitmiss]++ \
	    : 0 )
#else
#define	VFS_RECORD(vfs, op, hitmiss)
#endif
#endif /* NOTDEF */

#ifndef DIRBLKSIZ
#define DIRBLKSIZ	512	/* dir blk size if none defined */
#endif

#define TEXT		0
#define DATA		1
#define STACK		2
#define USERAREA	3
#define	SHAREDMEM	4
#define OTHER		5

user_t	*pruvad=0;		/* Used to map in other uareas. */

extern struct proc *procfind();
extern struct timeval time;
extern struct vnode proc_rootvnode;
extern struct var v;
extern int	piget();
extern pinode_t *picreate();
extern preg_t	*vtopreg();
extern preg_t		*xdup();

#define OWNER(CR, uid)	(((CR)->cr_uid == uid)? 0: (suser()? 0: u.u_error))

/*
* Open a /proc vnode.
*
* The processes are exclusive open with some initialization.
* The root directory is not exclusive open.
* Ex-open is enforced here and not earlier when the vnode (pinode)
* is created * because a stat of the "file" calls proc_getattr()
* after it * creates a vnode during proc_lookup().
* We don't want open processes not to be seen with "ls -l".
*
*/
/*ARGSUSED*/
static int
proc_open(vpp, flag, cred)
	struct vnode **vpp;	/* ptr-ptr to vnode in pinode to open */
	int flag;
	struct ucred *cred;
{
	struct proc *pp;		/* proc for the pinode		*/

	VFS_RECORD((*vpp)->v_vfsp, VS_OPEN, VS_CALL);

	if (VTOP(*vpp)->pi_id == PROCROOT) {
		return(0);
	}
	if((pp = procfind(VTOP(*vpp)->pi_id)) == (struct proc *)0) {
		return(ENOENT);
	}
	if (pp->p_flag & SPROPEN) {
		return(EBUSY);
	}

	/* First "open" of the process.
	* The process must have some status setup at this point.
	*/

	pp->p_trace = *vpp;
	pp->p_flag |= SPROPEN;

	return (0);
}

/*
 * We don't do anything at "close" time.
 * Instead, wait unitl the pinode is discarded on last close.
 */

/*ARGSUSED*/
static int
proc_close(vp, flag, count, cred)
	struct vnode *vp;
	int flag;
	int count;
	struct ucred *cred;
{
	struct proc *pp;		/* proc for the pinode		*/

	VFS_RECORD(vp->v_vfsp, VS_CLOSE, VS_CALL);

	if (VTOP(vp)->pi_id == PROCROOT) {
		return(0);
	}
	if((pp = procfind(VTOP(vp)->pi_id)) == (struct proc *)0) {
		/* XXX can the proc go away before close? */
		return(ENOENT);
	}

	ASSERT (pp->p_flag & SPROPEN);
	if (vp->v_count <= 1) {
		pp->p_trace = 0;
		pp->p_sigmask = 0;
		pp->p_flag &= ~(SPROPEN|SPROCTR);
	} else {
		cmn_err(CE_WARN,"!proc_close: count > 1 is unexpected\n");
	}

	return (0);
}

/*
 * read or write a vnode
 */
/*ARGSUSED*/
static int
proc_rdwr(vp, uiop, rw, ioflag, cred)
	struct vnode *vp;
	struct uio *uiop;
	enum uio_rw rw;
	int ioflag;
	struct ucred *cred;
{
	int	error;
	pinode_t *pip;

	switch(rw) {
	case UIO_READ:
		VFS_RECORD(vp->v_vfsp, VS_READ, VS_CALL);
		pip = VTOP(vp);
		if (pip->pi_id == PROCROOT) {
			error = proc_readroot(pip,uiop,rw,ioflag,cred);
		} else {
			error = proc_readproc(vp,uiop,rw,ioflag,cred);
		}
		break;
	case UIO_WRITE:
		VFS_RECORD(vp->v_vfsp, VS_WRITE, VS_CALL);
		pip = VTOP(vp);
		if (pip->pi_id == PROCROOT) {
			error = EISDIR;
		} else {
			error = proc_writeproc(vp,uiop,rw,ioflag,cred);
		}
		break;
	default:
		cmn_err(CE_WARN,"!proc_rdwr: illegal rw flag val %d\n",rw);
		/* XXX is EINVAL the best return value here ? */
		error = EINVAL;
		break;
	}
	return(error);
}

/* io-control ops
* There are no valid ioctl() calls for /proc.
*/

/*ARGSUSED*/
static int
proc_ioctl(vp, com, data, flag, cred)
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
proc_select(vp, which, cred)
	struct vnode *vp;
	int which;
	struct ucred *cred;
{
	VFS_RECORD(vp->v_vfsp, VS_SELECT, VS_CALL);
	proc_unimp();
	return (EINVAL);
}

/*ARGSUSED*/
static int
proc_getattr(vp, vap, cred)
	struct vnode *vp;
	struct vattr *vap;
	struct ucred *cred;
{
	struct proc *pp;
	pinode_t *pip;

	VFS_RECORD(vp->v_vfsp, VS_GETATTR, VS_CALL);

	pip = VTOP(vp);
	vap->va_type = vp->v_type;
	if (pip->pi_id == PROCROOT) {
		vap->va_nlink  = 2;
		vap->va_size = (v.v_proc + 2 ) * PROCDSIZ;
		vap->va_atime = time;
		vap->va_mtime = time;
		vap->va_ctime = time;
		vap->va_uid = 0;
		vap->va_gid = 0;
	} else {
retry:
		if ((pp = procfind(pip->pi_id)) == (struct proc *)0) {
			return(ENOENT);
		}
		vap->va_nlink  = 1;
		vap->va_size = ctob(pp->p_size);
		vap->va_atime = time;
		vap->va_mtime = time;
		if (pp->p_stat != SZOMB) {
			if (proc_umap(pp)) {
				goto retry;
			}
			vap->va_ctime = pruvad->u_bsd43_start;
			proc_unmap(pp);
		} else {
			vap->va_ctime = time;
		}
		vap->va_uid = pp->p_suid;
		vap->va_gid = pp->p_sgid;
	}
	vap->va_mode = pip->pi_mode;
	vap->va_fmode = -1; 		/* set to uninitialized state */
/* KLUDGE ALERT
 * The higher levels don't correctly merge the vnode mode field
 * and vnode type information into the expected UNIX inode mode information.
 * At this point, the information that the root vnode is a directory
 * manages to get forgotten.
 * Clearly someone above should be doing this, but for now...
 */
	if (pip->pi_id == PROCROOT) {
		vap->va_mode |= BSD43_S_IFDIR;
	}
/* END KLUDGE */
	vap->va_fsid = vp->v_rdev;
	vap->va_nodeid = -1;	/* XXX what is this really */
	vap->va_blocksize = vp->v_vfsp->vfs_bsize;
	vap->va_blocks = (vap->va_size + 1023)/1024;	/* round up */
	return(0);
}

static int
proc_setattr(vp, vap, cred)
	struct vnode *vp;
	struct vattr *vap;
	struct ucred *cred;
{
	pinode_t *pip;	/* pinode associated with vnode		*/
	ushort	pi_uid;
	ushort	pi_gid;
	int chtime = 0;
	int error = 0;

	VFS_RECORD(vp->v_vfsp, VS_SETATTR, VS_CALL);

	/*
	 * Cannot set these attributes
	 */
	if (
	   ((int)vap->va_type != -1)
	|| (vap->va_uid != -1)
	|| (vap->va_gid != -1)
	|| (vap->va_fsid != -1)
	|| (vap->va_nodeid != -1)
	|| (vap->va_nlink != -1)
	|| (vap->va_size != -1)
	|| (vap->va_blocksize != -1)
	|| (vap->va_rdev != -1)
	|| (vap->va_blocks != -1)
	|| (vap->va_atime.tv_sec != -1)
	) {
		return (EINVAL);
	}

	pip = VTOP(vp);
	/*
	 * Change file access modes.  Must be owner or su.
	 */
	if (vap->va_mode != (u_short)-1) {
	/* XXX make this rational */
		error = getowner(pip,&pi_uid,&pi_gid);
		if (error) {
			goto out;
		}
		error = OWNER(cred, pi_uid);
		if (error) {
			goto out;
		}
		pip->pi_mode = vap->va_mode;
	}
out:
	return (error);
}


/*
 * check the access characteristics of a vnode with respect to some mode.
 */

/*ARGSUSED*/
static int
proc_access(vp, mode, cred)
	struct vnode *vp;
	int mode;
	struct ucred *cred;
{
	int	error;
	VFS_RECORD(vp->v_vfsp, VS_ACCESS, VS_CALL);

	/* ILOCK(ip); XXX */
	error = piaccess(VTOP(vp), mode);
	/* iunlock(ip); XXX */
	return (error);
}


/*ARGSUSED*/
static int
proc_readlink(vp, uiop, cred)
	struct vnode *vp;
	struct uio *uiop;
	struct ucred *cred;
{
	VFS_RECORD(vp->v_vfsp, VS_READLINK, VS_CALL);
	proc_unimp();
	return (EINVAL);
}

/*ARGSUSED*/
static int
proc_fsync(vp, cred)
	struct vnode *vp;
	struct ucred *cred;
{
	VFS_RECORD(vp->v_vfsp, VS_FSYNC, VS_CALL);
	return (0);
}

/* On the termination of the last access,
 * clean up after an "open" file.
 * For a process, make it untraced.
 * free the storage for the pinode.
 */

/*ARGSUSED*/
static int
proc_inactive(vp, cred)
	struct vnode *vp;
	struct ucred *cred;
{
	pinode_t *pip;
	struct proc	*pp;

	VFS_RECORD(vp->v_vfsp, VS_INACTIVE, VS_CALL);

	pip = VTOP(vp);

	/* We shouldn't ever get the root pinode inactive because
	*  we artificially hold that open ourselves between
	*  mount and unmount.
	*/

	if (pip->pi_id == PROCROOT) {
		/* XXX how about panics if debug and complaints if not? */
		cmn_err(CE_PANIC,"!proc_inactive: - INACTIVE PROC ROOT NODE\n");
	}

	piremove(pip);
	return(0);
}

/*
 * Unix file system operations having to do with directory manipulation.
 */
/*ARGSUSED*/
static int
proc_lookup(dvp, nm, vpp, cred, pnp, flags)
	struct vnode *dvp;
	char *nm;
	struct vnode **vpp;
	struct ucred *cred;
	struct pathname *pnp;
	int flags;
{
	pinode_t *tpip;
	int	error;
	int	proc_no;
	struct proc *pp;

	VFS_RECORD(dvp->v_vfsp, VS_LOOKUP, VS_CALL);

	if (error = piaccess(VTOP(dvp), VEXEC)) {
		return(error);
	}

	/* Look for "" or "." */
	if ((nm[0] == '\0') ||
	    (nm[0] == '.'  && nm[1] == '\0')
	   ) {
		return(proc_root(dvp->v_vfsp,vpp));
	}
	/* check for ".."
	* Since we are the root of a mounted fs, the upper level takes
	* care of this for us and substitutes the mounted on vnode.
	* Complain if we get this, though, so that if the upper level
	* code stops handling it we will know what is going on.
	*/
	if (nm[0] == '.'  &&  nm[1] == '.'  &&  nm[2] == '\0') {
		cmn_err(CE_WARN,"!proc_lookup got \"..\" and that is invalid");
		error = EINVAL;
		goto fail;
	}
	/* gather up a process number.
	* We print strings with prefixed zeros, but treat search
	* strings numerically.
	* Accept strings with or without prefixed zeros: "000nn" or "nn".
	*/
	proc_no = 0;
	while (*nm) {
		if (*nm < '0' || *nm > '9') {
			error = ENOENT;
			goto fail;
		}
		proc_no = (10 * proc_no) + *nm++ - '0';
	}
	/* now see if this number is an exsting process */
	error = piget(proc_no,vpp);
fail:
	return(error);
}

static int
proc_create(dvp, nm, vap, exclusive, mode, vpp, cred)
	struct vnode *dvp;
	char *nm;
	struct vattr *vap;
	enum vcexcl exclusive;
	int mode;
	struct vnode **vpp;
	struct ucred *cred;
{
	VFS_RECORD(dvp->v_vfsp, VS_CREATE, VS_CALL);
	proc_unimp();
	return(EINVAL);
}

/*ARGSUSED*/
static int
proc_remove(vp, nm, cred)
	struct vnode *vp;
	char *nm;
	struct ucred *cred;
{
	VFS_RECORD(vp->v_vfsp, VS_REMOVE, VS_CALL);
	proc_unimp();
	return (EINVAL);
}

/*
 * Link a file or a directory.
 * If source is a directory, must be superuser.
 */
/*ARGSUSED*/
static int
proc_link(vp, tdvp, tnm, cred)
	struct vnode *vp;
	struct vnode *tdvp;
	char *tnm;
	struct ucred *cred;
{
	VFS_RECORD(vp->v_vfsp, VS_LINK, VS_CALL);
	proc_unimp();
	return (EINVAL);
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
proc_rename(sdvp, snm, tdvp, tnm, cred)
	struct vnode *sdvp;		/* old (source) parent vnode */
	char *snm;			/* old (source) entry name */
	struct vnode *tdvp;		/* new (target) parent vnode */
	char *tnm;			/* new (target) entry name */
	struct ucred *cred;
{
	VFS_RECORD(sdvp->v_vfsp, VS_RENAME, VS_CALL);
	proc_unimp();
	return(EINVAL);
}

/*ARGSUSED*/
static int
proc_mkdir(dvp, nm, vap, vpp, cred)
	struct vnode *dvp;
	char *nm;
	struct vattr *vap;
	struct vnode **vpp;
	struct ucred *cred;
{
	VFS_RECORD(dvp->v_vfsp, VS_MKDIR, VS_CALL);
	proc_unimp();
	return (EINVAL);
}

/*ARGSUSED*/
static int
proc_rmdir(vp, nm, cred)
	struct vnode *vp;
	char *nm;
	struct ucred *cred;
{
	VFS_RECORD(vp->v_vfsp, VS_RMDIR, VS_CALL);
	proc_unimp();
	return (EINVAL);
}


/* read directory entries
 *
 * Get a buffer full of directory entries for /proc.
 * Return as many filesystem-independent entries as will
 * fit into the buffer provided.  These are as compressed
 * as the structure allows (typcally 20 bytes).
 * The offset value returned is the offset for the following
 * (possibly empty) directory entry in the imaginary "on disk"
 * /proc root directory file that uses the Sys-V fixed-length
 * directory format with PROCDSIZ bytes per directory entry.
 *
 * Entries are created for "." and ".." then the proc table is
 * scanned and entries made up in the order that procs occupy
 * proc table slots.
 */

/*ARGSUSED*/
static int
proc_readdir(vp, uiop, cred)
	struct vnode *vp;
	struct uio *uiop;
	struct ucred *cred;
{
	struct bsd43_dirent *odp;
	int outcount = 0;
	u_int offset;
	u_int count;
	struct iovec t_iovec;
	caddr_t outbuf;
	u_int bufsize;
	int error = 0;
	int	procslot;
	int	j;
	int	id;
	struct proc *procp;

	VFS_RECORD(vp->v_vfsp, VS_READDIR, VS_CALL);

	count = uiop->uio_iov->iov_len;

	/*
	 * Get space to creat a buffer full of entries (+ 1 extra)
	 */
	bufsize = count + sizeof (struct bsd43_dirent);
	outbuf = kmem_alloc(bufsize);
	bzero (outbuf,bufsize);
	odp = (struct bsd43_dirent *)outbuf;

	/* Force offset to be valid (to guard against bogus lseek() values) */
	offset = uiop->uio_offset & ~(PROCDSIZ - 1);
	if(uiop->uio_offset != offset) {
		/* seek value can't possibly be right since it isn't
		 * at a directory entry boundry.
		 */
		cmn_err(CE_WARN,"!proc_readdir: force offset %d to %d\n",
			uiop->uio_offset, offset);
	}

	/* Create entries in file-system independent format */
	outcount = 0;
	for ( ; ;offset += PROCDSIZ) {

		odp->d_name[0] = 0;
		/* verify that this is a valid "entry" location in
		 * the current /proc directory.
		 * That is, ".", "..", or a valid proc table slot.
		 */
		procslot = (offset-(2*PROCDSIZ))/PROCDSIZ;
		if (offset >= (2*PROCDSIZ)  &&  procslot >= v.v_proc ) {
			break;
		}
		/*
		 * Invariants:
		 * Odp points at the next entry in the local buffer to fill.
		 * The name field is empty-- i.e. a null string.
		 * Offset is at the current "dir entry" in the root of /proc
		 * and this entry is within the size of the file.
		 */

		if (offset == 0) {	/* "." */
			odp->d_fileno = PROCROOT;
			(void) strcpy(odp->d_name, ".");
			odp->d_namlen = 1;
		} else if (offset == PROCDSIZ) { /* .. */
			odp->d_fileno = PROCROOT;
			(void) strcpy(odp->d_name, "..");
			odp->d_namlen = 2;
		} else {
			procp = &proc[procslot];
			if (procp->p_stat == 0) {
				continue;
			}
			id = procp->p_pid;
			odp->d_fileno = id;
			for (j = 4; j >= 0; j--) {
				register int rem;
				rem = id % 10;
				id /= 10;
				odp->d_name[j] = rem + '0';
			}
			odp->d_namlen = 5;
		}
		/* If we found a "file name" and filled it in,
		 * fill in the other fields and advance
		 * the pointer to the next place in the output buffer
		 */
		if (odp->d_name[0] != 0) {
			odp->d_reclen = DIRSIZ(odp);
			odp->d_off = offset + PROCDSIZ;
			outcount += odp->d_reclen;
			/* If we've built a buffer with more bytes than requested,
			 * decrement the count for the last entry put into the buffer.
			 * If no entry has fit into the buffer (this is the first)
			 * that is an error condition.
			 * In either case, break out of the loop and return.
			 */
			if (outcount > count) {
				outcount -= odp->d_reclen;
				if (outcount == 0) {
					/* no entries ret before exhausting count */
					error = EINVAL;
				}
				break;
			}
			odp = (struct bsd43_dirent *)((int)odp + odp->d_reclen);
		}
	
	}
	/*
	 * Invariants:
	 * Outcount is the number of bytes in the local buffer
	 * that contain bsd43_dirent entries and should be returned
	 * to the user.
	 * Outcount <= user-buffer-size
	 * Offset points at the first dir entry that was not
	 * put into the output buffer or not considered for it --
	 * continued scan of the directory should start there.
	 */


	/* Copy out the entry data */
	if ( ! error) {
		error = uiomove(outbuf, outcount, UIO_READ, uiop);
	}

	if ( ! error) {
		/* tell caller where to restart */
		uiop->uio_offset = offset;
	}
	kmem_free(outbuf,bufsize);
	return (error);
}


/*ARGSUSED*/
static int
proc_symlink(dvp, lnm, vap, tnm, cred)
	struct vnode *dvp;
	char *lnm;
	struct vattr *vap;
	char *tnm;
	struct ucred *cred;
{
	VFS_RECORD(dvp->v_vfsp, VS_SYMLINK, VS_MISS);
	proc_unimp();
	return(EINVAL);
}


static int
proc_bmap(vp, lbn, vpp, bnp)
	struct vnode *vp;
	daddr_t lbn;
	struct vnode **vpp;
	daddr_t *bnp;
{
	VFS_RECORD(vp->v_vfsp, VS_BMAP, VS_CALL);
	return (0);
}

/*
 * read a logical block and return it in a buffer
 */
/*ARGSUSED*/
static int
proc_bread(vp, lbn, bpp, sizep)
	struct vnode *vp;
	daddr_t lbn;
	struct buf **bpp;
	long *sizep;
{
	VFS_RECORD(vp->v_vfsp, VS_BREAD, VS_CALL);
	cmn_err(CE_CONT,"!proc_bread: bn=%d, len=%d\n",lbn,*sizep);
	proc_unimp();
	return(EINVAL);
}

/*
 * release a block returned by proc_bread
 */
/*ARGSUSED*/
static int
proc_brelse(vp, bp)
	struct vnode *vp;
	struct buf *bp;
{
	VFS_RECORD(vp->v_vfsp, VS_BRELSE, VS_CALL);
	return(proc_badop());
}

static int
proc_cmp(vp1, vp2)
	struct vnode *vp1, *vp2;
{
	VFS_RECORD(vp1->v_vfsp, VS_CMP, VS_CALL);
	return(1);	/* false */
}

/*ARGSUSED*/
static int
proc_realvp(vp, vpp)
	struct vnode *vp;
	struct vnode **vpp;
{
	VFS_RECORD(vp->v_vfsp, VS_REALVP, VS_CALL);
	proc_unimp();
	return (EINVAL);
}

static int
proc_badop()
{
	panic("proc_badop");
}

/*
 * Record-locking requests are passed to the local Lock-Manager daemon.
 */
static int
proc_lockctl(vp, ld, cmd, cred, clid)
	struct vnode *vp;
	struct flock *ld;
	int cmd;
	struct ucred *cred;
	int clid;
{
	VFS_RECORD(vp->v_vfsp, VS_LOCKCTL, VS_CALL);
	proc_unimp();
	return(EINVAL);
}

static int
proc_fid(vp, fidpp)
	struct vnode *vp;
	struct fid **fidpp;
{
	VFS_RECORD(vp->v_vfsp, VS_FID, VS_CALL);
	proc_unimp();
	return (EINVAL);
}

static int
proc_strategy(bp)
	struct buf *bp;
{
	VFS_RECORD(bp->b_vp->v_vfsp, VS_STRATEGY, VS_CALL);
	return(proc_badop());
}

static proc_break_count;
proc_break(op)
int	op;		/* vector number */
{
	cmn_err(CE_CONT,"!pop=%d ",op);
	proc_break_count += 1;
}



/* unimplemented routines call this for convenient trapping
 */
static int proc_uicount=0;
proc_unimp()
{
	cmn_err(CE_CONT,"UNIMPLEMENTED /PROC FILESYSTEM ROUTINE CALL\n");
	proc_uicount += 1;
}



/* File control handling.
* This is how we control the tracing operations and
* access the abstractions like get and set register values
* without understanding and munging around in the uarea.
*
* When the RISC/os kernel used the filesystem switch, there
* was a filesystem-specific fcntl() routine.
* Vnodes don't have one.
* SVR4 has a significantly different /proc implementation,
* so this code will be entirely replaced in the next major release.
* The not-as-aesthetic solution is that the /proc fcntl()
* calls are recognized at the system call level and this routine
* is called directly from there.
* The argument structure is based on the arguments for proc_ioctl().
* Because we are called directly from fcntl(),
* this routine must be careful to make sure the vnode is
* the right sort.
*/

/* ARGSUSED */
proc_fcntl(vp, com, data, flag, cred)
struct vnode	*vp;		/* vnode to perform operation on	*/
int		com;		/* control command			*/
caddr_t		data;		/* ptr to user data buffer		*/
int		flag;		/* flag from file struct for cntl access */
struct ucred	*cred;		/* credentials from file open		*/
{
	register struct proc *pp;
	preg_t *prp;
	unsigned int signo;
	int n;
	long sigmask_value;
	unsigned int regbuf[NPTRC_REGS]; /* LARGE (400bytes) kernel stack */
	unsigned int *regptr,*reglocptr;
	int i;
	pinode_t *pip;
	int	error=0;

	if (vp->v_op != &proc_vnodeops) {
		return(EINVAL);
	}

	pip = VTOP(vp);

cntlstart:
	if (com == F_CHKFL)
		return(0);
	switch (com) {
	default:
		return(EINVAL);

	case PFCGETPR:		/* read struct proc */
	case PFCNICE:		/* set nice priority */
	case PFCKILL:		/* send signal */
	case PFCSTOP:		/* stop process from running */
	case PFCWSTOP:		/* wait for process to stop */
	case PFCRUN:		/* make process runnable */
	case PFCSEXEC:		/* stop on exec */
	case PFCREXEC:		/* run on exec */
	case PFCSMASK:		/* set signal trace mask */
	case PFCGMASK:		/* get signal trace mask */
	case PFCCSIG:		/* clear current signal */
	case PFCEXCLU:		/* mark text for exclusive use */
	case PFCOPENT:		/* open text file for reading */
#ifdef notdef
	case PFCTERM:		/* force a process to terminate */
#endif /* notdef */
	case PFCSSTEP:		/* single step process */
	case PFCGETREGS:	/* get registers */
	case PFCPUTREGS:	/* put registers */
	case PFCGETSEG:		/* get process segment info */
	case PFCGETUSEG:	/* get process uarea info */
		if ((pp = procfind(pip->pi_id)) == NULL) {
			return(ENOENT);
		}
	}
	switch (com) {

	default:
		error = EINVAL;
		break;

	case PFCGETPR:		/* read struct proc */
		if (copyout((caddr_t) pp, data, sizeof(struct proc))) {
			error = EFAULT;
		}
		break;

	case PFCGETUSEG:	/* Get process uarea info */
		/* Return process's uarea info */
		{
		    struct pfcseg pseg;

		    pseg.seg_vaddr = (caddr_t)&u;
		    pseg.seg_size = ctob(USIZE);
		    pseg.seg_reserved = 0;
		    pseg.seg_pregtype = 0;
		    pseg.seg_regtype = 0;
		    if (copyout((caddr_t) &pseg, data, sizeof(struct pfcseg)))
			error = EFAULT;
		}
		break;

	case PFCGETSEG:		/* Get process segment info */
		/* Return process region/pregion info for the region starting
		 * at or after the specified address (preg_regva).
		 */
		{
		    struct pfcseg pseg;
		    caddr_t	vaddr;
		    preg_t	*prp;
		    extern preg_t	*vtonextpreg();

		    if (copyin(data, (caddr_t)&pseg, sizeof(struct pfcseg)))
			error = EFAULT;
		    else if ((prp = vtonextpreg(pp, pseg.seg_vaddr)) == NULL)
			/* no more regions with or beyond the specified addr */
			error = EINVAL;
		    else {
			reg_t	*rp = prp->p_reg;
			pseg.seg_vaddr = prp->p_regva + ctob(rp->r_pgoff);
			pseg.seg_size = ctob(rp->r_pgsz);
			pseg.seg_reserved = 0;
			pseg.seg_pregtype = prp->p_type;
			pseg.seg_regtype = rp->r_type;
		    }
			
		    if (copyout((caddr_t) &pseg, data, sizeof(struct pfcseg)))
			error = EFAULT;
		}
		break;

	case PFCNICE:		/* set nice priority */
		if (copyin(data, (caddr_t) &n, sizeof(int))) {
			error = EFAULT;
		} else {
			if ((n += pp->p_nice) >= 2*NZERO) {
				n = 2*NZERO -1;
			}
			if (n < 0) {
				n = 0;
			}
			if (n < pp->p_nice && !suser()) {
				break;
			}
			pp->p_nice = n;
		}
		break;

	case PFCKILL:		/* send signal */
		if (copyin(data, (caddr_t) &signo, sizeof(unsigned int))) {
			error = EFAULT;
		} else if (signo > NSIG) {
			error = EINVAL;
		} else {
			psignal(pp, signo);	
		}
		break;

	case PFCSTOP:		/* stop process from running */
		/*
		 * If sleeping, stop immediately; if already stopped,
		 * do nothing; otherwise flag it to be stopped the
		 * next time it tries to run.
		 */
		if (pp->p_stat == SSLEEP) {
			stop(pp);
		} else if (pp->p_stat != SSTOP) {
			pp->p_flag |= SPRSTOP;
		}

				/* fall through */
	case PFCWSTOP:		/* wait for process to stop */
		if (pp->p_stat != SSTOP && (pip->pi_xstat == -1)) {
			sleep((caddr_t)pp->p_trace, PZERO+1);
		}
		/* proc terminated ?? */
		if ((n=pip->pi_xstat) != -1) {
			/* copy out status */
			if (data != 0 &&
			    copyout((caddr_t) &n, data, sizeof(long))) {
				error = EFAULT;
			}
			/* will get ENOENT, below */
		}
		if (pp->p_pid != pip->pi_id || pp->p_stat == SZOMB) {
			error = ENOENT;
		} else if (pp->p_stat != SSTOP) {
			error = EINTR;
		}
		break;

	case PFCRUN:		/* make process runnable */
	case PFCSSTEP:		/* single step process */
		u.u_pcb.pcb_sstep = (com == PFCSSTEP);
		if (pp->p_stat == SSTOP && (pp->p_flag&STRC) == 0) {
			setrun(pp);
		}
		break;

	case PFCSEXEC:		/* stop on exec */
		pp->p_flag |=  SSEXEC;
		break;

	case PFCREXEC:		/* run on exec */
		pp->p_flag &= ~SSEXEC;
		break;

	case PFCSMASK:		/* set signal trace mask */
		if (copyin(data, (caddr_t) &sigmask_value, sizeof(long))) {
			error = EFAULT;
		} else if (pp->p_sigmask = sigmask_value) {
			pp->p_flag |= SPROCTR;
		} else {
			pp->p_flag &= ~SPROCTR;
		}
		break;

	case PFCGMASK:		/* get signal trace mask */
		if (copyout((caddr_t) &pp->p_sigmask, data, sizeof(long))) {
			error = EFAULT;
		}
		break;

	case PFCCSIG:		/* clear current signal */
		if (n = fsig(pp)) {
			pp->p_sig &= ~(1L<<(n-1));
		}
		break;

	case PFCEXCLU:		/* mark text for exclusive use */
		if ((prp = data ?
		  vtopreg(pp, data) : findpreg(pp, PT_TEXT)) == NULL) {
			error = ENOENT;
		} else if (prp->p_reg->r_type == RT_STEXT && prlock(pp) == 0) {
			if (prreglock(pp, prp->p_reg, 1) == -1) {
				prunlock(pp);
				goto cntlstart;
			}
			if (prp = xdup(pp, prp)) {
				/* XXX do I need some equivalent ? 
				if (prp->p_reg->r_iptr) {
					prele(prp->p_reg->r_iptr);
				}
				****************/
				regrele(prp->p_reg);
			}
			prunlock(pp);
		}
		break;

#if mips
#ifdef notdef
	case PFCTERM:		/* force a process to terminate */
		prterminate(pp);
		break;
#endif /* notdef */

#define PRUVAD ((char *) pruvad)
#define UR(x) (((uint *)(PRUVAD+ctob(USIZE)-EF_SIZE))[x])
	case PFCGETREGS:	/* get registers */
/*		uvad = (user_t *)PHYS_TO_K0(ctob(ubptbl(pp)->pgm.pg_pfn)); */
		/* XXX  check a result */
		proc_umap(pp);

		regptr = regbuf;
		*regptr++ = 0;
		reglocptr = (unsigned int *)&UR(EF_AT);
		for (i = 1; i < NGP_REGS; i++) {
			*regptr++ = *reglocptr++;
		}
		reglocptr = (unsigned int *)pruvad->u_pcb.pcb_fpregs;
		for (i = 0; i < NFP_REGS; i++) {
			*regptr++ = *reglocptr++;
		}
		reglocptr = (unsigned int *)pruvad->u_signal;
		for (i = 0; i < NSIG_HNDLRS; i++) {
			*regptr++ = *reglocptr++;
		}
		*regptr++ = UR(EF_EPC);
		*regptr++ = UR(EF_CAUSE);
		*regptr++ = UR(EF_BADVADDR);
		*regptr++ = UR(EF_MDHI);
		*regptr++ = UR(EF_MDLO);
		*regptr++ = pruvad->u_pcb.pcb_fpc_csr;
		*regptr++ = pruvad->u_pcb.pcb_fpc_eir;

		proc_unmap(pp);

		if (copyout(regbuf, data, sizeof(regbuf))) {
			error = EFAULT;
		}
		break;

	case PFCPUTREGS:	/* put registers into uarea */
/*		uvad = (user_t *)PHYS_TO_K0(ctob(ubptbl(pp)->pgm.pg_pfn)); */
		if (copyin(data, regbuf, sizeof(regbuf))) {
			error = EFAULT;
			break;
		}
		/* XXX  check a result */
		proc_umap(pp);
		regptr = &regbuf[1];	/* Register 0 is read only */
		reglocptr = (unsigned int *)&UR(EF_AT);
		for (i = 1; i < NGP_REGS; i++) {
			*reglocptr++ = *regptr++;
		}
		reglocptr = (unsigned int *)pruvad->u_pcb.pcb_fpregs;
		for (i = 0; i < NFP_REGS; i++) {
			*reglocptr++ = *regptr++;
		}
		regptr += NSIG_HNDLRS;	/* don't set handlers here */

		UR(EF_EPC) = *regptr++;
		regptr++;	/* EF_CAUSE is read only */
		regptr++;	/* EF_BADVADDR is read only */
		UR(EF_MDHI) = *regptr++;
		UR(EF_MDLO) = *regptr++;
		pruvad->u_pcb.pcb_fpc_csr = *regptr++;
		regptr++;	/* fpc_eir is read only */

		proc_unmap(pp);
		break;
#undef UR
#undef PRUVAD
#endif
	case PFCOPENT:		/* open text file for reading */
		if (data) {
			prp = vtopreg(pp, data);
		} else if ((prp = findpreg(pp, PT_TEXT)) == NULL) {
			prp = findpreg(pp, PT_DATA);
		}
#ifdef FIX_THIS
/* XXX */
		/* struct inode *ixp; */
		if (prp && (ixp = prp->p_reg->r_iptr)) {
			plock(ixp);
			ixp->i_count++;
			copen1(ixp, FREAD);
		} else {
			error = ENOENT;
		}
#else
cmn_err(CE_CONT,"PFCOPENT is currently uninplemented \n");
error = EINVAL;
#endif /* FIX THIS */
		break;
	}
	return(error);
}
