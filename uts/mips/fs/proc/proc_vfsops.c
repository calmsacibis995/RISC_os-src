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
#ident	"$Header: proc_vfsops.c,v 1.4.1.4 90/05/10 05:05:18 wje Exp $"


#ifndef lint
#endif

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
#include "sys/cmn_err.h"
#include "sys/sysmacros.h"
#include "sys/sbd.h"
#include "sys/immu.h"
#include "sys/region.h"
#include "sys/proc.h"
#include "sys/var.h"
#undef itoo  /* needed to avoid conflicts in fs.h */
#undef itod  /* needed to avoid conflicts in fs.h */
#undef NFS
#include "sys/mount.h"
#include "sys/debug.h"
#include "sys/fs/proc_fs.h"


/*
 * /proc filesystem virtual-filesystem operations.
 *
 */


/*
 * proc vfs operations.
 */
static int proc_mount();
static int proc_unmount();
int proc_root();
static int proc_statfs();
static int proc_sync();
static int proc_vget();
static int proc_mountroot();
static int proc_badvfsop();

struct vfsops proc_vfsops = {
	proc_mount,
	proc_unmount,
	proc_root,
	proc_statfs,
	proc_sync,
	proc_vget,
	proc_mountroot,
	proc_badvfsop,		/* XXX - swapvp */
};

extern struct vnodeops proc_vnodeops[];

struct proc_state proc_state={0};

pinode_t rootpinode= {0};	/* keep the root node around all the time */

		/* filesystem and "pack" names for filesys stats */
static char proc_fname[6] = { 'P', 'R', 'O', 'C', '\0', '\0'};
static char proc_fpack[6] = { '\0', '\0', '\0', '\0', '\0', '\0'};


/*
 * mount a proc filesystem
 *
 * If we have never initialized the /proc filesystem, do so on
 * the first call to mount.  This consists of 
 */
static int
proc_mount(vfsp, path, data)
	struct vfs *vfsp;
	char *path;
	caddr_t data;
{
	int error;
	dev_t dev;
	struct ufs_args args;
	struct vnode *vp;

	vp = &rootpinode.pi_vnode;

	if (proc_state.proc_init == 0) {


		proc_state.proc_init = 1;
	}
	if (proc_state.proc_mounted != 0) {
		return(EBUSY);
	}
	/*
	 * Don't allow remounts.
	 * XXX should allow change rdonly to read-write.
	 */
	if (vfsp->vfs_flag & VFS_REMOUNT) {
		return (EINVAL);
	}


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


	/*
	 * Mount the filesystem.
	 */

	vfsp->vfs_data = (caddr_t)0;
	vfsp->vfs_bsize = PROCBLKSIZ;
	vfsp->vfs_fsid.val[0] = 0;
	vfsp->vfs_fsid.val[1] = MOUNT_PROC;

	if (vp->v_count > 0) {
		error = EBUSY;
	}
	rootpinode.pi_next = (pinode_t *)0;	/* paranoia */
	rootpinode.pi_id = PROCROOT;
	rootpinode.pi_mode = 0555;	/* read and search for all */
	rootpinode.pi_xstat = 0;	/* paranoia */
	VN_INIT(vp, vfsp, VDIR, 0);
	/* XXX put the following init line into the VN_INIT macro */
	vp->v_vfsmountedhere = (struct vfs *)0;
	vp->v_flag |= VROOT;
	vp->v_op = proc_vnodeops;
	vp->v_data = (caddr_t)&rootpinode;


	if ( ! error) {
		proc_state.proc_vfsp = vfsp;
		proc_state.proc_mounted = 1;	/* no errors, mark mounted */
	}
	return (error);
}

/*
 * Called by vfs_mountroot to mount the filesystem as the root
 */
static int
proc_mountroot(vfsp, vpp, name, op)
	struct vfs *vfsp;
	struct vnode **vpp;
	char *name;
	mountrootop_t op;
{
	return (proc_badvfsop());
}

/*
 * unmount a proc filesystem
 */
static int
proc_unmount(vfsp)
	struct vfs *vfsp;
{
	/* If there are any vnodes active you can't unmount */
	if (! pi_empty()) {
		return(EBUSY);
	}
	if (rootpinode.pi_vnode.v_count > 1 ) {
		return(EBUSY);
	}
	
	rootpinode.pi_vnode.v_count = 0;

	proc_state.proc_vfsp = (struct vfs *)0;
	proc_state.proc_mounted = 0;
	return (0);
}

/*
 * find root of proc
 * There is a static pinode (vnode) for the root directory that is
 * valid whenever the filesystem is mounted.
 * The key is that the root dir vnode is held by the proc_mount()
 * proc_unmount() pair so a nonzero count is required if the filesytem
 * is open.
 */
int
proc_root(vfsp, vpp)
	struct vfs *vfsp;
	struct vnode **vpp;	/* return ptr to root vnode here	*/
{
	pinode_t *pip;		/* root pinode 			*/
	struct vnode	*vp;	/* vnode within root pinode	*/

	VFS_RECORD(vfsp, VS_ROOT, VS_CALL);

	ASSERT(proc_state.proc_mounted == 1);
	ASSERT(vfsp->vfs_fsid.val[0] == 0 && vfsp->vfs_fsid.val[1] == MOUNT_PROC);

	if (rootpinode.pi_vnode.v_count < 1) {
		return(EINVAL);
	}

	VN_HOLD(&rootpinode.pi_vnode);
	*vpp = &rootpinode.pi_vnode;
	return (0);
}


/*
 * Get file system statistics.
 *
 * THIS ROUTINE CAN BE CALLED WHEN THE FILESYSTEM IS NOT MOUNTED.
 * CHECK FOR, AND HANDLE, CASES WHERE vfsp IS NULL>
 *
 * This implemented in a straightforward way rather than
 * an efficient way, but presumably it doesn't happen very often.
 */
static int
proc_statfs(vfsp, sbp, vp)
	register struct vfs *vfsp;
	struct bsd43_statfs *sbp;
	struct vnode *vp;
{
	struct	proc *pp;
	int	pavail;
	if (vfsp != (struct vfs *) 0) {	/* The filesystem is mounted. */
		VFS_RECORD(vfsp, VS_STATFS, VS_CALL);
	}

	/* count the number of free process slots.
	 */
	for (pp = &proc[0], pavail=0; pp<&proc[v.v_proc]; pp++) {
		if (pp->p_stat == NULL) {
			pavail++;
		}
	}
	bzero(sbp, sizeof(*sbp));	/* neatness counts */
	sbp->f_type = 0;
	sbp->f_bsize = PROCBLKSIZ;
	sbp->f_blocks = -1;		/* undefined = -1; see statfs(2-BSD) */
	sbp->f_bfree = -1;		/* undefined = -1; see statfs(2-BSD) */
	sbp->f_bavail = -1;		/* undefined = -1; see statfs(2-BSD) */
	sbp->f_files = v.v_proc;
	sbp->f_ffree = pavail;
#ifdef RISCOS
	sbp->f_frsize = 0;		/* f_frsize defined only in kernel */
#endif
	if (vfsp != (struct vfs *) 0) {	/* The filesystem is mounted. */
		sbp->f_fsid = vfsp->vfs_fsid;
	}
	strcpy(proc_fname, sbp->f_fname, sizeof(sbp->f_fname));
	strcpy(proc_fpack, sbp->f_fpack, sizeof(sbp->f_fpack));
	return(0);
}

/*
 * Flush any pending I/O to file system vfsp.
 *
 * For /proc there is nothing to do.
 */
/*ARGSUSED*/
static int
proc_sync(vfsp)
	struct vfs *vfsp;
{

	return (0);
}


static int
proc_vget(vfsp, vpp, fidp)
	struct vfs *vfsp;
	struct vnode **vpp;
	struct fid *fidp;
{
	return (proc_badvfsop());
}

static int
proc_badvfsop()
{

	return (EINVAL);
}
