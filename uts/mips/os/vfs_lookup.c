/*
 * |-----------------------------------------------------------|
 * | Copyright (c) 1990 MIPS Computer Systems, Inc.            |
 * | All Rights Reserved                                       |
 * |-----------------------------------------------------------|
 * |          Restrictive Rights Legend                        |
 * | Use, duplication, or disclosure by the Government is      |
 * | subject to restrictions as set forth in                   |
 * | subparagraph (c)(1)(ii) of the Rights in Technical        |
 * | Data and Computer Software Clause of DFARS 52.227-7013.   |
 * |         MIPS Computer Systems, Inc.                       |
 * |         928 Arques Avenue                                 |
 * |         Sunnyvale, CA 94086                               |
 * |-----------------------------------------------------------|
 */
#ident	"$Header: vfs_lookup.c,v 1.5.1.4.1.1.1.5 91/01/16 16:32:39 beacker Exp $"

/*	@(#)vfs_lookup.c	2.4 88/06/19 4.0NFSSRC SMI;  from SMI 2.16 87/01/16	*/

/* Originally included param.h user.h uio.h vfs.h vnode.h dirent.h pathname.h */

#include "sys/types.h"
#include "sys/signal.h"		/* needed by user.h */
#include "sys/pcb.h"		/* needed by user.h */
#include "sys/user.h"
#include "sys/uio.h"
#include "sys/vfs.h"
#include "sys/vnode.h"
#include "sys/errno.h"
#include "bsd43/sys/dirent.h"
#include "sys/pathname.h"
#include "sys/sysinfo.h"
#include "sys/numips.h"

/*
 * lookup the user file name,
 * Handle allocation and freeing of pathname buffer, return error.
 */
lookupname(fnamep, seg, followlink, dirvpp, compvpp)
	char *fnamep;			/* user pathname */
	int seg;			/* addr space that name is in */
	enum symfollow followlink;	/* follow sym links */
	struct vnode **dirvpp;		/* ret for ptr to parent dir vnode */
	struct vnode **compvpp;		/* ret for ptr to component vnode */
{
	struct pathname lookpn;
	register int error;

	error = pn_get(fnamep, seg, &lookpn);
	if (error)
		return (error);
	else if ((POSIX_SYSCALL || SYSV_SYSCALL) && lookpn.pn_pathlen == 0) {
		/* was passed a null length string */
		pn_free(&lookpn);
		return (ENOENT);
	}
	error = lookuppn(&lookpn, followlink, dirvpp, compvpp);
	pn_free(&lookpn);
	return (error);
}

/*
 * Starting at current directory, translate pathname pnp to end.
 * Leave pathname of final component in pnp, return the vnode
 * for the final component in *compvpp, and return the vnode
 * for the parent of the final component in dirvpp.
 * If au_path is set, then return the resolved full pathname for the
 * pathname specified.
 *
 * This is the central routine in pathname translation and handles
 * multiple components in pathnames, separating them at /'s.  It also
 * implements mounted file systems and processes symbolic links.
 */
lookuppn(pnp, followlink, dirvpp, compvpp)
	register struct pathname *pnp;		/* pathaname to lookup */
	enum symfollow followlink;		/* (don't) follow sym links */
	struct vnode **dirvpp;			/* ptr for parent vnode */
	struct vnode **compvpp;			/* ptr for entry vnode */
{
	register struct vnode *vp;		/* current directory vp */
	register struct vnode *cvp;		/* current component vp */
	struct vnode *tvp;			/* non-reg temp ptr */
	register struct vfs *vfsp;		/* ptr to vfs for mount indir */
	char component[MAXNAMLEN+1];		/* buffer for component */
	register int error;
	register int nlink;
	int lookup_flags;

#if RISCOS
	++sysinfo.namei;
#endif
	nlink = 0;
	cvp = (struct vnode *)0;
	lookup_flags = dirvpp ? LOOKUP_DIR : 0;

	/*
	 * start at current directory.
	 */
	vp = u.u_cdir;
	VN_HOLD(vp);
begin:
	/*
	 * Each time we begin a new name interpretation (e.g.
	 * when first called and after each symbolic link is
	 * substituted), we allow the search to start at the
	 * root directory if the name starts with a '/', otherwise
	 * continuing from the current directory.
	 */
	component[0] = 0;
	if (pn_peekchar(pnp) == '/') {
		VN_RELE(vp);
		pn_skipslash(pnp);
		if (u.u_rdir)
			vp = u.u_rdir;
		else
			vp = rootdir;
		VN_HOLD(vp);
	}

	/*
	 * Eliminate any trailing slashes in the pathname.
	 */
	if (SYSV_SYSCALL || POSIX_SYSCALL)
	  	pn_fixslash(pnp);

next:
	/*
	 * Make sure we have a directory.
	 */
	if (vp->v_type != VDIR) {
		error = ENOTDIR;
		goto bad;
	}
	if (VOP_ACCESS(vp, VEXEC, u.u_cred)) {
		error = EACCES;
		goto bad;
	}
	/*
	 * Process the next component of the pathname.
	 */
	error = pn_stripcomponent(pnp, component);
	if (error)
		goto bad;

	/*
	 * Check for degenerate name (e.g. / or "")
	 * which is a way of talking about a directory,
	 * e.g. "/." or ".".
	 */
	if (component[0] == 0) {
		/*
		 * If the caller was interested in the parent then
		 * return an error since we don't have the real parent.
		 * Return EEXIST in this case, to disambiguate EINVAL.
		 */
		if (dirvpp != (struct vnode **)0) {
			VN_RELE(vp);
			return(EEXIST);
		}
		(void) pn_set(pnp, ".");
		if (compvpp != (struct vnode **)0) {
			*compvpp = vp;
		} else {
			VN_RELE(vp);
		}
		return(0);
	}

	/*
	 * Handle "..": two special cases.
	 * 1. If at root directory (e.g. after chroot)
	 *    then ignore it so can't get out.
	 * 2. If this vnode is the root of a mounted
	 *    file system, then replace it with the
	 *    vnode which was mounted on so we take the
	 *    .. in the other file system.
	 */
	if (strcmp(component, "..") == 0) {
checkforroot:
		if (VN_CMP(vp, u.u_rdir) || VN_CMP(vp, rootdir)) {
			cvp = vp;
			VN_HOLD(cvp);
			goto skip;
		}
		if (vp->v_flag & VROOT) {
			cvp = vp;
			vp = vp->v_vfsp->vfs_vnodecovered;
			VN_HOLD(vp);
			VN_RELE(cvp);
			cvp = (struct vnode *)0;
			goto checkforroot;
		}
	}

	/*
	 * Perform a lookup in the current directory.
	 */
	error = VOP_LOOKUP(vp, component, &tvp, u.u_cred, pnp, lookup_flags);
	cvp = tvp;
	if (error) {
		cvp = (struct vnode *)0;
		/*
		 * On error, if more pathname or if caller was not interested
		 * in the parent directory then hard error.
		 * If the path is unreadable, fail now with the right error.
		 */
		if (pn_pathleft(pnp) || dirvpp == (struct vnode **)0 ||
		    (error == EACCES))
			goto bad;
		(void) pn_set(pnp, component);
		*dirvpp = vp;
		if (compvpp != (struct vnode **)0) {
			*compvpp = (struct vnode *)0;
		}
		return (0);
	}

	/*
	 * If this vnode is mounted on, then we
	 * transparently indirect to the vnode which 
	 * is the root of the mounted file system.
	 * Before we do this we must check that an unmount is not
	 * in progress on this vnode. This maintains the fs status
	 * quo while a possibly lengthy unmount is going on.
	 */
mloop:
	while (vfsp = cvp->v_vfsmountedhere) {
		if (vfsp->vfs_flag & VFS_MLOCK) {
			vfsp->vfs_flag |= VFS_MWAIT;
#ifdef RISCOS
			if (sleep((caddr_t)vfsp, (PVFS | PCATCH))) {
			  error = EINTR;
			  goto bad;
			}
#else
			sleep((caddr_t)vfsp, PVFS);
#endif
			goto mloop;
		}
		error = VFS_ROOT(cvp->v_vfsmountedhere, &tvp);
		if (error)
			goto bad;
		VN_RELE(cvp);
		cvp = tvp;
	}

	/*
	 * If we hit a symbolic link and there is more path to be
	 * translated or this operation does not wish to apply
	 * to a link, then place the contents of the link at the
	 * front of the remaining pathname.
	 */
	if (cvp->v_type == VLNK &&
	    ((followlink == FOLLOW_LINK) || pn_pathleft(pnp))) {
		struct pathname linkpath;

		nlink++;
		if (nlink > MAXSYMLINKS) {
			error = ELOOP;
			goto bad;
		}
		error = getsymlink(cvp, &linkpath);
		if (error)
			goto bad;
		if (pn_pathleft(&linkpath) == 0)
			(void) pn_set(&linkpath, ".");
		error = pn_combine(pnp, &linkpath);	/* linkpath before pn */
		pn_free(&linkpath);
		if (error)
			goto bad;
		VN_RELE(cvp);
		cvp = (struct vnode *)0;
		goto begin;
	}

skip:
	/*
	 * Skip to next component of the pathname.
	 * If no more components, return last directory (if wanted)  and
	 * last component (if wanted).
	 */
#if RISCOS
	/*
	 * If SVR3, check for "/" or "/.", and gobble them.
	 * This needs to be done for two reasons which are SVID related:
	 * 
	 * 1) trailing "/" are ignored (no ENOTDIR when there is a
	 *    trailing / and the final component is not a directory),
	 * 2) we must not lose the directory pointer (no EEXIST when
	 *    "creating" an already-existent file, among other cases).
	 *
	 * Note the original pn_skipslash a few dozen lines below is
	 * a no-op for SYSV_SYSCALLs.
	 *
	 * 4.52 Mod: Gobble only /./ or ./ if previous component was
	 * a directory. If it wasn't, ENOTDIR error.
	 */
	if (SYSV_SYSCALL || POSIX_SYSCALL) {
skip2:
		pn_skipslash(pnp);
		if (pn_peekchar(pnp) == '.' && 
		    (pn_pathleft(pnp) == 2 && pnp->pn_path[1] == '/')) {
			if (vp->v_type != VDIR) {
				error = ENOTDIR;
				goto bad;
			}
			error = pn_stripcomponent(pnp, component);
			if (error)
				goto bad;
			goto skip2;
		}
	}
#endif
	if (pn_pathleft(pnp) == 0) {
		(void) pn_set(pnp, component);
		if (dirvpp != (struct vnode **)0) {
			/*
			 * Check that we have the real parent and not
			 * an alias of the last component.
			 * Return EEXIST in this case, to disambiguate EINVAL.
			 */
			if (VN_CMP(vp, cvp)) {
				VN_RELE(vp);
				VN_RELE(cvp);
				return(EEXIST);
			}
			*dirvpp = vp;
		} else {
			VN_RELE(vp);
		}
		if (compvpp != (struct vnode **)0) {
			*compvpp = cvp;
		} else {
			VN_RELE(cvp);
		}
		return (0);
	}
	/*
	 * skip over slashes from end of last component
	 */
	pn_skipslash(pnp);

	/*
	 * Searched through another level of directory:
	 * release previous directory handle and save new (result
	 * of lookup) as current directory.
	 */
	VN_RELE(vp);
	vp = cvp;
	cvp = (struct vnode *)0;
	goto next;

bad:
	/*
	 * Error. Release vnodes and return.
	 */
	if (cvp)
		VN_RELE(cvp);
	VN_RELE(vp);
	return (error);
}

/*
 * Gets symbolic link into pathname.
 */
int
getsymlink(vp, pnp)
	struct vnode *vp;
	struct pathname *pnp;
{
	struct iovec aiov;
	struct uio auio;
	register int error;

	pn_alloc(pnp);
	aiov.iov_base = pnp->pn_buf;
	aiov.iov_len = MAXPATHLEN;
	auio.uio_iov = &aiov;
	auio.uio_iovcnt = 1;
	auio.uio_offset = 0;
	auio.uio_segflg = UIO_SYSSPACE;
#if RISCOS
	auio.uio_fmode = 0; /* XXX */
#endif
	auio.uio_resid = MAXPATHLEN;
	error = VOP_READLINK(vp, &auio, u.u_cred);
	if (error)
		pn_free(pnp);
	pnp->pn_pathlen = MAXPATHLEN - auio.uio_resid;
	return (error);
}
