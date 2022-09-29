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
#ident	"$Header: nfs_export.c,v 1.3.1.2 90/05/10 05:03:42 wje Exp $"

/* @(#)nfs_export.c 1.2 88/05/23 NFSSRC4.0 from 1.11 88/02/08 SMI */

#include "sys/types.h"
#include "sys/param.h"
#include "bsd/sys/time.h"
#include "sys/vfs.h"
#include "sys/vnode.h"
#include "sys/socket.h"
#include "sys/errno.h"
#include "sys/uio.h"

#include "sys/signal.h"		/* needed by user.h */
#include "sys/pcb.h"		/* needed by user.h */
#include "sys/user.h"

#include "sys/file.h"
#include "sys/pathname.h"
#include "bsd/netinet/in.h"
#include "../rpc/types.h"
#include "../rpc/auth.h"
#include "../rpc/auth_unix.h"
#include "../rpc/auth_des.h"
#include "../rpc/svc.h"
#ifndef NFSSERVER
#define NFSSERVER 1
#endif NFSSERVER
#include "sys/fs/nfs.h"
#include "sys/fs/nfs_export.h"

static int nobody = -2;

#define eqfsid(fsid1, fsid2)	\
	(bcmp((char *)fsid1, (char *)fsid2, (int)sizeof(fsid_t)) == 0)

#define eqfid(fid1, fid2) \
	((fid1)->fid_len == (fid2)->fid_len && \
	bcmp((char *)(fid1)->fid_data, (char *)(fid2)->fid_data,  \
	(int)(fid1)->fid_len) == 0)

#define exportmatch(exi, fsid, fid) \
	(eqfsid(&(exi)->exi_fsid, fsid) && eqfid((exi)->exi_fid, fid))

#ifdef NFSDEBUG 
extern int nfsdebug; 
#endif

struct exportinfo *exported;	/* the list of exported filesystems */

/*
 * Old risc/os and BSD43 compatable exportfs system call
 */
old_exportfs(uap)
	register struct a {
		char	*dname;
		int	rootid;
		int	flags;
	} *uap;
{
	struct vnode *vp;
	struct export *kex;
	struct exportinfo **tail;
	struct exportinfo *exi;
	struct exportinfo *tmp;
	struct fid *fid;
	struct vfs *vfs;

	struct unixexport unix_export;
	struct exaddrlist root_addr_list, write_addr_list;

	if (! suser()) {
		u.u_error = EPERM;
		return;
	}

	/*
	 * Get the vfs id
	 */
	u.u_error = lookupname(uap->dname, UIO_USERSPACE, FOLLOW_LINK, 
		(struct vnode **) NULL, &vp);
	if (u.u_error) {
		return;	
	}
	u.u_error = VOP_FID(vp, &fid);
	vfs = vp->v_vfsp;
	VN_RELE(vp);
	if (u.u_error) {
		return;	
	}

	exi = (struct exportinfo *) mem_alloc(sizeof(struct exportinfo));
	exi->exi_fsid  = vfs->vfs_fsid;
	exi->exi_fid = fid;

	if (uap->flags & ~(EX_RDONLY | EX_RDMOSTLY)) {
		u.u_error = EINVAL;
		goto error_return;
	}

	kex = &exi->exi_export;
	kex->ex_flags = uap->flags;
	kex->ex_anon = nobody;
	kex->ex_auth = AUTH_UNIX;

	root_addr_list.naddrs = 1;
	root_addr_list.addrvec =
	  (struct sockaddr *)mem_alloc(sizeof(struct sockaddr));
	root_addr_list.addrvec->sa_family = AF_INET;
	bzero ((caddr_t *)root_addr_list.addrvec, sizeof(struct sockaddr));
	unix_export.rootaddrs = root_addr_list;
	kex->ex_u.exunix = unix_export;

	write_addr_list.naddrs = 1;
	write_addr_list.addrvec =
	  (struct sockaddr *)mem_alloc(sizeof(struct sockaddr));
	write_addr_list.addrvec->sa_family = AF_INET;
	bzero ((caddr_t *)write_addr_list.addrvec, sizeof(struct sockaddr));
	kex->ex_writeaddrs = write_addr_list;

	/*
	 * Commit the new information to the export list, making
	 * sure to delete the old entry for the fs, if one exists.
	 */
	tail = &exported;
	while (*tail != NULL) {
		if (exportmatch(*tail, &exi->exi_fsid, exi->exi_fid)) {
			tmp = *tail;
			*tail = (*tail)->exi_next;
			exportfree(tmp);
		} else {
			tail = &(*tail)->exi_next;
		}
	}
	exi->exi_next = NULL;
	*tail = exi;
	return;

error_return:	
	freefid(exi->exi_fid);
	mem_free((char *) exi, sizeof(struct exportinfo));
}

/*
 * NFS 4.0 and new risc/os exportfs system call
 */
exportfs(uap)
	register struct a {
		char *dname;
		struct export *uex;
	} *uap;
{
	struct vnode *vp;
	struct export *kex;
	struct exportinfo **tail;
	struct exportinfo *exi;
	struct exportinfo *tmp;
	struct fid *fid;
	struct vfs *vfs;

	if (! suser()) {
		u.u_error = EPERM;
		return;
	}

	/*
	 * Get the vfs id
	 */
	u.u_error = lookupname(uap->dname, UIO_USERSPACE, FOLLOW_LINK, 
		(struct vnode **) NULL, &vp);
	if (u.u_error) {
		return;	
	}
	u.u_error = VOP_FID(vp, &fid);
	vfs = vp->v_vfsp;
	VN_RELE(vp);
	if (u.u_error) {
		return;	
	}

	if (uap->uex == NULL) {
		u.u_error = unexport(&vfs->vfs_fsid, fid);
		freefid(fid);
		return;
	}
	exi = (struct exportinfo *) mem_alloc(sizeof(struct exportinfo));
	exi->exi_fsid  = vfs->vfs_fsid;
	exi->exi_fid = fid;
	kex = &exi->exi_export;

	/*
	 * Load in everything, and do sanity checking
	 */	
	u.u_error = copyin((caddr_t) uap->uex, (caddr_t) kex, 
		(u_int) sizeof(struct export));
	if (u.u_error) {
#ifdef RISCOS
		u.u_error = EFAULT;
#endif /* RISCOS */
		goto error_return;
	}

	if (kex->ex_flags & ~(EX_RDONLY | EX_RDMOSTLY)) {
		u.u_error = EINVAL;
		goto error_return;
	}
	if (kex->ex_flags & EX_RDMOSTLY) {
		u.u_error = loadaddrs(&kex->ex_writeaddrs);
		if (u.u_error) {
			goto error_return;
		}
	}
	switch (kex->ex_auth) {
	case AUTH_UNIX:
		u.u_error = loadaddrs(&kex->ex_unix.rootaddrs);
		break;
	case AUTH_DES:
		u.u_error = loadrootnames(kex);
		break;
	default:
		u.u_error = EINVAL;
	}
	if (u.u_error) {	
		goto error_return;
	}

	/*
	 * Commit the new information to the export list, making
	 * sure to delete the old entry for the fs, if one exists.
	 */
	tail = &exported;
	while (*tail != NULL) {
		if (exportmatch(*tail, &exi->exi_fsid, exi->exi_fid)) {
			tmp = *tail;
			*tail = (*tail)->exi_next;
			exportfree(tmp);
		} else {
			tail = &(*tail)->exi_next;
		}
	}
	exi->exi_next = NULL;
	*tail = exi;
	return;

error_return:	
	freefid(exi->exi_fid);
	mem_free((char *) exi, sizeof(struct exportinfo));
}

/*
 * Remove the exported directory from the export list
 */
unexport(fsid, fid)
	fsid_t *fsid;
	struct fid *fid;
{
	struct exportinfo **tail;	
	struct exportinfo *exi;

	tail = &exported;
	while (*tail != NULL) {
		if (exportmatch(*tail, fsid, fid)) {
			exi = *tail;
			*tail = (*tail)->exi_next;
			exportfree(exi);
			return (0);
		} else {
			tail = &(*tail)->exi_next;
		}
	}
	return (EINVAL);
}

/*
 * Get file handle system call.
 * Takes file name and returns a file handle for it.
 * Also recognizes the old getfh() which takes a file 
 * descriptor instead of a file name, and does the 
 * right thing. This compatibility will go away in 5.0.
 * It goes away because if a file descriptor refers to
 * a file, there is no simple way to find its parent 
 * directory.  
 */
nfs_getfh(uap)
	register struct a {
		char *fname;
		fhandle_t   *fhp;
	} *uap;
{
	register struct file *fp;
	fhandle_t fh;
	struct vnode *vp;
	struct vnode *dvp;
	struct exportinfo *exi;	
	int error;
	int oldgetfh = 0;

	if (!suser()) {
		u.u_error = EPERM;
		return;
	}
	if ((u_int)uap->fname < NOFILE) {
		/*
		 * old getfh()
		 */
		oldgetfh = 1;
		fp = getf((int)uap->fname);
		if (fp == NULL) {
			u.u_error = EINVAL;   
			return;
		}
		vp = (struct vnode *)fp->f_data;
		dvp = NULL;
	} else {
		/*
		 * new getfh()
		 */
		u.u_error = lookupname(uap->fname, UIO_USERSPACE, FOLLOW_LINK, 
				       &dvp, &vp);
		if (u.u_error == EEXIST) {
			/*
			 * if fname resolves to / we get EEXIST error
			 * since we wanted the parent vnode. Try again
		 	 * with NULL dvp.
			 */
			u.u_error = lookupname(uap->fname, UIO_USERSPACE,
				FOLLOW_LINK, (struct vnode **)NULL, &vp);
			dvp = NULL;
		}
		if (u.u_error == 0 && vp == NULL) {
			/*
			 * Last component of fname not found
			 */
			if (dvp) {
				VN_RELE(dvp);
			}
			u.u_error = ENOENT;
		}
		if (u.u_error) {
			return;
		}
	}
	error = findexivp(&exi, dvp, vp);
	if (!error) {
		error = makefh(&fh, vp, exi);
		if (!error) {
			error =	copyout((caddr_t)&fh, (caddr_t)uap->fhp, 
					sizeof(fh));
#ifdef RISCOS
			if (error)
				error = EFAULT;
#endif /* RISCOS */
		}
	}
	if (!oldgetfh) {
		/* 
		 * new getfh(): release vnodes
		 */
		VN_RELE(vp);
		if (dvp != NULL) {
			VN_RELE(dvp);
		}
	}
	u.u_error = error;
}

/*
 * Common code for both old getfh() and new getfh(). 
 * If old getfh(), then dvp is NULL.
 * Strategy: if vp is in the export list, then
 * return the associated file handle. Otherwise, ".."
 * once up the vp and try again, until the root of the
 * filesystem is reached.
 */
findexivp(exip, dvp, vp)
	struct exportinfo **exip;
	struct vnode *dvp;  /* parent of vnode want fhandle of */
	struct vnode *vp;   /* vnode we want fhandle of */
{
	struct fid *fid;
	int error;

	VN_HOLD(vp);
	if (dvp != NULL) {
		VN_HOLD(dvp);
	}
	for (;;) {
		error = VOP_FID(vp, &fid);
		if (error) {
			break;
		}
		*exip = findexport(&vp->v_vfsp->vfs_fsid, fid); 
		freefid(fid);
		if (*exip != NULL) {
			/*
			 * Found the export info
			 */
			error = 0;
			break;
		}

		/*
		 * We have just failed finding a matching export.
		 * If we're at the root of this filesystem, then
		 * it's time to stop (with failure).
		 */
		if (vp->v_flag & VROOT) {
			error = EINVAL;
			break;	
		}

		/*
		 * Now, do a ".." up vp. If dvp is supplied, use it,
	 	 * otherwise, look it up.
		 */
		if (dvp == NULL) {
			error = VOP_LOOKUP(vp, "..", &dvp, u.u_cred,
					   (struct pathname *)NULL, 0);
			if (error) {
				break;
			}
		}
		VN_RELE(vp);
		vp = dvp;
		dvp = NULL;
	}
	VN_RELE(vp);
	if (dvp != NULL) {
		VN_RELE(dvp);
	}
	return (error);
}

/*
 * Make an fhandle from a vnode
 */
makefh(fh, vp, exi)
	register fhandle_t *fh;
	struct vnode *vp;
	struct exportinfo *exi;
{
	struct fid *fidp;
	int error;

	error = VOP_FID(vp, &fidp);
	if (error || fidp == NULL) {
		/*
		 * Should be something other than EREMOTE
		 */
		return (EREMOTE);
	}
	if (fidp->fid_len + exi->exi_fid->fid_len + sizeof(fsid_t) 
		> NFS_FHSIZE) 
	{
		freefid(fidp);
		return (EREMOTE);
	}
	bzero((caddr_t) fh, sizeof(*fh));
	fh->fh_fsid.val[0] = vp->v_vfsp->vfs_fsid.val[0];
	fh->fh_fsid.val[1] = vp->v_vfsp->vfs_fsid.val[1];
	fh->fh_len = fidp->fid_len;
	bcopy(fidp->fid_data, fh->fh_data, fidp->fid_len);
	fh->fh_xlen = exi->exi_fid->fid_len;
	bcopy(exi->exi_fid->fid_data, fh->fh_xdata, fh->fh_xlen);
#ifdef NFSDEBUG
#ifdef mips
	dprint(nfsdebug, 4, "makefh: vp %x fsid %x %x len %d data %d %d %d %d\n",
		vp, fh->fh_fsid.val[0], fh->fh_fsid.val[1], fh->fh_len,
		*(ushort *)fh->fh_data,
	        *(ushort *)&fh->fh_data[sizeof(ushort)],
	        *(ushort *)&fh->fh_data[2*sizeof(ushort)],
	        *(ushort *)&fh->fh_data[3*sizeof(ushort)]);
#else
	dprint(nfsdebug, 4, "makefh: vp %x fsid %x %x len %d data %d %d\n",
		vp, fh->fh_fsid.val[0], fh->fh_fsid.val[1], fh->fh_len,
		*(int *)fh->fh_data, *(int *)&fh->fh_data[sizeof(int)]);
#endif /* mips */
#endif
	freefid(fidp);
	return (0);
}

/*
 * Find the export structure associated with the given filesystem
 */
struct exportinfo *
findexport(fsid, fid)
	fsid_t *fsid;	
	struct fid *fid;
{
	struct exportinfo *exi;

	for (exi = exported; exi != NULL; exi = exi->exi_next) {
		if (exportmatch(exi, fsid, fid)) {
			return (exi);
		}
	}
	return (NULL);
}

/*
 * Load from user space, a list of internet addresses into kernel space
 */
loadaddrs(addrs)
	struct exaddrlist *addrs;
{
	int error;
	int allocsize;
	struct sockaddr *uaddrs;

	if (addrs->naddrs > EXMAXADDRS) {
		return (EINVAL);
	}
	if (addrs->naddrs == 0) { /* Make this a no-op case */
	        return 0;
	 }

	allocsize = addrs->naddrs * sizeof(struct sockaddr);
	uaddrs = addrs->addrvec;

	addrs->addrvec = (struct sockaddr *)mem_alloc(allocsize);
	error = copyin((caddr_t)uaddrs, (caddr_t)addrs->addrvec,
		       (u_int)allocsize);	
	if (error) {
#ifdef RISCOS
		error = EFAULT;
#endif /* RISCOS */
		mem_free((char *)addrs->addrvec, allocsize);
	}
	return (error);
}

/*
 * Load from user space the root user names into kernel space
 * (AUTH_DES only)
 */
loadrootnames(kex)
	struct export *kex;
{
	int error;
	char *exnames[EXMAXROOTNAMES];
	int i;
	u_int len;
	char netname[MAXNETNAMELEN+1];
	u_int allocsize;

	if (kex->ex_des.nnames > EXMAXROOTNAMES) {
		return (EINVAL);
	}

#ifdef RISCOS
	if (kex->ex_des.nnames == 0) { /* Make this a no-op success */
	       return 0;
	}
#endif
	/*
	 * Get list of names from user space
	 */
	allocsize =  kex->ex_des.nnames * sizeof(char *);
	error = copyin((char *)kex->ex_des.rootnames, (char *)exnames,
		allocsize);
	if (error) {
#ifdef RISCOS
		error = EFAULT;
#endif /* RISCOS */
		return (error);
	}
	kex->ex_des.rootnames = (char **) mem_alloc(allocsize);
	bzero((char *) kex->ex_des.rootnames, allocsize);

	/*
	 * And now copy each individual name
	 */
	for (i = 0; i < kex->ex_des.nnames; i++) {
		error = copyinstr(exnames[i], netname, sizeof(netname), &len);
		if (error) {
			goto freeup;
		}
		kex->ex_des.rootnames[i] = mem_alloc(len + 1);
		bcopy(netname, kex->ex_des.rootnames[i], len);
		kex->ex_des.rootnames[i][len] = 0;
	}
	return (0);

freeup:
	freenames(kex);
	return (error);
}

/*
 * Figure out everything we allocated in a root user name list in
 * order to free it up. (AUTH_DES only)
 */
freenames(ex)
	struct export *ex;
{
	int i;

	for (i = 0; i < ex->ex_des.nnames; i++) {
		if (ex->ex_des.rootnames[i] != NULL) {
			mem_free((char *) ex->ex_des.rootnames[i],
				strlen(ex->ex_des.rootnames[i]) + 1);
		}
	}	
	mem_free((char *) ex->ex_des.rootnames, ex->ex_des.nnames * sizeof(char *));
}


/*
 * Free an entire export list node
 */
exportfree(exi)
	struct exportinfo *exi;
{
	struct export *ex;

	ex = &exi->exi_export;
	switch (ex->ex_auth) {
	case AUTH_UNIX:
#ifdef RISCOS
		if (ex->ex_unix.rootaddrs.naddrs > 0)
#endif
		  mem_free((char *)ex->ex_unix.rootaddrs.addrvec, 
		    (ex->ex_unix.rootaddrs.naddrs * sizeof(struct sockaddr)));
		break;
	case AUTH_DES:
		freenames(ex);
		break;
	}
	if (ex->ex_flags & EX_RDMOSTLY) {
#ifdef RISCOS
		if (ex->ex_writeaddrs.naddrs > 0)
#endif
		  mem_free((char *)ex->ex_writeaddrs.addrvec,
		    ex->ex_writeaddrs.naddrs * sizeof(struct sockaddr));
	}
	freefid(exi->exi_fid);
	mem_free(exi, sizeof(struct exportinfo));
}
