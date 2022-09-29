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
#ident	"$Header: nfs_server.c,v 1.6.1.15.1.3 90/07/12 14:32:19 hawkes Exp $"

/*
 * @(#)nfs_server.c 1.7 88/08/06 NFSSRC4.0 from 2.85 88/02/08 SMI
 *
 * Copyright (c) 1987 by Sun Microsystems, Inc.
 */

#include "sys/types.h"
#include "sys/param.h"
#include "sys/systm.h"

#include "sys/signal.h"		/* needed by user.h */
#include "sys/pcb.h"		/* needed by user.h */
#include "sys/user.h"

#include "sys/buf.h"
#ifndef NFSSERVER
#define NFSSERVER 1
#endif NFSSERVER
#include "sys/vfs.h"
#include "sys/vnode.h"
#include "sys/pathname.h"
#include "sys/uio.h"
#include "sys/file.h"
#include "sys/socketvar.h"
#include "bsd43/sys/stat.h"
#include "sys/errno.h"
#include "sys/mbuf.h"
#include "sys/socket.h"
#include "bsd/netinet/in.h"
#include "../rpc/types.h"
#include "../rpc/auth.h"
#include "../rpc/auth_unix.h"
#include "../rpc/auth_des.h"
#include "../rpc/svc.h"
#include "../rpc/xdr.h"
#include "sys/fs/nfs.h"
#include "sys/fs/nfs_export.h"
#include "bsd43/sys/dirent.h"
#include "sys/kmem.h"
#include "sys/debug.h"
#include "sys/cmn_err.h"
#ifdef RISCOS
#include "sys/fs/nfs_rnode.h"
#endif /* RISCOS */


#ifdef RISCOS
int dup_writes = 0;
int dup_creates = 0;
int dup_removes = 0;
int dup_links = 0;
int dup_symlinks = 0;
int dup_mkdirs = 0;
int dup_rmdirs = 0;
int dup_renames = 0;
int dup_setattrs = 0;
int throwaways = 0;
#endif /* RISCOS */

#ifdef RISCOS
extern int _nfs_sync_writes;	/* a boolean value */
#endif
extern struct vnodeops nfs_vnodeops;
/*
 * rpc service program version range supported
 */
#define	VERSIONMIN	2
#define	VERSIONMAX	2

/*
 * Returns true iff exported filesystem is read-only
 */
#define rdonly(exi, req) (((exi)->exi_export.ex_flags & EX_RDONLY) || \
			  (((exi)->exi_export.ex_flags & EX_RDMOSTLY) && \
			   !hostinlist((struct sockaddr *)\
					svc_getcaller((req)->rq_xprt), \
					&(exi)->exi_export.ex_writeaddrs)))

struct vnode	*fhtovp();
struct file	*getsock();
void		svcerr_progvers();
void		rfs_dispatch();

#ifdef NFSDEBUG
extern int nfsdebug;
#endif

struct {
	int	ncalls;		/* number of calls received */
	int	nbadcalls;	/* calls that failed */
	int	reqs[32];	/* count for each request */
} svstat;

#ifdef RISCOS
int	nfs_chars = 61680;
#else
int	nfs_chars = 50000;
#endif /* RISCOS */
int	nfsd_count = 0;

/*
 * NFS Server system call.
 * Does all of the work of running a NFS server.
 * sock is the fd of an open UDP socket.
 */
nfs_svc(uap)
	struct a {
		int	sock;
	} *uap;
{
	struct socket   *so;
	struct file	*fp;
	SVCXPRT *xprt;
	u_long vers;
	int error;

	fp = getsock(uap->sock);
	if (fp == NULL) {
		u.u_error = EBADF;
		return;
	}
	so = (struct socket *)fp->f_data;

	/*
	 * Allocate extra space for this socket, to minimize
	 * lost requests for NFS.  We don't want everyone to do
	 * this, so do it here, rather than in udp_usrreq().
	 */
#ifdef RISCOS
		error = soreserve(so, nfs_chars, nfs_chars);
#else
	error = soreserve(so, nfs_chars,
		 nfs_chars + 2 *(sizeof (struct sockaddr)));
#endif /* RISCOS */
	if (error) {
		u.u_error = error;
		return;
	}

	/* Now, release client memory; we never return back to user */
	vrelvm();

	xprt = svckudp_create(so, NFS_PORT);
	for (vers = VERSIONMIN; vers <= VERSIONMAX; vers++) {
		(void) svc_register(xprt, NFS_PROGRAM, vers, rfs_dispatch,
		    FALSE);
	}
#ifdef RISCOS
	if (setjmp(u.u_qsav)) {
#else
	if (setjmp(&u.u_qsave)) {
#endif
		if(--nfsd_count == 0)
			for (vers = VERSIONMIN; vers <= VERSIONMAX; vers++){
				svc_unregister(NFS_PROGRAM, vers);
			}
		SVC_DESTROY(xprt);
		u.u_error = EINTR;	/* not needed; may help debugging */
		exit(0);
	} else {
		nfsd_count++;
		svc_run(xprt);  /* never returns */
	}
	/* NOTREACHED */
}


/*
 * These are the interface routines for the server side of the
 * Networked File System.  See the NFS protocol specification
 * for a description of this interface.
 */


/*
 * Get file attributes.
 * Returns the current attributes of the file with the given fhandle.
 */
int
rfs_getattr(fhp, ns, exi)
	fhandle_t *fhp;
	register struct nfsattrstat *ns;
	struct exportinfo *exi;
{
	int error;
	register struct vnode *vp;
	struct vattr va;

#ifdef NFSDEBUG
	dprint(nfsdebug, 4, "rfs_getattr fh %x %x %d\n",
	    fhp->fh_fsid.val[0], fhp->fh_fsid.val[1], fhp->fh_len);
#endif
	vp = fhtovp(fhp, exi);
	if (vp == NULL) {
		ns->ns_status = NFSERR_STALE;
		return;
	}
	error = VOP_GETATTR(vp, &va, u.u_cred);
	if (!error) {
		vattr_to_nattr(&va, &ns->ns_attr);
	}
	ns->ns_status = puterrno(error);
	VN_RELE(vp);
#ifdef NFSDEBUG
	dprint(nfsdebug, 5, "rfs_getattr: returning %d\n", error);
#endif
}

/*
 * Set file attributes.
 * Sets the attributes of the file with the given fhandle.  Returns
 * the new attributes.
 */
int
rfs_setattr(args, ns, exi, req)
	struct nfssaargs *args;
	register struct nfsattrstat *ns;
	struct exportinfo *exi;
	struct svc_req *req;
{
	int error = 0;		/* Sun bug: 1019968; set to 0 */
	register struct vnode *vp;
	struct vattr va;

#ifdef NFSDEBUG
	dprint(nfsdebug, 4, "rfs_setattr fh %x %x %d\n",
	    args->saa_fh.fh_fsid.val[0], args->saa_fh.fh_fsid.val[1],
	    args->saa_fh.fh_len);
#endif
	vp = fhtovp(&args->saa_fh, exi);
	if (vp == NULL) {
		ns->ns_status = NFSERR_STALE;
		return;
	}
	if (rdonly(exi, req)) {
		error = EROFS;
	} else {
		error = 0;
		sattr_to_vattr(&args->saa_sa, &va);
		/*
		 * Allow SysV-compatible option to set access and
		 * modified times if root, owner, or write access.
		 *
		 * XXX - Until an NFS Protocol Revision, this may be
		 *       simulated by setting the client time in the
		 *       tv_sec field of the access and modified times
		 *       and setting the tv_usec field of the modified
		 *       time to an invalid value (1,000,000).  This
		 *       may be detected by servers modified to do the
		 *       right thing, but will not be disastrous on
		 *       unmodified servers.
		 *
		 * XXX - For now, va_mtime.tv_usec == -1 flags this in
		 *       VOP_SETATTR().
		 */
		if ((va.va_mtime.tv_sec != -1) &&
		    (va.va_mtime.tv_usec == 1000000)) {
			va.va_mtime.tv_sec = 0;
			va.va_mtime.tv_usec = -1;
			va.va_atime.tv_sec = -1;
			va.va_atime.tv_usec = -1;
		}

		/*
		 *  Handle case where we are extending a file via setattr.
		 *  The case we have to watch out for is when there are no
		 *  blocks currently allocated to the file.
		 *  For now, just invoke VOP_RDWR to write a single zero
		 *  at the end of the file.
		 */
		if (vp->v_type == VREG && va.va_size != (u_long)-1) {
			struct vattr tva;
			struct iovec iov;
			struct uio uio;
			char zero = 0;

			/* get current attributes */
			error = VOP_GETATTR(vp, &tva, u.u_cred);
			if (!error && va.va_size > tva.va_size) {
				/* need to extend the file */
				iov.iov_base = &zero;
				iov.iov_len = 1;
				uio.uio_iov = &iov;
				uio.uio_iovcnt = 1;
				uio.uio_seg = UIO_SYSSPACE;
				uio.uio_offset = va.va_size - 1;
#if RISCOS
				uio.uio_fmode = FWRITE;
#endif
				uio.uio_resid = 1;
				error = VOP_RDWR(vp, &uio, UIO_WRITE,
					IO_SYNC, u.u_cred);
			}
		}
		if(!error) {
			error = VOP_SETATTR(vp, &va, u.u_cred);
			if (!error) {
				error = VOP_GETATTR(vp, &va, u.u_cred);
				if (!error) {
					vattr_to_nattr(&va, &ns->ns_attr);
				}
			}
		}
	}
	ns->ns_status = puterrno(error);
	VN_RELE(vp);
#ifdef NFSDEBUG
	dprint(nfsdebug, 5, "rfs_setattr: returning %d\n", error);
#endif
}

/*
 * Directory lookup.
 * Returns an fhandle and file attributes for file name in a directory.
 */
int
rfs_lookup(da, dr, exi)
	struct nfsdiropargs *da;
	register struct  nfsdiropres *dr;
	struct exportinfo *exi;
{
	int error;
	register struct vnode *dvp;
	struct vnode *vp;
	struct vattr va;

#ifdef NFSDEBUG
	dprint(nfsdebug, 4, "rfs_lookup %s fh %x %x %x %d\n",
	    da->da_name, da->da_fhandle.fh_fsid.val[0],
	    da->da_fhandle.fh_fsid.val[1], da->da_fhandle.fh_len);
#endif
#ifdef RISCOS		/* Sun bug: 1015159 */
	/* Disallow NULL paths */
	if ((da->da_name == (char *) NULL) || (*da->da_name == '\0')) {
		dr->dr_status = NFSERR_ACCES;
		return;
	}
#endif
	dvp = fhtovp(&da->da_fhandle, exi);
	if (dvp == NULL) {
		dr->dr_status = NFSERR_STALE;
		return;
	}

	/*
	 * do lookup.
	 */
	error = VOP_LOOKUP(dvp, da->da_name, &vp, u.u_cred,
				(struct pathname *) NULL, 0);
	if (error) {
		vp = (struct vnode *)NULL;
	} else {
		error = VOP_GETATTR(vp, &va, u.u_cred);
		if (!error) {
			vattr_to_nattr(&va, &dr->dr_attr);
			error = makefh(&dr->dr_fhandle, vp, exi);
		}
	}
	dr->dr_status = puterrno(error);
	if (vp) {
		VN_RELE(vp);
	}
	VN_RELE(dvp);
#ifdef NFSDEBUG
	dprint(nfsdebug, 5, "rfs_lookup: returning %d\n", error);
#endif
}

/*
 * Read symbolic link.
 * Returns the string in the symbolic link at the given fhandle.
 */
int
rfs_readlink(fhp, rl, exi)
	fhandle_t *fhp;
	register struct nfsrdlnres *rl;
	struct exportinfo *exi;
{
	int error;
	struct iovec iov;
	struct uio uio;
	struct vnode *vp;

#ifdef NFSDEBUG
	dprint(nfsdebug, 4, "rfs_readlink fh %x %x %d\n",
	    fhp->fh_fsid.val[0], fhp->fh_fsid.val[1], fhp->fh_len);
#endif
	vp = fhtovp(fhp, exi);
	if (vp == NULL) {
		rl->rl_status = NFSERR_STALE;
		return;
	}

	/*
	 * Allocate data for pathname.  This will be freed by rfs_rlfree.
	 */
	rl->rl_data = (char *)kmem_alloc((u_int)MAXPATHLEN);

	/*
	 * Set up io vector to read sym link data
	 */
	iov.iov_base = rl->rl_data;
	iov.iov_len = NFS_MAXPATHLEN;
	uio.uio_iov = &iov;
	uio.uio_iovcnt = 1;
	uio.uio_segflg = UIO_SYSSPACE;
	uio.uio_offset = 0;
#if RISCOS
	uio.uio_fmode = 0; /* XXX */
#endif
	uio.uio_resid = NFS_MAXPATHLEN;

	/*
	 * read link
	 */
	error = VOP_READLINK(vp, &uio, u.u_cred);

	/*
	 * Clean up
	 */
	if (error) {
		kmem_free((caddr_t)rl->rl_data, (u_int)NFS_MAXPATHLEN);
		rl->rl_count = 0;
		rl->rl_data = NULL;
	} else {
		rl->rl_count = NFS_MAXPATHLEN - uio.uio_resid;
	}
	rl->rl_status = puterrno(error);
	VN_RELE(vp);
#ifdef NFSDEBUG
	dprint(nfsdebug, 5, "rfs_readlink: returning '%s' %d\n",
	    rl->rl_data, error);
#endif
}

/*
 * Free data allocated by rfs_readlink
 */
rfs_rlfree(rl)
	struct nfsrdlnres *rl;
{

	if (rl->rl_data) {
		kmem_free((caddr_t)rl->rl_data, (u_int)NFS_MAXPATHLEN);
	}
}

int nfsreadmap = 1;

/*
 * Read data.
 * Returns some data read from the file at the given fhandle.
 */
int
rfs_read(ra, rr, exi)
	struct nfsreadargs *ra;
	register struct nfsrdresult *rr;
	struct exportinfo *exi;
{
	int error;
	struct vnode *vp;
	struct vattr va;
	struct iovec iov;
	struct uio uio;

#ifdef NFSDEBUG
	dprint(nfsdebug, 4, "rfs_read %d from fh %x %x %d\n",
	    ra->ra_count, ra->ra_fhandle.fh_fsid.val[0],
	    ra->ra_fhandle.fh_fsid.val[1], ra->ra_fhandle.fh_len);
#endif
	rr->rr_data = NULL;
	rr->rr_count = 0;
	vp = fhtovp(&ra->ra_fhandle, exi);
	if (vp == NULL) {
		rr->rr_status = NFSERR_STALE;
		return;
	}
	if (vp->v_type != VREG) {
		printf("rfs_read: attempt to read from non-file\n");
		error = EISDIR;
	} else {
		error = VOP_GETATTR(vp, &va, u.u_cred);
	}
	if (error) {
		goto bad;
	}

	/*
	 * This is a kludge to allow reading of files created
	 * with no read permission.  The owner of the file
	 * is always allowed to read it.
	 */
	if (u.u_uid != va.va_uid) {
		error = VOP_ACCESS(vp, VREAD, u.u_cred);
		if (error) {
			/*
			 * Exec is the same as read over the net because
			 * of demand loading.
			 */
			error = VOP_ACCESS(vp, VEXEC, u.u_cred);
		}
		if (error) {
			goto bad;
		}
	}

	if (ra->ra_offset >= va.va_size) {
		rr->rr_count = 0;
		vattr_to_nattr(&va, &rr->rr_attr);
		goto done;			/* hit EOF */
	}

	/*
	 * Allocate space for data.  This will be freed by xdr_rdresult
	 * when it is called with x_op = XDR_FREE.
	 */
	rr->rr_data = kmem_alloc((u_int)ra->ra_count);

	/*
	 * Set up io vector
	 */
	iov.iov_base = rr->rr_data;
	iov.iov_len = ra->ra_count;
	uio.uio_iov = &iov;
	uio.uio_iovcnt = 1;
	uio.uio_segflg = UIO_SYSSPACE;
	uio.uio_offset = ra->ra_offset;
#if RISCOS
	uio.uio_fmode = FREAD;
#endif
	uio.uio_resid = ra->ra_count;
	/*
	 * For now we assume no append mode and
	 * ignore totcount (read ahead)
	 */
	error = VOP_RDWR(vp, &uio, UIO_READ, IO_SYNC, u.u_cred);
	if (error) {
		goto bad;
	}
#ifdef RISCOS		/* Fix Sun bug: 1010637 */
	else {
		/*
		 * Get attributes again so we send the latest attribute 
		 * to the client side for his cache.
		 */
		if (error = VOP_GETATTR(vp, &va, u.u_cred))
			goto bad;
	}
#endif
	vattr_to_nattr(&va, &rr->rr_attr);
	rr->rr_count = ra->ra_count - uio.uio_resid;
#ifndef RISCOS
	/*
	 * free the unused part of the data allocated
	 */
	if (uio.uio_resid) {
		kmem_free(rr->rr_data + rr->rr_count, (u_int)uio.uio_resid);
	}
#endif
bad:
	if (error && rr->rr_data != NULL) {
		kmem_free(rr->rr_data, (u_int)ra->ra_count);
		rr->rr_data = NULL;
		rr->rr_count = 0;
	}
done:
	rr->rr_status = puterrno(error);
	VN_RELE(vp);
#ifdef NFSDEBUG
	dprint(nfsdebug, 5, "rfs_read returning %d, count = %d\n",
	    error, rr->rr_count);
#endif
}

/*
 * Free data allocated by rfs_read.
 */
rfs_rdfree(rr)
	struct nfsrdresult *rr;
{

	if (rr->rr_data != NULL) {
		kmem_free(rr->rr_data, (u_int)rr->rr_count);
	}
}

/*
 * Write data to file.
 * Returns attributes of a file after writing some data to it.
 */
int
rfs_write(wa, ns, exi, req)
	struct nfswriteargs *wa;
	struct nfsattrstat *ns;
	struct exportinfo *exi;
	struct svc_req *req;
{
	register int error;
	register struct vnode *vp;
	struct vattr va;
	struct iovec iov;
	struct uio uio;
#ifdef RISCOS
	int ioflag = (_nfs_sync_writes == 0) ? IO_SYNC : 0;
#endif

#ifdef NFSDEBUG
	dprint(nfsdebug, 4, "rfs_write: %d bytes fh %x %x %d\n",
	    wa->wa_count, wa->wa_fhandle.fh_fsid.val[0],
	    wa->wa_fhandle.fh_fsid.val[1], wa->wa_fhandle.fh_len);
#endif
	vp = fhtovp(&wa->wa_fhandle, exi);
	if (vp == NULL) {
		ns->ns_status = NFSERR_STALE;
		return;
	}
	if (rdonly(exi, req)) {
		error = EROFS;
#ifdef RISCOS		/* Fix Sun bug: km04, left out 'else' */
	} else if (vp->v_type != VREG) {
#else
	} if (vp->v_type != VREG) {
#endif
		printf("rfs_write: attempt to write to non-file\n");
		error = EISDIR;
	} else {
		error = VOP_GETATTR(vp, &va, u.u_cred);
	}
	if (!error) {
		if (u.u_uid != va.va_uid) {
			/*
			 * This is a kludge to allow writes of files created
			 * with read only permission.  The owner of the file
			 * is always allowed to write it.
			 */
			error = VOP_ACCESS(vp, VWRITE, u.u_cred);
		}
		if (!error) {
#ifdef RISCOS
			/* This allows us to perform ASYNC NFS writes
			 * which is against the protocol but faster.
			 * Assume that both flags are not set because
			 * of check in vfs_setflags().
			 */
#if NFSDEBUG
			ASSERT(!((vp->v_vfsp->vfs_flag & VFS_NFSSYNC) && (vp->v_vfsp->vfs_flag & VFS_NFSASYNC)));
#endif
			if (vp->v_vfsp == NULL)
				panic("nfs_server");
			else if (vp->v_vfsp->vfs_flag & VFS_NFSSYNC)
				ioflag |= IO_SYNC;
			else if (vp->v_vfsp->vfs_flag & VFS_NFSASYNC)
				ioflag &= ~IO_SYNC;
#endif
			if (wa->wa_data) {
				iov.iov_base = wa->wa_data;
				iov.iov_len = wa->wa_count;
				uio.uio_iov = &iov;
				uio.uio_iovcnt = 1;
				uio.uio_segflg = UIO_SYSSPACE;
				uio.uio_offset = wa->wa_offset;
#ifdef RISCOS
				uio.uio_fmode = FWRITE;
#endif
				uio.uio_resid = wa->wa_count;
				/*
				 * for now we assume no append mode
				 */
				error = VOP_RDWR(vp, &uio, UIO_WRITE, ioflag,
				    u.u_cred);
			} else { 	/* mbuf hack */
				register struct mbuf *m;
				register struct iovec *iovp;
				register int iovcnt;
				static caddr_t *base10, *base40;

				iovcnt = 0;
				for (m = (struct mbuf *)wa->wa_mbuf; m;
					m = m->m_next) {
					iovcnt++;
				}
				if (iovcnt < 10) {
					iovp = (struct iovec *)kmem_fast_alloc(
					    (caddr_t *)&base10,
					    sizeof (struct iovec) * 10, 10);
				} else if (iovcnt < 40) {
					iovp = (struct iovec *)kmem_fast_alloc(
					    (caddr_t *)&base40,
					    sizeof (struct iovec) * 40, 10);
				} else {
					iovp = (struct iovec *)kmem_alloc(
					    (u_int)(sizeof (struct iovec) *
					    iovcnt));
				}
				mbuf_to_iov((struct mbuf *)wa->wa_mbuf, iovp);
				uio.uio_iov = iovp;
				uio.uio_iovcnt = iovcnt;
				uio.uio_segflg = UIO_SYSSPACE;
				uio.uio_offset = wa->wa_offset;
#if RISCOS
				uio.uio_fmode = FWRITE;
#endif
				uio.uio_resid = wa->wa_count;
				/*
				 * for now we assume no append mode
				 */
				error = VOP_RDWR(vp, &uio, UIO_WRITE, ioflag,
				    u.u_cred);
				if (iovcnt < 10) {
					kmem_fast_free((caddr_t *)&base10,
					    (caddr_t)iovp);
				} else if (iovcnt < 40) {
					kmem_fast_free((caddr_t *)&base40,
					    (caddr_t)iovp);
				} else {
					kmem_free((caddr_t)iovp,
					    (u_int)(sizeof (struct iovec) *
					    iovcnt));
				}
			}
		}
	}
	if (!error) {
		/*
		 * Get attributes again so we send the latest mod
		 * time to the client side for his cache.
		 */
		error = VOP_GETATTR(vp, &va, u.u_cred);
	}
	ns->ns_status = puterrno(error);
	if (!error) {
		vattr_to_nattr(&va, &ns->ns_attr);
	}
	VN_RELE(vp);
#ifdef NFSDEBUG
	dprint(nfsdebug, 5, "rfs_write: returning %d\n", error);
#endif
}

static
mbuf_to_iov(m, iov)
	register struct mbuf *m;
	register struct iovec *iov;
{

	while (m) {
		iov->iov_base = mtod(m, caddr_t);
		iov->iov_len = m->m_len;
		iov++;
		m = m->m_next;
	}
}

/*
 * Create a file.
 * Creates a file with given attributes and returns those attributes
 * and an fhandle for the new file.
 */
int
rfs_create(args, dr, exi, req)
	struct nfscreatargs *args;
	struct  nfsdiropres *dr;
	struct exportinfo *exi;
	struct svc_req *req;
{
	register int error;
	struct vattr va;
	struct vnode *vp;
	register struct vnode *dvp;
#ifdef RISCOS		/* Sun bug: 1015159; change all references to 'name' */
	register char *name = args->ca_da.da_name;
	struct timeval duptime;
	int dupmark;
	struct rnode *drp;
#endif

#ifdef NFSDEBUG
	dprint(nfsdebug, 4, "rfs_create: %s dfh %x %x %d\n",
	    name, args->ca_da.da_fhandle.fh_fsid.val[0],
	    args->ca_da.da_fhandle.fh_fsid.val[1], args->ca_da.da_fhandle.fh_len);
#endif
#ifdef RISCOS		/* Sun bug: 1015159 */
	/* Disallow NULL paths */
	if ((name == (char *)NULL) || (*name == '\0')) {
		dr->dr_status = NFSERR_ACCES;
		return;
	}
#endif	
	sattr_to_vattr(&args->ca_sa, &va);
	/*
	 * This is a completely gross hack to make mknod
	 * work over the wire until we can wack the protocol
	 */
#define IFMT		0170000		/* type of file */
#define IFCHR		0020000		/* character special */
#define IFBLK		0060000		/* block special */
#define	IFSOCK		0140000		/* socket */
	if ((va.va_mode & IFMT) == IFCHR) {
		va.va_type = VCHR;
		if (va.va_size == (u_long)NFS_FIFO_DEV)
			va.va_type = VFIFO;	/* xtra kludge for named pipe */
		else
			va.va_rdev = (dev_t)va.va_size;
		va.va_size = 0;
#ifndef RISCOS		/* Sun bug: 1017303 */
		va.va_mode &= ~IFMT;
#endif
	} else if ((va.va_mode & IFMT) == IFBLK) {
		va.va_type = VBLK;
		va.va_rdev = (dev_t)va.va_size;
		va.va_size = 0;
#ifndef RISCOS		/* Sun bug: 1017303 */
		va.va_mode &= ~IFMT;
#endif
	} else if ((va.va_mode & IFMT) == IFSOCK) {
		va.va_type = VSOCK;
	} else {
		va.va_type = VREG;
	}
#ifdef RISCOS		/* Sun bug: 1017303 */
	va.va_mode &= ~IFMT;
#endif
	/*
	 * XXX - Should get exclusive flag and use it.
	 */
	dvp = fhtovp(&args->ca_da.da_fhandle, exi);
	if (dvp == NULL) {
		dr->dr_status = NFSERR_STALE;
		return;
	}
	if (rdonly(exi, req)) {
		error = EROFS;
	} else {
#ifdef RISCOS
		drp = vtor(dvp);
		if (va.va_size == 0 &&
			svckudp_dup(req, &duptime, &dupmark) &&
			duptime.tv_sec == drp->r_attr.va_ctime.tv_sec &&
			duptime.tv_usec == drp->r_attr.va_ctime.tv_usec) {
			++dup_creates;
			/*
			 * XXX - This is not quite right (Need to save more
			 *	info for the dup's to return)
			 */
			error = VOP_LOOKUP(dvp, name, &vp,
				u.u_cred, (struct pathname *)NULL, 0);
		}
		else {
			error = VOP_CREATE(dvp, name,
				&va, NONEXCL, VWRITE, &vp, u.u_cred);
		}

		if (!error) {
			error = VOP_GETATTR(vp, &va, u.u_cred);
			if (!error) {
				vattr_to_nattr(&va, &dr->dr_attr);
				error = makefh(&dr->dr_fhandle, vp, exi);
			}
			VN_RELE(vp);
		}

		if (error) {
			svckudp_dupsave(req, time, DUP_FAIL);
		} else {
			svckudp_dupsave(req, drp->r_attr.va_ctime, DUP_DONE);
		}
	}
#else
		/*
		 * Very strange. If we are going to trunc the file
		 * (va_size == 0) we check the cache first to be sure
		 * this is not a delayed retransmission, otherwise we
		 * do the create and if it fails check the cache to see
		 * if it was a retransmission.
		 * XXX this really points out a protocol bug:
		 * The server should always do exclusive create.
		 */
		if (va.va_size == 0 && svckudp_dup(req)) {
			error = VOP_LOOKUP(dvp, name, &vp,
			    u.u_cred, (struct pathname *) NULL, 0);
		} else {
			error = VOP_CREATE(dvp, name,
			    &va, NONEXCL, VWRITE, &vp, u.u_cred);
			if (error && svckudp_dup(req)) {
				error = VOP_LOOKUP(dvp, name,
				    &vp, u.u_cred, (struct pathname *) NULL, 0);
			} else if (!error) {
				svckudp_dupsave(req);
			}
		}
	}
	if (!error) {
		error = VOP_GETATTR(vp, &va, u.u_cred);
		if (!error) {
			vattr_to_nattr(&va, &dr->dr_attr);
			error = makefh(&dr->dr_fhandle, vp, exi);
		}
		VN_RELE(vp);
	}
#endif /* RISCOS */
	dr->dr_status = puterrno(error);
	VN_RELE(dvp);
#ifdef NFSDEBUG
	dprint(nfsdebug, 5, "rfs_create: returning %d\n", error);
#endif
}

/*
 * Remove a file.
 * Remove named file from parent directory.
 */
int
rfs_remove(da, status, exi, req)
	struct nfsdiropargs *da;
	enum nfsstat *status;
	struct exportinfo *exi;
	struct svc_req *req;
{
	int error;
	register struct vnode *vp;
#ifdef RISCOS
	struct timeval duptime;
	int dupmark;
	struct rnode *rp;
#endif /* RISCOS */

#ifdef NFSDEBUG
	dprint(nfsdebug, 4, "rfs_remove %s dfh %x %x %d\n",
	    da->da_name, da->da_fhandle.fh_fsid.val[0],
	    da->da_fhandle.fh_fsid.val[1], da->da_fhandle.fh_len);
#endif
#ifdef RISCOS			/* Sun bug: 1015159 */
	/* Disallow NULL paths */
	if ((da->da_name == (char *) NULL) || (*da->da_name == '\0')) {
		*status = NFSERR_ACCES;
		return;
	}
#endif
	vp = fhtovp(&da->da_fhandle, exi);
	if (vp == NULL) {
#ifdef RISCOS
		/*
		 * See if this is a duplicate of a request that worked
		 * outside the throwaway window.
		 */
		if (svckudp_dup(req, &duptime, &dupmark)) {
			*status = NFS_OK;
			++dup_removes;
			return;
		}
#endif /* RISCOS */
		*status = NFSERR_STALE;
		return;
	}
	if (rdonly(exi, req)) {
		error = EROFS;
	} else {
#ifdef RISCOS
		error = VOP_REMOVE(vp, da->da_name, u.u_cred);

		if (error) {
			svckudp_dupsave(req, time, DUP_FAIL);
		} else {
			rp = vtor(vp);
			svckudp_dupsave(req, rp->r_attr.va_ctime, DUP_DONE);
		}
#else
		error = VOP_REMOVE(vp, da->da_name, u.u_cred);
		if (error == ENOENT) {
			/*
			 * check for dup request
			 */
			if (svckudp_dup(req)) {
				error = 0;
			}
		} else if (!error) {
			svckudp_dupsave(req);
		}
#endif /* RISCOS */
	}
	*status = puterrno(error);
	VN_RELE(vp);
#ifdef NFSDEBUG
	dprint(nfsdebug, 5, "rfs_remove: %s returning %d\n",
	    da->da_name, error);
#endif
}

/*
 * rename a file
 * Give a file (from) a new name (to).
 */
int
rfs_rename(args, status, exi, req)
	struct nfsrnmargs *args;
	enum nfsstat *status;
	struct exportinfo *exi;
	struct svc_req *req;
{
	int error;
	register struct vnode *fromvp;
	register struct vnode *tovp;
#ifdef RISCOS
	struct timeval duptime;
	int dupmark;
	struct rnode *fromrp;
#endif /* RISCOS */

#ifdef NFSDEBUG
	dprint(nfsdebug, 4, "rfs_rename %s ffh %x %x %d -> %s tfh %x %x %d\n",
	    args->rna_from.da_name,
	    args->rna_from.da_fhandle.fh_fsid.val[0],
	    args->rna_from.da_fhandle.fh_fsid.val[1],
	    args->rna_from.da_fhandle.fh_len,
	    args->rna_to.da_name,
	    args->rna_to.da_fhandle.fh_fsid.val[0],
	    args->rna_to.da_fhandle.fh_fsid.val[1],
	    args->rna_to.da_fhandle.fh_len);
#endif
#ifdef RISCOS
	/* Disallow NULL paths */
	if ((args->rna_from.da_name == (char *) NULL) ||
	    (*args->rna_from.da_name == '\0') ||
	    (args->rna_to.da_name == (char *) NULL) ||
	    (*args->rna_to.da_name == '\0')) {
		*status = NFSERR_ACCES;
		return;
	}
#endif
	fromvp = fhtovp(&args->rna_from.da_fhandle, exi);
	if (fromvp == NULL) {
		*status = NFSERR_STALE;
		return;
	}
	if (rdonly(exi, req)) {
		error = EROFS;
		goto fromerr;
	}
	tovp = fhtovp(&args->rna_to.da_fhandle, exi);
	if (tovp == NULL) {
		*status = NFSERR_STALE;
		VN_RELE(fromvp);
		return;
	}
#ifdef RISCOS
	fromrp = vtor(fromvp);
	if (svckudp_dup(req, &duptime, &dupmark) &&
		duptime.tv_sec == fromrp->r_attr.va_ctime.tv_sec &&
		duptime.tv_usec == fromrp->r_attr.va_ctime.tv_usec) {
		++dup_renames;
		/*
		 * XXX - This is not quite right (Need to save more
		 *	info for the dup's to return)
		 */
		error = 0;
	} else
		error = VOP_RENAME(fromvp, args->rna_from.da_name,
		    tovp, args->rna_to.da_name, u.u_cred);

	if (error) {
		svckudp_dupsave(req, time, DUP_FAIL);
	} else {
		svckudp_dupsave(req, fromrp->r_attr.va_ctime, DUP_DONE);
	}
#else
	error = VOP_RENAME(fromvp, args->rna_from.da_name,
		tovp, args->rna_to.da_name, u.u_cred);
#endif /* RISCOS */
	VN_RELE(tovp);
fromerr:
	VN_RELE(fromvp);
	*status = puterrno(error);
#ifndef RISCOS
	if (error == 0) {
		svckudp_dupsave(req);
	} else if (svckudp_dup(req)) {
		*status = NFS_OK;
	}
#endif /* RISCOS */
#ifdef NFSDEBUG
	dprint(nfsdebug, 5, "rfs_rename: returning %d\n", error);
#endif
}

/*
 * Link to a file.
 * Create a file (to) which is a hard link to the given file (from).
 */
int
rfs_link(args, status, exi, req)
	struct nfslinkargs *args;
	enum nfsstat *status;
	struct exportinfo *exi;
	struct svc_req *req;
{
	int error;
	register struct vnode *fromvp;
	register struct vnode *tovp;
#ifdef RISCOS
	struct timeval duptime;
	int dupmark;
	struct rnode *fromrp;
#endif /* RISCOS */

#ifdef NFSDEBUG
	dprint(nfsdebug, 4, "rfs_link ffh %x %x %d -> %s tfh %x %x %d\n",
	    args->la_from.fh_fsid.val[0], args->la_from.fh_fsid.val[1],
	    args->la_from.fh_len, args->la_to.da_name,
	    args->la_to.da_fhandle.fh_fsid.val[0],
	    args->la_to.da_fhandle.fh_fsid.val[1],
	    args->la_to.da_fhandle.fh_len);
#endif
#ifdef RISCOS		/* Sun bug: 1015159 */
	/* Disallow NULL paths */
	if ((args->la_to.da_name == (char *) NULL) ||
	    (*args->la_to.da_name == '\0')) {
		*status = NFSERR_ACCES;
		return;
	}
#endif
	fromvp = fhtovp(&args->la_from, exi);
	if (fromvp == NULL) {
		*status = NFSERR_STALE;
		return;
	}
	tovp = fhtovp(&args->la_to.da_fhandle, exi);
	if (tovp == NULL) {
		*status = NFSERR_STALE;
		VN_RELE(fromvp);
		return;
	}
	if (rdonly(exi, req)) {
		error = EROFS;
	} else {
#ifdef RISCOS
		fromrp = vtor(fromvp);
		if (svckudp_dup(req, &duptime, &dupmark) &&
			duptime.tv_sec == fromrp->r_attr.va_ctime.tv_sec &&
			duptime.tv_usec == fromrp->r_attr.va_ctime.tv_usec) {
			++dup_links;
			/*
			 * XXX - This is not quite right (Need to save more
			 *	info for the dup's to return)
			 */
			error = 0;
		}
		else
			error = VOP_LINK(fromvp, tovp,
				args->la_to.da_name, u.u_cred);

		if (error) {
			svckudp_dupsave(req, time, DUP_FAIL);
		} else {
			svckudp_dupsave(req, fromrp->r_attr.va_ctime, DUP_DONE);
		}
#else
		error = VOP_LINK(fromvp, tovp, args->la_to.da_name, u.u_cred);
		if (error == EEXIST) {
			/*
			 * check for dup request
			 */
			if (svckudp_dup(req)) {
				error = 0;
			}
		} else if (!error) {
			svckudp_dupsave(req);
		}
#endif /* RISCOS */
	}
	*status = puterrno(error);
	VN_RELE(fromvp);
	VN_RELE(tovp);
#ifdef NFSDEBUG
	dprint(nfsdebug, 5, "rfs_link: returning %d\n", error);
#endif
}

/*
 * Symbolicly link to a file.
 * Create a file (to) with the given attributes which is a symbolic link
 * to the given path name (to).
 */
int
rfs_symlink(args, status, exi, req)
	struct nfsslargs *args;
	enum nfsstat *status;
	struct exportinfo *exi;
	struct svc_req *req;
{
	int error;
	struct vattr va;
	register struct vnode *vp;
#ifdef RISCOS
	struct timeval duptime;
	int dupmark;
	struct rnode *rp;
#endif /* RISCOS */

#ifdef NFSDEBUG
	dprint(nfsdebug, 4, "rfs_symlink %s ffh %x %x %d -> %s\n",
	    args->sla_from.da_name,
	    args->sla_from.da_fhandle.fh_fsid.val[0],
	    args->sla_from.da_fhandle.fh_fsid.val[1],
	    args->sla_from.da_fhandle.fh_len,
	    args->sla_tnm);
#endif
#ifdef RISCOS		/* Sun bug: 1015159 */
	/* Disallow NULL paths */
	if ((args->sla_from.da_name == (char *) NULL) ||
	    (*args->sla_from.da_name == '\0')) {
		*status = NFSERR_ACCES;
		return;
	}
#endif
	sattr_to_vattr(&args->sla_sa, &va);
	va.va_type = VLNK;
	vp = fhtovp(&args->sla_from.da_fhandle, exi);
	if (vp == NULL) {
		*status = NFSERR_STALE;
		return;
	}
	if (rdonly(exi, req)) {
		error = EROFS;
	} else {
#ifdef RISCOS
		rp = vtor(vp);
		if (svckudp_dup(req, &duptime, &dupmark) &&
			duptime.tv_sec == rp->r_attr.va_ctime.tv_sec &&
			duptime.tv_usec == rp->r_attr.va_ctime.tv_usec) {
			++dup_symlinks;
			/*
			 * XXX - This is not quite right (Need to save more
			 *	info for the dup's to return)
			 */
			error = 0;
		}
		else
			error = VOP_SYMLINK(vp, args->sla_from.da_name,
					    &va, args->sla_tnm, u.u_cred);

		if (error) {
			svckudp_dupsave(req, time, DUP_FAIL);
		} else {
			svckudp_dupsave(req, rp->r_attr.va_ctime, DUP_DONE);
		}
#else
		error = VOP_SYMLINK(vp, args->sla_from.da_name,
		    &va, args->sla_tnm, u.u_cred);
		if (error == EEXIST) {
			/*
			 * check for dup request
			 */
			if (svckudp_dup(req)) {
				error = 0;
			}
		} else if (!error) {
			svckudp_dupsave(req);
		}
#endif /* RISCOS */
	}
	*status = puterrno(error);
	VN_RELE(vp);
#ifdef NFSDEBUG
	dprint(nfsdebug, 5, "rfs_symlink: returning %d\n", error);
#endif
}

/*
 * Make a directory.
 * Create a directory with the given name, parent directory, and attributes.
 * Returns a file handle and attributes for the new directory.
 */
int
rfs_mkdir(args, dr, exi, req)
	struct nfscreatargs *args;
	struct  nfsdiropres *dr;
	struct exportinfo *exi;
	struct svc_req *req;
{
	int error;
	struct vattr va;
	struct vnode *dvp;
	register struct vnode *vp;
#ifdef RISCOS		/* Sun bug: 1015159; change all references to 'name' */
	register char *name = args->ca_da.da_name;
	struct timeval duptime;
	int dupmark;
	struct rnode *rp;
#endif

#ifdef NFSDEBUG
	dprint(nfsdebug, 4, "rfs_mkdir %s fh %x %x %d\n",
	    name, args->ca_da.da_fhandle.fh_fsid.val[0],
	    args->ca_da.da_fhandle.fh_fsid.val[1], args->ca_da.da_fhandle.fh_len);
#endif
#ifdef RISCOS		/* Sun bug: 1015159 */
	/* Disallow NULL paths */
	if ((name == (char *) NULL) || (*name == '\0')) {
		dr->dr_status = NFSERR_ACCES;
		return;
	}
#endif
	sattr_to_vattr(&args->ca_sa, &va);
	va.va_type = VDIR;
	/*
	 * Should get exclusive flag and pass it on here
	 */
	vp = fhtovp(&args->ca_da.da_fhandle, exi);
	if (vp == NULL) {
		dr->dr_status = NFSERR_STALE;
		return;
	}
	if (rdonly(exi, req)) {
		error = EROFS;
	} else {
#ifdef RISCOS
		rp = vtor(vp);
		if (svckudp_dup(req, &duptime, &dupmark) &&
			duptime.tv_sec == rp->r_attr.va_ctime.tv_sec &&
			duptime.tv_usec == rp->r_attr.va_ctime.tv_usec) {
			++dup_mkdirs;
			/*
			 * XXX - This is not quite right (Need to save more
			 *	info for the dup's to return)
			 */
			error = VOP_LOOKUP(vp, name, &dvp, u.u_cred,
				(struct pathname *) NULL, 0);
			if (!error) {
				error = VOP_GETATTR(dvp, &va, u.u_cred);
			}
		}
		else {
			error = VOP_MKDIR(vp, name, &va,
				&dvp, u.u_cred);
		}
		if (!error) {
			vattr_to_nattr(&va, &dr->dr_attr);
			error = makefh(&dr->dr_fhandle, dvp, exi);
			VN_RELE(dvp);
		}

		if (error) {
			svckudp_dupsave(req, time, DUP_FAIL);
		} else {
			svckudp_dupsave(req, rp->r_attr.va_ctime, DUP_DONE);
		}

#else
		error = VOP_MKDIR(vp, name, &va, &dvp, u.u_cred);
		if (error == EEXIST) {
			/*
			 * check for dup request
			 */
			if (svckudp_dup(req)) {
				error = VOP_LOOKUP(vp, name,
					  &dvp, u.u_cred,
					  (struct pathname *) NULL, 0);
				if (!error) {
					error = VOP_GETATTR(vp, &va, u.u_cred);
				}
			}
		}
		if (!error) {
			vattr_to_nattr(&va, &dr->dr_attr);
			error = makefh(&dr->dr_fhandle, dvp, exi);
			VN_RELE(dvp);
		}
		if (!error) {
			svckudp_dupsave(req);
		}
#endif /* RISCOS */
	}
	dr->dr_status = puterrno(error);
	VN_RELE(vp);
#ifdef NFSDEBUG
	dprint(nfsdebug, 5, "rfs_mkdir: returning %d\n", error);
#endif
}

/*
 * Remove a directory.
 * Remove the given directory name from the given parent directory.
 */
int
rfs_rmdir(da, status, exi, req)
	struct nfsdiropargs *da;
	enum nfsstat *status;
	struct exportinfo *exi;
	struct svc_req *req;
{
	int error;
	register struct vnode *vp;
#ifdef RISCOS
	struct timeval duptime;
	int dupmark;
	struct rnode *rp;
#endif /* RISCOS */

#ifdef NFSDEBUG
	dprint(nfsdebug, 4, "rfs_rmdir %s fh %x %x %d\n",
	    da->da_name, da->da_fhandle.fh_fsid.val[0],
	    da->da_fhandle.fh_fsid.val[1], da->da_fhandle.fh_len);
#endif
#ifdef RISCOS		/* Sun bug: 1015159 */
	if ((da->da_name == (char *) NULL) || (*da->da_name == '\0')) {
		*status = NFSERR_ACCES;
		return;
	}
#endif
	vp = fhtovp(&da->da_fhandle, exi);
	if (vp == NULL) {
		*status = NFSERR_STALE;
		return;
	}
	if (rdonly(exi, req)) {
		error = EROFS;
	} else {
#ifdef RISCOS
		rp = vtor(vp);
		if (svckudp_dup(req, &duptime, &dupmark) &&
			duptime.tv_sec == rp->r_attr.va_ctime.tv_sec &&
			duptime.tv_usec == rp->r_attr.va_ctime.tv_usec) {
				++dup_rmdirs;
				/*
				 * XXX - This is not quite right (Need to save more
				 *	info for the dup's to return)
				 */
				error = 0;
		}
		else {
			error = VOP_RMDIR(vp, da->da_name, u.u_cred);
		}

		if (error) {
			svckudp_dupsave(req, time, DUP_FAIL);
		} else {
			svckudp_dupsave(req, rp->r_attr.va_ctime, DUP_DONE);
		}
#else
		error = VOP_RMDIR(vp, da->da_name, u.u_cred);
		if (error == ENOENT) {
			/*
			 * check for dup request
			 */
			if (svckudp_dup(req)) {
				error = 0;
			}
		} else if (!error) {
			svckudp_dupsave(req);
		}
#endif /* RISCOS */
	}
	*status = puterrno(error);
	VN_RELE(vp);
#ifdef NFSDEBUG
	dprint(nfsdebug, 5, "nfs_rmdir returning %d\n", error);
#endif
}

int
rfs_readdir(rda, rd, exi)
	struct nfsrddirargs *rda;
	register struct nfsrddirres  *rd;
	struct exportinfo *exi;
{
	int error;
	struct iovec iov;
	struct uio uio;
	register struct vnode *vp;

#ifdef NFSDEBUG
	dprint(nfsdebug, 4, "rfs_readdir fh %x %x %d count %d\n",
	    rda->rda_fh.fh_fsid.val[0], rda->rda_fh.fh_fsid.val[1],
	    rda->rda_fh.fh_len, rda->rda_count);
#endif
	vp = fhtovp(&rda->rda_fh, exi);
	if (vp == NULL) {
		rd->rd_status = NFSERR_STALE;
		return;
	}
#ifdef RISCOS		/* Sun bug: 1014138 */
	/* An NFS readdir to a regular file was crashing server. */
	if (vp->v_type != VDIR) {
		cmn_err(CE_NOTE,"rfs_readdir: attempt to read non-directory\n");
		error = ENOTDIR;
		goto bad;
	}
#endif
	/*
	 * check read access of dir.  we have to do this here because
	 * the opendir doesn't go over the wire.
	 */
	error = VOP_ACCESS(vp, VREAD, u.u_cred);
	if (error) {
		goto bad;
	}

	if (rda->rda_count == 0) {
		rd->rd_size = 0;
		rd->rd_eof = FALSE;
		rd->rd_entries = NULL;
		rd->rd_bufsize = 0;
		goto bad;
	}

	rda->rda_count = MIN(rda->rda_count, NFS_MAXDATA);

	/*
	 * Allocate data for entries.  This will be freed by rfs_rdfree.
	 */
#ifdef RISCOS
	rd->rd_entries = (struct bsd43_dirent *)kmem_alloc((u_int)rda->rda_count);
#else
	rd->rd_entries = (struct dirent *)kmem_alloc((u_int)rda->rda_count);
#endif
	rd->rd_bufsize = rda->rda_count;

	/*
	 * Set up io vector to read directory data
	 */
	iov.iov_base = (caddr_t)rd->rd_entries;
	iov.iov_len = rda->rda_count;
	uio.uio_iov = &iov;
	uio.uio_iovcnt = 1;
	uio.uio_segflg = UIO_SYSSPACE;
	uio.uio_offset = rda->rda_offset;
#if RISCOS
	uio.uio_fmode = FREAD;
#endif
	uio.uio_resid = rda->rda_count;

	/*
	 * read directory
	 */
	error = VOP_READDIR(vp, &uio, u.u_cred);

	/*
	 * Clean up
	 */
	if (error) {
		rd->rd_size = 0;
		goto bad;
	}

	/*
	 * set size and eof
	 */
	if (rda->rda_count && uio.uio_resid == rda->rda_count) {
		rd->rd_size = 0;
		rd->rd_eof = TRUE;
	} else {
		rd->rd_size = rda->rda_count - uio.uio_resid;
		rd->rd_eof = FALSE;
	}

bad:
	rd->rd_status = puterrno(error);
	VN_RELE(vp);
#ifdef NFSDEBUG
	dprint(nfsdebug, 5, "rfs_readdir: returning %d\n", error);
#endif
}

rfs_rddirfree(rd)
	struct nfsrddirres *rd;
{
	if (rd->rd_entries)
		kmem_free((caddr_t)rd->rd_entries, (u_int)rd->rd_bufsize);
}

rfs_statfs(fh, fs, exi)
	fhandle_t *fh;
	register struct nfsstatfs *fs;
	struct exportinfo *exi;
{
	int error;
#ifdef RISCOS
	struct bsd43_statfs sb;
#else
	struct statfs sb;
#endif
	register struct vnode *vp;

#ifdef NFSDEBUG
	dprint(nfsdebug, 4, "rfs_statfs fh %x %x %d\n",
	    fh->fh_fsid.val[0], fh->fh_fsid.val[1], fh->fh_len);
#endif
	vp = fhtovp(fh, exi);
	if (vp == NULL) {
		fs->fs_status = NFSERR_STALE;
		return;
	}
#ifdef RISCOS
	bzero(&sb, sizeof(sb));
#endif
	error = VFS_STATFS(vp->v_vfsp, &sb);
	fs->fs_status = puterrno(error);
	if (!error) {
		fs->fs_tsize = nfstsize();
		fs->fs_bsize = sb.f_bsize;
		fs->fs_blocks = sb.f_blocks;
		fs->fs_bfree = sb.f_bfree;
		fs->fs_bavail = sb.f_bavail;
	}
	VN_RELE(vp);
#ifdef NFSDEBUG
	dprint(nfsdebug, 5, "rfs_statfs returning %d\n", error);
#endif
}

/*ARGSUSED*/
rfs_null(argp, resp)
	caddr_t *argp;
	caddr_t *resp;
{
	/* do nothing */
	return (0);
}

/*ARGSUSED*/
rfs_error(argp, resp)
	caddr_t *argp;
	caddr_t *resp;
{

	return (EOPNOTSUPP);
}

int
nullfree()
{
}

/*
 * rfs dispatch table
 * Indexed by version, proc
 */

struct rfsdisp {
	int	  (*dis_proc)();	/* proc to call */
	xdrproc_t dis_xdrargs;		/* xdr routine to get args */
	int	  dis_argsz;		/* sizeof args */
	xdrproc_t dis_xdrres;		/* xdr routine to put results */
	int	  dis_ressz;		/* size of results */
	int	  (*dis_resfree)();	/* frees space allocated by proc */
} rfsdisptab[][RFS_NPROC]  = {
	{
	/*
	 * VERSION 2
	 * Changed rddirres to have eof at end instead of beginning
	 */
	/* RFS_NULL = 0 */
	{rfs_null, xdr_void, 0,
	    xdr_void, 0, nullfree},
	/* RFS_GETATTR = 1 */
	{rfs_getattr, xdr_fhandle, sizeof (fhandle_t),
	    xdr_attrstat, sizeof (struct nfsattrstat), nullfree},
	/* RFS_SETATTR = 2 */
	{rfs_setattr, xdr_saargs, sizeof (struct nfssaargs),
	    xdr_attrstat, sizeof (struct nfsattrstat), nullfree},
	/* RFS_ROOT = 3 *** NO LONGER SUPPORTED *** */
	{rfs_error, xdr_void, 0,
	    xdr_void, 0, nullfree},
	/* RFS_LOOKUP = 4 */
	{rfs_lookup, xdr_diropargs, sizeof (struct nfsdiropargs),
	    xdr_diropres, sizeof (struct nfsdiropres), nullfree},
	/* RFS_READLINK = 5 */
	{rfs_readlink, xdr_fhandle, sizeof (fhandle_t),
	    xdr_rdlnres, sizeof (struct nfsrdlnres), rfs_rlfree},
	/* RFS_READ = 6 */
	{rfs_read, xdr_readargs, sizeof (struct nfsreadargs),
	    xdr_rdresult, sizeof (struct nfsrdresult), rfs_rdfree},
	/* RFS_WRITECACHE = 7 *** NO LONGER SUPPORTED *** */
	{rfs_error, xdr_void, 0,
	    xdr_void, 0, nullfree},
	/* RFS_WRITE = 8 */
	{rfs_write, xdr_writeargs, sizeof (struct nfswriteargs),
	    xdr_attrstat, sizeof (struct nfsattrstat), nullfree},
	/* RFS_CREATE = 9 */
	{rfs_create, xdr_creatargs, sizeof (struct nfscreatargs),
	    xdr_diropres, sizeof (struct nfsdiropres), nullfree},
	/* RFS_REMOVE = 10 */
	{rfs_remove, xdr_diropargs, sizeof (struct nfsdiropargs),
	    xdr_enum, sizeof (enum nfsstat), nullfree},
	/* RFS_RENAME = 11 */
	{rfs_rename, xdr_rnmargs, sizeof (struct nfsrnmargs),
	    xdr_enum, sizeof (enum nfsstat), nullfree},
	/* RFS_LINK = 12 */
	{rfs_link, xdr_linkargs, sizeof (struct nfslinkargs),
	    xdr_enum, sizeof (enum nfsstat), nullfree},
	/* RFS_SYMLINK = 13 */
	{rfs_symlink, xdr_slargs, sizeof (struct nfsslargs),
	    xdr_enum, sizeof (enum nfsstat), nullfree},
	/* RFS_MKDIR = 14 */
	{rfs_mkdir, xdr_creatargs, sizeof (struct nfscreatargs),
	    xdr_diropres, sizeof (struct nfsdiropres), nullfree},
	/* RFS_RMDIR = 15 */
	{rfs_rmdir, xdr_diropargs, sizeof (struct nfsdiropargs),
	    xdr_enum, sizeof (enum nfsstat), nullfree},
	/* RFS_READDIR = 16 */
	{rfs_readdir, xdr_rddirargs, sizeof (struct nfsrddirargs),
	    xdr_putrddirres, sizeof (struct nfsrddirres), rfs_rddirfree},
	/* RFS_STATFS = 17 */
	{rfs_statfs, xdr_fhandle, sizeof (fhandle_t),
	    xdr_statfs, sizeof (struct nfsstatfs), nullfree},
	}
};

struct rfsspace {
	struct rfsspace *rs_next;
	caddr_t		rs_dummy;
};

struct rfsspace *rfsfreesp = NULL;

int rfssize = 0;

caddr_t
rfsget()
{
	int i;
	struct rfsdisp *dis;
	caddr_t ret;

	if (rfssize == 0) {
		for (i = 0; i < 1 + VERSIONMAX - VERSIONMIN; i++) {
			for (dis = &rfsdisptab[i][0];
			    dis < &rfsdisptab[i][RFS_NPROC];
			    dis++) {
				rfssize = MAX(rfssize, dis->dis_argsz);
				rfssize = MAX(rfssize, dis->dis_ressz);
			}
		}
	}

	if (rfsfreesp) {
		ret = (caddr_t)rfsfreesp;
		rfsfreesp = rfsfreesp->rs_next;
	} else {
		ret = kmem_alloc((u_int)rfssize);
	}
	return (ret);
}

rfsput(rs)
	struct rfsspace *rs;
{

	rs->rs_next = rfsfreesp;
	rfsfreesp = rs;
}


/*
 * If nfs_portmon is set, then clients are required to use
 * privileged ports (ports < IPPORT_RESERVED) in order to get NFS services.
 */
int nfs_portmon = 0;


void
rfs_dispatch(req, xprt)
	struct svc_req *req;
	register SVCXPRT *xprt;
{
	int which;
	int vers;
	caddr_t	*args = NULL;
	caddr_t	*res = NULL;
	register struct rfsdisp *disp = NULL;
	struct ucred *tmpcr;
	struct ucred *newcr = NULL;
	int error;
	struct exportinfo *exi;
#ifdef RISCOS
	int dup_xid = 0;
#endif /* RISCOS */

	svstat.ncalls++;
	error = 0;
#ifdef RISCOS
	/* check for in-progress request in server cache. */
	/* if not found, this request is added and flagged */
	/* in-progress. */
	if (svckudp_dupbusy(req)) {
		dup_xid = 1;
		throwaways++;
		goto done;
	}
#endif /* RISCOS */
	which = req->rq_proc;
	if (which < 0 || which >= RFS_NPROC) {
#ifdef NFSDEBUG
		dprint(nfsdebug, 2,
		    "rfs_dispatch: bad proc %d\n", which);
#endif
		svcerr_noproc(req->rq_xprt);
		error++;
		printf("nfs_server: bad proc number\n");
		goto done;
	}
	vers = req->rq_vers;
	if (vers < VERSIONMIN || vers > VERSIONMAX) {
#ifdef NFSDEBUG
		dprint(nfsdebug, 2,
		    "rfs_dispatch: bad vers %d low %d high %d\n",
		    vers, VERSIONMIN, VERSIONMAX);
#endif
		svcerr_progvers(req->rq_xprt, (u_long)VERSIONMIN,
		    (u_long)VERSIONMAX);
		error++;
		printf("nfs_server: bad version number\n");
		goto done;
	}
	vers -= VERSIONMIN;
	disp = &rfsdisptab[vers][which];

	/*
	 * Clean up as if a system call just started
	 */
	u.u_error = 0;

	/*
	 * Allocate args struct and deserialize into it.
	 */
	args = (caddr_t *)rfsget();
	bzero((caddr_t)args, (u_int)rfssize);
	if ( ! SVC_GETARGS(xprt, disp->dis_xdrargs, args)) {
		svcerr_decode(xprt);
		error++;
		printf("nfs_server: bad getargs\n");
		goto done;
	}

	/*
	 * Find export information and check authentication,
	 * setting the credential if everything is ok.
	 */
	if (which != RFS_NULL) {
		/*
		 * XXX: this isn't really quite correct. Instead of doing
		 * this blind cast, we should extract out the fhandle for
		 * each NFS call. What's more, some procedures (like rename)
		 * have more than one fhandle passed in, and we should check
		 * that the two fhandles point to the same exported path.
		 */
		fhandle_t *fh = (fhandle_t *) args;

		newcr = crget();
		tmpcr = u.u_cred;
		u.u_cred = newcr;
		exi = findexport(&fh->fh_fsid, (struct fid *) &fh->fh_xlen);
		if (exi != NULL && !checkauth(exi, req, newcr)) {
			extern char *inet_ntoa();

			svcerr_weakauth(xprt);
			error++;
			printf(
"nfs_server: weak authentication, source IP address=%s\n",
			inet_ntoa(req->rq_xprt->xp_raddr.sin_addr));
			goto done;
		}
	}

	/*
	 * Allocate results struct.
	 */
	res = (caddr_t *)rfsget();
	bzero((caddr_t)res, (u_int)rfssize);

	svstat.reqs[which]++;

	/*
	 * Call service routine with arg struct and results struct
	 */
	(*disp->dis_proc)(args, res, exi, req);

done:
#ifdef RISCOS
	/* mark this request not in-progress in server cache */
	if (!dup_xid)
		svckudp_dupdone(req);
#endif /* RISCOS */

	/*
	 * Free arguments struct
	 */
#ifdef RISCOS		/* Sun bug: 1015592 */
	if (!SVC_FREEARGS(xprt, (disp ? disp->dis_xdrargs : NULL ), args) ) {
#else
	if (!SVC_FREEARGS(xprt, disp->dis_xdrargs, args) ) {
#endif
		printf("nfs_server: bad freeargs\n");
		error++;
	}
	if (args != NULL) {
		rfsput((struct rfsspace *)args);
	}

	/*
	 * Serialize and send results struct
	 */
	if (!error) {
#ifdef RISCOS		/* Error may not be set if throwaway */
		if (disp && !svc_sendreply(xprt,disp->dis_xdrres, (caddr_t)res))
#else
		if (!svc_sendreply(xprt, disp->dis_xdrres, (caddr_t)res))
#endif
		{ 
			printf("nfs_server: bad sendreply\n");
			error++;
		}
	}

	/*
	 * Free results struct
	 */
	if (res != NULL) {
#ifdef RISCOS	/* May not need this, but better to be safe */
		if ( disp && disp->dis_resfree != nullfree )
#else
		if ( disp->dis_resfree != nullfree )
#endif
		{
			(*disp->dis_resfree)(res);
		}
		rfsput((struct rfsspace *)res);
	}
	/*
	 * restore original credentials
	 */
	if (newcr) {
		u.u_cred = tmpcr;
		crfree(newcr);
	}
	svstat.nbadcalls += error;
}

sattr_to_vattr(sa, vap)
	register struct nfssattr *sa;
	register struct vattr *vap;
{

	vattr_null(vap);
	vap->va_mode = sa->sa_mode;
	vap->va_uid = (uid_t)sa->sa_uid;
	vap->va_gid = (gid_t)sa->sa_gid;
	vap->va_size = sa->sa_size;
	vap->va_atime.tv_sec  = sa->sa_atime.tv_sec;
	vap->va_atime.tv_usec = sa->sa_atime.tv_usec;
	vap->va_mtime.tv_sec  = sa->sa_mtime.tv_sec;
	vap->va_mtime.tv_usec = sa->sa_mtime.tv_usec;
}

/*
 * Convert an fhandle into a vnode.
 * Uses the file id (fh_len + fh_data) in the fhandle to get the vnode.
 * WARNING: users of this routine must do a VN_RELE on the vnode when they
 * are done with it.
 */
struct vnode *
fhtovp(fh, exi)
	fhandle_t *fh;
	struct exportinfo *exi;
{
	register struct vfs *vfsp;
	struct vnode *vp;
	int error;

	if (exi == NULL) {
		return (NULL);	/* not exported */
	}
	vfsp = getvfs(&fh->fh_fsid);
	if (vfsp == NULL) {
		return (NULL);
	}
	error = VFS_VGET(vfsp, &vp, (struct fid *)&(fh->fh_len));
	if (error || vp == NULL) {
#ifdef NFSDEBUG
		dprint(nfsdebug, 1, "fhtovp(%x) couldn't vget\n", fh);
#endif
		return (NULL);
	}
	return (vp);
}

/*
 * Determine if two addresses are equal
 * Only AF_INET supported for now
 */
eqaddr(addr1, addr2)
	struct sockaddr *addr1;
	struct sockaddr *addr2;
{

	if (addr1->sa_family != addr2->sa_family) {
		return (0);
	}
	switch (addr1->sa_family) {
	case AF_INET:
		return (((struct sockaddr_in *) addr1)->sin_addr.s_addr ==
			((struct sockaddr_in *) addr2)->sin_addr.s_addr);
	case AF_NS:
		/* coming soon? */
		break;
	}
	return (0);
}

hostinlist(sa, addrs)
	struct sockaddr *sa;
	struct exaddrlist *addrs;
{
	int i;

	for (i = 0; i < addrs->naddrs; i++) {
		if (eqaddr(sa, &addrs->addrvec[i])) {
			return (1);
		}
	}
	return (0);
}

/*
 * Check to see if the given name corresponds to a
 * root user of the exported filesystem.
 */
rootname(ex, netname)
	struct export *ex;
	char *netname;
{
	int i;
	int namelen;

	namelen = strlen(netname) + 1;
	for (i = 0; i < ex->ex_des.nnames; i++) {
		if (bcmp(netname, ex->ex_des.rootnames[i], namelen) == 0) {
			return (1);
		}
	}
	return (0);
}

checkauth(exi, req, cred)
	struct exportinfo *exi;
	struct svc_req *req;
	struct ucred *cred;
{
	struct authunix_parms *aup;
	struct authdes_cred *adc;
	int flavor;
	gid_t *gp;
	short grouplen;
#ifdef RISCOS
	int grpinx;
#endif

	/*
	 * Check for privileged port number
	 */
	if (nfs_portmon &&
	    ntohs(req->rq_xprt->xp_raddr.sin_port) >= IPPORT_RESERVED) {
		printf("NFS request from unprivileged port.\n");
		return (0);
	}

	/*
	 * Set uid, gid, and gids to auth params
	 */
	flavor = req->rq_cred.oa_flavor;
	if (flavor != exi->exi_export.ex_auth) {
		flavor = AUTH_NULL;
	}
	switch (flavor) {
	case AUTH_NULL:
		cred->cr_uid = exi->exi_export.ex_anon;
		cred->cr_gid = exi->exi_export.ex_anon;
		gp = cred->cr_groups;
		break;

	case AUTH_UNIX:
		aup = (struct authunix_parms *)req->rq_clntcred;
		if (aup->aup_uid == 0 &&
		    !hostinlist((struct sockaddr *)svc_getcaller(req->rq_xprt),
				&exi->exi_export.ex_unix.rootaddrs)) {
			cred->cr_uid = exi->exi_export.ex_anon;
			cred->cr_gid = exi->exi_export.ex_anon;
			gp = cred->cr_groups;
		} else {
			cred->cr_uid = aup->aup_uid;
			cred->cr_gid = aup->aup_gid;
#ifdef RISCOS
			for (grpinx = 0; grpinx < aup->aup_len; grpinx++) {
			  cred->cr_groups[grpinx] =
			    (gid_t)(aup->aup_gids[grpinx]);
			}
#else
		        /*
			 * NOTE: The following reference code is wrong since
			 * it is moving 32-bit ints into an unsigned 16-bit
			 * array and hoping that the correct sign conversion
			 * will happen and the groups id's will be aligned
			 * correctly; this code doesn't do the job!
			 */
			bcopy((caddr_t)aup->aup_gids, (caddr_t)cred->cr_groups,
			    aup->aup_len * sizeof (cred->cr_groups[0]));
#endif
			gp = &cred->cr_groups[aup->aup_len];
		}
		break;


	case AUTH_DES:
		adc = (struct authdes_cred *)req->rq_clntcred;
		if (adc->adc_fullname.window > exi->exi_export.ex_des.window) {
			return (0);
		}
		if (!authdes_getucred(adc, &cred->cr_uid, &cred->cr_gid,
		    &grouplen, cred->cr_groups)) {
			if (rootname(&exi->exi_export,
			    adc->adc_fullname.name)) {
				cred->cr_uid = 0;
			} else {
				cred->cr_uid = exi->exi_export.ex_anon;
			}
			cred->cr_gid = exi->exi_export.ex_anon;
			grouplen = 0;
		}
		gp = &cred->cr_groups[grouplen];
		break;

	default:
		return (0);
	}
	while (gp < &cred->cr_groups[NGROUPS]) {
		*gp++ = NOGROUP;
	}
	return (cred->cr_uid != (uid_t) -1); /* Sun bug: km05; cast to uid_t */
}
