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
#ident	"$Header: nfs_client.c,v 1.4.1.6 90/05/10 05:02:31 wje Exp $"
/*
 * @(#)nfs_client.c 2.4 88/08/04 NFSSRC4.0
 * @(#)nfs_client.c 1.5 88/02/08 Sun Microsystems, Inc.
 * 
 * Original reference port includes list:
 * param.h systm.h time.h kernel.h vnode.h vfs.h vfs_stat.h errno.h
 * buf.h stat.h ucred.h ../rpc/types.h ../netinet/in.h  ../nfs/nfs.h
 * ../nfs/nfs_clnt.h ../nfs/rnode.h
 */
#include "sys/types.h"
#include "sys/param.h"
#include "sys/sysmacros.h"
#include "sys/systm.h"

#include "sys/signal.h"		/* needed by user.h */
#include "sys/pcb.h"		/* needed by user.h */
#include "sys/user.h"

#include "bsd/sys/time.h"
#include "sys/vnode.h"
#include "sys/vfs.h"
#include "sys/vfs_stat.h"
#include "sys/errno.h"

#include "sys/buf.h"
#include "sys/stat.h"
#include "sys/ucred.h"

#include "sys/immu.h"		/* needed by proc.h */
#include "sys/sbd.h"		/* needed by proc.h */
#include "sys/region.h"		/* needed by proc.h */
#include "sys/proc.h"

#include "../rpc/types.h"
#include "bsd/netinet/in.h"		/* XXX this is for sockaddr_in! */
#include "sys/fs/nfs.h"
#include "sys/fs/nfs_clnt.h"
#include "sys/fs/nfs_rnode.h"
#include "sys/kmem.h"

#ifdef NFSDEBUG
extern int nfsdebug;
#endif

/*
 * Attributes caching:
 *
 * Attributes are cached in the rnode in struct vattr form.
 * There is a time associated with the cached attributes (r_attrtime)
 * which tells whether the attributes are valid. The time is initialized
 * to the difference between current time and the modify time of the vnode
 * when new attributes are cached. This allows the attributes for
 * files that have changed recently to be timed out sooner than for files
 * that have not changed for a long time. There are minimum and maximum
 * timeout values that can be set per mount point.
 */

/*
 * Validate caches by checking cached attributes. If they have timed out
 * get the attributes from the server and compare mtimes. If mtimes are
 * different purge all caches for this vnode.
 */
nfs_validate_caches(vp, cred)
	struct vnode *vp;
	struct ucred *cred;
{
	struct vattr va;

	return (nfsgetattr(vp, &va, cred));
}

/*
 * This procedure is called when the error returned is ESTALE.
 * It should NOT be called from any of the biod processes since the
 * general nfs flush cache procedure is called which can deadlock/sleep
 * forever in the blkflush() procedure. The remote error is saved in the
 * rnode to allow upper level code to short-circuit long RPC timeouts
 * on subsequent rpc/nfs operations on this vnode which would eventually
 * timeout.
 *
 * Avoiding calling the nfs cache purge operation from procedures in
 * vfs_bio.c avoids two deadlock cases. One is when the rnode locks are
 * shared between the upper level and the biod process. In this case
 * nfs_purge_caches will deadlock due to attempting to get a rnode lock
 * while the upper level already has one. The second case arose when sleeps
 * were issued on busy buffers but iodone was NOT being called in the stale
 * handle case to release/wakeup the process waiting for the buffers. This
 * caused the biod processes to sleep forever. This second problem arose
 * because the nfs purge could call blkflush from either nfsread/nfswrite
 * which would then call bwrite creating the buffer sleep problem.
 */
nfs_purge_stale_fh(vp)
	struct vnode *vp;
{
	struct rnode *rp;

	nfs_purge_caches(vp);
	rp = vtor(vp);
	rp->r_error = ESTALE;
}

nfs_purge_caches(vp)
	struct vnode *vp;
{
	struct rnode *rp;
	register long nodeid;
	register u_long size;

	rp = vtor(vp);
#ifdef NFSDEBUG
	dprint(nfsdebug, 4, "nfs_purge_caches: rnode %x\n", rp);
#endif
	RLOCK(rp);
	nodeid = rp->r_attr.va_nodeid;
	size = rp->r_attr.va_size;
	sync_vp(vp);
	RUNLOCK(rp);

	PURGE_ATTRCACHE(vp);
	dnlc_purge_vp(vp);
	binvalfree(vp);

	/* Invalidate any regions supporting this vp */
	xinval(vp);

	/* Flush out page cache associated with vp */
	flushpgch(vp, nodeid, size);
}

int
nfs_cache_check(vp, mtime)
	struct vnode *vp;
	struct timeval mtime;
{
	if (!CACHE_VALID(vtor(vp), mtime)) {
		nfs_purge_caches(vp);
	}
}

/*
 * Set attributes cache for given vnode using nfsattr.
 */
nfs_attrcache(vp, na)
	struct vnode *vp;
	struct nfsfattr *na;
{
#ifdef RISCOS
	/*
	 * Fixes Sun bug 1011512; Cached attributes not invalidated on
	 * locked files. The fix was to not cache attributes if VNOCACHE
	 * set in vnode, and don't use cache attributes when cache mtime is 0
	 */
       if ((vp->v_flag & VNOCACHE) || vtomi(vp)->mi_noac) {
                return;
	}
#else
	if (vtomi(vp)->mi_noac) {
		return;
	}
#endif
	nattr_to_vattr(vp, na, &vtor(vp)->r_attr);
	set_attrcache_time(vp);
}

/*
 * Set attributes cache for given vnode using vnode attributes.
 */
nfs_attrcache_va(vp, va)
	struct vnode *vp;
	struct vattr *va;
{
#ifdef RISCOS
        /*
         * Fixes Sun bug 1011512; Cached attributes not invalidated on
         * locked files. The fix was to not cache attributes if VNOCACHE
         * set in vnode, and don't use cache attributes when cache mtime is 0
         */
        if ((vp->v_flag & VNOCACHE) || vtomi(vp)->mi_noac) {
                return;
	 }
#else
	if (vtomi(vp)->mi_noac) {
		return;
	}
#endif
	vtor(vp)->r_attr = *va;
	vp->v_type = va->va_type;
	set_attrcache_time(vp);
}

set_attrcache_time(vp)
	struct vnode *vp;
{
	struct rnode *rp;
	int delta;

	rp = vtor(vp);
	rp->r_attrtime = time;
	/*
	 * Delta is the number of seconds that we will cache
	 * attributes of the file.  It is based on the number of seconds
	 * since the last change (i.e. files that changed recently
	 * are likely to change soon), but there is a minimum and
	 * a maximum for regular files and for directories.
	 */
	delta = (time.tv_sec - rp->r_attr.va_mtime.tv_sec) >> 4;
	if (delta < 0)
		delta = 0;	/* avoid problems with unsigned comparisons */
	if (vp->v_type == VDIR) {
		if (delta < vtomi(vp)->mi_acdirmin) {
			delta = vtomi(vp)->mi_acdirmin;
		} else if (delta > vtomi(vp)->mi_acdirmax) {
			delta = vtomi(vp)->mi_acdirmax;
		}
	} else {
		if (delta < vtomi(vp)->mi_acregmin) {
			delta = vtomi(vp)->mi_acregmin;
		} else if (delta > vtomi(vp)->mi_acregmax) {
			delta = vtomi(vp)->mi_acregmax;
		}
	}
	rp->r_attrtime.tv_sec += delta;
}

/*
 * Fill in attribute from the cache. If valid return 1 otherwise 0;
 */
int
nfs_getattr_cache(vp, vap)
	struct vnode *vp;
	struct vattr *vap;
{
	struct rnode *rp;

	rp = vtor(vp);
	if (timercmp(&time, &rp->r_attrtime, <)) {
		/*
		 * Cached attributes are valid
		 */
		*vap = rp->r_attr;
		return (1);
	}
	return (0);
}

/*
 * Get attributes over-the-wire.
 * Return 0 if successful, otherwise error.
 */
int
nfs_getattr_otw(vp, vap, cred)
	struct vnode *vp;
	struct vattr *vap;
	struct ucred *cred;
{
	int error;
	struct nfsattrstat *ns;
	ns = (struct nfsattrstat *)kmem_alloc(sizeof (*ns));
	error = rfscall(vtomi(vp), RFS_GETATTR, xdr_fhandle,
	    (caddr_t)vtofh(vp), xdr_attrstat, (caddr_t)ns, cred);
	if (error == 0) {
		error = geterrno(ns->ns_status);
		if (error == 0) {
			nattr_to_vattr(vp, &ns->ns_attr, vap);
		} else {
			PURGE_STALE_FH(error, vp);
		}
	}
	kmem_free((caddr_t)ns, sizeof (*ns));
	return (error);
}

/*
 * Return either cached or remote attributes. If get remote attr
 * use them to check and invalidate caches, then cache the new attributes.
 */
int
nfsgetattr(vp, vap, cred)
	struct vnode *vp;
	struct vattr *vap;
	struct ucred *cred;
{
	int error;

	if (nfs_getattr_cache(vp, vap)) {
		/*
		 * got cached attributes, we're done.
		 */
		return (0);
	}
	error = nfs_getattr_otw(vp, vap, cred);
	if (error == 0) {
		nfs_cache_check(vp, vap->va_mtime);
		nfs_attrcache_va(vp, vap);
	}
	return (error);
}

nattr_to_vattr(vp, na, vap)
	register struct vnode *vp;
	register struct nfsfattr *na;
	register struct vattr *vap;
{
	struct rnode *rp;

	rp = vtor(vp);
#if RISCOS
	vap->va_fmode = -1; 		/* set to uninitialized state */
#endif
	vap->va_type = (enum vtype)na->na_type;
	vap->va_mode = na->na_mode;
	vap->va_uid = na->na_uid;
	vap->va_gid = na->na_gid;
	vap->va_fsid = 0xffff & (long)make_nfsdev(vtomi(vp)->mi_mntno);
	vap->va_nodeid = na->na_nodeid;
	vap->va_nlink = na->na_nlink;
	if (rp->r_size < na->na_size || ((rp->r_flags & RDIRTY) == 0)){
		rp->r_size = vap->va_size = na->na_size;
	} else {
		vap->va_size = rp->r_size;
	}
	vap->va_atime.tv_sec  = na->na_atime.tv_sec;
	vap->va_atime.tv_usec = na->na_atime.tv_usec;
	vap->va_mtime.tv_sec  = na->na_mtime.tv_sec;
	vap->va_mtime.tv_usec = na->na_mtime.tv_usec;
	vap->va_ctime.tv_sec  = na->na_ctime.tv_sec;
	vap->va_ctime.tv_usec = na->na_ctime.tv_usec;
	vap->va_rdev = na->na_rdev;
	vap->va_blocks = na->na_blocks;
	switch(na->na_type) {

	case NFBLK:
		vap->va_blocksize = BLKDEV_IOSIZE;
		break;

	case NFCHR:
#ifdef RISCOS
		vap->va_blocksize = FSMAXBSIZE;
#else
		vap->va_blocksize = MAXBSIZE;
#endif
		break;

	default:
		vap->va_blocksize = na->na_blocksize;
		break;
	}
	/*
	 * This bit of ugliness is a *TEMPORARY* hack to preserve the
	 * over-the-wire protocols for named-pipe vnodes.  It remaps the
	 * special over-the-wire type to the VFIFO type. (see note in nfs.h)
	 *
	 * BUYER BEWARE:
	 *  If you are porting the NFS to a non-SUN server, you probably
	 *  don't want to include the following block of code.  The
	 *  over-the-wire special file types will be changing with the
	 *  NFS Protocol Revision.
	 */
	if (NA_ISFIFO(na)) {
		vap->va_type = VFIFO;
		vap->va_mode = (vap->va_mode & ~S_IFMT) | S_IFIFO;
		vap->va_rdev = 0;
		vap->va_blocksize = na->na_blocksize;
	}
}
