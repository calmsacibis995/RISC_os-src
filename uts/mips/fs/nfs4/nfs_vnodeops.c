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
#ident	"$Header: nfs_vnodeops.c,v 1.3.1.13.1.2.1.4 90/11/17 11:49:49 beacker Exp $"

/*
 * @(#)nfs_vnodeops.c 2.9 88/07/15 NFSSRC4.0 from 2.156 88/03/22 SMI
 * Copyright (c) 1988 by Sun Microsystems, Inc.
 */

#include "sys/types.h"
#include "sys/errno.h"
#include "sys/param.h"
#include "sys/systm.h"
#include "sys/debug.h"

#include "sys/signal.h"		/* needed by user.h */
#include "sys/pcb.h"		/* needed by user.h */
#include "sys/user.h"

#include "sys/vnode.h"
#include "sys/vfs.h"
#include "sys/vfs_stat.h"
#include "sys/file.h"
#include "sys/uio.h"
#include "sys/buf.h"
#ifdef RISCOS
#include "bsd43/sys/resource.h"
#endif
/* #include "kernel.h" */
/* #include "mman.h" */

#include "sys/immu.h"		/* needed by proc.h */
#include "sys/sbd.h"		/* needed by proc.h */
#include "sys/region.h"		/* needed by proc.h */
#include "sys/proc.h"

#include "bsd/netinet/in.h"

#include "sys/pathname.h"
#include "sys/dirent.h"
#include "sys/conf.h"
/* #include "trace.h" */
/* #include "cmap.h" */
/* #include "../h/vm.h" */
/* #include "../h/map.h" */
/* #include "../machine/pte.h" */

#include "../rpc/types.h"
#include "../rpc/auth.h"
#include "../rpc/clnt.h"
#include "../rpc/xdr.h"
#include "sys/fs/nfs.h"
#include "sys/fs/nfs_clnt.h"
#include "sys/fs/nfs_rnode.h"

#include "../klm/lockmgr.h"
#include "sys/kmem.h"

#ifdef NFSDEBUG
extern int nfsdebug;
#endif

struct vnode *makenfsnode();
struct vnode *dnlc_lookup();
char *newname();
int setdirgid();
u_int setdirmode();

/*
 * Error flags used to return information about certain errors from do_bio()
 */
#define	NFS_EOF		-98

#define ISVDEV(t) ((t == VBLK) || (t == VCHR) || (t == VFIFO))

/*
 * These are the vnode ops routines which implement the vnode interface to
 * the networked file system.  These routines just take their parameters,
 * make them look networkish by putting the right info into interface structs,
 * and then calling the appropriate remote routine(s) to do the work.
 *
 * Note on directory name lookup cacheing:  we desire that all operations
 * on a given client machine come out the same with or without the cache.
 * To correctly do this, we serialize all operations on a given directory,
 * by using RLOCK and RUNLOCK around rfscalls to the server.  This way,
 * we cannot get into races with ourself that would cause invalid information
 * in the cache.  Other clients (or the server itself) can cause our
 * cached information to become invalid, the same as with data pages.
 * Also, if we do detect a stale fhandle, we purge the directory cache
 * relative to that vnode.  This way, the user won't get burned by the
 * cache repeatedly.
 */

int nfs_open();
int nfs_close();
int nfs_rdwr();
int nfs_ioctl();
int nfs_select();
int nfs_getattr();
int nfs_setattr();
int nfs_access();
int nfs_lookup();
int nfs_create();
int nfs_remove();
int nfs_link();
int nfs_rename();
int nfs_mkdir();
int nfs_rmdir();
int nfs_readdir();
int nfs_symlink();
int nfs_readlink();
int nfs_fsync();
int nfs_inactive();
int nfs_lockctl();
int nfs_noop();
int nfs_badop();
int nfs_dump();
int nfs_cmp();
int nfs_realvp();
int nfs_bmap();
int nfs_strategy();

struct buf *async_bufhead;
struct vnodeops nfs_vnodeops = {
	nfs_open,
	nfs_close,
	nfs_rdwr,
	nfs_ioctl,
	nfs_select,
	nfs_getattr,
	nfs_setattr,
	nfs_access,
	nfs_lookup,
	nfs_create,
	nfs_remove,
	nfs_link,
	nfs_rename,
	nfs_mkdir,
	nfs_rmdir,
	nfs_readdir,
	nfs_symlink,
	nfs_readlink,
	nfs_fsync,
	nfs_inactive,
	nfs_bmap,
	nfs_strategy,
	nfs_badop,
	nfs_badop,
	nfs_lockctl,
	nfs_noop,
	nfs_dump,
	nfs_cmp,
	nfs_realvp,
};

/*ARGSUSED*/
int
nfs_open(vpp, flag, cred)
	register struct vnode **vpp;
	int flag;
	struct ucred *cred;
{
	int error;
	struct vattr va;

#ifdef NFSDEBUG
	dprint(nfsdebug, 4, "nfs_open %s %x flag %d\n",
	    vtomi(*vpp)->mi_hostname, *vpp, flag);
#endif
/*
 *	This code fixes a problem with 4.0 having to do
 *	with close to open consistancy. We force open
 *	to go over the wire for the attributes so the
 *	consistancy is guaranteed.
 */
	error = nfs_getattr_otw(*vpp,&va,cred);
	if(error == 0) {
		nfs_cache_check(*vpp,va.va_mtime);
		nfs_attrcache_va(*vpp,&va);
	}
#ifdef RISCOS
	else { 	/* Fix to sun bug # 1014464 */
               /*
                *      If we fail because of a stale NFS file handle,
                *      restart the system call (nfs_getattr_otw will
                *      have flushed all relevant caches).
                */
               if (error == ESTALE)
                       u.u_eosys = RESTARTSYS;
	}
#endif
	return (error);
}

nfs_close(vp, flag, count, cred)
	struct vnode *vp;
	int flag;
	int count;
	struct ucred *cred;
{
	register struct rnode *rp;

	if (count > 1)
		return (0);

#ifdef NFSDEBUG
	dprint(nfsdebug, 4, "nfs_close %s %x flag %d\n",
	    vtomi(vp)->mi_hostname, vp, flag);
#endif
	rp = vtor(vp);

	/*
	 * If the file is an unlinked file, then flush the lookup
	 * cache so that nfs_inactive will be called if this is
	 * the last reference.  Otherwise, if the file was open
	 * for writing or we had an asynchronous write error, we
	 * force the "sync on close" semantic by calling sync_vp.
	 */
	if (flag & FWRITE || rp->r_unldvp != NULL || rp->r_error) {
		sync_vp(vp);
	}
	if (rp->r_unldvp != NULL || rp->r_error) {
		binvalfree(vp);
		dnlc_purge_vp(vp);
	}
	return (flag & FWRITE? rp->r_error : 0);
}

nfs_rdwr(vp, uiop, rw, ioflag, cred)
	register struct vnode *vp;
	struct uio *uiop;
	enum uio_rw rw;
	int ioflag;
	struct ucred *cred;
{
	int error = 0;
	struct rnode *rp;

#ifdef NFSDEBUG
	dprint(nfsdebug, 4,
	    "nfs_rdwr: %s %x rw %s offset %x len %d cred 0x%x\n",
	    vtomi(vp)->mi_hostname, vp, rw == UIO_READ ? "READ" : "WRITE",
	    uiop->uio_offset, uiop->uio_iov->iov_len, cred);
#endif
	if (vp->v_type != VREG) {
		return (EISDIR);
	}

	rp = vtor(vp);
	if (rw == UIO_WRITE || (rw == UIO_READ && rp->r_cred == NULL)) {
		crhold(cred);
		if (rp->r_cred) {
			crfree(rp->r_cred);
		}
		rp->r_cred = cred;
	}

#ifdef notdef
	if (ioflag & IO_UNIT) {
		RLOCK(rp);
	}
#endif
	if ((ioflag & IO_APPEND) && rw == UIO_WRITE) {
		struct vattr va;

		RLOCK(rp);
		error = nfs_getattr(vp, &va, cred);
		if (error == 0) {
			uiop->uio_offset = va.va_size;
		}
	}

	if (error == 0) {
		error = rwvp(vp, uiop, rw, ioflag, cred);
	}

	if ((ioflag & IO_APPEND) && rw == UIO_WRITE) {
		RUNLOCK(rp);
	}
#ifdef notdef
	if (ioflag & IO_UNIT) {
		RUNLOCK(rp);
	}
#endif
#ifdef NFSDEBUG
	dprint(nfsdebug, 5, "nfs_rdwr returning %d\n", error);
#endif
	return (error);
}

rwvp(vp, uio, rw, ioflag, cred)
	register struct vnode *vp;
	register struct uio *uio;
	enum uio_rw rw;
	int ioflag;
	struct ucred *cred;
{
	struct rnode *rp;
	u_int flags;
	register int on;		/* offset into bn */
	register int n;			/* leftover bytes in current bn */
	int error = 0;
	int eof = 0;
	struct buf *bp;
	daddr_t bn;			/* block no. wrt block size of vp */
	int size;			/* blk size of vp: and-off low 8 bits */
	struct vattr va;
	struct vnode *mapped_vp;
	daddr_t mapped_bn;		/* block no. wrt DEV_BSIZE and bn */
	daddr_t mapped_rabn;
	register u_long nodeid;

	if (uio->uio_resid == 0) {
		return (0);
	}
	if (uio->uio_offset < 0 || (uio->uio_offset + uio->uio_resid) < 0) {
		return (EINVAL);
	}
#ifndef RISCOS
	/* we check this in vno_rw(vfs_io.c) */
	if (rw == UIO_WRITE && vp->v_type == VREG &&
	    uio->uio_offset+uio->uio_resid >
	    u.u_rlimit[RLIMIT_FSIZE].rlim_cur) {
		psignal(u.u_procp, SIGXFSZ);
		return (EFBIG);
	}
#endif
	rp = vtor(vp);
	RLOCK(rp);
	size = vtoblksz(vp);
	size &= ~(DEV_BSIZE - 1);
	if (size <= 0) {
		panic("rwvp: zero size");
	}
	do {
		bn = uio->uio_offset / size;
		on = uio->uio_offset % size;
		n = MIN((unsigned)(size - on), uio->uio_resid);
		VOP_BMAP(vp, bn, &mapped_vp, &mapped_bn);

		if (vp->v_flag & VNOCACHE) {
			bp = geteblk(size);
			if (rw == UIO_READ) {
				error = nfsread(vp, bp->b_un.b_addr+on,
				 uio->uio_offset, n, &(bp->b_resid), cred, &va);
				if (error) {
					brelse(bp);
					goto bad;
				}
			}
		} else if (rw == UIO_READ) {
			if ((long) bn < 0) {
				bp = geteblk(size);
				clrbuf(bp);
			} else {
				if (incore(mapped_vp, mapped_bn)) {
					/*
					 * get attributes to check whether in
					 * core data is stale
					 */
					(void) nfs_validate_caches(mapped_vp, cred);
				}
				if (rp->r_lastr + 1 == bn) {
					VOP_BMAP(vp, bn + 1,
					    &mapped_vp, &mapped_rabn);
					bp = breada(mapped_vp, mapped_bn, size,
						mapped_rabn, size);
				} else {
					bp = bread(mapped_vp, mapped_bn, size);
				}
			  }
		} else {
			int i, count, base_db;

			if (rp->r_error) {
				error = rp->r_error;
				goto bad;
			}
#if RISCOS
			/* The block number needs to be given in terms of 512
			 * bytes, not 'size', which is the mount settable NFS
			 * block size and defaulted to 8k.
			 * NOTE: (uio->uio_offset != bn*size) for
			 * uio->uio_offset < size.  NBPC_I is machine indepen-
			 * dent.  (NBPC_I/DEV_BSIZE-1) == 7
			 */
			base_db =
			  btodb(uio->uio_offset) & ~(NBPC_I/DEV_BSIZE - 1);

			/* use 'n' since 'size' would invalidate too many */
			count = howmany(n, DEV_BSIZE);
			nodeid = (vtor(vp))->r_attr.va_nodeid;
			for (i = 0; i < count; i += NBPC_I/DEV_BSIZE) {
				error = punhash(vp, base_db+i, nodeid);
				if (error) goto bad;
			}
#else
			count = howmany(size, DEV_BSIZE);
			for (i = 0; i < count; i += CLBYTES/DEV_BSIZE)
				if (mfind(vp, (daddr_t)(bn + i)))
					munhash(vp, (daddr_t)(bn + i));
#endif
			if (n == size) {
				bp = getblk(mapped_vp, mapped_bn, size);
			} else {
				bp = bread(mapped_vp, mapped_bn, size);
			}
		}
		if (bp->b_flags & B_ERROR) {
			error = geterror(bp);
			brelse(bp);
			goto bad;
		}

		if (rw == UIO_READ) {
			int diff;
			rp->r_lastr = bn;
			diff = rp->r_size - uio->uio_offset;
			if (diff <= 0) {
				brelse(bp);
				error = 0;
				goto bad;
			}
			if (diff < n) {
				n = diff;
				eof = 1;
			}
		}
		u.u_error = uiomove(bp->b_un.b_addr+on, n, rw, uio);

		if (rw == UIO_READ) {
			brelse(bp);
		} else {
			/*
			 * r_size is the maximum number of bytes known
			 * to be in the file.
			 * Make sure it is at least as high as the last
			 * byte we just wrote into the buffer.
			 */
			if (rp->r_size < uio->uio_offset) {
				rp->r_size = uio->uio_offset;
			}
			if (vp->v_flag & VNOCACHE) {
				error = nfswrite(vp, bp->b_un.b_addr+on,
				    uio->uio_offset-n, n, cred);
				brelse(bp);
			} else  {
				rp->r_flags |= RDIRTY;
				if (n + on == size) {
					bp->b_flags |= B_AGE;
					bawrite(bp);
				} else {
					bdwrite(bp);
				}
			}
		}
	} while (u.u_error == 0 && uio->uio_resid > 0 && !eof);
	if (error == 0)				/* XXX */ 
		error = u.u_error;		/* XXX */

bad:
#ifdef RISCOS
	if (rw == UIO_READ) {
		if (rp->r_flags & RPURGECACHE) {
		  rp->r_flags &= ~RPURGECACHE;
		  nfs_purge_caches(vp);
		}
	 }
#endif
	RUNLOCK(rp);
	return (error);
}

/*
 * Write to file.  Writes to remote server in largest size
 * chunks that the server can handle.  Write is synchronous. 
 */
nfswrite(vp, base, offset, count, cred)
	struct vnode *vp;
	caddr_t base;
	u_int offset;
	long count;
	struct ucred *cred;
{
	struct rnode *rp;
	struct nfswriteargs wa;
	struct nfsattrstat *ns;
	int error, tsize;

#ifdef NFSDEBUG
	dprint(nfsdebug, 4, "nfswrite %s %x offset = %d, count = %d\n",
	    vtomi(vp)->mi_hostname, vp, offset, count);
#endif

	ns = (struct nfsattrstat *)kmem_alloc(sizeof (*ns));
	do {
		tsize = MIN(vtomi(vp)->mi_stsize, count);
		wa.wa_data = base;
		wa.wa_fhandle = *vtofh(vp);
		wa.wa_begoff = offset;
		wa.wa_totcount = tsize;
		wa.wa_count = tsize;
		wa.wa_offset = offset;

		rp = vtor(vp);
		if (rp->r_error == ESTALE) {
		  error = rp->r_error;
		}
		else {
		  error = rfscall(vtomi(vp), RFS_WRITE, xdr_writeargs,
		    (caddr_t)&wa, xdr_attrstat, (caddr_t)ns, cred);
		}

		if (!error) {
			error = geterrno(ns->ns_status);
			if (error == ESTALE) { /* avoid nfs purge here */
			  rp->r_error = ESTALE;
			}
		}
#ifdef NFSDEBUG
		dprint(nfsdebug, 3, "nfswrite: sent %d of %d, error %d\n",
		    tsize, count, error);
#endif

		count -= tsize;
		base += tsize;
		offset += tsize;
	} while (!error && count);

	if (!error) {
		nfs_attrcache(vp, &ns->ns_attr);
	}
	kmem_free((caddr_t)ns, sizeof (*ns));
	switch (error) {
	case 0:
		break;

	case EDQUOT:
		/* Until we add more support, this is the best we can do
		 * for an error message.  Since the buffer has been
		 * disconnected from the buffer write requestor, all is
		 * lost.
		 */
		printf("NFS write error: on host %s quota reached for uid: %d\n",
		       vtomi(vp)->mi_hostname, cred->cr_ruid);
		break;

	case ENOSPC:
		printf("NFS write error: on host %s remote file system full\n",
		   vtomi(vp)->mi_hostname);
		break;

	default:
		printf("NFS write error %d on host %s fh ",
		    error, vtomi(vp)->mi_hostname);
		printfhandle((caddr_t)vtofh(vp));
		printf("\n");
		break;
	}
#ifdef NFSDEBUG
	dprint(nfsdebug, 5, "nfswrite: returning %d\n", error);
#endif
	return (error);
}

/*
 * Print a file handle
 */
printfhandle(fh)
	caddr_t fh;
{
	int i;
	int fhint[NFS_FHSIZE / sizeof (int)];

	bcopy(fh, (caddr_t)fhint, sizeof (fhint));
	for (i = 0; i < (sizeof (fhint) / sizeof (int)); i++) {
		printf("%x ", fhint[i]);
	}
}

/*
 * Read from a file.  Reads data in largest chunks our interface can handle.
 */
int
nfsread(vp, base, offset, count, residp, cred, vap)
	struct vnode *vp;
	caddr_t base;
	u_int offset;
	long count;
	long *residp;
	struct ucred *cred;
	struct vattr *vap;
{
	struct rnode *rp;
	struct nfsreadargs ra;
	struct nfsrdresult rr;
 	int error, tsize;

#ifdef NFSDEBUG
	dprint(nfsdebug, 4, "nfsread %s %x offset = %d, totcount = %d\n",
	    vtomi(vp)->mi_hostname, vp, offset, count);
#endif

	do {
		tsize = MIN(vtomi(vp)->mi_tsize, count);
		rr.rr_data = base;
		ra.ra_fhandle = *vtofh(vp);
		ra.ra_offset = offset;
		ra.ra_totcount = tsize;
		ra.ra_count = tsize;

		rp = vtor(vp);
		if (rp->r_error == ESTALE) {
		  error = ESTALE;
		}
		else {
		  error = rfscall(vtomi(vp), RFS_READ, xdr_readargs,
			  (caddr_t)&ra,	xdr_rdresult, (caddr_t)&rr, cred);
		}

		if (!error) {
			error = geterrno(rr.rr_status);
			if (error == ESTALE) { /* avoid nfs purge here */
			  rp->r_error = ESTALE;
			}
		}
#ifdef NFSDEBUG
		dprint(nfsdebug, 3, "nfsread: got %d of %d, error %d\n",
		    tsize, count, error);
#endif
		if (!error) {
			count -= rr.rr_count;
			base += rr.rr_count;
			offset += rr.rr_count;
		}
	} while (!error && count && rr.rr_count == tsize);

	*residp = count;

	if (!error) {
		nattr_to_vattr(vp, &rr.rr_attr, vap);
	}
#ifdef NFSDEBUG
	dprint(nfsdebug, 5, "nfsread: returning %d, resid %d\n",
		error, *residp);
#endif
	return (error);
}

/*ARGSUSED*/
nfs_ioctl(vp, com, data, flag, cred)
	struct vnode *vp;
	int com;
	caddr_t data;
	int flag;
	struct ucred *cred;
{

	return (EOPNOTSUPP);
}

/*ARGSUSED*/
int
nfs_select(vp, which, cred)
	struct vnode *vp;
	int which;
	struct ucred *cred;
{

	return (EOPNOTSUPP);
}

int
nfs_getattr(vp, vap, cred)
	struct vnode *vp;
	struct vattr *vap;
	struct ucred *cred;
{
	int error;
	struct rnode *rp = vtor(vp);

#ifdef NFSDEBUG
	dprint(nfsdebug, 4, "nfs_getattr %s %x\n", vtomi(vp)->mi_hostname, vp);
#endif
	RLOCK(rp);
	(void) sync_vp(vp);
	RUNLOCK(rp);
	error = nfsgetattr(vp, vap, cred);
#ifdef NFSDEBUG
	dprint(nfsdebug, 5, "nfs_getattr: returns %d\n", error);
#endif
	return (error);
}

int
nfs_setattr(vp, vap, cred)
	register struct vnode *vp;
	register struct vattr *vap;
	struct ucred *cred;
{
	int error;
	struct nfssaargs args;
	struct nfsattrstat *ns;
	extern int (*caller())();

#ifdef NFSDEBUG
	dprint(nfsdebug, 4, "nfs_setattr %s %x\n", vtomi(vp)->mi_hostname, vp);
#endif

	ns = (struct nfsattrstat *)kmem_alloc(sizeof (*ns));
	if ((vap->va_nlink != -1) || (vap->va_blocksize != -1) ||
	    (vap->va_rdev != -1) || (vap->va_blocks != -1) ||
	    (vap->va_ctime.tv_sec != -1) || (vap->va_ctime.tv_usec != -1)) {
		error = EINVAL;
	} else {
		RLOCK(vtor(vp));
		sync_vp(vp);
		RUNLOCK(vtor(vp));
		if (vap->va_size != -1) {
			(vtor(vp))->r_size = vap->va_size;
		}

		/*
		 * Allow SysV-compatible option to set access and
		 * modified times if root, owner, or write access.
		 *
		 * XXX - For now, va_mtime.tv_usec == -1 flags this.
		 *
		 * XXX - Until an NFS Protocol Revision, this may be
		 *       simulated by setting the client time in the
		 *       tv_sec field of the access and modified times
		 *       and setting the tv_usec field of the modified
		 *       time to an invalid value (1,000,000).  This
		 *       may be detected by servers modified to do the
		 *       right thing, but will not be disastrous on
		 *       unmodified servers.
		 */
		if ((vap->va_mtime.tv_sec != -1) &&
		    (vap->va_mtime.tv_usec == -1)) {
			vap->va_atime = time;
			vap->va_mtime.tv_sec = time.tv_sec;
			vap->va_mtime.tv_usec = 1000000;
		}

		vattr_to_sattr(vap, &args.saa_sa);
		bcopy((caddr_t)vtofh(vp),(caddr_t)&args.saa_fh,sizeof(fhandle_t));
		error = rfscall(vtomi(vp), RFS_SETATTR, xdr_saargs,
		    (caddr_t)&args, xdr_attrstat, (caddr_t)ns, cred);
		if (error == 0) {
			error = geterrno(ns->ns_status);
			if (error == 0) {
				nfs_cache_check(vp, ns->ns_attr.na_mtime);
				nfs_attrcache(vp, &ns->ns_attr);
			} else {
				PURGE_STALE_FH(error, vp);
			}
		}
	}
	kmem_free((caddr_t)ns, sizeof (*ns));
#ifdef NFSDEBUG
	dprint(nfsdebug, 5, "nfs_setattr: returning %d\n", error);
#endif
	return (error);
}

int
nfs_access(vp, mode, cred)
	struct vnode *vp;
	int mode;
	struct ucred *cred;
{
	struct vattr va;
	gid_t *gp;

#ifdef NFSDEBUG
	dprint(nfsdebug, 4, "nfs_access %s %x mode %d uid %d\n",
	    vtomi(vp)->mi_hostname, vp, mode, cred->cr_uid);
#endif
	u.u_error = nfsgetattr(vp, &va, cred);
	if (u.u_error) {
		return (u.u_error);
	}
	/*
	 * If you're the super-user,
	 * you always get access.
	 */
	if (cred->cr_uid == 0)
		return (0);
	/*
	 * Access check is based on only
	 * one of owner, group, public.
	 * If not owner, then check group.
	 * If not a member of the group,
	 * then check public access.
	 */
	if (cred->cr_uid != va.va_uid) {
		mode >>= 3;
		if (cred->cr_gid == va.va_gid)
			goto found;
		gp = cred->cr_groups;
		for (; gp < &cred->cr_groups[NGROUPS] && *gp != NOGROUP; gp++)
			if (va.va_gid == *gp)
				goto found;
		mode >>= 3;
	}
found:
	if ((va.va_mode & mode) == mode) {
		return (0);
	}
#ifdef NFSDEBUG
	dprint(nfsdebug, 5, "nfs_access: returning %d\n", u.u_error);
#endif
	u.u_error = EACCES;
	return (EACCES);
}

int
nfs_readlink(vp, uiop, cred)
	struct vnode *vp;
	struct uio *uiop;
	struct ucred *cred;
{
	int error;
	struct nfsrdlnres rl;

#ifdef NFSDEBUG
	dprint(nfsdebug, 4, "nfs_readlink %s %x\n", vtomi(vp)->mi_hostname, vp);
#endif
	if (vp->v_type != VLNK)
		return (ENXIO);
	rl.rl_data = (char *)kmem_alloc(NFS_MAXPATHLEN);
	error = rfscall(vtomi(vp), RFS_READLINK, xdr_fhandle,
	    (caddr_t)vtofh(vp), xdr_rdlnres, (caddr_t)&rl, cred);
	if (!error) {
		error = geterrno(rl.rl_status);
		if (!error) {
			error = uiomove(rl.rl_data, (int)rl.rl_count,
			    UIO_READ, uiop);
		} else {
			PURGE_STALE_FH(error, vp);
		}
	}
	kmem_free((caddr_t)rl.rl_data, NFS_MAXPATHLEN);
#ifdef NFSDEBUG
	dprint(nfsdebug, 5, "nfs_readlink: returning %d\n", error);
#endif
	return (error);
}

/*ARGSUSED*/
int
nfs_fsync(vp, cred)
	struct vnode *vp;
	struct ucred *cred;
{
	register struct rnode *rp;

#ifdef NFSDEBUG
	dprint(nfsdebug, 4, "nfs_fsync %s %x\n", vtomi(vp)->mi_hostname, vp);
#endif
	rp = vtor(vp);
	RLOCK(rp);
	flush_vp(vp);
	RUNLOCK(rp);
	return (rp->r_error);
}

/*
 * Weirdness: if the file was removed while it was open it got renamed
 * (by nfs_remove) instead.  Here we remove the renamed file.
 */
/*ARGSUSED*/
int
nfs_inactive(vp, cred)
	register struct vnode *vp;
	struct ucred *cred;
{
	register struct rnode *rp;
	struct nfsdiropargs da;
	enum nfsstat status;
	int error;

#ifdef NFSDEBUG
	dprint(nfsdebug, 4, "nfs_inactive %s, %x\n",
	    vtomi(vp)->mi_hostname, vp);
#endif

	/*
	 * Pull rnode off of the hash list so it won't be found.
	 */
	rp = vtor(vp);
	rp_rmhash(rp);

	/*
	 * Flush the buffer cache and the page cache.
	 */
	binvalfree(vp);
	flushpgch(vp, rp->r_attr.va_nodeid, rp->r_attr.va_size);

	if (rp->r_unldvp != NULL) {
		rp->r_flags &= ~RDIRTY;
		RLOCK(vtor(rp->r_unldvp));
		setdiropargs(&da, rp->r_unlname, rp->r_unldvp);
		error = rfscall(vtomi(rp->r_unldvp), RFS_REMOVE,
		    xdr_diropargs, (caddr_t)&da,
		    xdr_enum, (caddr_t)&status, rp->r_unlcred);
		if (error == 0)
			error = geterrno(status);
		RUNLOCK(vtor(rp->r_unldvp));
		VN_RELE(rp->r_unldvp);
		rp->r_unldvp = NULL;
		kmem_free((caddr_t)rp->r_unlname, NFS_MAXNAMLEN);
		rp->r_unlname = NULL;
		crfree(rp->r_unlcred);
		rp->r_unlcred = NULL;
	} 

#ifdef RISCOS
	rnodefree(rp);        /* Required to avoid global kernel name conflict */
#else
	rfree(rp);
#endif

#ifdef NFSDEBUG
	dprint(nfsdebug, 5, "nfs_inactive done\n");
#endif
	return (error);
}

int nfs_dnlc = 1;	/* use dnlc */

/*
 * Remote file system operations having to do with directory manipulation.
 */
/* ARGSUSED */
int
nfs_lookup(dvp, nm, vpp, cred, pnp, flags)
	struct vnode *dvp;
	char *nm;
	struct vnode **vpp;
	struct ucred *cred;
	struct pathname *pnp;
	int flags;
{
	int error;
	struct nfsdiropargs da;
	struct  nfsdiropres *dr;

#ifdef NFSDEBUG
	dprint(nfsdebug, 4, "nfs_lookup %s %x '%s'\n",
	    vtomi(dvp)->mi_hostname, dvp, nm);
#endif
	/*
	 * Before checking dnlc, validate caches
	 */
	error = nfs_validate_caches(dvp, cred);
	if (error) {
		return (error);
	}

	RLOCK(vtor(dvp));
	*vpp = (struct vnode *)dnlc_lookup(dvp, nm, cred);
	if (*vpp) {
		VN_HOLD(*vpp);
#ifdef RISCOS
		/* Sun bug id 1007839.
		 * The bug is when the 'x' access was removed from a
		 * directory files within that directory were accessible
		 * by system calls such as `stat', `access', `chmod', and
		 * `chown'.
		 *
		 * Make sure we can search this directory (after the fact).
		 * It's done here because over the wire lookups verify
		 * permissions on the server.  VOP_ACCESS will one day
		 * go over the wire, so let's use it sparingly.
		 */
                 error = VOP_ACCESS(dvp, VEXEC, cred);
                 if (error) {
		   VN_RELE(*vpp);
		   RUNLOCK(vtor(dvp));
		   return(error);
                 }
#endif
	} else {
		dr = (struct  nfsdiropres *)kmem_alloc(sizeof (*dr));
		setdiropargs(&da, nm, dvp);

		error = rfscall(vtomi(dvp), RFS_LOOKUP, xdr_diropargs,
		    (caddr_t)&da, xdr_diropres, (caddr_t)dr, cred);
		if (error == 0) {
			error = geterrno(dr->dr_status);
			PURGE_STALE_FH(error, dvp);
		}
		if (error == 0) {
			*vpp = makenfsnode(&dr->dr_fhandle,
			    &dr->dr_attr, dvp->v_vfsp);
			if (nfs_dnlc) {
				dnlc_enter(dvp, nm, *vpp, cred);
			}
		} else {
			*vpp = (struct vnode *)0;
		}

		kmem_free((caddr_t)dr, sizeof (*dr));
	}
	/*
	 * If vnode is a device create special vnode
	 */
	if (!error && ISVDEV((*vpp)->v_type)) {
		struct vnode *newvp;

		newvp = specvp(*vpp, (*vpp)->v_rdev, (*vpp)->v_type);
		VN_RELE(*vpp);
		*vpp = newvp;
	}
	RUNLOCK(vtor(dvp));
#ifdef NFSDEBUG
	dprint(nfsdebug, 5, "nfs_lookup returning %d vp = %x\n", error, *vpp);
#endif
	return (error);
}

/*ARGSUSED*/
int
nfs_create(dvp, nm, va, exclusive, mode, vpp, cred)
	struct vnode *dvp;
	char *nm;
	struct vattr *va;
	enum vcexcl exclusive;
	int mode;
	struct vnode **vpp;
	struct ucred *cred;
{
	int error;
	struct nfscreatargs args;
	struct  nfsdiropres *dr;

#ifdef NFSDEBUG
	dprint(nfsdebug, 4, "nfs_create %s %x '%s' excl=%d, mode=%o\n",
	    vtomi(dvp)->mi_hostname, dvp, nm, exclusive, mode);
#endif
	if (exclusive == EXCL) {
		/*
		 * This is buggy: there is a race between the lookup and the
		 * create.  We should send the exclusive flag over the wire.
		 */
		error = nfs_lookup(dvp, nm, vpp, cred, (struct pathname *) NULL, 0);
		if (!error) {
			VN_RELE(*vpp);
			return (EEXIST);
		}
	}
	*vpp = (struct vnode *)0;

	dr = (struct  nfsdiropres *)kmem_alloc(sizeof (*dr));
	setdiropargs(&args.ca_da, nm, dvp);

	/*
	 * Decide what the group-id of the created file should be.
	 * Set it in attribute list as advisory...then do a setattr
	 * if the server didn't get it right the first time.
	 */
	va->va_gid = (short) setdirgid(dvp);

	/*
	 * This is a completely gross hack to make mknod
	 * work over the wire until we can wack the protocol
	 */
#define	IFCHR		0020000		/* character special */
#define	IFBLK		0060000		/* block special */
#define	IFSOCK		0140000		/* socket */
	if (va->va_type == VCHR) {
		va->va_mode |= IFCHR;
		va->va_size = (u_long)va->va_rdev;
	} else if (va->va_type == VBLK) {
		va->va_mode |= IFBLK;
		va->va_size = (u_long)va->va_rdev;
	} else if (va->va_type == VFIFO) {
		va->va_mode |= IFCHR;		/* xtra kludge for namedpipe */
		va->va_size = (u_long)NFS_FIFO_DEV;	/* blech */
	} else if (va->va_type == VSOCK) {
		va->va_mode |= IFSOCK;
	}

	vattr_to_sattr(va, &args.ca_sa);
	RLOCK(vtor(dvp));
	dnlc_remove(dvp, nm);
	error = rfscall(vtomi(dvp), RFS_CREATE, xdr_creatargs, (caddr_t)&args,
	    xdr_diropres, (caddr_t)dr, cred);
	PURGE_ATTRCACHE(dvp);	/* mod time changed */
	if (!error) {
		error = geterrno(dr->dr_status);
		if (!error) {
			short gid;

			*vpp = makenfsnode(&dr->dr_fhandle, &dr->dr_attr,
			    dvp->v_vfsp);
			if (va->va_size == 0) {
				(vtor(*vpp))->r_size = 0;
			}
			if (nfs_dnlc) {
				dnlc_enter(dvp, nm, *vpp, cred);
			}

			/*
			 * Make sure the gid was set correctly.
			 * If not, try to set it (but don't lose
			 * any sleep over it).
			 */
			gid = va->va_gid;
			nattr_to_vattr(*vpp, &dr->dr_attr, va);
			if (gid != va->va_gid) {
				struct vattr vattr;

				vattr_null(&vattr);
				vattr.va_gid = gid;
				(void) nfs_setattr(*vpp, &vattr, cred);
				va->va_gid = gid;
			}

			/*
			 * If vnode is a device create special vnode
			 */
			if (ISVDEV((*vpp)->v_type)) {
				struct vnode *newvp;

				newvp = specvp(*vpp, (*vpp)->v_rdev, (*vpp)->v_type);
				VN_RELE(*vpp);
				*vpp = newvp;
			}
		} else {
			PURGE_STALE_FH(error, dvp);
		}
	}
	RUNLOCK(vtor(dvp));
	kmem_free((caddr_t)dr, sizeof (*dr));
#ifdef NFSDEBUG
	dprint(nfsdebug, 5, "nfs_create returning %d\n", error);
#endif
	return (error);
}

/*
 * Weirdness: if the vnode to be removed is open
 * we rename it instead of removing it and nfs_inactive
 * will remove the new name.
 */
int
nfs_remove(dvp, nm, cred)
	struct vnode *dvp;
	char *nm;
	struct ucred *cred;
{
	int error;
	struct nfsdiropargs da;
	enum nfsstat status;
	struct vnode *vp;
	struct vnode *oldvp;
	struct vnode *realvp;
	char *tmpname;

#ifdef NFSDEBUG
	dprint(nfsdebug, 4, "nfs_remove %s %x '%s'\n",
	    vtomi(dvp)->mi_hostname, vp, nm);
#endif

	status = NFS_OK;
	error = nfs_lookup(dvp, nm, &vp, cred, (struct pathname *) NULL, 0);
	/*
	 * Lookup may have returned a non-nfs vnode!
	 * get the real vnode.
	 */
	if (error == 0 && VOP_REALVP(vp, &realvp) == 0) {
		oldvp = vp;
		vp = realvp;
	} else {
		oldvp = NULL;
	}
	if (error == 0 && vp != NULL) {
		RLOCK(vtor(dvp));
		/*
		 * We need to flush the buffer and name cache so we can
		 * check the real reference count on the vnode
		 */
#ifdef RISCOS
		binvalfree(vp);
#endif
		dnlc_purge_vp(vp);

		if ((vp->v_count > 1) && vtor(vp)->r_unldvp == NULL) {
			tmpname = newname();
			RUNLOCK(vtor(dvp));
			error = nfs_rename(dvp, nm, dvp, tmpname, cred);
			RLOCK(vtor(dvp));
			if (error) {
				kmem_free((caddr_t)tmpname, NFS_MAXNAMLEN);
			} else {
				VN_HOLD(dvp);
				vtor(vp)->r_unldvp = dvp;
				vtor(vp)->r_unlname = tmpname;
				if (vtor(vp)->r_unlcred != NULL) {
					crfree(vtor(vp)->r_unlcred);
				}
				crhold(cred);
				vtor(vp)->r_unlcred = cred;
			}
		} else {
			vtor(vp)->r_flags &= ~RDIRTY;
			setdiropargs(&da, nm, dvp);
			error = rfscall(vtomi(dvp), RFS_REMOVE, xdr_diropargs,
			    (caddr_t)&da, xdr_enum, (caddr_t)&status, cred);
			PURGE_ATTRCACHE(dvp);	/* mod time changed */
			PURGE_ATTRCACHE(vp);	/* link count changed */
			PURGE_STALE_FH(error ? error : geterrno(status), dvp);
		}
		RUNLOCK(vtor(dvp));
		if (oldvp) {
			bflush(oldvp);
			VN_RELE(oldvp);
		} else {
			bflush(vp);
			VN_RELE(vp);
		}
	}
	if (error == 0) {
		error = geterrno(status);
	}
#ifdef NFSDEBUG
	dprint(nfsdebug, 5, "nfs_remove: returning %d\n", error);
#endif
	return (error);
}

int
nfs_link(vp, tdvp, tnm, cred)
	struct vnode *vp;
	struct vnode *tdvp;
	char *tnm;
	struct ucred *cred;
{
	int error;
	struct nfslinkargs args;
	enum nfsstat status;
	struct vnode *realvp;

	if (VOP_REALVP(vp, &realvp) == 0) {
		vp = realvp;
	}

#ifdef NFSDEBUG
	dprint(nfsdebug, 4, "nfs_link from %s %x to %s %x '%s'\n",
	    vtomi(vp)->mi_hostname, vp, vtomi(tdvp)->mi_hostname, tdvp, tnm);
#endif

	args.la_from = *vtofh(vp);
	setdiropargs(&args.la_to, tnm, tdvp);
	RLOCK(vtor(tdvp));
	error = rfscall(vtomi(vp), RFS_LINK, xdr_linkargs, (caddr_t)&args,
	    xdr_enum, (caddr_t)&status, cred);

	PURGE_ATTRCACHE(tdvp);	/* mod time changed */
	PURGE_ATTRCACHE(vp);	/* link count changed */
	RUNLOCK(vtor(tdvp));

	if (!error) {
		error = geterrno(status);
		PURGE_STALE_FH(error, vp);
		PURGE_STALE_FH(error, tdvp);
	}

#ifdef NFSDEBUG
	dprint(nfsdebug, 5, "nfs_link returning %d\n", error);
#endif
	return (error);
}

int
nfs_rename(odvp, onm, ndvp, nnm, cred)
	struct vnode *odvp;
	char *onm;
	struct vnode *ndvp;
	char *nnm;
	struct ucred *cred;
{
	int error;
	enum nfsstat status;
	struct nfsrnmargs args;

#ifdef NFSDEBUG
	dprint(nfsdebug, 4, "nfs_rename from %s %x '%s' to %s %x '%s'\n",
	    vtomi(odvp)->mi_hostname, odvp, onm,
	    vtomi(ndvp)->mi_hostname, ndvp, nnm);
#endif
	if (!strcmp(onm, ".") || !strcmp(onm, "..") || !strcmp(nnm, ".") ||
	    !strcmp (nnm, "..")) {
		error = EINVAL;
	} else {
		RLOCK(vtor(odvp));
		dnlc_remove(odvp, onm);
		dnlc_remove(ndvp, nnm);
		if (ndvp != odvp) {
			RLOCK(vtor(ndvp));
		}
		setdiropargs(&args.rna_from, onm, odvp);
		setdiropargs(&args.rna_to, nnm, ndvp);
		error = rfscall(vtomi(odvp), RFS_RENAME, xdr_rnmargs,
		    (caddr_t)&args, xdr_enum, (caddr_t)&status, cred);

		PURGE_ATTRCACHE(odvp);	/* mod time changed */
		PURGE_ATTRCACHE(ndvp);	/* mod time changed */
		RUNLOCK(vtor(odvp));
		if (ndvp != odvp) {
			RUNLOCK(vtor(ndvp));
		}
		if (!error) {
			error = geterrno(status);
			PURGE_STALE_FH(error, odvp);
			PURGE_STALE_FH(error, ndvp);
		}
	}
#ifdef NFSDEBUG
	dprint(nfsdebug, 5, "nfs_rename returning %d\n", error);
#endif
	return (error);
}

int
nfs_mkdir(dvp, nm, va, vpp, cred)
	struct vnode *dvp;
	char *nm;
	register struct vattr *va;
	struct vnode **vpp;
	struct ucred *cred;
{
	int error;
	struct nfscreatargs args;
	struct  nfsdiropres *dr;

#ifdef NFSDEBUG
	dprint(nfsdebug, 4, "nfs_mkdir %s %x '%s'\n",
	    vtomi(dvp)->mi_hostname, dvp, nm);
#endif
	dr = (struct nfsdiropres *)kmem_alloc(sizeof (*dr));
	setdiropargs(&args.ca_da, nm, dvp);

	/*
	 * Decide what the group-id and set-gid bit of the created directory
	 * should be.  May have to do a setattr to get the gid right.
	 */
	va->va_gid = (short) setdirgid(dvp);
	va->va_mode = (u_short) setdirmode(dvp, va->va_mode);

	vattr_to_sattr(va, &args.ca_sa);
	RLOCK(vtor(dvp));
	dnlc_remove(dvp, nm);
	error = rfscall(vtomi(dvp), RFS_MKDIR, xdr_creatargs, (caddr_t)&args,
	    xdr_diropres, (caddr_t)dr, cred);
	PURGE_ATTRCACHE(dvp);	/* mod time changed */
	RUNLOCK(vtor(dvp));
	if (!error) {
		error = geterrno(dr->dr_status);
		PURGE_STALE_FH(error, dvp);
	}
	if (!error) {
		gid_t gid;

		/*
		 * Due to a pre-4.0 server bug the attributes that come back
		 * on mkdir are not correct. Use them only to set the vnode
		 * type in makenfsnode.
		 */
		*vpp = makenfsnode(&dr->dr_fhandle, &dr->dr_attr, dvp->v_vfsp);
		PURGE_ATTRCACHE(*vpp);

		if (nfs_dnlc) {
			dnlc_enter(dvp, nm, *vpp, cred);
		}

		/*
		 * Make sure the gid was set correctly.
		 * If not, try to set it (but don't lose
		 * any sleep over it).
		 */
		gid = va->va_gid;
		nattr_to_vattr(*vpp, &dr->dr_attr, va);
		if (gid != va->va_gid) {
			vattr_null(va);
			va->va_gid = gid;
			(void) nfs_setattr(*vpp, va, cred);
		}
	} else {
		*vpp = (struct vnode *)0;
	}
	kmem_free((caddr_t)dr, sizeof (*dr));
#ifdef NFSDEBUG
	dprint(nfsdebug, 5, "nfs_mkdir returning %d\n", error);
#endif
	return (error);
}

int
nfs_rmdir(dvp, nm, cred)
	struct vnode *dvp;
	char *nm;
	struct ucred *cred;
{
	int error;
	enum nfsstat status;
	struct nfsdiropargs da;

#ifdef NFSDEBUG
	dprint(nfsdebug, 4, "nfs_rmdir %s %x '%s'\n",
	    vtomi(dvp)->mi_hostname, dvp, nm);
#endif
	setdiropargs(&da, nm, dvp);
	RLOCK(vtor(dvp));
	dnlc_purge_vp(dvp);
	error = rfscall(vtomi(dvp), RFS_RMDIR, xdr_diropargs, (caddr_t)&da,
	    xdr_enum, (caddr_t)&status, cred);
	PURGE_ATTRCACHE(dvp);	/* mod time changed */
	RUNLOCK(vtor(dvp));
	if (!error) {
		error = geterrno(status);
		PURGE_STALE_FH(error, dvp);
	}
#ifdef NFSDEBUG
	dprint(nfsdebug, 5, "nfs_rmdir returning %d\n", error);
#endif
	return (error);
}

int
nfs_symlink(dvp, lnm, tva, tnm, cred)
	struct vnode *dvp;
	char *lnm;
	struct vattr *tva;
	char *tnm;
	struct ucred *cred;
{
	int error;
	struct nfsslargs args;
	enum nfsstat status;

#ifdef NFSDEBUG
	dprint(nfsdebug, 4, "nfs_symlink %s %x '%s' to '%s'\n",
	    vtomi(dvp)->mi_hostname, dvp, lnm, tnm);
#endif
	setdiropargs(&args.sla_from, lnm, dvp);
	vattr_to_sattr(tva, &args.sla_sa);
	args.sla_tnm = tnm;
	error = rfscall(vtomi(dvp), RFS_SYMLINK, xdr_slargs, (caddr_t)&args,
	    xdr_enum, (caddr_t)&status, cred);
	PURGE_ATTRCACHE(dvp);	/* mod time changed */
	if (!error) {
		error = geterrno(status);
		PURGE_STALE_FH(error, dvp);
	}
#ifdef NFSDEBUG
	dprint(nfsdebug, 5, "nfs_sysmlink: returning %d\n", error);
#endif
	return (error);
}

/*
 * Read directory entries.
 * There are some weird things to look out for here.  The uio_offset
 * field is either 0 or it is the offset returned from a previous
 * readdir.  It is an opaque value used by the server to find the
 * correct directory block to read.  The byte count must be at least
 * vtoblksz(vp) bytes.  The count field is the number of blocks to
 * read on the server.  This is advisory only, the server may return
 * only one block's worth of entries.  Entries may be compressed on
 * the server.
 */
int
nfs_readdir(vp, uiop, cred)
	struct vnode *vp;
	register struct uio *uiop;
	struct ucred *cred;
{
	int error = 0;
	struct iovec *iovp;
	unsigned count;
	struct nfsrddirargs rda;
	struct nfsrddirres  rd;
	struct rnode *rp;

	rp = vtor(vp);
	if ((rp->r_flags & REOF) && (rp->r_size == (u_long)uiop->uio_offset)) {
		return (0);
	}
	iovp = uiop->uio_iov;
	count = iovp->iov_len;
#ifdef NFSDEBUG
	dprint(nfsdebug, 4, "nfs_readdir %s %x count %d offset %ld\n",
	    vtomi(vp)->mi_hostname, vp, count, uiop->uio_offset);
#endif
	/*
	 * XXX We should do some kind of test for count >= DEV_BSIZE
	 */
	if (uiop->uio_iovcnt != 1) {
		return (EINVAL);
	}
	count = MIN(count, vtomi(vp)->mi_tsize);
	rda.rda_count = count;
	rda.rda_offset = uiop->uio_offset;
	bcopy((caddr_t)vtofh(vp),(caddr_t)&rda.rda_fh,sizeof(fhandle_t));
	rd.rd_size = count;
#ifdef RISCOS
	rd.rd_entries = (struct bsd43_dirent *)kmem_alloc((u_int)count);
#else
	rd.rd_entries = (struct dirent *)kmem_alloc((u_int)count);
#endif
	error = rfscall(vtomi(vp), RFS_READDIR, xdr_rddirargs, (caddr_t)&rda,
	    xdr_getrddirres, (caddr_t)&rd, cred);
	if (!error) {
		error = geterrno(rd.rd_status);
		PURGE_STALE_FH(error, vp);
	}
	if (!error) {
		/*
		 * move dir entries to user land
		 */
		if (rd.rd_size) {
			error = uiomove((caddr_t)rd.rd_entries,
			    (int)rd.rd_size, UIO_READ, uiop);
			rda.rda_offset = rd.rd_offset;
			uiop->uio_offset = rd.rd_offset;
		}
		if (rd.rd_eof) {
			rp->r_flags |= REOF;
			rp->r_size = uiop->uio_offset;
		}
	}
	kmem_free((caddr_t)rd.rd_entries, (u_int)count);
#ifdef NFSDEBUG
	dprint(nfsdebug, 5, "nfs_readdir: returning %d resid %d, offset %ld\n",
	    error, uiop->uio_resid, uiop->uio_offset);
#endif
	return (error);
}


static int async_daemon_ready;		/* number of async biod's ready */
static int async_daemon_count;		/* number of existing biod's */

int nfs_wakeup_one_biod = 1;


int
nfs_strategy(bp)
	register struct buf *bp;
{
	register struct buf *bp1;

#ifdef NFSDEBUG
	dprint(nfsdebug, 4, "nfs_strategy bp %x\n", bp);
#endif
	/*
	 * If there was an asynchronous write error on this rnode
	 * then we just return the old error code. This continues
	 * until the rnode goes away (zero ref count). We do this because
	 * there can be many procs writing this rnode.
	 */
	if (vtor(bp->b_vp)->r_error) {
		bp->b_error = vtor(bp->b_vp)->r_error;
		bp->b_flags |= B_ERROR;
		iodone(bp);
		return;
	}

	if (async_daemon_count && bp->b_flags & B_ASYNC) {
		if(async_bufhead) {
			bp1 = async_bufhead;
			while (bp1->b_actf) {
				bp1 = bp1->b_actf;
			}
			bp1->b_actf = bp;
		} else {
			async_bufhead = bp;
		}
		bp->b_actf = (struct buf *)NULL;
		if (nfs_wakeup_one_biod == 1) {
			wakeup_one((caddr_t) &async_bufhead);
		} else {
			wakeup((caddr_t) &async_bufhead);
		}
	} else {
		do_bio(bp);
	}
}


async_daemon()
{
	register struct buf *bp;
	struct rnode *rp;

#ifdef SVFORK
	if((u.u_procp->p_flag & SVFORK) == 0)
		vrelvm();	/* First, release resources */
#endif SVFORK
	async_daemon_count++;

	for (;;) {

	  if (setjmp(u.u_qsav)) {

finish_exit:
		if (async_daemon_count == 0) {
			/*
			 * We already were processing requests below
			 * and we were signaled again.  So this time,
			 * just give up and abort all the requests.
			 */
			bp = async_bufhead;
			while (bp) {
				bp->b_flags |= B_ERROR;
				async_bufhead = bp->b_actf;
				/*
				 * Since we are always ASYNC iodone
				 * will free the buf.
				 */
				iodone(bp);
				bp = async_bufhead;
			}
		} else {
			async_daemon_count--;
			async_daemon_ready--;
			/*
			 * If we were the last async daemon,
			 * process all the queued requests.
			 */
			if (async_daemon_count == 0) {
				bp = async_bufhead;
				while (bp) {
					async_bufhead = bp->b_actf;
					/*
					 * Since we are ASYNC do_bio will
					 * free the bp.
					 */
					do_bio(bp);
					bp = async_bufhead;
				}
			}
		}
		exit(0);
	      }

	  async_daemon_ready++;
	  while (async_bufhead == (struct buf *)NULL) {
	    if (sleep((caddr_t)&async_bufhead, ((PZERO+1) | PCATCH)))
	      goto finish_exit;
	  }
	  async_daemon_ready--;
	  bp = async_bufhead;
	  async_bufhead = bp->b_actf;
	  do_bio(bp);
	}
}

/*
 * XXX stuff to keep track of blown caches
 */
int nfs_cache_blown;
fhandle_t nfs_cache_blown_fh;

int
do_bio(bp)
	register struct buf *bp;
{
	register struct rnode *rp = vtor(bp->b_vp);
	struct vattr va;
	long count;
	int error;
	int read, async;

	read = bp->b_flags & B_READ;
	async = bp->b_flags & B_ASYNC;
#ifdef NFSDEBUG
	dprint(nfsdebug, 4,
       "do_bio: addr %x, blk %ld, offset %ld, size %ld, B_READ %x B_ASYNC %x\n",
	    bp->b_un.b_addr, bp->b_blkno, dbtob(bp->b_blkno),
	    bp->b_bcount, read, async);
#endif

	if (read) {
		error = bp->b_error = nfsread(bp->b_vp, bp->b_un.b_addr,
		    (u_int)dbtob(bp->b_blkno), bp->b_bcount,
		    &bp->b_resid, rp->r_cred, &va);
		if (!error) {
			if (bp->b_resid) {
				/*
				 * Didn't get it all because we hit EOF,
				 * zero all the memory beyond the EOF.
				 */
				bzero(bp->b_un.b_addr +
				      (bp->b_bcount - bp->b_resid),
				      (u_int)bp->b_resid);
			}
                        if (bp->b_resid == bp->b_bcount &&
                            dbtob(bp->b_blkno) >= rp->r_size) {
                                /*
                                 * We didn't read anything at all as we are
                                 * past EOF.  Return an error indicator back
                                 */
				error = NFS_EOF;
			}
		}
	} else {
		/*
		 * If the write fails and it was asynchronous
		 * all future writes will get an error.
		 */
		if (rp->r_error == 0) {
			count =  (bp->b_flags & B_SWAP) ? bp->b_bcount : 
			    MIN(bp->b_bcount, rp->r_size - dbtob(bp->b_blkno));
			if (count < 0) {
				panic("do_bio: write count < 0");
			}
			error = bp->b_error = nfswrite(bp->b_vp,
			    bp->b_un.b_addr, (u_int)dbtob(bp->b_blkno),
			    count, rp->r_cred);
#ifdef RISCOS
                        if ((bp->b_flags & B_ASYNC) && bp->b_error) {
                                rp->r_error = bp->b_error;
			}
#endif
		} else
			error = bp->b_error = rp->r_error;
	}

	if (error != 0 && error != NFS_EOF) {
		bp->b_flags |= B_ERROR;
	}

	/*
	 * Call iodone() to free the bp and pages.
	 */
	iodone(bp);
	if (error == 0 && read) {
#ifdef RISCOS
	        if (!CACHE_VALID(rp, va.va_mtime)) {
		/*
		 * Cache purging MUST be done in nfs layer to avoid sleep
		 * deadlocks with the buffers examined by blkflush.
		 * Note: This modification ASSUMES NO sleep can occur anywhere
		 * back up the return stack to the nfs read or the nfs caches
		 * will not get properly flushed.
		 */
		  rp->r_flags |= RPURGECACHE;
		}
		nfs_attrcache_va(rtov(rp), &va);
#else
		nfs_cache_check(rtov(rp), va.va_mtime);
		nfs_attrcache_va(rtov(rp), &va);
#endif
	}

#ifdef NFSDEBUG
	dprint(nfsdebug, 5, "do_bio: error %d, bp %x B_READ %x B_ASYNC %d\n",
	    error, bp, read, async);
#endif
	return (error);
}


nfs_noop()
{
	return (EREMOTE);
}

/*
 * Record-locking requests are passed to the local Lock-Manager daemon.
 */
int
nfs_lockctl(vp, ld, cmd, cred, clid)
	struct vnode *vp;
	struct flock *ld;
	int cmd;
	struct ucred *cred;
	int clid;
{
	struct vfs *vfsp;
	struct mntinfo *mi;
	lockhandle_t lh;

#ifndef lint
	if (sizeof (lh.lh_id) != sizeof (fhandle_t))
		panic("fhandle and lockhandle-id are not the same size!");
#endif
	/*
	 * If we are setting a lock mark the vnode VNOCACHE so the page
	 * cache does not give inconsistent results on locked files shared
	 * between clients.  The VNOCACHE flag is never turned off as long
	 * as the vnode is active because it is hard to figure out when the
	 * last lock is gone.
	 * XXX - what if some already has the vnode mapped in?
	 */
	if (((vp->v_flag & VNOCACHE) == 0) &&
	    (ld->l_type != F_UNLCK) && (cmd != F_GETLK)) {
		vp->v_flag |= VNOCACHE;
		binvalfree(vp);
	}
	lh.lh_vp = vp;
	lh.lh_servername = vtomi(vp)->mi_hostname;
	bcopy((caddr_t)vtofh(vp), (caddr_t)&lh.lh_id, sizeof (fhandle_t));

	/* Check if mount options selected interruptable operations */
	vfsp = vp->v_vfsp;
	mi = vftomi(vfsp);
	lh.interruptable = (mi->mi_hard && mi->mi_int) ? TRUE : FALSE;
	return (klm_lockctl(&lh, ld, cmd, cred, clid));
}
int
nfs_cmp(vp1, vp2)
	struct vnode *vp1, *vp2;
{

	return (vp1 == vp2);
}

/*ARGSUSED*/
int
nfs_realvp(vp, vpp)
	struct vnode *vp;
	struct vnode **vpp;
{

	return (EINVAL);
}
sync_vp(vp)
	struct vnode *vp;
{
	if (vtor(vp)->r_flags & RDIRTY) {
		flush_vp(vp);
	}
}

flush_vp(vp)
	struct vnode *vp;
{
	register struct rnode *rp;
	register off_t offset;
	register int blksize;

	rp = vtor(vp);
	bflush(vp);	/* start delayed writes */
	blksize = vtoblksz(vp);
	for (offset = 0; offset < rp->r_size; offset += blksize) {
		blkflush(vp, (daddr_t)(offset / DEV_BSIZE), (long)blksize);
	}
	rp->r_flags &= ~RDIRTY;
}

/*
 * Convert from file system blocks to device blocks
 */
int
nfs_bmap(vp, bn, vpp, bnp)
	struct vnode *vp;	/* file's vnode */
	daddr_t bn;		/* fs block number */
	struct vnode **vpp;	/* RETURN vp of device */
	daddr_t *bnp;		/* RETURN device block number */
{
	int bsize;		/* server's block size in bytes */

#ifdef NFSDEBUG
	dprint(nfsdebug, 4, "nfs_bmap %s %x blk %d\n",
	    vtomi(vp)->mi_hostname, vp, bn);
#endif
	if (vpp)
		*vpp = vp;
	if (bnp) {
		bsize = vtoblksz(vp);
		*bnp = bn * (bsize / DEV_BSIZE);
	}
	return (0);
}

