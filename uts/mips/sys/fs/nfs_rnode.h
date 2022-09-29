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
/* $Header: nfs_rnode.h,v 1.2.1.3 90/05/10 06:17:21 wje Exp $ */

/*      @(#)rnode.h	1.4 88/07/15 NFSSRC4.0 from 1.21 88/02/08 SMI      */
/*	Copyright (C) 1988, Sun Microsystems Inc. */

#ifndef _SYS_FS_NFS_RNODE_
#define _SYS_FS_NFS_RNODE_	1

/*
 * Remote file information structure.
 * The rnode is the "inode" for remote files.  It contains all the
 * information necessary to handle remote file on the client side.
 */
struct rnode {
	struct rnode	*r_freef;	/* free list forward pointer */
	struct rnode	*r_freeb;	/* free list back pointer */
	struct rnode	*r_hash;	/* rnode hash chain */
	struct vnode	r_vnode;	/* vnode for remote file */
	fhandle_t	r_fh;		/* file handle */
	u_short		r_flags;	/* flags, see below */
	short		r_error;	/* async write error */
	daddr_t		r_lastr;	/* last block read (read-ahead) */
	short		r_owner;	/* proc index for locker of rnode */
	short		r_count;	/* number of rnode locks for r_owner */
	struct ucred	*r_cred;	/* current credentials */
	struct ucred	*r_unlcred;	/* unlinked credentials */
	char		*r_unlname;	/* unlinked file name */
	struct vnode	*r_unldvp;	/* parent dir of unlinked file */
	struct vattr	r_attr;		/* cached vnode attributes */
	struct timeval	r_attrtime;	/* time attributes become invalid */
};

#define r_size	r_attr.va_size		/* file size in bytes */

/*
 * Flags
 */
#define	RLOCKED		0x01		/* rnode is in use */
#define	RWANT		0x02		/* someone wants a wakeup */
#define	RATTRVALID	0x04		/* Attributes in the rnode are valid */
#define	REOF		0x08		/* EOF encountered on read */
#define	RDIRTY		0x10		/* dirty pages from write operation */
#define	RPURGECACHE	0x20		/* Purge nfs caches after read complete*/

/*
 * Convert between vnode and rnode
 */
#define	rtov(rp)	(&(rp)->r_vnode)
#define	vtor(vp)	((struct rnode *)((vp)->v_data))
#define	vtofh(vp)	(&(vtor(vp)->r_fh))
#define	rtofh(rp)	(&(rp)->r_fh)

/*
 * Lock and unlock rnodes.
 */
#ifdef SWLOCKS
#define	RLOCK(rp) { \
	while (((rp)->r_flags & RLOCKED) && \
	    (rp)->r_owner != u.u_procp - proc) { \
		(rp)->r_flags |= RWANT; \
		(void) sleep((caddr_t)(rp), PINOD); \
	} \
	(rp)->r_owner = u.u_procp - proc; \
	(rp)->r_count++; \
	(rp)->r_flags |= RLOCKED; \
	if (((rp)->r_vnode.v_flag & VISSWAP) != 0) \
		u.u_procp->p_swlocks++; \
}

#define	RUNLOCK(rp) { \
	if (--(rp)->r_count < 0) \
		panic("RUNLOCK"); \
	if (((rp)->r_vnode.v_flag & VISSWAP) != 0) \
		u.u_procp->p_swlocks--; \
	if ((rp)->r_count == 0) { \
		(rp)->r_flags &= ~RLOCKED; \
		if ((rp)->r_flags & RWANT) { \
			(rp)->r_flags &= ~RWANT; \
			wakeup((caddr_t)(rp)); \
		} \
	} \
}
#else SWLOCKS
#define	RLOCK(rp) { \
	while (((rp)->r_flags & RLOCKED) && \
	    (rp)->r_owner != u.u_procp - proc) { \
		(rp)->r_flags |= RWANT; \
		(void) sleep((caddr_t)(rp), PINOD); \
	} \
	(rp)->r_owner = u.u_procp - proc; \
	(rp)->r_count++; \
	(rp)->r_flags |= RLOCKED; \
}

#define	RUNLOCK(rp) { \
	if (--(rp)->r_count < 0) \
		panic("RUNLOCK"); \
	if ((rp)->r_count == 0) { \
		(rp)->r_flags &= ~RLOCKED; \
		if ((rp)->r_flags & RWANT) { \
			(rp)->r_flags &= ~RWANT; \
			wakeup((caddr_t)(rp)); \
		} \
	} \
}
#endif SWLOCKS

#endif _SYS_FS_NFS_RNODE_
