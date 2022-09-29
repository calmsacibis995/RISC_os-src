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
/* $Header: lockmgr.h,v 1.2.1.2.1.1.1.2 90/11/17 11:49:26 beacker Exp $ */
/*
 * @(#)lockmgr.h 2.1 88/05/24 4.0NFSSRC Copyright 1988 Sun Microsystems, Inc.
 * @(#)lockmgr.h 1.6                    Copyright 1988 Sun Microsystems, Inc.
 *
 * Header file for Kernel<->Network Lock-Manager implementation
 */

/* NOTE: size of a lockhandle-id should track the size of an fhandle */
#define KLM_LHSIZE	32

/* the lockhandle uniquely describes any file in a domain */
typedef struct {
	struct vnode *lh_vp;			/* vnode of file */
	char *lh_servername;			/* file server machine name */
#ifdef RISCOS
	bool_t interruptable;	/* TRUE if fs interruptable mount option */
#endif
	struct {				/* fhandle (sort of) */
		struct __lh_ufsid {
			fsid_t		__lh_fsid;
			struct fid	__lh_fid;
		} __lh_ufs;
#define KLM_LHPAD	(KLM_LHSIZE - sizeof (struct __lh_ufsid))
		char	__lh_pad[KLM_LHPAD];
	} lh_id;
} lockhandle_t;
#define lh_fsid	lh_id.__lh_ufs.__lh_fsid
#define lh_fid	lh_id.__lh_ufs.__lh_fid


/* define 'well-known' information */
#define KLM_PROTO	IPPROTO_UDP

/* define public routines */
int  klm_lockctl();
