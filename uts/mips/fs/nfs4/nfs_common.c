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
#ident	"$Header: nfs_common.c,v 1.2.1.4 90/05/10 05:02:41 wje Exp $"
/*
 * @(#)nfs_common.c 1.2 88/05/23 NFSSRC4.0 from 1.2 88/02/08 SMI
 */
#include "sys/errno.h"
#include "sys/param.h"
#include "sys/socket.h"
#include "bsd43/sys/stat.h"
#include "bsd/sys/time.h"
#include "sys/vfs.h"
#include "sys/vnode.h"
#include "../rpc/types.h"
#include "bsd/netinet/in.h"
#include "bsd/net/if.h"
#include "sys/fs/nfs.h"

#ifdef NFSDEBUG
int nfsdebug = 2;
#endif

/*
 * Returns the Unix error associated with the nfsstat number passed to
 * this procedure.
 * Note: For the most part the "enum nffsstat" and System V errno.h files
 * are equivalent numbers for a major subset of the NFS errors listed in
 * the enumerated type; BUT this is not a strick 1-for-1 mapping in all cases.
 */
int nfs_geterrno(nfs_status)
enum nfsstat nfs_status;
{
int error;

 switch (nfs_status)
 { /* convert the cases that aren't a trivial sign conversion */

 case NFSERR_OPNOTSUPP:
  error = EOPNOTSUPP;
  break;

 case NFSERR_NAMETOOLONG:
  error = ENAMETOOLONG;
  break;

 case NFSERR_NOTEMPTY:
  error = ENOTEMPTY;
  break;

 case NFSERR_DQUOT:
  error = EDQUOT;
  break;

 case NFSERR_STALE:
  error = ESTALE;
  break;

 case NFSERR_WFLUSH:
  error = ENFSWFLUSH;
  break;

 default:
  error = (int)nfs_status;
  break;
 }

return error;
}

/*
 * Returns the NFS error associated with the unix error number passed
 * into this procedure.
 * Note: For the most part the "enum nffsstat" and System V errno.h files
 * are equivalent numbers for a major subset of the NFS errors listed in
 * the enumerated type; BUT this is not a strick 1-for-1 mapping in all cases.
 */
enum nfsstat nfs_puterrno(error)
int error;
{
enum nfsstat nfs_status;

 switch (error)
 { /* convert the cases that aren't a trivial assignment conversion */

 case EOPNOTSUPP:
  nfs_status = NFSERR_OPNOTSUPP;
  break;

 case ENAMETOOLONG:
  nfs_status = NFSERR_NAMETOOLONG;
  break;

 case ENOTEMPTY:
  nfs_status = NFSERR_NOTEMPTY;
  break;

 case EDQUOT:
  nfs_status = NFSERR_DQUOT;
  break;

 case ESTALE:
  nfs_status = NFSERR_STALE;
  break;

 case ENFSWFLUSH:
  nfs_status = NFSERR_WFLUSH;
  break;

 default:
  nfs_status = (enum nfsstat)error;
  break;
 }

return nfs_status;
}

/*
 * Returns the prefered transfer size in bytes based on
 * what network interfaces are available.
 */
nfstsize()
{
	register struct ifnet *ifp;

	for (ifp = ifnet; ifp; ifp = ifp->if_next) {
		if (ifp->if_name[0] == 'e' && ifp->if_name[1] == 'c') {
#ifdef NFSDEBUG
			dprint(nfsdebug, 3, "nfstsize: %d\n", ECTSIZE);
#endif
			return (ECTSIZE);
		}
	}
#ifdef NFSDEBUG
	dprint(nfsdebug, 3, "nfstsize: %d\n", IETSIZE);
#endif
	return (IETSIZE);
}

/*
 * Utilities used by both client and server.
 */

vattr_to_nattr(vap, na)
	register struct vattr *vap;
	register struct nfsfattr *na;
{

	na->na_type = (enum nfsftype)vap->va_type;
	na->na_mode = vap->va_mode;
	na->na_uid = (int)vap->va_uid;
	na->na_gid = (int)vap->va_gid;
	na->na_fsid = vap->va_fsid;
	na->na_nodeid = vap->va_nodeid;
	na->na_nlink = vap->va_nlink;
	na->na_size = vap->va_size;
	na->na_atime.tv_sec  = vap->va_atime.tv_sec;
	na->na_atime.tv_usec = vap->va_atime.tv_usec;
	na->na_mtime.tv_sec  = vap->va_mtime.tv_sec;
	na->na_mtime.tv_usec = vap->va_mtime.tv_usec;
	na->na_ctime.tv_sec  = vap->va_ctime.tv_sec;
	na->na_ctime.tv_usec = vap->va_ctime.tv_usec;
	na->na_rdev = vap->va_rdev;
	na->na_blocks = vap->va_blocks;
	na->na_blocksize = vap->va_blocksize;

	/*
	 * This bit of ugliness is a *TEMPORARY* hack to preserve the
	 * over-the-wire protocols for named-pipe vnodes.  It remaps the
	 * VFIFO type to the special over-the-wire type. (see note in nfs.h)
	 *
	 * BUYER BEWARE:
	 *  If you are porting the NFS to a non-SUN server, you probably
	 *  don't want to include the following block of code.  The
	 *  over-the-wire special file types will be changing with the
	 *  NFS Protocol Revision.
	 */
	if (vap->va_type == VFIFO)
		NA_SETFIFO(na);
}

#ifdef NFSDEBUG
/*
 * Utilities used by both client and server
 * Standard levels:
 * 0) no debugging
 * 1) hard failures
 * 2) soft failures
 * 3) current test software
 * 4) main procedure entry points
 * 5) main procedure exit points
 * 6) utility procedure entry points
 * 7) utility procedure exit points
 * 8) obscure procedure entry points
 * 9) obscure procedure exit points
 * 10) random stuff
 * 11) all <= 1
 * 12) all <= 2
 * 13) all <= 3
 * ...
 */

/*VARARGS2*/
dprint(var, level, str, a1, a2, a3, a4, a5, a6, a7, a8, a9)
	int var;
	int level;
	char *str;
	int a1, a2, a3, a4, a5, a6, a7, a8, a9;
{
	if (var == level || (var > 10 && (var - 10) >= level))
		printf(str, a1, a2, a3, a4, a5, a6, a7, a8, a9);
}
#endif
