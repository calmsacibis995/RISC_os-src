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
#ident	"$Header: mountxdr.c,v 1.2.1.2 90/05/09 14:44:22 wje Exp $"
/*
 * @(#)mountxdr.c 1.4  88/05/08 4.0NFSSRC Copyr 1988 Sun Microsystems, Inc.
 * @(#)mountxdr.c 1.15
 */

#ifdef KERNEL
#include "sys/types.h"
#include "sys/errno.h"
#include "../rpc/rpc.h"
#undef NFSSERVER
#include "bsd/sys/time.h"
#include "sys/fs/nfs.h"         /* original ../nfs/nfs.h */
#include "../rpcsvc/mount.h"

#else
#include <rpc/rpc.h>
#include <errno.h>
#undef NFSSERVER
#include <sys/vfs.h>

#ifdef SYSTYPE_BSD43
#define bsd43_bool_t bool_t
#include <nfs/nfs.h>
#endif
#ifdef SYSTYPE_SYSV
#include <sys/fs/nfs.h>
#endif
#include <rpcsvc/mount.h>
#include <stdio.h>
#define xdr_dev_t xdr_short
#endif

xdr_fhstatus(xdrs, fhsp)
	XDR *xdrs;
	struct fhstatus *fhsp;
{
	if (!xdr_int(xdrs, &fhsp->fhs_status))
		return (FALSE);
	if (fhsp->fhs_status == 0) {
		if (!xdr_fhandle(xdrs, &fhsp->fhs_fh))
			return (FALSE);
	}
	return (TRUE);
}

#ifndef KERNEL

xdr_fhandle(xdrs, fhp)
	XDR *xdrs;
	fhandle_t *fhp;
{
	if (xdr_opaque(xdrs, (char *) fhp, NFS_FHSIZE)) {
		return (TRUE);
	}
	return (FALSE);
}



bool_t
xdr_path(xdrs, pathp)
	XDR *xdrs;
	char **pathp;
{
	if (xdr_string(xdrs, pathp, 1024)) {
		return(TRUE);
	}
	return(FALSE);
}



/* 
 * body of a mountlist
 */
bool_t
xdr_mountbody(xdrs, mlp)
	XDR *xdrs;
	struct mountlist *mlp;
{
	if (!xdr_path(xdrs, &mlp->ml_name))
		return FALSE;
	if (!xdr_path(xdrs, &mlp->ml_path))
		return FALSE;
	return(TRUE);
}

bool_t
xdr_mountlist(xdrs, mlp)
	register XDR *xdrs;
	register struct mountlist **mlp;
{
	/*
	 * more_elements is pre-computed in case the direction is
	 * XDR_ENCODE or XDR_FREE.  more_elements is overwritten by
	 * xdr_bool when the direction is XDR_DECODE.
	 */
	int more_elements;
	register int freeing = (xdrs->x_op == XDR_FREE);
	register struct mountlist **nxt;

	while (TRUE) {
		more_elements = (*mlp != NULL);
		if (! xdr_bool(xdrs, &more_elements))
			return (FALSE);
		if (! more_elements)
			return (TRUE);  /* we are done */
		/*
		 * the unfortunate side effect of non-recursion is that in
		 * the case of freeing we must remember the nxt object
		 * before we free the current object ...
		 */
		if (freeing)
			nxt = &((*mlp)->ml_nxt); 
		if (! xdr_reference(xdrs, (char **) mlp, 
			sizeof(struct mountlist), xdr_mountbody)) 
		{
			return (FALSE);
		}
		mlp = (freeing) ? nxt : &((*mlp)->ml_nxt);
	}
}

/*
 * Strange but true: the boolean that tells if another element
 * in the list is present has already been checked.  We handle the
 * body of this element then check on the next element.  YUK.
 */
bool_t
xdr_groups(xdrs, gr)
	register XDR *xdrs;
	register struct groups *gr;
{
	/*
	 * more_elements is pre-computed in case the direction is
	 * XDR_ENCODE or XDR_FREE.  more_elements is overwritten by
	 * xdr_bool when the direction is XDR_DECODE.
	 */
	int more_elements;

	if (! xdr_path(xdrs, &(gr->g_name)))
		return (FALSE);
	more_elements = (gr->g_next != NULL);
	if (! xdr_bool(xdrs, &more_elements))
		return (FALSE);
	if (! more_elements) {
		gr->g_next = NULL;
		return (TRUE);  /* we are done */
	}
	return (xdr_reference(xdrs, (char **) &(gr->g_next), 
		sizeof(struct groups), xdr_groups));
}

/* 
 * body of a exportlist
 */
bool_t
xdr_exportbody(xdrs, ex)
	XDR *xdrs;
	struct exports *ex;
{
	int more_elements;

	if (!xdr_path(xdrs, &ex->ex_name))
		return FALSE;
	more_elements = (ex->ex_groups != NULL);
	if (! xdr_bool(xdrs, &more_elements))
		return (FALSE);
	if (! more_elements) {
		ex->ex_groups = NULL;
		return (TRUE);  /* we are done */
	}
	if (! xdr_reference(xdrs, (char **) &(ex->ex_groups), 
		sizeof(struct groups), xdr_groups)) 
	{
		return (FALSE);
	}
	return(TRUE);
}


/*
 * Encodes the export list structure "exports" on the
 * wire as:
 * bool_t eol;
 * if (!eol) {
 * 	char *name;
 *	struct groups *groups;
 * }
 * where groups look like:
 * if (!eog) {
 *	char *gname;
 * }
 */
bool_t
xdr_exports(xdrs, exp)
	register XDR *xdrs;
	register struct exports **exp;
{
	/*
	 * more_elements is pre-computed in case the direction is
	 * XDR_ENCODE or XDR_FREE.  more_elements is overwritten by
	 * xdr_bool when the direction is XDR_DECODE.
	 */
	int more_elements;
	register int freeing = (xdrs->x_op == XDR_FREE);
	register struct exports **nxt;

	while (TRUE) {
		more_elements = (*exp != NULL);
		if (! xdr_bool(xdrs, &more_elements))
			return (FALSE);
		if (! more_elements)
			return (TRUE);  /* we are done */
		/*
		 * the unfortunate side effect of non-recursion is that in
		 * the case of freeing we must remember the nxt object
		 * before we free the current object ...
		 */
		if (freeing)
			nxt = &((*exp)->ex_next); 
		if (! xdr_reference(xdrs, (char **) exp, 
			sizeof(struct exports), xdr_exportbody)) 
		{
			return (FALSE);
		}
		exp = (freeing) ? nxt : &((*exp)->ex_next);
	}
}
#endif
