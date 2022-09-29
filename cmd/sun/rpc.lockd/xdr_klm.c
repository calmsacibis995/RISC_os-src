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
#ident	"$Header: xdr_klm.c,v 1.1.1.2.1.1.1.2 90/11/17 12:00:29 beacker Exp $"
/* @(#)nfs.cmds:nfs/lockd/xdr_klm.c	1.1 */
#include <rpc/rpc.h>
#include "klm_prot.h"


bool_t
xdr_klm_stats(xdrs, objp)
	XDR *xdrs;
	klm_stats *objp;
{
	if (!xdr_enum(xdrs, (enum_t *)objp)) {
		return (FALSE);
	}
	return (TRUE);
}

bool_t
xdr_klm_lock(xdrs, objp)
	XDR *xdrs;
	klm_lock *objp;
{
	if (!xdr_string(xdrs, &objp->server_name, LM_MAXSTRLEN)) {
		return (FALSE);
	}
	if (!xdr_netobj(xdrs, &objp->fh)) {
		return (FALSE);
	}
	if (!xdr_int(xdrs, &objp->base)) {
		return (FALSE);
	}
	if (!xdr_int(xdrs, &objp->length)) {
		return (FALSE);
	}
	if (!xdr_int(xdrs, &objp->type)) {
		return (FALSE);
	}
	if (!xdr_int(xdrs, &objp->granted)) {
		return (FALSE);
	}
	if (!xdr_int(xdrs, &objp->color)) {
		return (FALSE);
	}
	if (!xdr_int(xdrs, &objp->LockID)) {
		return (FALSE);
	}
	if (!xdr_int(xdrs, &objp->pid)) {
		return (FALSE);
	}
	if (!xdr_int(xdrs, &objp->class)) {
		return (FALSE);
	}
	if (!xdr_long(xdrs, &objp->rsys)) {
		return (FALSE);
	}
	if (!xdr_long(xdrs, &objp->rpid)) {
		return (FALSE);
	}
	return (TRUE);
}

bool_t
xdr_klm_holder(xdrs, objp)
	XDR *xdrs;
	klm_holder *objp;
{
	if (!xdr_bool(xdrs, &objp->exclusive)) {
		return (FALSE);
	}
	if (!xdr_int(xdrs, &objp->base)) {
		return (FALSE);
	}
	if (!xdr_int(xdrs, &objp->length)) {
		return (FALSE);
	}
	if (!xdr_int(xdrs, &objp->type)) {
		return (FALSE);
	}
	if (!xdr_int(xdrs, &objp->granted)) {
		return (FALSE);
	}
	if (!xdr_int(xdrs, &objp->color)) {
		return (FALSE);
	}
	if (!xdr_int(xdrs, &objp->LockID)) {
		return (FALSE);
	}
	if (!xdr_int(xdrs, &objp->pid)) {
		return (FALSE);
	}
	if (!xdr_int(xdrs, &objp->class)) {
		return (FALSE);
	}
	if (!xdr_long(xdrs, &objp->rsys)) {
		return (FALSE);
	}
	if (!xdr_long(xdrs, &objp->rpid)) {
		return (FALSE);
	}
	return (TRUE);
}

bool_t
xdr_klm_stat(xdrs, objp)
	XDR *xdrs;
	klm_stat *objp;
{
	if (!xdr_klm_stats(xdrs, &objp->stat)) {
		return (FALSE);
	}
	return (TRUE);
}

bool_t
xdr_klm_testrply(xdrs, objp)
	XDR *xdrs;
	klm_testrply *objp;
{
	if (!xdr_klm_stats(xdrs, &objp->stat)) {
		return (FALSE);
	}
	switch (objp->stat) {
	case klm_denied:
		if (!xdr_klm_holder(xdrs, &objp->klm_testrply_u.holder)) {
			return (FALSE);
		}
		break;
	}
	return (TRUE);
}

bool_t
xdr_klm_lockargs(xdrs, objp)
	XDR *xdrs;
	klm_lockargs *objp;
{
	if (!xdr_bool(xdrs, &objp->block)) {
		return (FALSE);
	}
	if (!xdr_bool(xdrs, &objp->exclusive)) {
		return (FALSE);
	}
	if (!xdr_klm_lock(xdrs, &objp->alock)) {
		return (FALSE);
	}
	return (TRUE);
}

bool_t
xdr_klm_testargs(xdrs, objp)
	XDR *xdrs;
	klm_testargs *objp;
{
	if (!xdr_bool(xdrs, &objp->exclusive)) {
		return (FALSE);
	}
	if (!xdr_klm_lock(xdrs, &objp->alock)) {
		return (FALSE);
	}
	return (TRUE);
}

bool_t
xdr_klm_unlockargs(xdrs, objp)
	XDR *xdrs;
	klm_unlockargs *objp;
{
	if (!xdr_klm_lock(xdrs, &objp->alock)) {
		return (FALSE);
	}
	return (TRUE);
}
