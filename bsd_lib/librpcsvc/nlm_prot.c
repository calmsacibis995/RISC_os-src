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
#ident	"$Header: nlm_prot.c,v 1.2.1.2 90/05/09 14:44:34 wje Exp $"
#include <rpc/rpc.h>
#include "nlm_prot.h"


bool_t
xdr_nlm_stats(xdrs, objp)
	XDR *xdrs;
	nlm_stats *objp;
{
	if (!xdr_enum(xdrs, (enum_t *)objp)) {
		return (FALSE);
	}
	return (TRUE);
}




bool_t
xdr_nlm_holder(xdrs, objp)
	XDR *xdrs;
	nlm_holder *objp;
{
	if (!xdr_bool(xdrs, &objp->exclusive)) {
		return (FALSE);
	}
	if (!xdr_int(xdrs, &objp->svid)) {
		return (FALSE);
	}
	if (!xdr_netobj(xdrs, &objp->oh)) {
		return (FALSE);
	}
	if (!xdr_u_int(xdrs, &objp->l_offset)) {
		return (FALSE);
	}
	if (!xdr_u_int(xdrs, &objp->l_len)) {
		return (FALSE);
	}
	return (TRUE);
}




bool_t
xdr_nlm_testrply(xdrs, objp)
	XDR *xdrs;
	nlm_testrply *objp;
{
	if (!xdr_nlm_stats(xdrs, &objp->stat)) {
		return (FALSE);
	}
	switch (objp->stat) {
	case nlm_denied:
		if (!xdr_nlm_holder(xdrs, &objp->nlm_testrply_u.holder)) {
			return (FALSE);
		}
		break;
	}
	return (TRUE);
}




bool_t
xdr_nlm_stat(xdrs, objp)
	XDR *xdrs;
	nlm_stat *objp;
{
	if (!xdr_nlm_stats(xdrs, &objp->stat)) {
		return (FALSE);
	}
	return (TRUE);
}




bool_t
xdr_nlm_res(xdrs, objp)
	XDR *xdrs;
	nlm_res *objp;
{
	if (!xdr_netobj(xdrs, &objp->cookie)) {
		return (FALSE);
	}
	if (!xdr_nlm_stat(xdrs, &objp->stat)) {
		return (FALSE);
	}
	return (TRUE);
}




bool_t
xdr_nlm_testres(xdrs, objp)
	XDR *xdrs;
	nlm_testres *objp;
{
	if (!xdr_netobj(xdrs, &objp->cookie)) {
		return (FALSE);
	}
	if (!xdr_nlm_testrply(xdrs, &objp->stat)) {
		return (FALSE);
	}
	return (TRUE);
}




bool_t
xdr_nlm_lock(xdrs, objp)
	XDR *xdrs;
	nlm_lock *objp;
{
	if (!xdr_string(xdrs, &objp->caller_name, LM_MAXSTRLEN)) {
		return (FALSE);
	}
	if (!xdr_netobj(xdrs, &objp->fh)) {
		return (FALSE);
	}
	if (!xdr_netobj(xdrs, &objp->oh)) {
		return (FALSE);
	}
	if (!xdr_int(xdrs, &objp->svid)) {
		return (FALSE);
	}
	if (!xdr_u_int(xdrs, &objp->l_offset)) {
		return (FALSE);
	}
	if (!xdr_u_int(xdrs, &objp->l_len)) {
		return (FALSE);
	}
	return (TRUE);
}




bool_t
xdr_nlm_lockargs(xdrs, objp)
	XDR *xdrs;
	nlm_lockargs *objp;
{
	if (!xdr_netobj(xdrs, &objp->cookie)) {
		return (FALSE);
	}
	if (!xdr_bool(xdrs, &objp->block)) {
		return (FALSE);
	}
	if (!xdr_bool(xdrs, &objp->exclusive)) {
		return (FALSE);
	}
	if (!xdr_nlm_lock(xdrs, &objp->alock)) {
		return (FALSE);
	}
	if (!xdr_bool(xdrs, &objp->reclaim)) {
		return (FALSE);
	}
	if (!xdr_int(xdrs, &objp->state)) {
		return (FALSE);
	}
	return (TRUE);
}




bool_t
xdr_nlm_cancargs(xdrs, objp)
	XDR *xdrs;
	nlm_cancargs *objp;
{
	if (!xdr_netobj(xdrs, &objp->cookie)) {
		return (FALSE);
	}
	if (!xdr_bool(xdrs, &objp->block)) {
		return (FALSE);
	}
	if (!xdr_bool(xdrs, &objp->exclusive)) {
		return (FALSE);
	}
	if (!xdr_nlm_lock(xdrs, &objp->alock)) {
		return (FALSE);
	}
	return (TRUE);
}




bool_t
xdr_nlm_testargs(xdrs, objp)
	XDR *xdrs;
	nlm_testargs *objp;
{
	if (!xdr_netobj(xdrs, &objp->cookie)) {
		return (FALSE);
	}
	if (!xdr_bool(xdrs, &objp->exclusive)) {
		return (FALSE);
	}
	if (!xdr_nlm_lock(xdrs, &objp->alock)) {
		return (FALSE);
	}
	return (TRUE);
}




bool_t
xdr_nlm_unlockargs(xdrs, objp)
	XDR *xdrs;
	nlm_unlockargs *objp;
{
	if (!xdr_netobj(xdrs, &objp->cookie)) {
		return (FALSE);
	}
	if (!xdr_nlm_lock(xdrs, &objp->alock)) {
		return (FALSE);
	}
	return (TRUE);
}




bool_t
xdr_fsh_mode(xdrs, objp)
	XDR *xdrs;
	fsh_mode *objp;
{
	if (!xdr_enum(xdrs, (enum_t *)objp)) {
		return (FALSE);
	}
	return (TRUE);
}




bool_t
xdr_fsh_access(xdrs, objp)
	XDR *xdrs;
	fsh_access *objp;
{
	if (!xdr_enum(xdrs, (enum_t *)objp)) {
		return (FALSE);
	}
	return (TRUE);
}




bool_t
xdr_nlm_share(xdrs, objp)
	XDR *xdrs;
	nlm_share *objp;
{
	if (!xdr_string(xdrs, &objp->caller_name, LM_MAXSTRLEN)) {
		return (FALSE);
	}
	if (!xdr_netobj(xdrs, &objp->fh)) {
		return (FALSE);
	}
	if (!xdr_netobj(xdrs, &objp->oh)) {
		return (FALSE);
	}
	if (!xdr_fsh_mode(xdrs, &objp->mode)) {
		return (FALSE);
	}
	if (!xdr_fsh_access(xdrs, &objp->access)) {
		return (FALSE);
	}
	return (TRUE);
}




bool_t
xdr_nlm_shareargs(xdrs, objp)
	XDR *xdrs;
	nlm_shareargs *objp;
{
	if (!xdr_netobj(xdrs, &objp->cookie)) {
		return (FALSE);
	}
	if (!xdr_nlm_share(xdrs, &objp->share)) {
		return (FALSE);
	}
	if (!xdr_bool(xdrs, &objp->reclaim)) {
		return (FALSE);
	}
	return (TRUE);
}




bool_t
xdr_nlm_shareres(xdrs, objp)
	XDR *xdrs;
	nlm_shareres *objp;
{
	if (!xdr_netobj(xdrs, &objp->cookie)) {
		return (FALSE);
	}
	if (!xdr_nlm_stats(xdrs, &objp->stat)) {
		return (FALSE);
	}
	if (!xdr_int(xdrs, &objp->sequence)) {
		return (FALSE);
	}
	return (TRUE);
}




bool_t
xdr_nlm_notify(xdrs, objp)
	XDR *xdrs;
	nlm_notify *objp;
{
	if (!xdr_string(xdrs, &objp->name, MAXNAMELEN)) {
		return (FALSE);
	}
	if (!xdr_long(xdrs, &objp->state)) {
		return (FALSE);
	}
	return (TRUE);
}


