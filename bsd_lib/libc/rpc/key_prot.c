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
#ident	"$Header: key_prot.c,v 1.2.1.2 90/05/07 20:58:07 wje Exp $"
/*
 * @(#)key_prot.c 1.3 88/07/27 4.0NFSSRC Copyr 1988 Sun Micro
 * @(#)key_prot.c 1.4 88/02/08 SMI
 */

#ifdef KERNEL
#include "../rpc/rpc.h"
#include "../rpc/key_prot.h"
#else
#include <rpc/rpc.h>
#include <rpc/key_prot.h>
#endif


/* 
 * Compiled from key_prot.x using rpcgen.
 * DO NOT EDIT THIS FILE!
 * This is NOT source code!
 */


bool_t
xdr_keystatus(xdrs, objp)
	XDR *xdrs;
	keystatus *objp;
{
	if (!xdr_enum(xdrs, (enum_t *)objp)) {
		return (FALSE);
	}
	return (TRUE);
}


#ifndef KERNEL


bool_t
xdr_keybuf(xdrs, objp)
	XDR *xdrs;
	keybuf objp;
{
	if (!xdr_opaque(xdrs, objp, HEXKEYBYTES)) {
		return (FALSE);
	}
	return (TRUE);
}


#endif


bool_t
xdr_netnamestr(xdrs, objp)
	XDR *xdrs;
	netnamestr *objp;
{
	if (!xdr_string(xdrs, objp, MAXNETNAMELEN)) {
		return (FALSE);
	}
	return (TRUE);
}




bool_t
xdr_cryptkeyarg(xdrs, objp)
	XDR *xdrs;
	cryptkeyarg *objp;
{
	if (!xdr_netnamestr(xdrs, &objp->remotename)) {
		return (FALSE);
	}
#ifdef RISCOS
	if (!xdr_deskey(xdrs, &objp->deskey)) {
#else
	if (!xdr_des_block(xdrs, &objp->deskey)) {
#endif
		return (FALSE);
	}
	return (TRUE);
}




bool_t
xdr_cryptkeyres(xdrs, objp)
	XDR *xdrs;
	cryptkeyres *objp;
{
	if (!xdr_keystatus(xdrs, &objp->status)) {
		return (FALSE);
	}
	switch (objp->status) {
	case KEY_SUCCESS:
#ifdef RISCOS
		if (!xdr_deskey(xdrs, &objp->cryptkeyres_u.deskey)) {
#else
		if (!xdr_des_block(xdrs, &objp->cryptkeyres_u.deskey)) {
#endif
			return (FALSE);
		}
		break;
	}
	return (TRUE);
}




bool_t
xdr_unixcred(xdrs, objp)
	XDR *xdrs;
	unixcred *objp;
{
	if (!xdr_int(xdrs, &objp->uid)) {
		return (FALSE);
	}
	if (!xdr_int(xdrs, &objp->gid)) {
		return (FALSE);
	}
	if (!xdr_array(xdrs, (char **)&objp->gids.gids_val, (u_int *)&objp->gids.gids_len, MAXGIDS, sizeof(int), xdr_int)) {
		return (FALSE);
	}
	return (TRUE);
}




bool_t
xdr_getcredres(xdrs, objp)
	XDR *xdrs;
	getcredres *objp;
{
	if (!xdr_keystatus(xdrs, &objp->status)) {
		return (FALSE);
	}
	switch (objp->status) {
	case KEY_SUCCESS:
		if (!xdr_unixcred(xdrs, &objp->getcredres_u.cred)) {
			return (FALSE);
		}
		break;
	}
	return (TRUE);
}


