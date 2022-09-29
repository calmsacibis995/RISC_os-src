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
#ident	"$Header: authdes_prot.c,v 1.2.1.2 90/05/07 20:56:18 wje Exp $"
/*
 * @(#)authdes_prot.c 1.3 88/07/27 4.0NFSSRC SMI
 *
 * authdes_prot.c, XDR routines for DES authentication
 */

#ifdef KERNEL
#include "../rpc/types.h"
#include "../rpc/xdr.h"
#include "../rpc/auth.h"
#include "../rpc/auth_des.h"
#else
#include <rpc/types.h>
#include <rpc/xdr.h>
#include <rpc/auth.h>
#include <rpc/auth_des.h>
#endif

#define ATTEMPT(xdr_op) if (!(xdr_op)) return (FALSE)

bool_t
xdr_authdes_cred(xdrs, cred)
	XDR *xdrs;
	struct authdes_cred *cred;
{
	/*
	 * Unrolled xdr
	 */
	ATTEMPT(xdr_enum(xdrs, (enum_t *)&cred->adc_namekind));
	switch (cred->adc_namekind) {
	case ADN_FULLNAME:
		ATTEMPT(xdr_string(xdrs, &cred->adc_fullname.name, MAXNETNAMELEN));
		ATTEMPT(xdr_opaque(xdrs, (caddr_t)&cred->adc_fullname.key, sizeof(des_block)));
		ATTEMPT(xdr_opaque(xdrs, (caddr_t)&cred->adc_fullname.window, sizeof(cred->adc_fullname.window)));
		return (TRUE);
	case ADN_NICKNAME:
		ATTEMPT(xdr_opaque(xdrs, (caddr_t)&cred->adc_nickname, sizeof(cred->adc_nickname)));
		return (TRUE);
	default:
		return (FALSE);
	}
}


bool_t
xdr_authdes_verf(xdrs, verf)
	register XDR *xdrs;
	register struct authdes_verf *verf;	
{
	/*
 	 * Unrolled xdr
 	 */
	ATTEMPT(xdr_opaque(xdrs, (caddr_t)&verf->adv_xtimestamp, sizeof(des_block)));
	ATTEMPT(xdr_opaque(xdrs, (caddr_t)&verf->adv_int_u, sizeof(verf->adv_int_u)));
	return (TRUE);
}
