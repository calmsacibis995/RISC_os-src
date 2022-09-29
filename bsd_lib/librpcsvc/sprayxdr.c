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
#ident	"$Header: sprayxdr.c,v 1.2.1.2 90/05/09 14:48:46 wje Exp $"
#ifndef lint
static char sccsid[] = 	"@(#)sprayxdr.c	1.3 88/05/08 4.0NFSSRC Copyr 1988 Sun Micro";
#endif

/* 
 * Copyright (c) 1988 by Sun Microsystems, Inc.
 * @(#) from SUN 1.7
 */

#include <rpc/rpc.h>
#include <rpcsvc/spray.h>

xdr_sprayarr(xdrsp, arrp)
	XDR *xdrsp;
	struct sprayarr *arrp;
{
	if (!xdr_bytes(xdrsp, (char **) &arrp->data, &arrp->lnth, SPRAYMAX))
		return(0);
	return(1);
}

xdr_spraycumul(xdrsp, cumulp)
	XDR *xdrsp;
	struct spraycumul *cumulp;
{
	if (!xdr_u_int(xdrsp, &cumulp->counter))
		return(0);
	if (!xdr_timeval(xdrsp, &cumulp->clock))
		return(0);
	return(1);
}
