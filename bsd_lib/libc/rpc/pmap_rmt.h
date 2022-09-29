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
/* $Header: pmap_rmt.h,v 1.2.1.2 90/05/07 21:00:23 wje Exp $ */
/*
 * @(#)pmap_rmt.h 1.1 88/03/04 4.0NFSSRC SMI
 * @(#)pmap_rmt.h 1.2 88/02/08 SMI	
 */

/*
 * Structures and XDR routines for parameters to and replies from
 * the portmapper remote-call-service.
 */

struct rmtcallargs {
	u_long prog, vers, proc, arglen;
	caddr_t args_ptr;
	xdrproc_t xdr_args;
};

bool_t xdr_rmtcall_args();

struct rmtcallres {
	u_long *port_ptr;
	u_long resultslen;
	caddr_t results_ptr;
	xdrproc_t xdr_results;
};

bool_t xdr_rmtcallres();
