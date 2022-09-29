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
#ident	"$Header: rpccommondata.c,v 1.2.1.2 90/05/07 21:00:52 wje Exp $"
#if defined(LIBC_SCCS) && !defined(lint)
static char sccsid[] = 	"@(#)rpccommondata.c	1.2 88/07/27 4.0NFSSRC Copyr 1988 Sun Micro";
#endif

/* 
 * Copyright (c) 1988 by Sun Microsystems, Inc.
 * @(#) from SUN X.X
 */

#include <rpc/rpc.h>
/*
 * This file should only contain common data (global data) that is exported 
 * by public interfaces 
 */
struct opaque_auth _null_auth;
fd_set svc_fdset;
struct rpc_createerr rpc_createerr;
