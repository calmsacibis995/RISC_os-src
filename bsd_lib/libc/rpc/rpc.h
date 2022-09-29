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
/* $Header: rpc.h,v 1.2.1.2 90/05/07 21:00:29 wje Exp $ */
/*
 * @(#)rpc.h 1.1 86/09/24 SMI
 *
 * rpc.h, Just includes the billions of rpc header files necessary to 
 * do remote procedure calling.
 */
#ifndef __RPC_HEADER__
#define __RPC_HEADER__

#ifdef KERNEL
#include "../rpc/types.h"	/* some typedefs */

#ifdef RISCOS
#include "bsd/netinet/in.h"
#else
#include "../netinet/in.h"
#endif RISCOS

/* external data representation interfaces */
#include "../rpc/xdr.h"		/* generic (de)serializer */

/* Client side only authentication */
#include "../rpc/auth.h"	/* generic authenticator (client side) */

/* Client side (mostly) remote procedure call */
#include "../rpc/clnt.h"	/* generic rpc stuff */

/* semi-private protocol headers */
#include "../rpc/rpc_msg.h"	/* protocol for rpc messages */
#include "../rpc/auth_unix.h"	/* protocol for unix style cred */

/* Server side only remote procedure callee */
#include "../rpc/svc.h"		/* service manager and multiplexer */
#include "../rpc/svc_auth.h"	/* service side authenticator */
#else KERNEL

#include <rpc/types.h>		/* some typedefs */
#ifdef SYSTYPE_BSD43
#include <netinet/in.h>
#endif
#ifdef SYSTYPE_SYSV
#include <bsd/netinet/in.h>
#endif

/* external data representation interfaces */
#include <rpc/xdr.h>		/* generic (de)serializer */

/* Client side only authentication */
#include <rpc/auth.h>		/* generic authenticator (client side) */

/* Client side (mostly) remote procedure call */
#include <rpc/clnt.h>		/* generic rpc stuff */

/* semi-private protocol headers */
#include <rpc/rpc_msg.h>	/* protocol for rpc messages */
#include <rpc/auth_unix.h>	/* protocol for unix style cred */

/* Server side only remote procedure callee */
#include <rpc/svc.h>		/* service manager and multiplexer */
#include <rpc/svc_auth.h>	/* service side authenticator */

#endif KERNEL

#endif !__RPC_HEADER__
