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
/* $Header: types.h,v 1.3.1.2 90/05/07 21:03:24 wje Exp $ */
/*
 * @(#)types.h 1.5  88/05/02 4.0NFSSRC SMI
 * @(#)types.h 1.20 88/02/08 SMI      
 *
 * Rpc additions to <sys/types.h>
 */

#ifndef __TYPES_RPC_HEADER__
#define __TYPES_RPC_HEADER__

#define	bool_t	int
#define	enum_t	int
#define __dontcare__	-1

#ifndef FALSE
#	define	FALSE	(0)
#endif

#ifndef TRUE
#	define	TRUE	(1)
#endif

#ifndef NULL
#	define NULL 0
#endif

#ifndef KERNEL
extern char *malloc();
#define mem_alloc(bsize)	malloc(bsize)
#define mem_free(ptr, bsize)	free(ptr)
#else
#ifndef kmem_alloc
extern char *kmem_alloc();
#endif
#define mem_alloc(bsize)	kmem_alloc((u_int)bsize)
#define mem_free(ptr, bsize)	kmem_free((caddr_t)(ptr), (u_int)(bsize))
#endif

#ifdef KERNEL
#include "sys/types.h"

#ifndef BSD43_SYS_TIME_
#include "bsd/sys/time.h"
#endif 

#else

#ifdef SYSTYPE_BSD43
#include <sys/types.h>
#include <sys/time.h>
#endif
#ifdef SYSTYPE_SYSV
#include <bsd/sys/types.h>
#include <bsd/sys/time.h>
#endif

#endif KERNEL

#endif /* ndef __TYPES_RPC_HEADER__ */
