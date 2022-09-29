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
/* $Header: user.h,v 1.8.1.3 90/05/10 04:58:44 wje Exp $ */
/*
 * Copyright (c) 1982, 1986 Regents of the University of California.
 * All rights reserved.  The Berkeley software License Agreement
 * specifies the terms and conditions for redistribution.
 *
 *	@(#)user.h	7.1 (Berkeley) 6/4/86
 */
/*----- COPYRIGHT (END) ------------------------------------------------------*/

#ifndef BSD43_
#    include <bsd43/bsd43_.h>
#endif BSD43_


#ifdef KERNEL
#include "../machine/pcb.h"
#include "dmap.h"
#include "time.h"
#include "resource.h"
#include "ptrace.h"
#ifdef mips
#include "../machine/debug.h"
#endif mips
#else
#include <bsd43/machine/pcb.h>
#include <bsd43/sys/dmap.h>
#include <bsd43/sys/time.h>
#include <bsd43/sys/resource.h>
#include <bsd43/sys/ptrace.h>
#ifdef mips
#include <bsd43/machine/debug.h>
#endif mips
#endif

#include <sysv/sys/user.h>

/* The following BSD fields are mapped to corresponding fields in sysV */

#define	BSD43_MAXCOMLEN	16		/* <= MAXNAMLEN, >= sizeof(ac_comm) */
#define u_qsave	u_qsav

#define	BSD43_UF_EXCLOSE	UF_EXCLOSE  /* auto-close on exec */
#define	BSD43_UF_MAPPED 	UF_MAPPED   /* mapped from device */
#define BSD43_UF_FDLOCK		UF_FDLOCK   /* file desc locked (SysV style) */

#ifdef	notdef

/* The following fields in old BSD user structure are no longer supported:
 */
   struct	bsd43_(dmap) u_dmap;		/* disk map for data segment */
   struct	bsd43_(dmap) u_smap;		/* disk map for stack segment */
   struct	bsd43_(dmap) u_cdmap, u_csmap;	/* shadows of u_dmap, u_smap, for
   bsd43_(label_t) u_ssave;		/* label variable for swapping */
   bsd43_(size_t)  u_odsize, u_ossize;	/* for (clumsy) expansion swaps */
   time_t	u_outime;		/* user time at last sample */
   int	u_lastfile;		/* high-water mark of u_ofile */
   int	u_XXX[3];
   struct	bsd43_(quota) *u_quota;		/* user's quota structure */
   int	u_qflags;		/* per process quota flags */
 
#endif
 
#ifdef KERNEL
#define	bsd43_crhold(cr)	(cr)->cr_ref++
struct bsd43_(ucred) *bsd43_(crget)();
struct bsd43_(ucred) *bsd43_(crcopy)();
struct bsd43_(ucred) *bsd43_(crdup)();
#endif KERNEL

/* u_eosys values */
#define	BSD43_FULLRESTORE	1
#define	BSD43_RESTARTSYS	2
#define BSD43_NORMALRETURN	3

/* u_error codes */
#ifdef KERNEL
#include "errno.h"
#else
#include <bsd43/errno.h>
#endif

#ifdef KERNEL

#ifdef mips
/*
 * This "declaration" tells front end it can use r0 relative addressing.
 */
#define	bsd43_u	(*(struct bsd43_(user) *)BSD43_UADDR)
/*
 * Since u is strange, use up for debugging purposes.
 */
struct bsd43_(user) *bsd43_(up);

extern char BSD43_(VA_swaputl)[];
#define	bsd43_swaputl	(*(struct bsd43_(user) *)BSD43_(VA_swaputl))

extern char BSD43_(VA_forkutl)[];
#define	bsd43_forkutl	(*(struct bsd43_(user) *)BSD43_(VA_forkutl))

extern char BSD43_(VA_vfutl)[];
#define	bsd43_vfutl	(*(struct bsd43_(user) *)BSD43_(VA_vfutl))
#endif mips
#endif KERNEL



