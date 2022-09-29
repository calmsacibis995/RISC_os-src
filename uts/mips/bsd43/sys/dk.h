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
/* $Header: dk.h,v 1.6.3.2 90/05/10 04:50:19 wje Exp $ */
/*
 * Copyright (c) 1982, 1986 Regents of the University of California.
 * All rights reserved.  The Berkeley software License Agreement
 * specifies the terms and conditions for redistribution.
 *
 *	@(#)dk.h	7.1 (Berkeley) 6/4/86
 */
/*----- COPYRIGHT (END) ------------------------------------------------------*/

#ifndef BSD43_
#    include <bsd43/bsd43_.h>
#endif BSD43_


/*
 * Instrumentation
 */
#define	BSD43_CPUSTATES	4

#define	BSD43_CP_USER		0
#define	BSD43_CP_NICE		1
#define	BSD43_CP_SYS		2
#define	BSD43_CP_IDLE		3

#define	BSD43_DK_NDRIVE	4

#ifdef KERNEL
long	bsd43_(cp_time)[BSD43_CPUSTATES];
int	bsd43_(dk_ndrive);
int	bsd43_(dk_busy);
long	bsd43_(dk_time)[BSD43_DK_NDRIVE];
long	bsd43_(dk_seek)[BSD43_DK_NDRIVE];
long	bsd43_(dk_xfer)[BSD43_DK_NDRIVE];
long	bsd43_(dk_wds)[BSD43_DK_NDRIVE];
#ifdef mips
int	bsd43_(dk_bps)[BSD43_DK_NDRIVE];
int	bsd43_(dk_mspw)[BSD43_DK_NDRIVE];
#else
float	bsd43_(dk_bps)[BSD43_DK_NDRIVE];
float	bsd43_(dk_mspw)[BSD43_DK_NDRIVE];
#endif mips

long	bsd43_(tk_nin);
long	bsd43_(tk_nout);
#endif

/*----- NUMIPS: STRIP MACRO PREFIXES (BEGIN) ---------------------------------*/
/* 
 * Strip off "BSD43_" and "bsd43_" for use with standard BSD code.
 * (GENERATED AUTOMATICALLY -- DON'T EDIT BY HAND)
 */
#ifdef SYSTYPE_BSD43
#   define CPUSTATES BSD43_CPUSTATES
#   define CP_IDLE BSD43_CP_IDLE
#   define CP_NICE BSD43_CP_NICE
#   define CP_SYS BSD43_CP_SYS
#   define CP_USER BSD43_CP_USER
#   define DK_NDRIVE BSD43_DK_NDRIVE
#endif SYSTYPE_BSD43
/*----- NUMIPS: STRIP MACRO PREFIXES (END) -----------------------------------*/


