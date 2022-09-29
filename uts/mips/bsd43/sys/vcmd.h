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
/* $Header: vcmd.h,v 1.7.3.2 90/05/10 04:59:09 wje Exp $ */
/*
 * Copyright (c) 1982, 1986 Regents of the University of California.
 * All rights reserved.  The Berkeley software License Agreement
 * specifies the terms and conditions for redistribution.
 *
 *	@(#)vcmd.h	7.1 (Berkeley) 6/4/86
 */
/*----- COPYRIGHT (END) ------------------------------------------------------*/

#ifndef BSD43_
#    include <bsd43/bsd43_.h>
#endif BSD43_


#ifndef BSD43__IOCTL_
#ifdef KERNEL
#include "ioctl.h"
#else
#include <bsd43/sys/ioctl.h>
#endif
#endif

#define	BSD43_VPRINT		0100
#define	BSD43_VPLOT		0200
#define	BSD43_VPRINTPLOT	0400
#define	BSD43_VPC_TERMCOM	0040
#define	BSD43_VPC_FFCOM	0020
#define	BSD43_VPC_EOTCOM	0010
#define	BSD43_VPC_CLRCOM	0004
#define	BSD43_VPC_RESET	0002

#define	BSD43_VGETSTATE	BSD43__IOR(v, 0, int)
#define	BSD43_VSETSTATE	BSD43__IOW(v, 1, int)

/*----- NUMIPS: STRIP MACRO PREFIXES (BEGIN) ---------------------------------*/
/* 
 * Strip off "BSD43_" and "bsd43_" for use with standard BSD code.
 * (GENERATED AUTOMATICALLY -- DON'T EDIT BY HAND)
 */
#ifdef SYSTYPE_BSD43
#   define VGETSTATE BSD43_VGETSTATE
#   define VPC_CLRCOM BSD43_VPC_CLRCOM
#   define VPC_EOTCOM BSD43_VPC_EOTCOM
#   define VPC_FFCOM BSD43_VPC_FFCOM
#   define VPC_RESET BSD43_VPC_RESET
#   define VPC_TERMCOM BSD43_VPC_TERMCOM
#   define VPLOT BSD43_VPLOT
#   define VPRINT BSD43_VPRINT
#   define VPRINTPLOT BSD43_VPRINTPLOT
#   define VSETSTATE BSD43_VSETSTATE
#endif SYSTYPE_BSD43
/*----- NUMIPS: STRIP MACRO PREFIXES (END) -----------------------------------*/


