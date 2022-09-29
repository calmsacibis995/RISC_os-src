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
/* $Header: vmparam.h,v 1.6.3.2 90/05/10 05:00:31 wje Exp $ */
/*
 * Copyright (c) 1982, 1986 Regents of the University of California.
 * All rights reserved.  The Berkeley software License Agreement
 * specifies the terms and conditions for redistribution.
 *
 *	@(#)vmparam.h	7.1 (Berkeley) 6/4/86
 */
/*----- COPYRIGHT (END) ------------------------------------------------------*/

#ifndef BSD43_
#    include <bsd43/bsd43_.h>
#endif BSD43_


/*
 * Machine dependent constants
 */
#ifdef KERNEL
#include "../machine/vmparam.h"
#else
#include <bsd43/machine/vmparam.h>
#endif

#ifdef vax
#if defined(KERNEL) && !defined(LOCORE)
int	bsd43_(klseql);
int	bsd43_(klsdist);
int	bsd43_(klin);
int	bsd43_(kltxt);
int	bsd43_(klout);
#endif
#endif vax

#ifdef mips
#if defined(KERNEL) && defined(LANGUAGE_C)
extern int	bsd43_(klseql);
extern int	bsd43_(klsdist);
extern int	bsd43_(klin);
extern int	bsd43_(kltxt);
extern int	bsd43_(klout);
#endif
#endif mips

/*----- NUMIPS: STRIP MACRO PREFIXES (BEGIN) ---------------------------------*/
/* 
 * Strip off "BSD43_" and "bsd43_" for use with standard BSD code.
 * (GENERATED AUTOMATICALLY -- DON'T EDIT BY HAND)
 */
#ifdef SYSTYPE_BSD43
#endif SYSTYPE_BSD43
/*----- NUMIPS: STRIP MACRO PREFIXES (END) -----------------------------------*/


