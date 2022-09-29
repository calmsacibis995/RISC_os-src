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
/* $Header: mman.h,v 1.6.3.2 90/05/10 04:52:29 wje Exp $ */
/*
 * Copyright (c) 1982, 1986 Regents of the University of California.
 * All rights reserved.  The Berkeley software License Agreement
 * specifies the terms and conditions for redistribution.
 *
 *	@(#)mman.h	7.1 (Berkeley) 6/4/86
 */
/*----- COPYRIGHT (END) ------------------------------------------------------*/

#ifndef BSD43_
#    include <bsd43/bsd43_.h>
#endif BSD43_


/* protections are chosen from these bits, or-ed together */
#define	BSD43_PROT_READ	0x1		/* pages can be read */
#define	BSD43_PROT_WRITE	0x2		/* pages can be written */
#define	BSD43_PROT_EXEC	0x4		/* pages can be executed */

/* sharing types: choose either SHARED or PRIVATE */
#define	BSD43_MAP_SHARED	1		/* share changes */
#define	BSD43_MAP_PRIVATE	2		/* changes are private */

/* advice to madvise */
#define	BSD43_MADV_NORMAL	0		/* no further special treatment */
#define	BSD43_MADV_RANDOM	1		/* expect random page references */
#define	BSD43_MADV_SEQUENTIAL	2		/* expect sequential page references */
#define	BSD43_MADV_WILLNEED	3		/* will need these pages */
#define	BSD43_MADV_DONTNEED	4		/* dont need these pages */

/*----- NUMIPS: STRIP MACRO PREFIXES (BEGIN) ---------------------------------*/
/* 
 * Strip off "BSD43_" and "bsd43_" for use with standard BSD code.
 * (GENERATED AUTOMATICALLY -- DON'T EDIT BY HAND)
 */
#ifdef SYSTYPE_BSD43
#   define MADV_DONTNEED BSD43_MADV_DONTNEED
#   define MADV_NORMAL BSD43_MADV_NORMAL
#   define MADV_RANDOM BSD43_MADV_RANDOM
#   define MADV_SEQUENTIAL BSD43_MADV_SEQUENTIAL
#   define MADV_WILLNEED BSD43_MADV_WILLNEED
#   define MAP_PRIVATE BSD43_MAP_PRIVATE
#   define MAP_SHARED BSD43_MAP_SHARED
#   define PROT_EXEC BSD43_PROT_EXEC
#   define PROT_READ BSD43_PROT_READ
#   define PROT_WRITE BSD43_PROT_WRITE
#endif SYSTYPE_BSD43
/*----- NUMIPS: STRIP MACRO PREFIXES (END) -----------------------------------*/


