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
/* $Header: sysmips.h,v 1.6.3.2 90/05/10 04:56:01 wje Exp $ */
#ifndef BSD43_
#    include <bsd43/bsd43_.h>
#endif BSD43_


/*
 * Defines for MIPS specific system calls.  See ../sys/sysmips.c for more info.
 */

#define BSD43_MIPS_VECTOR_SIZE 0x200
#define BSD43_MIPS_VECTOR_DIVIDER 0x100

/* those that are implemented all or in part in the sysmips() routine */
#define BSD43_MIPS_UNAME	0x000
#define BSD43_MIPS_FPSIGINTR	0x001
#define BSD43_MIPS_FPU	0x002
#define BSD43_MIPS_FIXADE	0x003

/* those that are entirely implemented in a broken out procedure */
#define BSD43_MIPS_KOPT	0x100
#define BSD43_MIPS_HWCONF	0x101
#define BSD43_MIPS_GETRUSAGE	0x102
#define BSD43_MIPS_WAIT3	0x103
#define BSD43_MIPS_CACHEFLUSH	0x104
#define BSD43_MIPS_CACHECTL	0x105

/*----- NUMIPS: STRIP MACRO PREFIXES (BEGIN) ---------------------------------*/
/* 
 * Strip off "BSD43_" and "bsd43_" for use with standard BSD code.
 * (GENERATED AUTOMATICALLY -- DON'T EDIT BY HAND)
 */
#ifdef SYSTYPE_BSD43
#   define MIPS_CACHECTL BSD43_MIPS_CACHECTL
#   define MIPS_CACHEFLUSH BSD43_MIPS_CACHEFLUSH
#   define MIPS_FIXADE BSD43_MIPS_FIXADE
#   define MIPS_FPSIGINTR BSD43_MIPS_FPSIGINTR
#   define MIPS_FPU BSD43_MIPS_FPU
#   define MIPS_GETRUSAGE BSD43_MIPS_GETRUSAGE
#   define MIPS_HWCONF BSD43_MIPS_HWCONF
#   define MIPS_KOPT BSD43_MIPS_KOPT
#   define MIPS_UNAME BSD43_MIPS_UNAME
#   define MIPS_VECTOR_DIVIDER BSD43_MIPS_VECTOR_DIVIDER
#   define MIPS_VECTOR_SIZE BSD43_MIPS_VECTOR_SIZE
#   define MIPS_WAIT3 BSD43_MIPS_WAIT3
#endif SYSTYPE_BSD43
/*----- NUMIPS: STRIP MACRO PREFIXES (END) -----------------------------------*/


