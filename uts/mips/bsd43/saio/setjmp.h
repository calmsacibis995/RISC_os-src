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
/* $Header: setjmp.h,v 1.6.3.2 90/05/10 04:47:30 wje Exp $ */
#ifndef BSD43_
#    include <bsd43/bsd43_.h>
#endif BSD43_


/*
 * jmp_buf indices
 */
#define	BSD43_JB_PC		0
#define	BSD43_JB_SP		1
#define	BSD43_JB_FP		2
#define	BSD43_JB_S0		3
#define	BSD43_JB_S1		4
#define	BSD43_JB_S2		5
#define	BSD43_JB_S3		6
#define	BSD43_JB_S4		7
#define	BSD43_JB_S5		8
#define	BSD43_JB_S6		9
#define	BSD43_JB_S7		10

#define	BSD43_JB_SIZE		11

#ifndef LOCORE
typedef int bsd43_(jmp_buf)[BSD43_JB_SIZE];	/* caller saved regs, sp, pc */
#endif

/*----- NUMIPS: STRIP MACRO PREFIXES (BEGIN) ---------------------------------*/
/* 
 * Strip off "BSD43_" and "bsd43_" for use with standard BSD code.
 * (GENERATED AUTOMATICALLY -- DON'T EDIT BY HAND)
 */
#ifdef SYSTYPE_BSD43
#   define JB_FP BSD43_JB_FP
#   define JB_PC BSD43_JB_PC
#   define JB_S0 BSD43_JB_S0
#   define JB_S1 BSD43_JB_S1
#   define JB_S2 BSD43_JB_S2
#   define JB_S3 BSD43_JB_S3
#   define JB_S4 BSD43_JB_S4
#   define JB_S5 BSD43_JB_S5
#   define JB_S6 BSD43_JB_S6
#   define JB_S7 BSD43_JB_S7
#   define JB_SIZE BSD43_JB_SIZE
#   define JB_SP BSD43_JB_SP
#endif SYSTYPE_BSD43
/*----- NUMIPS: STRIP MACRO PREFIXES (END) -----------------------------------*/


