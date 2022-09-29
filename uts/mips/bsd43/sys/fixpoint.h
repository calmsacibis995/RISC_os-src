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
/* $Header: fixpoint.h,v 1.6.3.2 90/05/10 04:51:46 wje Exp $ */
#ifndef BSD43_
#    include <bsd43/bsd43_.h>
#endif BSD43_


/*
 * Fix-point arithmetic package
 */

/*
 * Basic fix-point types
 */
/*
 * TODO: should probably move this over to types.h so that avenrun is
 * not defined in vm_sched.c
 */
typedef	int 		bsd43_(fix);
typedef	unsigned int	bsd43_(ufix);

/*
 * Number of fraction bits.
 */
#define BSD43_FBITS		8

/*
 * Conversion to fix-point representation
 * works with int, float, double, char, ....
 */
#define	BSD43_TO_FIX(x)	((bsd43_(fix))((x)*(1<<BSD43_FBITS)))

/*
 * Conversion from fix-point to various integer datatypes
 */
#define	BSD43_FIX_TO_SHORT(x)		((short)((x)>>BSD43_FBITS))
#define	BSD43_FIX_TO_INT(x)		((int)((x)>>BSD43_FBITS))

/*
 * Conversion from fix-point to double
 */
#define	BSD43_FIX_TO_DBL(x)	(((double)(x))/(1<<BSD43_FBITS))

/*
 * Multiplication/division of 2 fix-point values
 */
#define	BSD43_MUL_2FIX(x, y)	(((x)*(y))>>BSD43_FBITS)
#define	BSD43_DIV_2FIX(x, y)	(((x)<<BSD43_FBITS)/(y))

/*----- NUMIPS: STRIP MACRO PREFIXES (BEGIN) ---------------------------------*/
/* 
 * Strip off "BSD43_" and "bsd43_" for use with standard BSD code.
 * (GENERATED AUTOMATICALLY -- DON'T EDIT BY HAND)
 */
#ifdef SYSTYPE_BSD43
#   define DIV_2FIX BSD43_DIV_2FIX
#   define FBITS BSD43_FBITS
#   define FIX_TO_DBL BSD43_FIX_TO_DBL
#   define FIX_TO_INT BSD43_FIX_TO_INT
#   define FIX_TO_SHORT BSD43_FIX_TO_SHORT
#   define MUL_2FIX BSD43_MUL_2FIX
#   define TO_FIX BSD43_TO_FIX
#endif SYSTYPE_BSD43
/*----- NUMIPS: STRIP MACRO PREFIXES (END) -----------------------------------*/


