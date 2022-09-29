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
/* $Header: varargs.h,v 1.6.3.2 90/05/10 04:59:02 wje Exp $ */
#ifndef BSD43_
#    include <bsd43/bsd43_.h>
#endif BSD43_


typedef char *bsd43_(va_list);
#define bsd43_va_dcl int va_alist;
#define bsd43_va_start(list) list = (char *) &va_alist
#define bsd43_va_end(list)
#ifdef u370
#define bsd43_va_arg(list, mode) ((mode *)(list = \
	(char *) ((int)list + 2*sizeof(mode) - 1 & -sizeof(mode))))[-1]
#else
#ifdef host_mips
#define bsd43_va_arg(list, mode) ((mode *)(list = \
	(char *) (sizeof(mode) > 4 ? ((int)list + 2*8 - 1) & -8 \
				   : ((int)list + 2*4 - 1) & -4)))[-1]
#else
#define bsd43_va_arg(list, mode) ((mode *)(list += sizeof(mode)))[-1]
#endif
#endif


/*----- NUMIPS: STRIP MACRO PREFIXES (BEGIN) ---------------------------------*/
/* 
 * Strip off "BSD43_" and "bsd43_" for use with standard BSD code.
 * (GENERATED AUTOMATICALLY -- DON'T EDIT BY HAND)
 */
#ifdef SYSTYPE_BSD43
#   define va_arg bsd43_va_arg
#   define va_dcl bsd43_va_dcl
#   define va_end bsd43_va_end
#   define va_start bsd43_va_start
#endif SYSTYPE_BSD43
/*----- NUMIPS: STRIP MACRO PREFIXES (END) -----------------------------------*/


