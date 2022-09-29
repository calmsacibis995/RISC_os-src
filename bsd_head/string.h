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
/* $Header: string.h,v 1.2.2.2 90/05/07 20:08:54 wje Exp $ */
#ifndef BSD43_
#    include <bsd43/bsd43_.h>
#endif BSD43_


#include <bsd43/strings.h>

/*
 * these next few are obsolete trash
 */

extern char *bsd43_(strcpyn)();
extern char *bsd43_(strcatn)();
extern int   bsd43_(strcmpn)();

/*
 * and the rest are Sys5 functions supported just so
 * Sys5 progs will compile easily.
 */

extern char *bsd43_(strchr)();
extern char *bsd43_(strrchr)();
extern char *bsd43_(strpbrk)();
extern int bsd43_(strspn)();
extern int bsd43_(strcspn)();
extern char *bsd43_(strtok)();

/*----- NUMIPS: STRIP MACRO PREFIXES (BEGIN) ---------------------------------*/
/* 
 * Strip off "BSD43_" and "bsd43_" for use with standard BSD code.
 * (GENERATED AUTOMATICALLY -- DON'T EDIT BY HAND)
 */
#ifdef SYSTYPE_BSD43
#endif SYSTYPE_BSD43
/*----- NUMIPS: STRIP MACRO PREFIXES (END) -----------------------------------*/


