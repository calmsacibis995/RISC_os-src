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
/* $Header: undef.h,v 1.6.3.2 90/05/10 04:48:27 wje Exp $ */
#ifndef BSD43_
#    include <bsd43/bsd43_.h>
#endif BSD43_


/*
 * This file contains macros and defines that have the
 * same name in System V and BSD 4.2 but different meanings.
 * When it is necessary to include sys/param.h and sysv/param.h
 * you should include this file after each one.  This will prevent
 * anyone from using this symbols until some rational method of 
 * distinguishing which symbol you want to use.
 */
#undef BSD43_SSIZE
#undef BSD43_SINCR
#undef BSD43_NCARGS
#undef BSD43_PSLEP
#undef BSD43_PUSER
#undef BSD43_NBPW
#undef BSD43_USERMODE
#undef BSD43_BASEPRI
#undef bsd43_ctos
#undef bsd43_stoc
#undef bsd43_ctod
#undef bsd43_ctob
#undef bsd43_btoc
#undef bsd43_major
#undef bsd43_minor
#undef bsd43_makedev

/*----- NUMIPS: STRIP MACRO PREFIXES (BEGIN) ---------------------------------*/
/* 
 * Strip off "BSD43_" and "bsd43_" for use with standard BSD code.
 * (GENERATED AUTOMATICALLY -- DON'T EDIT BY HAND)
 */
#ifdef SYSTYPE_BSD43
#endif SYSTYPE_BSD43
/*----- NUMIPS: STRIP MACRO PREFIXES (END) -----------------------------------*/


