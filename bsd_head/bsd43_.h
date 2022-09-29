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
/* $Header: bsd43_.h,v 1.2.2.2 90/05/07 20:07:22 wje Exp $ */
#ifdef SYSTYPE_BSD43
#   define BSD43_(x) x
#   define bsd43_(x) x
#else
#   define BSD43_(x) BSD43_/**/x
#   define bsd43_(x) bsd43_/**/x
#endif
