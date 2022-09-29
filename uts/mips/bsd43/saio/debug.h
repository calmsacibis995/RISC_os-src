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
/* $Header: debug.h,v 1.6.3.2 90/05/10 04:46:02 wje Exp $ */
#ifndef BSD43_
#    include <bsd43/bsd43_.h>
#endif BSD43_


/*
 * debug.h -- definition of standalone debugging flags
 */

#define	BSD43_DBG_PROTOCOL	0x1		/* serial line protocol */
#define	BSD43_DBG_RMTDBG	0x2		/* remote debug protocol */
#define	BSD43_DBG_PROTOOOB	0x4		/* print oob protocol input */
#define	BSD43_DBG_BFS		0x8		/* debug bfs protocol */

/*----- NUMIPS: STRIP MACRO PREFIXES (BEGIN) ---------------------------------*/
/* 
 * Strip off "BSD43_" and "bsd43_" for use with standard BSD code.
 * (GENERATED AUTOMATICALLY -- DON'T EDIT BY HAND)
 */
#ifdef SYSTYPE_BSD43
#   define DBG_BFS BSD43_DBG_BFS
#   define DBG_PROTOCOL BSD43_DBG_PROTOCOL
#   define DBG_PROTOOOB BSD43_DBG_PROTOOOB
#   define DBG_RMTDBG BSD43_DBG_RMTDBG
#endif SYSTYPE_BSD43
/*----- NUMIPS: STRIP MACRO PREFIXES (END) -----------------------------------*/


