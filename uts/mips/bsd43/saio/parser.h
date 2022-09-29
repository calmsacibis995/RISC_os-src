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
/* $Header: parser.h,v 1.6.3.2 90/05/10 04:46:56 wje Exp $ */
#ifndef BSD43_
#    include <bsd43/bsd43_.h>
#endif BSD43_


/*
 * parser.h -- definitions for command parser routines
 */

/*
 * range type for parse_range()
 */
#define	BSD43_ADDR_RANGE	0		/* address:address */
#define	BSD43_CNT_RANGE	1		/* address#count */
#define	BSD43_ERROR_RANGE	-1		/* syntax error */

/*
 * cmd_table -- interface between parser and command execution routines
 * Add new commands by making entry here.
 */
struct bsd43_(cmd_table) {
	char *ct_string;		/* command name */
	int (*ct_routine)();		/* implementing routine */
	char *ct_usage;			/* syntax */
};

#define	BSD43_LINESIZE	128		/* line buffer size */

/*----- NUMIPS: STRIP MACRO PREFIXES (BEGIN) ---------------------------------*/
/* 
 * Strip off "BSD43_" and "bsd43_" for use with standard BSD code.
 * (GENERATED AUTOMATICALLY -- DON'T EDIT BY HAND)
 */
#ifdef SYSTYPE_BSD43
#   define ADDR_RANGE BSD43_ADDR_RANGE
#   define CNT_RANGE BSD43_CNT_RANGE
#   define ERROR_RANGE BSD43_ERROR_RANGE
#   define LINESIZE BSD43_LINESIZE
#endif SYSTYPE_BSD43
/*----- NUMIPS: STRIP MACRO PREFIXES (END) -----------------------------------*/


