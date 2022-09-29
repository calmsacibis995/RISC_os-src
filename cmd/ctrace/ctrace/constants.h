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
/* $Header: constants.h,v 1.6.2.2.1.3 90/07/23 09:12:19 hawkes Exp $ */

/*	Copyright (c) 1984 AT&T	*/
/*	  All Rights Reserved  	*/

/*	THIS IS UNPUBLISHED PROPRIETARY SOURCE CODE OF AT&T	*/
/*	The copyright notice above does not evidence any   	*/
/*	actual or intended publication of such source code.	*/

/*	ctrace - C program debugging tool
 *
 *	preprocessor constant definitions
 *
 */

#define IDMAX		8	/* maximum significant identifier chars */
#define INDENTMAX	40	/* maximum left margin indentation chars */
#define LOOPMAX		20	/* statement trace buffer size */
#define SAVEMAX		80	/* parser function header and brace text storage size */
#define	STMTMAX		400	/* maximum statement text length */
#define TOKENMAX	30	/* maximum non-string token length */
#define	TRACE_DFLT	10	/* default number of traced variables */
#define TRACEMAX	20	/* maximum number of traced variables */
#undef	YYLMAX		
#define YYLMAX		STMTMAX + TOKENMAX	/* scanner line buffer size */
#define YYMAXDEPTH	300	/* yacc stack size */

#define NO_LINENO	0
#define	PP_COMMAND	"/usr/lib/cpp -C -DCTRACE"    /* preprocessor command */
#define	RUNTIME		LIB/runtime.c"	/* run-time code package file */
