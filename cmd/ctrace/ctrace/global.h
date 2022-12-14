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
/* $Header: global.h,v 1.5.2.2 90/05/09 15:33:32 wje Exp $ */

/*	Copyright (c) 1984 AT&T	*/
/*	  All Rights Reserved  	*/

/*	THIS IS UNPUBLISHED PROPRIETARY SOURCE CODE OF AT&T	*/
/*	The copyright notice above does not evidence any   	*/
/*	actual or intended publication of such source code.	*/

/*	ctrace - C program debugging tool
 *
 *	global type and data definitions
 *
 */
#include <stdio.h>
#include "constants.h"

enum	bool {no, yes};

enum	trace_type {
	normal, assign, prefix, postfix, string, strres
};
struct symbol_struct {
	int	start, end;
	enum	symbol_type {
		constant, variable, repeatable, side_effect
	} type;
};

/* main.c global data */
extern	enum	bool suppress;	/* suppress redundant trace output (-s) */
extern	enum	bool pound_line;/* input preprocessed so suppress #line	*/
extern	int	tracemax;	/* maximum traced variables per statement (-t number) */
extern	char	*filename;	/* input file name */
extern	enum	bool trace;	/* indicates if this function should be traced */

/* parser.y global data */
extern	enum	bool fcn_body;	/* function body indicator */

/* scanner.l global data */
extern	char	indentation[];	/* left margin indentation (blanks and tabs ) */
extern	char	yytext[];	/* statement text */
extern	enum	bool too_long;  /* statement too long to fit in buffer */
extern	int	last_yychar;	/* used for parser error handling */
extern	int	token_start;	/* start of this token in the text */
extern	int	yyleng;		/* length of the text */
extern	int	yylineno;	/* number of current input line */
extern	FILE	*yyin;		/* input file descriptor */

/* lookup.c global data */
extern	enum	bool stdio_preprocessed;	/* stdio.h already preprocessed */
extern	enum	bool setjmp_preprocessed;	/* setjmp.h already preprocessed */
