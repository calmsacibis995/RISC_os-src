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
/* $Header: check.h,v 1.5.2.2 90/05/09 18:35:46 wje Exp $ */

/*	Copyright (c) 1984 AT&T	*/
/*	  All Rights Reserved  	*/

/*	THIS IS UNPUBLISHED PROPRIETARY SOURCE CODE OF AT&T	*/
/*	The copyright notice above does not evidence any   	*/
/*	actual or intended publication of such source code.	*/

/*
	global variables for chk* commands
*/

enum	mode {
	YESNO,
	LIST,
	REGEX,
	REGEXR,
	ARITH,
	CMD
};

extern int	anychar;	/* flag to permit non-printing and non-ASCII
				characters input */
extern char	*defaultstr;	/* default value returned for empty input */
extern int	echo;		/* flag for printing of answer */
extern int	errcode;	/* error exit code */
extern int	esc;		/* flag to allow escape to shell */
extern int	force;		/* flag to insist on correct answer */
extern char	*helpstr;	/* this string gives help */
extern char	*helpmsg;	/* this string is help message */
extern char	*killpid;	/* kill this process ID upon quit */
extern char	*quitname;	/* what the quit string is called */
extern char	*quitstr;	/* this string causes a quit */
extern int	trimspace;	/* flag to trim white space from begining and
				end of input */
extern char	*usagestr;	/* custom part of the usage message */

#define	CYCLEDEF	25
#define	LINELEN		74
#define	MAXLINE		256
#define	MAXTEST		10
