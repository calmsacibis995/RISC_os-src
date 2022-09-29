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
/* $Header: cmderr.h,v 1.5.2.2 90/05/09 18:36:19 wje Exp $ */

/*	Copyright (c) 1984 AT&T	*/
/*	  All Rights Reserved  	*/

/*	THIS IS UNPUBLISHED PROPRIETARY SOURCE CODE OF AT&T	*/
/*	The copyright notice above does not evidence any   	*/
/*	actual or intended publication of such source code.	*/

/*
	Include file for error message interface.
	Command and library version.
*/

#define	cmderr	errtag( __FILE__, __LINE__ ),  errtext

extern	void	erradvice();
extern	void	errafter();	/* routine run after text is printed */
extern	void	errbefore();	/* routine run before text is printed */
extern	void	errtag();
extern	void	errtext();
extern	void	errtofix();
extern	void	errverb();
extern	int	errexit;	/* exit(2) code to use if error causes exit */
extern	char	**errmessage;	/* error messages that depend on severity */
extern	char	*pgm_name;

/* severities */
#define	CINFO	0
#define	CWARN	1
#define	CERROR	2
#define	CHALT	3

/* special errtext() argument that prints a standard message based on errno */
#define	CERRNO	1

/* list of additional arguments, after format, to errtext(), errbefore() and
   errafter() */
#define	ErrArgList	a1, a2, a3, a4, a5, a6, a7, a8, a9, a10
