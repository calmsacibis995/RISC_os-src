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
/* $Header: ex_argv.h,v 1.5.2.2 90/05/10 00:53:23 wje Exp $ */

/*	Copyright (c) 1984 AT&T	*/
/*	  All Rights Reserved  	*/

/*	THIS IS UNPUBLISHED PROPRIETARY SOURCE CODE OF AT&T	*/
/*	The copyright notice above does not evidence any   	*/
/*	actual or intended publication of such source code.	*/

/* Copyright (c) 1981 Regents of the University of California */
/*
 * The current implementation of the argument list is poor,
 * using an argv even for internally done "next" commands.
 * It is not hard to see that this is restrictive and a waste of
 * space.  The statically allocated glob structure could be replaced
 * by a dynamically allocated argument area space.
 */
var char	**argv;
var char	**argv0;
var char	*args;
var char	*args0;
var short	argc;
var short	argc0;
var short	morargc;		/* Used with "More files to edit..." */

var int	firstln;		/* From +lineno */
var char	*firstpat;		/* From +/pat	*/

struct	glob {
	short	argc;			/* Index of current file in argv */
	short	argc0;			/* Number of arguments in argv */
	char	*argv[NARGS + 1];	/* WHAT A WASTE! */
	char	argspac[NCARGS + sizeof (int)];
};
var struct	glob frob;
