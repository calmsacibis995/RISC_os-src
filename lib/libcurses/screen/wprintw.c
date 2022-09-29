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
#ident	"$Header: wprintw.c,v 1.2.1.2 90/05/10 02:30:42 wje Exp $"
/*	Copyright (c) 1984 AT&T	*/
/*	  All Rights Reserved  	*/

/*	THIS IS UNPUBLISHED PROPRIETARY SOURCE CODE OF AT&T	*/
/*	The copyright notice above does not evidence any   	*/
/*	actual or intended publication of such source code.	*/

/*
 * printw and friends
 *
 */

# include	"curses_inc.h"
# include	<varargs.h>

/*
 *	This routine implements a printf on the given window.
 */
/*VARARGS*/
wprintw(va_alist)
va_dcl
{
	va_list ap;
	register WINDOW	*win;
	register char * fmt;

	va_start(ap);
	win = va_arg(ap, WINDOW *);
	fmt = va_arg(ap, char *);
	return vwprintw(win, fmt, ap);
}
