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
#ident	"$Header: vwprintw.c,v 1.2.1.2 90/05/10 02:26:38 wje Exp $"
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
 *	This routine actually executes the printf and adds it to the window
 *
 *	This code now uses the vsprintf routine, which portably digs
 *	into stdio.  We provide a vsprintf for older systems that don't
 *	have one.
 */

/*VARARGS2*/
vwprintw(win, fmt, ap)
register WINDOW	*win;
register char * fmt;
va_list ap;
{
	char	buf[BUFSIZ];
	register int n;

	n = vsprintf(buf, fmt, ap);
	va_end(ap);
	if (n == ERR)
		return ERR;
	return waddstr(win, buf);
}
