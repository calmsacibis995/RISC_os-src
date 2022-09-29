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
#ident	"$Header: V2._sprintw.c,v 1.2.1.2 90/05/09 20:04:09 wje Exp $"
/*	Copyright (c) 1984 AT&T	*/
/*	  All Rights Reserved  	*/

/*	THIS IS UNPUBLISHED PROPRIETARY SOURCE CODE OF AT&T	*/
/*	The copyright notice above does not evidence any   	*/
/*	actual or intended publication of such source code.	*/


# include	"curses_inc.h"
# include	<varargs.h>

#ifdef _VR2_COMPAT_CODE
/*
	This is only here for compatibility with SVR2 curses.
	It will go away someday. Programs should reference
	vwprintw() instead.
 */

_sprintw(win, fmt, ap)
register WINDOW	*win;
register char * fmt;
va_list ap;
{
	return vwprintw(win, fmt, ap);
}
#endif
