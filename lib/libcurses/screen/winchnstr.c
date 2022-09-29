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
#ident	"$Header: winchnstr.c,v 1.2.1.2 90/05/10 02:29:22 wje Exp $"
/*	Copyright (c) 1984 AT&T	*/
/*	  All Rights Reserved  	*/

/*	THIS IS UNPUBLISHED PROPRIETARY SOURCE CODE OF AT&T	*/
/*	The copyright notice above does not evidence any   	*/
/*	actual or intended publication of such source code.	*/

#include	"curses_inc.h"

/*
 * Read in ncols worth of data from window win and assign the
 * chars to string. NULL terminate string upon completion.
 * Return the number of chtypes copied.
 */

winchnstr(win,string,ncols)
register	WINDOW	*win;
chtype		*string;
int		ncols;
{
    register	chtype	*ptr = &(win->_y[win->_cury][win->_curx]);
    register	int	counter;

    /*
     * Adjust ncols if it will exceed the maximum for the
     * window being referenced
     */

    if ((win->_curx + ncols) > win->_maxx)
	ncols = win->_maxx - win->_curx;

    for (counter = 0; counter < ncols; counter++)
	*string++ = *ptr++;

    *string = (chtype) 0;
    return (counter);
}
