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
#ident	"$Header: winnstr.c,v 1.2.1.2 90/05/10 02:29:34 wje Exp $"
/*	Copyright (c) 1984 AT&T	*/
/*	  All Rights Reserved  	*/

/*	THIS IS UNPUBLISHED PROPRIETARY SOURCE CODE OF AT&T	*/
/*	The copyright notice above does not evidence any   	*/
/*	actual or intended publication of such source code.	*/

#include	"curses_inc.h"

/*
 * Copy n chars in window win from current cursor position to end
 * of window into char buffer str.  Return the number of chars copied.
 */

winnstr(win, str, ncols)
register	WINDOW	*win;
register	char	*str;
register	int	ncols;
{
    register	int	counter = 0;
    int			cy = win->_cury;
    register	chtype	*ptr = &(win->_y[cy][win->_curx]),
			*pmax = &(win->_y[cy][win->_maxx]);

    if (ncols == -1)
	ncols = MAXINT;

    while (counter < ncols)
    {
	str[counter++] = *ptr++ & A_CHARTEXT;

	if (ptr == pmax)
	{
	    if (++cy == win->_maxy)
		break;

	    ptr = &(win->_y[cy][0]);
	    pmax = ptr + win->_maxx;
	}
    }
    str[counter] = '\0';
    return (counter);
}
