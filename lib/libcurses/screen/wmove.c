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
#ident	"$Header: wmove.c,v 1.2.1.2 90/05/10 02:30:30 wje Exp $"
/*	Copyright (c) 1984 AT&T	*/
/*	  All Rights Reserved  	*/

/*	THIS IS UNPUBLISHED PROPRIETARY SOURCE CODE OF AT&T	*/
/*	The copyright notice above does not evidence any   	*/
/*	actual or intended publication of such source code.	*/

#include	"curses_inc.h"

/* This routine moves the cursor to the given point */

wmove(win, y, x)
register	WINDOW	*win;
register	int	y, x;
{
#ifdef	DEBUG
    if (outf)
    {
	fprintf(outf, "MOVE to win ");
	if (win == stdscr)
	    fprintf(outf, "stdscr ");
	else
	    fprintf(outf, "%o ", win);
	fprintf(outf, "(%d, %d)\n", y, x);
    }
#endif	/* DEBUG */
    if (x < 0 || y < 0 || x >= win->_maxx || y >= win->_maxy)
	return (ERR);
    win->_curx = x;
    win->_cury = y;
    win->_flags |= _WINMOVED;
    return (win->_immed ? wrefresh(win) : OK);
}
