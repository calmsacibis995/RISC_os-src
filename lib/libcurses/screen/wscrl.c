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
#ident	"$Header: wscrl.c,v 1.2.1.2 90/05/10 02:31:07 wje Exp $"
/*	Copyright (c) 1984 AT&T	*/
/*	  All Rights Reserved  	*/

/*	THIS IS UNPUBLISHED PROPRIETARY SOURCE CODE OF AT&T	*/
/*	The copyright notice above does not evidence any   	*/
/*	actual or intended publication of such source code.	*/

#include	"curses_inc.h"

/* Scroll the given window up/down n lines. */

wscrl(win, n)
register	WINDOW	*win;
{
    register	int	curx, cury, savimmed, savsync;

#ifdef	DEBUG
    if (outf)
	if (win == stdscr)
	    fprintf(outf, "scroll(stdscr, %d)\n", n);
	else
	    if (win == curscr)
		fprintf(outf, "scroll(curscr, %d)\n", n);
	    else
		fprintf(outf, "scroll(%x, %d)\n", win, n);
#endif	/* DEBUG */
    if (!win->_scroll || (win->_flags & _ISPAD))
	return (ERR);

    savimmed = win->_immed;
    savsync = win->_sync;
    win->_immed = win->_sync = FALSE;

    curx = win->_curx; cury = win->_cury;

    if (cury >= win->_tmarg && cury <= win->_bmarg)
	win->_cury = win->_tmarg;
    else
	win->_cury = 0;

    (void) winsdelln(win, -n);
    win->_curx = curx;
    win->_cury = cury;

    win->_sync = savsync;

    if (win->_sync)
	wsyncup(win);

    return ((win->_immed = savimmed) ? wrefresh(win) : OK);
}
