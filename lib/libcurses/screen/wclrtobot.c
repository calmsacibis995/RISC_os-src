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
#ident	"$Header: wclrtobot.c,v 1.2.1.2 90/05/10 02:28:09 wje Exp $"
/*	Copyright (c) 1984 AT&T	*/
/*	  All Rights Reserved  	*/

/*	THIS IS UNPUBLISHED PROPRIETARY SOURCE CODE OF AT&T	*/
/*	The copyright notice above does not evidence any   	*/
/*	actual or intended publication of such source code.	*/

#include	"curses_inc.h"

/* This routine erases everything on the window. */
wclrtobot(win)
register	WINDOW	*win;
{
    register	int	savimmed, savsync;
    register	int	cury = win->_cury;
    register	int	curx = win->_curx;

    if (win != curscr)
    {
	savimmed = win->_immed;
	savsync = win->_sync;
	win->_immed = win->_sync = FALSE;
    }

    /* set region to be clear */
    if (cury >= win->_tmarg && cury <= win->_bmarg)
	win->_cury = win->_bmarg;
    else
	win->_cury = win->_maxy - 1;

    win->_curx = 0;
    for ( ; win->_cury > cury; win->_cury--)
	(void) wclrtoeol(win);
    win->_curx = curx;
    (void) wclrtoeol(win);

    if (win == curscr)
	return (OK);

    /* not curscr */
    win->_sync = savsync;

    if (win->_sync)
	wsyncup(win);
    
    win->_flags |= _WINCHANGED;
    return ((win->_immed = savimmed) ? wrefresh(win) : OK);
}
