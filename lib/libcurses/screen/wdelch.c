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
#ident	"$Header: wdelch.c,v 1.2.1.2 90/05/10 02:28:20 wje Exp $"
/*	Copyright (c) 1984 AT&T	*/
/*	  All Rights Reserved  	*/

/*	THIS IS UNPUBLISHED PROPRIETARY SOURCE CODE OF AT&T	*/
/*	The copyright notice above does not evidence any   	*/
/*	actual or intended publication of such source code.	*/

#include	"curses_inc.h"

/*
 * This routine performs a delete-char on the line,
 * leaving (_cury, _curx) unchanged.
 */

wdelch(win)
register	WINDOW	*win;
{
    register	chtype	*temp1, *temp2;
    register	chtype	*end;
    register	int	cury = win->_cury;
    register	int	curx = win->_curx;

    end = &win->_y[cury][win->_maxx - 1];
    temp2 = &win->_y[cury][curx + 1];
    temp1 = temp2 - 1;
    while (temp1 < end)
	*temp1++ = *temp2++;
    *temp1 = win->_bkgd;

#ifdef	_VR3_COMPAT_CODE
    if (_y16update)
	(*_y16update)(win, 1, win->_maxx - curx, cury, curx);
#endif	/* _VR3_COMPAT_CODE */

    win->_lastch[cury] = win->_maxx - 1;
    if (win->_firstch[cury] > curx)
	win->_firstch[cury] = curx;

    win->_flags |= _WINCHANGED;

    if (win->_sync)
	wsyncup(win);

    return (win->_immed ? wrefresh(win) : OK);
}
