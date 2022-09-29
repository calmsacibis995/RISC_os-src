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
#ident	"$Header: setsyx.c,v 1.2.1.2 90/05/10 02:21:49 wje Exp $"
/*	Copyright (c) 1984 AT&T	*/
/*	  All Rights Reserved  	*/

/*	THIS IS UNPUBLISHED PROPRIETARY SOURCE CODE OF AT&T	*/
/*	The copyright notice above does not evidence any   	*/
/*	actual or intended publication of such source code.	*/

/*
 * Set the current screen coordinates (y, x).
 *
 * This routine may be called before doupdate(). It tells doupdate()
 * where to leave the cursor instead of the location of (x, y) of the
 * last window that was wnoutrefreshed or pnoutrefreshed.
 * If x and y are negative, then the cursor will be left wherever
 * curses decides to leave it, as if leaveok() had been TRUE for the
 * last window refreshed.
 */
#include	"curses_inc.h"

setsyx(y, x)
int	y, x;
{
    if (y < 0 && x < 0)
    {
	SP->virt_scr->_leave = TRUE;
    }
    else
    {
	_virtscr->_cury = y + SP->Yabove;
	_virtscr->_curx = x;
	_virtscr->_leave = FALSE;
	_virtscr->_flags |= _WINMOVED;
    }
    return (OK);
}
