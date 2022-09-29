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
#ident	"$Header: wechochar.c,v 1.2.1.2 90/05/10 02:28:52 wje Exp $"
/*	Copyright (c) 1984 AT&T	*/
/*	  All Rights Reserved  	*/

/*	THIS IS UNPUBLISHED PROPRIETARY SOURCE CODE OF AT&T	*/
/*	The copyright notice above does not evidence any   	*/
/*	actual or intended publication of such source code.	*/

#include	"curses_inc.h"

/*
 *  These routines short-circuit much of the innards of curses in order to get
 *  a single character output to the screen quickly! It is used by getch()
 *  and getstr().
 *
 *  wechochar(WINDOW *win, chtype ch) is functionally equivalent to
 *  waddch(WINDOW *win, chtype ch), wrefresh(WINDOW *win)
 */

wechochar (win, ch)
register WINDOW *win;
chtype ch;
{
    int	saveimm = win->_immed, rv;

    immedok(win,TRUE);
    rv = waddch(win,ch);
    win->_immed = saveimm;
    return (rv);
}
