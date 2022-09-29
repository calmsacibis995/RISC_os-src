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
#ident	"$Header: mvderwin.c,v 1.2.1.2 90/05/10 02:15:11 wje Exp $"
/*	Copyright (c) 1984 AT&T	*/
/*	  All Rights Reserved  	*/

/*	THIS IS UNPUBLISHED PROPRIETARY SOURCE CODE OF AT&T	*/
/*	The copyright notice above does not evidence any   	*/
/*	actual or intended publication of such source code.	*/

#include	"curses_inc.h"

/*
 * Move a derived window inside its parent window.
 * This routine does not change the screen relative
 * parameters begx and begy. Thus, it can be used to
 * display different parts of the parent window at
 * the same screen coordinate.
 */

mvderwin(win, pary, parx)
WINDOW	*win;
int	pary, parx;
{
    register	int	y, maxy, maxx;
    register	WINDOW	*par;
    chtype		obkgd, **wc, **pc;
    short		*begch, *endch;

    if ((par = win->_parent) == NULL)
	goto bad;
    if (pary == win->_pary && parx == win->_parx)
	return (OK);

    maxy = win->_maxy-1;
    maxx = win->_maxx-1;
    if ((parx + maxx) >= par->_maxx || (pary + maxy) >= par->_maxy)
bad:
	return (ERR);

    /* save all old changes */
    wsyncup(win);

    /* rearrange pointers */
    win->_parx = parx;
    win->_pary = pary;
    wc = win->_y;
    pc = par->_y + pary;
    begch = win->_firstch;
    endch = win->_lastch;
    for (y = 0; y <= maxy; ++y, ++wc, ++pc, ++begch, ++endch)
    {
	*wc = *pc + parx;
	*begch = 0;
	*endch = maxx;
    }

    /* change background to our own */
    obkgd = win->_bkgd;
    win->_bkgd = par->_bkgd;
    return (wbkgd(win, obkgd));
}
