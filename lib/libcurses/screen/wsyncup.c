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
#ident	"$Header: wsyncup.c,v 1.2.1.2 90/05/10 02:31:52 wje Exp $"
/*	Copyright (c) 1984 AT&T	*/
/*	  All Rights Reserved  	*/

/*	THIS IS UNPUBLISHED PROPRIETARY SOURCE CODE OF AT&T	*/
/*	The copyright notice above does not evidence any   	*/
/*	actual or intended publication of such source code.	*/

#include	"curses_inc.h"

/* Sync the changes in a window with its ancestors. */

void
wsyncup(win)
register	WINDOW	*win;
{
    register	short	*wbch, *wech, *pbch, *pech;
    register	int	wy, px, py, endy, bch, ech;
    register	WINDOW	*par;

    for (par = win->_parent; par != NULL; win = par, par = par->_parent)
    {
	py = win->_pary;
	px = win->_parx;
	endy = win->_maxy;

	wbch = win->_firstch;
	wech = win->_lastch;
	pbch = par->_firstch+ py;
	pech = par->_lastch+ py;

	/*
	 * I don't think we need check WINCHANGED first.
	 * The reasoning is that all internal calls will have come
	 * from a function that did change the window.  And assumably
	 * all external calls will work the same way.
	 */
	par->_flags |= _WINCHANGED;
	/* check each line */
	for (wy = 0; wy < endy; ++wy, ++wbch, ++wech, ++pbch, ++pech)
	    if (*wbch != _INFINITY)
	    {
		bch = px + *wbch;
		ech = px + *wech;
		if (*pbch > bch)
		    *pbch = bch;
		if (*pech < ech)
		    *pech = ech;
	    }
    }
}

void
wcursyncup(win)
register	WINDOW	*win;
{
    register	WINDOW	*par = win->_parent;

    while (par != NULL)
    {
	par->_cury = win->_cury + win->_pary;
	par->_curx = win->_curx + win->_parx;
	par = (win = par)->_parent;
    }
}
