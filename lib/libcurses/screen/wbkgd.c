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
#ident	"$Header: wbkgd.c,v 1.2.1.2 90/05/10 02:27:51 wje Exp $"
/*	Copyright (c) 1984 AT&T	*/
/*	  All Rights Reserved  	*/

/*	THIS IS UNPUBLISHED PROPRIETARY SOURCE CODE OF AT&T	*/
/*	The copyright notice above does not evidence any   	*/
/*	actual or intended publication of such source code.	*/

#include	"curses_inc.h"

/* Change the background of a window.  nbkgd :	new background. */

wbkgd(win, nbkgd)
register	WINDOW	*win;
chtype	nbkgd;
{
    register	int	x, y, maxx;
    register	chtype	*wcp, obkgda, obkgdc, nbkgda, nbkgdc, acolor, c;
    register	short	*begch, *endch;

    /* if 'nbkgd' contains color information, but this is not a color   */
    /* terminal, erase that information.				*/

    if ((nbkgd & A_COLOR) && (SP->_pairs_tbl == NULL))
	 nbkgd &= ~A_COLOR;

    if (nbkgd == win->_bkgd)
	return (OK);

    obkgdc = _CHAR(win->_bkgd);
    obkgda = _ATTR(win->_bkgd);

    nbkgdc = _CHAR(nbkgd);
    nbkgda = _ATTR(nbkgd);
    if (nbkgdc < ' ' || nbkgdc == _CTRL('?'))
	nbkgdc = obkgdc, nbkgd = nbkgdc|nbkgda;

    win->_bkgd = nbkgd;

    /* delete the old background from the attribute field and replace    */
    /* it with the new background.  Note: if the same attribute was      */
    /* first set by wbkgd() and then by wattron(), or vice versa, it	 */
    /* will be deleted, so the effect of wattron() will be lost.	 */
    /* This applies to both video and color attributes.			 */

    if (acolor = (win->_attrs & A_COLOR))
    {
	if (acolor == (obkgda & A_COLOR))
	{
	    win->_attrs = _ATTR((win->_attrs & ~obkgda) | nbkgda);
	}
	else
	{
	    win->_attrs = _ATTR((win->_attrs & (~obkgda | A_COLOR)) |
			        (nbkgda & ~A_COLOR));
	}
    }
    else
        win->_attrs = _ATTR((win->_attrs & ~obkgda) | nbkgda);

    maxx = win->_maxx - 1;
    begch = win->_firstch;
    endch = win->_lastch;
    for (y = win->_maxy-1; y >= 0; --y, ++begch, ++endch)
    {
	for (x = maxx, wcp = win->_y[y]; x-- >= 0; ++wcp)
	{
	    if ((c = _CHAR(*wcp)) == obkgdc)
		c = nbkgdc;
	    if (acolor = (*wcp & A_COLOR))
	    {
		if (acolor == (obkgda & A_COLOR))
	    	    *wcp = c | _ATTR((*wcp & ~obkgda) | nbkgda);
		else
		    *wcp = c | _ATTR((*wcp & (~obkgda | A_COLOR)) |
				     (nbkgda & ~A_COLOR));
	    }
	    else
	    	*wcp = c | _ATTR((*wcp & ~obkgda) | nbkgda);
	}
	*begch = 0;
	*endch = maxx;
    }

    win->_flags |= _WINCHANGED;
    if (win->_sync)
	wsyncup(win);

    return (win->_immed ? wrefresh(win) : OK);
}
