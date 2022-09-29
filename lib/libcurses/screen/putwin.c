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
#ident	"$Header: putwin.c,v 1.2.1.2 90/05/10 02:18:27 wje Exp $"
/*	Copyright (c) 1984 AT&T	*/
/*	  All Rights Reserved  	*/

/*	THIS IS UNPUBLISHED PROPRIETARY SOURCE CODE OF AT&T	*/
/*	The copyright notice above does not evidence any   	*/
/*	actual or intended publication of such source code.	*/

#include	"curses_inc.h"

/*
 * Write a window to a file.
 *
 * win:	the window to write out.
 * filep:	the file to write to.
 */

putwin(win, filep)
WINDOW	*win;
FILE	*filep;
{
    int			maxx, nelt;
    register	chtype	**wcp, **ecp;

    /* write everything from _cury to _bkgd inclusive */
    nelt = sizeof(WINDOW) - sizeof(win->_y) - sizeof(win->_parent) -
	   sizeof(win->_parx) - sizeof(win->_pary) -
	   sizeof(win->_ndescs) - sizeof(win->_delay);

    if (fwrite((char *) &(win->_cury), 1, nelt, filep) != nelt)
	goto err;

    /* Write the character image */
    maxx = win->_maxx;
    ecp = (wcp = win->_y) + win->_maxy;
    while (wcp < ecp)
	if (fwrite((char *) *wcp++, sizeof(chtype), maxx, filep) != maxx)
err:
	    return (ERR);

    return (OK);
}
