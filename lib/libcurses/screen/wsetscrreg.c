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
#ident	"$Header: wsetscrreg.c,v 1.2.1.2 90/05/10 02:31:13 wje Exp $"
/*	Copyright (c) 1984 AT&T	*/
/*	  All Rights Reserved  	*/

/*	THIS IS UNPUBLISHED PROPRIETARY SOURCE CODE OF AT&T	*/
/*	The copyright notice above does not evidence any   	*/
/*	actual or intended publication of such source code.	*/

#include	"curses_inc.h"

/*
 *	Change scrolling region. Since we depend on the values
 *	of tmarg and bmarg in various ways, this can no longer
 *	be a macro.
 */

wsetscrreg(win,topy,boty)
WINDOW	*win;
int	topy, boty;
{
    if (topy < 0 || topy >= win->_maxy || boty < 0 || boty >= win->_maxy)
	return (ERR);

    win->_tmarg = topy;
    win->_bmarg = boty;
    return (OK);
}
