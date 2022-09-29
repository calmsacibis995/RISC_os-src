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
#ident	"$Header: clrtoeol.c,v 1.1.1.2 90/05/07 21:34:15 wje Exp $"
/*
 * Copyright (c) 1981 Regents of the University of California.
 * All rights reserved.
 *
 * Redistribution and use in source and binary forms are permitted
 * provided that the above copyright notice and this paragraph are
 * duplicated in all such forms and that any documentation,
 * advertising materials, and other materials related to such
 * distribution and use acknowledge that the software was developed
 * by the University of California, Berkeley.  The name of the
 * University may not be used to endorse or promote products derived
 * from this software without specific prior written permission.
 * THIS SOFTWARE IS PROVIDED ``AS IS'' AND WITHOUT ANY EXPRESS OR
 * IMPLIED WARRANTIES, INCLUDING, WITHOUT LIMITATION, THE IMPLIED
 * WARRANTIES OF MERCHANTIBILITY AND FITNESS FOR A PARTICULAR PURPOSE.
 */

#ifndef lint
static char sccsid[] = "@(#)clrtoeol.c	5.3 (Berkeley) 6/30/88";
#endif /* not lint */

# include	"curses.ext"

/*
 *	This routine clears up to the end of line
 *
 */
wclrtoeol(win)
reg WINDOW	*win; {

	reg char	*sp, *end;
	reg int		y, x;
	reg char	*maxx;
	reg int		minx;

	y = win->_cury;
	x = win->_curx;
	end = &win->_y[y][win->_maxx];
	minx = _NOCHANGE;
	maxx = &win->_y[y][x];
	for (sp = maxx; sp < end; sp++)
		if (*sp != ' ') {
			maxx = sp;
			if (minx == _NOCHANGE)
				minx = sp - win->_y[y];
			*sp = ' ';
		}
	/*
	 * update firstch and lastch for the line
	 */
	touchline(win, y, win->_curx, win->_maxx - 1);
# ifdef DEBUG
	fprintf(outf, "CLRTOEOL: minx = %d, maxx = %d, firstch = %d, lastch = %d\n", minx, maxx - win->_y[y], win->_firstch[y], win->_lastch[y]);
# endif
}
