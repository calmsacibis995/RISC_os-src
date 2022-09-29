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
#ident	"$Header: wwclreos.c,v 1.1.2.2 90/05/07 19:59:18 wje Exp $"

#include "ww.h"
#include "tt.h"

wwclreos(w, row, col)
register struct ww *w;
{
	register i;
	int cleared = 0;

	/*
	 * Quick and dirty check for windows that cover the bottom
	 * portion of the screen.  Not meant to be complete.
	 */
	if (tt.tt_clreos && w->ww_i.b == wwnrow && w->ww_i.l == 0 &&
	    w->ww_i.r == wwncol && wwvisible(w)) {
		register j;
		register union ww_char *s;

		i = row;
		(*tt.tt_move)(i, col);
		(*tt.tt_clreos)();
		/*
		 * We have to fix wwos becuase wwclreol1 won't do that.
		 */
		s = &wwos[i][col];
		for (j = wwncol - col; --j >= 0;)
			s++->c_w = ' ';
		for (i++; i < wwnrow; i++) {
			s = wwos[i];
			for (j = wwncol; --j >= 0;)
				s++->c_w = ' ';
		}
		cleared = 1;
	}
	wwclreol1(w, row, col, cleared);
	for (i = row + 1; i < w->ww_b.b; i++)
		wwclreol1(w, i, w->ww_b.l, cleared);
}
