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
#ident	"$Header: wwclreol.c,v 1.1.2.2 90/05/07 19:58:44 wje Exp $"

#include "ww.h"
#include "tt.h"

/*
 * Clear w to the end of line.
 * If cleared is true, then the screen line has already been cleared
 * previously.
 */
wwclreol1(w, row, col, cleared)
register struct ww *w;
int row, col;
char cleared;
{
	register i;

	/*
	 * Clear the buffer right off
	 */
	{
		register union ww_char *buf;

		buf = &w->ww_buf[row][col]; 
		for (i = w->ww_b.r - col; --i >= 0;)
			buf++->c_w = ' ';
	}

	/*
	 * If can't see it, just return.
	 */
	if (row < w->ww_i.t || row >= w->ww_i.b
	    || w->ww_i.r <= 0 || w->ww_i.r <= col)
		return;

	if (col < w->ww_i.l)
		col = w->ww_i.l;

	/*
	 * Now find out how much is actually cleared, and fix wwns.
	 */
	if (cleared) {
		register union ww_char *s;
		register char *smap, *win;

		i = col;
		smap = &wwsmap[row][i];
		s = &wwns[row][i];
		win = &w->ww_win[row][i];
		for (i = w->ww_i.r - i; --i >= 0;) {
			if (*smap++ != w->ww_index)
				continue;
			s++->c_w = ' ' | *win++ << WWC_MSHIFT;
		}
	} else {
		register union ww_char *s;
		register char *smap, *win;
		int ntouched = 0;
		int gain = 0;

		i = col;
		smap = &wwsmap[row][i];
		s = wwns[row];
		win = w->ww_win[row];
		for (; i < w->ww_i.r; i++) {
			if (*smap++ != w->ww_index) {
				if (s[i].c_w != ' ')
					gain--;
			} else if (win[i] == 0) {
				if (s[i].c_w != ' ') {
					gain++;
					ntouched++;
					s[i].c_w = ' ';
				}
			} else {
				short c = ' ' | win[i] << WWC_MSHIFT;
				if (s[i].c_w == c)
					gain--;
				else {
					s[i].c_w = c;
					ntouched++;
				}
			}
		}
		s += i;
		for (i = wwncol - i; --i >= 0;)
			if (s++->c_w != ' ')
				gain--;
		if (ntouched > 0)
			wwtouched[row] |= WWU_TOUCHED;
		/*
		 * Can/should we use clear eol?
		 */
		if (tt.tt_clreol != 0 && gain > 4) {
			/* clear to the end */
			(*tt.tt_move)(row, col);
			(*tt.tt_clreol)();
			s = &wwos[row][col];
			for (i = wwncol - col; --i >= 0;)
				s++->c_w = ' ';
		}
	}
}
