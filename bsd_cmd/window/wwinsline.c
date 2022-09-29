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
#ident	"$Header: wwinsline.c,v 1.1.2.2 90/05/07 20:01:16 wje Exp $"

#include "ww.h"

wwinsline(w, row)
register struct ww *w;
int row;
{
	register i;
	register union ww_char **cpp, **cqq;
	register union ww_char *cp;
	int row1, row2;
	char deleted;
	int visible;

	/*
	 * Scroll first.
	 */
	if ((row1 = row) < w->ww_i.t) {
		row1 = w->ww_i.t;
		visible = 0;
	} else
		visible = 1;
	if ((row2 = w->ww_b.b) > w->ww_i.b) {
		row2 = w->ww_i.b;
	}
	deleted = wwscroll1(w, row1, row2, -1, visible);

	/*
	 * Fix the buffer.
	 * But leave clearing the last line for wwclreol().
	 */
	cpp = &w->ww_buf[w->ww_b.b];
	cqq = cpp - 1;
	cp = *cqq;
	for (i = w->ww_b.b - row; --i > 0;)
		*--cpp = *--cqq;
	*cqq = cp;

	/*
	 * Now clear the last line.
	 */
	if (visible)
		wwclreol1(w, row, w->ww_b.l, deleted);
	else
		for (i = w->ww_b.nc; --i >= 0;)
			cp++->c_w = ' ';
}
