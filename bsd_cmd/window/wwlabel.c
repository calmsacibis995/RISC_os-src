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
#ident	"$Header: wwlabel.c,v 1.1.2.2 90/05/07 20:01:27 wje Exp $"

#include "ww.h"
#include "char.h"

/*
 * Label window w on f,
 * at 1 line above w and 'where' columns from it's left edge.
 * Gross, but it works.
 */
wwlabel(w, f, where, l, mode)
struct ww *w;
struct ww *f;
char *l;
{
	int row;
	register j;
	int jj;
	register char *win;
	register union ww_char *buf;
	register union ww_char *ns;
	register char *fmap;
	register char *smap;
	char touched;
	char *p;

	if (f->ww_fmap == 0)
		return;

	row = w->ww_w.t - 1;
	if (row < f->ww_i.t || row >= f->ww_i.b)
		return;
	win = f->ww_win[row];
	buf = f->ww_buf[row];
	fmap = f->ww_fmap[row];
	ns = wwns[row];
	smap = wwsmap[row];
	touched = wwtouched[row];
	mode <<= WWC_MSHIFT;

	jj = MIN(w->ww_i.r, f->ww_i.r);
	j = w->ww_i.l + where;
	while (j < jj && *l)
		for (p = unctrl(*l++); j < jj && *p; j++, p++) {
			/* can't label if not already framed */
			if (win[j] & WWM_GLS)
				continue;
			if (smap[j] != f->ww_index)
				buf[j].c_w = mode | *p;
			else {
				ns[j].c_w = (buf[j].c_w = mode | *p)
						^ win[j] << WWC_MSHIFT;
				touched |= WWU_TOUCHED;
			}
			fmap[j] |= WWF_LABEL;
		}
	wwtouched[row] = touched;
}
