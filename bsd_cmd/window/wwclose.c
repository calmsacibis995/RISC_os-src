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
#ident	"$Header: wwclose.c,v 1.1.2.2 90/05/07 19:58:37 wje Exp $"

#include "ww.h"
#include <signal.h>

wwclose(w)
register struct ww *w;
{
	wwindex[w->ww_index] = 0;
	if (w->ww_pty >= 0)
		(void) close(w->ww_pty);
	if (w->ww_socket >= 0)
		(void) close(w->ww_socket);
	wwfree((char **)w->ww_win, w->ww_w.t);
	wwfree((char **)w->ww_buf, w->ww_b.t);
	if (w->ww_fmap != 0)
		wwfree((char **)w->ww_fmap, w->ww_w.t);
	free((char *)(w->ww_nvis + w->ww_w.t));
	if (w->ww_ob != 0)
		free(w->ww_ob);
	free((char *)w);
}
