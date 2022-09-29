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
#ident	"$Header: cmd6.c,v 1.1.2.2 90/05/07 19:52:39 wje Exp $"

#include "defs.h"
#include "string.h"
#include "char.h"

/*
 * Debugging commands.
 */

c_debug()
{
	register struct ww *w;

	if (!terse)
		wwputs("[m(smap) n(ns) o(os) s(string) v(nvis) w(win)]? ", cmdwin);
	wwcurtowin(cmdwin);
	while (wwpeekc() < 0)
		wwiomux();
	if (!terse)
		wwputc('\n', cmdwin);
	switch (wwgetc()) {
	case 'm':
		wwdumpsmap();
		break;
	case 'n':
		wwdumpns();
		break;
	case 'o':
		wwdumpos();
		break;
	case 's':
		debug_str();
		break;
	case 'v':
		if ((w = getwin()) != 0)
			wwdumpnvis(w);
		break;
	case 'w':
		if ((w = getwin()) != 0)
			wwdumpwin(w);
		break;
	default:
		wwbell();
	}
}

#ifdef STR_DEBUG
debug_str()
{
	register struct ww *w;
	struct string *s;

	if ((w = openiwin(wwnrow - 3, "Allocated Strings")) == 0) {
		error("Can't open string window: %s.", wwerror());
		return;
	}
	for (s = str_head.s_forw; s != &str_head; s = s->s_forw) {
		if (more(w, 0) == 2)
			goto out;
		wwprintf(w, "(0x%x)\t\"%s\"\n", s->s_data, s->s_data);
	}
	waitnl(w);
out:
	closeiwin(w);
}
#else
debug_str()
{
	error("No string debugging.");
}
#endif
