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
#ident	"$Header: ttinit.c,v 1.1.2.2 90/05/07 19:56:41 wje Exp $"

#include "ww.h"
#include "tt.h"

int tt_h19();
int tt_h29();
int tt_f100();
int tt_tvi925();
int tt_wyse75();
int tt_wyse60();
int tt_zentec();
int tt_generic();
struct tt_tab tt_tab[] = {
	{ "h19",	3, tt_h19 },
	{ "h29",	3, tt_h29 },
	{ "f100",	4, tt_f100 },
	{ "tvi925",	6, tt_tvi925 },
	{ "wyse75",	6, tt_wyse75 },
	{ "wyse60",	6, tt_wyse60 },
	{ "w60",	3, tt_wyse60 },
	{ "zentec",	6, tt_zentec },
	{ "generic",	0, tt_generic },
	0
};

ttinit()
{
	register struct tt_tab *tp;
	register char *p, *q;
	register char *t;
	struct winsize winsize;

	tt_strp = tt_strings;

	/*
	 * Set output buffer size to about 1 second of output time.
	 */
	tt_obp = tt_ob;
	tt_obe = tt_ob + MIN(wwbaud/10, sizeof tt_ob);

	/*
	 * Use the standard name of the terminal (i.e. the second
	 * name in termcap).
	 */
	for (p = wwtermcap; *p && *p != '|' && *p != ':'; p++)
		;
	if (*p == '|')
		p++;
	for (q = p; *q && *q != '|' && *q != ':'; q++)
		;
	if (q != p && (t = malloc((unsigned) (q - p + 1))) != 0) {
		wwterm = t;
		while (p < q)
			*t++ = *p++;
		*t = 0;
	}
	for (tp = tt_tab; tp->tt_name != 0; tp++)
		if (strncmp(tp->tt_name, wwterm, tp->tt_len) == 0)
			break;
	if (tp->tt_name == 0) {
		wwerrno = WWE_BADTERM;
		return -1;
	}
	if ((*tp->tt_func)() < 0) {
		wwerrno = WWE_CANTDO;
		return -1;
	}
	if (ioctl(0, TIOCGWINSZ, (char *)&winsize) >= 0 &&
	    winsize.ws_row != 0 && winsize.ws_col != 0) {
		tt.tt_nrow = winsize.ws_row;
		tt.tt_ncol = winsize.ws_col;
	}
	return 0;
}

fixttmodes()
{
	(*tt.tt_setmodes)(0);
}
