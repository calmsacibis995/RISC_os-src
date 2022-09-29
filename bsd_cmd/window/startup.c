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
#ident	"$Header: startup.c,v 1.1.2.2 90/05/07 19:55:25 wje Exp $"

#include "defs.h"
#include "value.h"
#include "var.h"
#include "char.h"
#include "local.h"

doconfig()
{
	char buf[100];
	char *home;
	static char runcom[] = RUNCOM;

	if ((home = getenv("HOME")) == 0)
		home = ".";
	return dosource(sprintf(buf, "%.*s/%s",
		(sizeof buf - sizeof runcom) / sizeof (char) - 1,
		home, runcom));
}

/*
 * The default is two windows of equal size.
 */
dodefault()
{
	struct ww *w;
	register r = wwnrow / 2 - 1;

	if (openwin(1, r + 2, 0, wwnrow - r - 2, wwncol, nbufline,
				(char *) 0, 1, 1, shellfile, shell) == 0)
		return;
	if ((w = openwin(0, 1, 0, r, wwncol, nbufline,
				(char *) 0, 1, 1, shellfile, shell)) == 0)
		return;
	wwprintf(w, "Escape character is %s.\r\n", unctrl(escapec));
}

setvars()
{
	/* try to use a random ordering to balance the tree */
	(void) var_setnum("nrow", wwnrow);
	(void) var_setnum("ncol", wwncol);
	(void) var_setnum("baud", wwbaud);
	(void) var_setnum("m_rev", WWM_REV);
	(void) var_setnum("m_blk", WWM_BLK);
	(void) var_setnum("m_ul", WWM_UL);
	(void) var_setnum("m_grp", WWM_GRP);
	(void) var_setnum("m_dim", WWM_DIM);
	(void) var_setnum("m_usr", WWM_USR);
	(void) var_setstr("term", wwterm);
	(void) var_setnum("modes", wwavailmodes);
}
