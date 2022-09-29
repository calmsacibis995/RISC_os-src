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
#ident	"$Header: cmd4.c,v 1.1.2.2 90/05/07 19:52:28 wje Exp $"

#include "defs.h"

c_colon()
{
	char oldterse = terse;
	char buf[512];

	setterse(0);
	wwputc(':', cmdwin);
	wwgets(buf, wwncol - 3, cmdwin);
	wwputc('\n', cmdwin);
	wwcurtowin(cmdwin);
	setterse(oldterse);
	if (dolongcmd(buf, (struct value *)0, 0) < 0)
		error("Out of memory.");
}
