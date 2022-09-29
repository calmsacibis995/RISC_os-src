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
#ident	"$Header: parties.c,v 1.1.2.2 90/05/10 03:24:21 wje Exp $"

/*
 * Copyright (c) 1983 Regents of the University of California.
 * All rights reserved.  The Berkeley software License Agreement
 * specifies the terms and conditions for redistribution.
 */

#ifndef lint
static char sccsid[] = "@(#)parties.c	5.1 (Berkeley) 5/29/85";
#endif not lint

#include "externs.h"

meleeing(from, to)
struct ship *from;
register struct ship *to;
{
	register struct BP *p = from->file->OBP;
	register struct BP *q = p + NBP;

	for (; p < q; p++)
		if (p->turnsent && p->toship == to)
			return 1;
	return 0;
}

boarding(from, isdefense)
register struct ship *from;
char isdefense;
{
	register struct BP *p = isdefense ? from->file->DBP : from->file->OBP;
	register struct BP *q = p + NBP;

	for (; p < q; p++)
		if (p->turnsent)
			return 1;
	return 0;
}

unboard(ship, to, isdefense)
register struct ship *ship, *to;
register char isdefense;
{
	register struct BP *p = isdefense ? ship->file->DBP : ship->file->OBP;
	register n;

	for (n = 0; n < NBP; p++, n++)
		if (p->turnsent && (p->toship == to || isdefense || ship == to))
			Write(isdefense ? W_DBP : W_OBP, ship, 0, n, 0, 0, 0);
}
