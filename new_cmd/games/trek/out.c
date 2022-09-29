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
#ident	"$Header: out.c,v 1.1.2.2 90/05/10 03:35:25 wje Exp $"

/*
 * Copyright (c) 1980 Regents of the University of California.
 * All rights reserved.  The Berkeley software License Agreement
 * specifies the terms and conditions for redistribution.
 */

#ifndef lint
static char sccsid[] = "@(#)out.c	5.1 (Berkeley) 5/30/85";
#endif not lint

# include	"trek.h"

/*
**  Announce Device Out
*/

out(dev)
int	dev;
{
	register struct device	*d;

	d = &Device[dev];
	printf("%s reports %s ", d->person, d->name);
	if (d->name[length(d->name) - 1] == 's')
		printf("are");
	else
		printf("is");
	printf(" damaged\n");
}
