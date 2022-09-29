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
#ident	"$Header: systemname.c,v 1.1.2.2 90/05/10 03:38:09 wje Exp $"

/*
 * Copyright (c) 1980 Regents of the University of California.
 * All rights reserved.  The Berkeley software License Agreement
 * specifies the terms and conditions for redistribution.
 */

#ifndef lint
static char sccsid[] = "@(#)systemname.c	5.1 (Berkeley) 1/29/86";
#endif not lint

# include	"trek.h"

/*
**  RETRIEVE THE STARSYSTEM NAME
**
**	Very straightforward, this routine just gets the starsystem
**	name.  It returns zero if none in the specified quadrant
**	(which, by the way, is passed it).
**
**	This routine knows all about such things as distressed
**	starsystems, etc.
*/

char *systemname(q1)
struct quad	*q1;
{
	register struct quad	*q;
	register int		i;

	q = q1;

	i = q->qsystemname;
	if (i & Q_DISTRESSED)
		i = Event[i & Q_SYSTEM].systemname;

	i &= Q_SYSTEM;
	if (i == 0)
		return (0);
	return (Systemname[i]);
}
