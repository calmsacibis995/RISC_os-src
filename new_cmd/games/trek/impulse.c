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
#ident	"$Header: impulse.c,v 1.1.2.2 90/05/10 03:33:51 wje Exp $"

/*
 * Copyright (c) 1980 Regents of the University of California.
 * All rights reserved.  The Berkeley software License Agreement
 * specifies the terms and conditions for redistribution.
 */

#ifndef lint
static char sccsid[] = "@(#)impulse.c	5.1 (Berkeley) 5/30/85";
#endif not lint

# include	"trek.h"

/**
 **	move under impulse power
 **/

impulse()
{
	int			course;
	register int		power;
	double			dist, time;
	register int		percent;
	extern double		move();

	if (Ship.cond == DOCKED)
		return (printf("Scotty: Sorry captain, but we are still docked.\n"));
	if (damaged(IMPULSE))
		return (out(IMPULSE));
	if (getcodi(&course, &dist))
		return;
	power = 20 + 100 * dist;
	percent = 100 * power / Ship.energy + 0.5;
	if (percent >= 85)
	{
		printf("Scotty: That would consume %d%% of our remaining energy.\n",
			percent);
		if (!getynpar("Are you sure that is wise"))
			return;
		printf("Aye aye, sir\n");
	}
	time = dist / 0.095;
	percent = 100 * time / Now.time + 0.5;
	if (percent >= 85)
	{
		printf("Spock: That would take %d%% of our remaining time.\n",
			percent);
		if (!getynpar("Are you sure that is wise"))
			return;
		printf("(He's finally gone mad)\n");
	}
	Move.time = move(0, course, time, 0.095);
	Ship.energy -= 20 + 100 * Move.time * 0.095;
}
