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
#ident	"$Header: rest.c,v 1.1.2.2 90/05/10 03:36:34 wje Exp $"

/*
 * Copyright (c) 1980 Regents of the University of California.
 * All rights reserved.  The Berkeley software License Agreement
 * specifies the terms and conditions for redistribution.
 */

#ifndef lint
static char sccsid[] = "@(#)rest.c	5.1 (Berkeley) 5/30/85";
#endif not lint

# include	"trek.h"
# include	"getpar.h"

/*
**  REST FOR REPAIRS
**
**	You sit around and wait for repairs to happen.  Actually, you
**	sit around and wait for anything to happen.  I do want to point
**	out however, that Klingons are not as patient as you are, and
**	they tend to attack you while you are resting.
**
**	You can never rest through a long range tractor beam.
**
**	In events() you will be given an opportunity to cancel the
**	rest period if anything momentous happens.
*/

rest()
{
	double			t;
	register int		percent;

	/* get the time to rest */
	t = getfltpar("How long");
	if (t <= 0.0)
		return;
	percent = 100 * t / Now.time + 0.5;
	if (percent >= 70)
	{
		printf("Spock: That would take %d%% of our remaining time.\n",
			percent);
		if (!getynpar("Are you really certain that is wise"))
			return;
	}
	Move.time = t;

	/* boundary condition is the LRTB */
	t = Now.eventptr[E_LRTB]->date - Now.date;
	if (Ship.cond != DOCKED && Move.time > t)
		Move.time = t + 0.0001;
	Move.free = 0;
	Move.resting = 1;
}
