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
#ident	"$Header: score.c,v 1.1.2.2 90/05/10 03:36:47 wje Exp $"

/*
 * Copyright (c) 1980 Regents of the University of California.
 * All rights reserved.  The Berkeley software License Agreement
 * specifies the terms and conditions for redistribution.
 */

#ifndef lint
static char sccsid[] = "@(#)score.c	5.1 (Berkeley) 5/30/85";
#endif not lint

# include	"trek.h"
# include	"getpar.h"

/*
**  PRINT OUT THE CURRENT SCORE
*/

long score()
{
	register int		u;
	register int		t;
	long			s;
	double			r;
	extern struct cvntab	Skitab[];

	printf("\n*** Your score:\n");
	s = t = Param.klingpwr / 4 * (u = Game.killk);
	if (t != 0)
		printf("%d Klingons killed\t\t\t%6d\n", u, t);
	r = Now.date - Param.date;
	if (r < 1.0)
		r = 1.0;
	r = Game.killk / r;
	s += (t = 400 * r);
	if (t != 0)
		printf("Kill rate %.2f Klingons/stardate  \t%6d\n", r, t);
	r = Now.klings;
	r /= Game.killk + 1;
	s += (t = -400 * r);
	if (t != 0)
		printf("Penalty for %d klingons remaining\t%6d\n", Now.klings, t);
	if (Move.endgame > 0)
	{
		s += (t = 100 * (u = Game.skill));
		printf("Bonus for winning a %s%s game\t\t%6d\n", Skitab[u - 1].abrev, Skitab[u - 1].full, t);
	}
	if (Game.killed)
	{
		s -= 500;
		printf("Penalty for getting killed\t\t  -500\n");
	}
	s += (t = -100 * (u = Game.killb));
	if (t != 0)
		printf("%d starbases killed\t\t\t%6d\n", u, t);
	s += (t = -100 * (u = Game.helps));
	if (t != 0)
		printf("%d calls for help\t\t\t%6d\n", u, t);
	s += (t = -5 * (u = Game.kills));
	if (t != 0)
		printf("%d stars destroyed\t\t\t%6d\n", u, t);
	s += (t = -150 * (u = Game.killinhab));
	if (t != 0)
		printf("%d inhabited starsystems destroyed\t%6d\n", u, t);
	if (Ship.ship != ENTERPRISE)
	{
		s -= 200;
		printf("penalty for abandoning ship\t\t  -200\n");
	}
	s += (t = 3 * (u = Game.captives));
	if (t != 0)
		printf("%d Klingons captured\t\t\t%6d\n", u, t);
	s += (t = -(u = Game.deaths));
	if (t != 0)
		printf("%d casualties\t\t\t\t%6d\n", u, t);
	printf("\n***  TOTAL\t\t\t%14ld\n", s);
	return (s);
}
