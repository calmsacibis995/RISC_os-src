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
#ident	"$Header: snova.c,v 1.1.2.2 90/05/10 03:37:24 wje Exp $"

/*
 * Copyright (c) 1980 Regents of the University of California.
 * All rights reserved.  The Berkeley software License Agreement
 * specifies the terms and conditions for redistribution.
 */

#ifndef lint
static char sccsid[] = "@(#)snova.c	5.1 (Berkeley) 5/30/85";
#endif not lint

# include	"trek.h"

/*
**  CAUSE SUPERNOVA TO OCCUR
**
**	A supernova occurs.  If 'ix' < 0, a random quadrant is chosen;
**	otherwise, the current quadrant is taken, and (ix, iy) give
**	the sector quadrants of the star which is blowing up.
**
**	If the supernova turns out to be in the quadrant you are in,
**	you go into "emergency override mode", which tries to get you
**	out of the quadrant as fast as possible.  However, if you
**	don't have enough fuel, or if you by chance run into something,
**	or some such thing, you blow up anyway.  Oh yeh, if you are
**	within two sectors of the star, there is nothing that can
**	be done for you.
**
**	When a star has gone supernova, the quadrant becomes uninhab-
**	itable for the rest of eternity, i.e., the game.  If you ever
**	try stopping in such a quadrant, you will go into emergency
**	override mode.
*/

snova(x, y)
int	x, y;
{
	int			qx, qy;
	register int		ix, iy;
	int			f;
	int			dx, dy;
	int			n;
	register struct quad	*q;

	f = 0;
	ix = x;
	if (ix < 0)
	{
		/* choose a quadrant */
		while (1)
		{
			qx = ranf(NQUADS);
			qy = ranf(NQUADS);
			q = &Quad[qx][qy];
			if (q->stars > 0)
				break;
		}
		if (Ship.quadx == qx && Ship.quady == qy)
		{
			/* select a particular star */
			n = ranf(q->stars);
			for (ix = 0; ix < NSECTS; ix++)
			{
				for (iy = 0; iy < NSECTS; iy++)
					if (Sect[ix][iy] == STAR || Sect[ix][iy] == INHABIT)
						if ((n -= 1) <= 0)
							break;
				if (n <= 0)
					break;
			}
			f = 1;
		}
	}
	else
	{
		/* current quadrant */
		iy = y;
		qx = Ship.quadx;
		qy = Ship.quady;
		q = &Quad[qx][qy];
		f = 1;
	}
	if (f)
	{
		/* supernova is in same quadrant as Enterprise */
		printf("\nRED ALERT: supernova occuring at %d,%d\n", ix, iy);
		dx = ix - Ship.sectx;
		dy = iy - Ship.secty;
		if (dx * dx + dy * dy <= 2)
		{
			printf("***  Emergency override attem");
			sleep(1);
			printf("\n");
			lose(L_SNOVA);
		}
		q->scanned = 1000;
	}
	else
	{
		if (!damaged(SSRADIO))
		{
			q->scanned = 1000;
			printf("\nUhura: Captain, Starfleet Command reports a supernova\n");
			printf("  in quadrant %d,%d.  Caution is advised\n", qx, qy);
		}
	}

	/* clear out the supernova'ed quadrant */
	dx = q->klings;
	dy = q->stars;
	Now.klings -= dx;
	if (x >= 0)
	{
		/* Enterprise caused supernova */
		Game.kills += dy;
		if (q->bases)
			killb(qx, qy, -1);
		Game.killk += dx;
	}
	else
		if (q->bases)
			killb(qx, qy, 0);
	killd(qx, qy, (x >= 0));
	q->stars = -1;
	q->klings = 0;
	if (Now.klings <= 0)
	{
		printf("Lucky devil, that supernova destroyed the last klingon\n");
		win();
	}
	return;
}
