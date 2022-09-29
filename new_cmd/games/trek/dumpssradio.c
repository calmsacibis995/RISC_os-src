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
#ident	"$Header: dumpssradio.c,v 1.1.2.2 90/05/10 03:33:03 wje Exp $"

/*
 * Copyright (c) 1980 Regents of the University of California.
 * All rights reserved.  The Berkeley software License Agreement
 * specifies the terms and conditions for redistribution.
 */

#ifndef lint
static char sccsid[] = "@(#)dumpssradio.c	5.1 (Berkeley) 5/30/85";
#endif not lint

# include	"trek.h"

/**
 **	output hidden distress calls
 **/

dumpssradio()
{
	register struct event	*e;
	register int		j;
	register int		chkrest;

	chkrest = 0;
	for (j = 0; j < MAXEVENTS; j++)
	{
		e = &Event[j];
		/* if it is not hidden, then just ignore it */
		if ((e->evcode & E_HIDDEN) == 0)
			continue;
		if (e->evcode & E_GHOST)
		{
			unschedule(e);
			printf("Starsystem %s in quadrant %d,%d is no longer distressed\n",
				systemname(e), e->x, e->y);
			continue;
		}

		switch (e->evcode)
		{

		  case E_KDESB:
			printf("Starbase in quadrant %d,%d is under attack\n",
				e->x, e->y);
			chkrest++;
			break;

		  case E_ENSLV:
		  case E_REPRO:
			printf("Starsystem %s in quadrant %d,%d is distressed\n",
				systemname(e), e->x, e->y);
			chkrest++;
			break;

		}
	}

	return (chkrest);
}
