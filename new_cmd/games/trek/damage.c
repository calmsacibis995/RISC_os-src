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
#ident	"$Header: damage.c,v 1.1.2.2 90/05/10 03:31:40 wje Exp $"

/*
 * Copyright (c) 1980 Regents of the University of California.
 * All rights reserved.  The Berkeley software License Agreement
 * specifies the terms and conditions for redistribution.
 */

#ifndef lint
static char sccsid[] = "@(#)damage.c	5.1 (Berkeley) 5/30/85";
#endif not lint

# include	"trek.h"

/*
**  Schedule Ship.damages to a Device
**
**	Device `dev1' is damaged in an amount `dam'.  Dam is measured
**	in stardates, and is an additional amount of damage.  It should
**	be the amount to occur in non-docked mode.  The adjustment
**	to docked mode occurs automatically if we are docked.
**
**	Note that the repair of the device occurs on a DATE, meaning
**	that the dock() and undock() have to reschedule the event.
*/

damage(dev1, dam)
int	dev1;		/*  device index */
double	dam;		/* time to repair */
{
	register int		i;
	register struct event	*e;
	int			f;
	register int		dev;

	/* ignore zero damages */
	if (dam <= 0.0)
		return;
	dev = dev1;

	printf("\t%s damaged\n", Device[dev].name);

	/* find actual length till it will be fixed */
	if (Ship.cond == DOCKED)
		dam *= Param.dockfac;
	/* set the damage flag */
	f = damaged(dev);
	if (!f)
	{
		/* new damages -- schedule a fix */
		schedule(E_FIXDV, dam, 0, 0, dev);
		return;
	}
	/* device already damaged -- add to existing damages */
	/* scan for old damages */
	for (i = 0; i < MAXEVENTS; i++)
	{
		e = &Event[i];
		if (e->evcode != E_FIXDV || e->systemname != dev)
			continue;
		/* got the right one; add on the new damages */
		reschedule(e, e->date - Now.date + dam);
		return;
	}
	syserr("Cannot find old damages %d\n", dev);
}
