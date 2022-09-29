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
#ident	"$Header: check_out.c,v 1.1.2.2 90/05/10 03:31:14 wje Exp $"

/*
 * Copyright (c) 1980 Regents of the University of California.
 * All rights reserved.  The Berkeley software License Agreement
 * specifies the terms and conditions for redistribution.
 */

#ifndef lint
static char sccsid[] = "@(#)check_out.c	5.1 (Berkeley) 5/30/85";
#endif not lint

# include	"trek.h"

/*
**  CHECK IF A DEVICE IS OUT
**
**	The indicated device is checked to see if it is disabled.  If
**	it is, an attempt is made to use the starbase device.  If both
**	of these fails, it returns non-zero (device is REALLY out),
**	otherwise it returns zero (I can get to it somehow).
**
**	It prints appropriate messages too.
*/

check_out(device)
int	device;
{
	register int	dev;

	dev = device;

	/* check for device ok */
	if (!damaged(dev))
		return (0);

	/* report it as being dead */
	out(dev);

	/* but if we are docked, we can go ahead anyhow */
	if (Ship.cond != DOCKED)
		return (1);
	printf("  Using starbase %s\n", Device[dev].name);
	return (0);
}
