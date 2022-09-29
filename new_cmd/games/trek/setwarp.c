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
#ident	"$Header: setwarp.c,v 1.1.2.2 90/05/10 03:37:00 wje Exp $"

/*
 * Copyright (c) 1980 Regents of the University of California.
 * All rights reserved.  The Berkeley software License Agreement
 * specifies the terms and conditions for redistribution.
 */

#ifndef lint
static char sccsid[] = "@(#)setwarp.c	5.1 (Berkeley) 5/30/85";
#endif not lint

# include	"trek.h"
# include	"getpar.h"

/*
**  SET WARP FACTOR
**
**	The warp factor is set for future move commands.  It is
**	checked for consistancy.
*/

setwarp()
{
	double	warpfac;

	warpfac = getfltpar("Warp factor");
	if (warpfac < 0.0)
		return;
	if (warpfac < 1.0)
		return (printf("Minimum warp speed is 1.0\n"));
	if (warpfac > 10.0)
		return (printf("Maximum speed is warp 10.0\n"));
	if (warpfac > 6.0)
		printf("Damage to warp engines may occur above warp 6.0\n");
	Ship.warp = warpfac;
	Ship.warp2 = Ship.warp * warpfac;
	Ship.warp3 = Ship.warp2 * warpfac;
}
