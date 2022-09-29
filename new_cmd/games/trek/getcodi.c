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
#ident	"$Header: getcodi.c,v 1.1.2.2 90/05/10 03:33:26 wje Exp $"

/*
 * Copyright (c) 1980 Regents of the University of California.
 * All rights reserved.  The Berkeley software License Agreement
 * specifies the terms and conditions for redistribution.
 */

#ifndef lint
static char sccsid[] = "@(#)getcodi.c	5.1 (Berkeley) 5/30/85";
#endif not lint

# include	"getpar.h"

/*
**  get course and distance
**
**	The user is asked for a course and distance.  This is used by
**	move, impulse, and some of the computer functions.
**
**	The return value is zero for success, one for an invalid input
**	(meaning to drop the request).
*/

getcodi(co, di)
int	*co;
double	*di;
{

	*co = getintpar("Course");

	/* course must be in the interval [0, 360] */
	if (*co < 0 || *co > 360)
		return (1);
	*di = getfltpar("Distance");

	/* distance must be in the interval [0, 15] */
	if (*di <= 0.0 || *di > 15.0)
		return (1);

	/* good return */
	return (0);
}
