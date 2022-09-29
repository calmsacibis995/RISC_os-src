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
#ident	"$Header: circle.c,v 1.1.1.2 90/05/09 14:34:29 wje Exp $"

/*
 * Copyright (c) 1980, 1986 Regents of the University of California.
 * All rights reserved.  The Berkeley software License Agreement
 * specifies the terms and conditions for redistribution.
 */

#ifndef lint
static char sccsid[] = "@(#)circle.c	6.1 (Berkeley) 8/29/86";
#endif not lint


#include "grnplot.h"

/*---------------------------------------------------------
 *	Circle draws a circle.
 *
 *	Results:	None.
 *
 *	Side Effects:
 *	A circle of radius r is drawn at (x,y).
 *	The current position is set to (x,y);
 *---------------------------------------------------------
 */
circle(x, y, r)
int x, y, r;
{
	if (!ingrnfile) erase();
	endvector();
	printf("ARC\n");
	outxy(x,y);
	outxy(x+r,y);
	outxy(x,y+r);
	outxy(x,y-r);
	outxy(x+r,y);
	outxy(x-r,y);
	printf("*\n%d 0\n0\n",linestyle);
	curx=x;
	cury=y;
}
