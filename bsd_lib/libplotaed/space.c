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
#ident	"$Header: space.c,v 1.1.2.2 90/05/07 21:52:13 wje Exp $"

/*
 * Copyright (c) 1980 Regents of the University of California.
 * All rights reserved.  The Berkeley software License Agreement
 * specifies the terms and conditions for redistribution.
 */

#ifndef lint
static char sccsid[] = "@(#)space.c	5.2 (Berkeley) 4/30/85";
#endif not lint


#include "aed.h"

/*---------------------------------------------------------
 *	Space sets up the world-to-screen transformation so
 *	that the rectangular area described by (x0, y0) and
 *	(x1, y1) will all be on-screen.
 *
 *	Results:	None.
 *
 *	Side Effects:
 *	Our own variables scale, xbot, and ybot are changed.
 *---------------------------------------------------------
 */
space(x0, y0, x1, y1)
int x0, y0, x1, y1;
{
    int xscale, yscale, xsize, ysize;
    xscale = (GRXMAX<<12)/(x1-x0);
    yscale = (GRYMAX<<12)/(y1-y0);
    if (xscale > yscale) scale = yscale;
    else scale = xscale;
    scale = (scale*9)/10 - 1;
    if (scale<1) scale = 1;
    xsize = (2048*GRXMAX)/scale + 1;
    xbot = (x1+x0)/2 - xsize;
    ysize = (2048*GRYMAX)/scale + 1;
    ybot = (y1+y0)/2 - ysize;
}
