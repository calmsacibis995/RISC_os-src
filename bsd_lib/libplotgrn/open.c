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
#ident	"$Header: open.c,v 1.1.1.2 90/05/09 14:36:58 wje Exp $"

/*
 * Copyright (c) 1980, 1986 Regents of the University of California.
 * All rights reserved.  The Berkeley software License Agreement
 * specifies the terms and conditions for redistribution.
 */

#ifndef lint
static char sccsid[] = "@(#)open.c	6.1 (Berkeley) 8/29/86";
#endif not lint


/*
 * converts plot to grn
 */

#include "grnplot.h"

int curx, cury;		/* Current world position */
int xbot=0, ybot=0;	/* Coordinates of screen lower-left corner */
double scale=1;		/* The number of pixels per 2**12 units
			 * of world coordinates.
			 */
int linestyle = 5;
int	ingrnfile = 0;
int	invector = 0;


/*---------------------------------------------------------
 *	Openpl initializes the graphics display and clears its screen.
 *
 *	Results:	None.
 *
 *	Side Effects:
 *
 *	Errors:		None.
 *---------------------------------------------------------
 */
openpl()
{}
