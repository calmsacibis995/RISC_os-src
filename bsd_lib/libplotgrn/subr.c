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
#ident	"$Header: subr.c,v 1.1.1.2 90/05/09 14:37:29 wje Exp $"

/*
 * Copyright (c) 1980, 1986 Regents of the University of California.
 * All rights reserved.  The Berkeley software License Agreement
 * specifies the terms and conditions for redistribution.
 */

#ifndef lint
static char sccsid[] = "@(#)subr.c	6.1 (Berkeley) 8/29/86";
#endif not lint


#include "grnplot.h"


/*---------------------------------------------------------
 *	This local routine outputs an x-y coordinate pair in the standard
 *	format required by the grn file.
 *
 *	Results:	None.
 *	
 *	Side Effects:
 *
 *	Errors:		None.
 *---------------------------------------------------------
 */
outxy(x, y)
int x, y;			/* The coordinates to be output.  Note:
				 * these are world coordinates, not screen
				 * ones.  We scale in this routine.
				 */
{
    printf("%.2f %.2f\n", (x - xbot)*scale,(y - ybot)*scale);
}

outcurxy()
{
	outxy(curx,cury);
}

startvector()
{
	if (!ingrnfile) erase();
	if (invector) return;
	invector = 1;
	printf("VECTOR\n");
	outcurxy();
}

endvector()
{
	if (!invector) return;
	invector = 0;
	printf("*\n%d 0\n0\n",linestyle);
}
