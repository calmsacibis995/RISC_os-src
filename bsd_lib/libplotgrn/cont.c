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
#ident	"$Header: cont.c,v 1.1.1.2 90/05/09 14:34:52 wje Exp $"

/*
 * Copyright (c) 1980, 1986 Regents of the University of California.
 * All rights reserved.  The Berkeley software License Agreement
 * specifies the terms and conditions for redistribution.
 */

#ifndef lint
static char sccsid[] = "@(#)cont.c	6.1 (Berkeley) 8/29/86";
#endif not lint


#include "grnplot.h"

/*---------------------------------------------------------
 *	Cont plots a line between (curx, cury) and (x, y).
 *
 *	Results:	None.
 *	Side Effects:	As above.
 *---------------------------------------------------------
 */
cont(x, y)
int x, y;
{
    startvector();
    curx = x;
    cury = y;
    outcurxy();
}
