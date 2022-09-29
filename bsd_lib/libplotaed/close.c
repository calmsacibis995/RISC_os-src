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
#ident	"$Header: close.c,v 1.1.2.2 90/05/07 21:50:54 wje Exp $"

/*
 * Copyright (c) 1980 Regents of the University of California.
 * All rights reserved.  The Berkeley software License Agreement
 * specifies the terms and conditions for redistribution.
 */

#ifndef lint
static char sccsid[] = "@(#)close.c	5.2 (Berkeley) 4/30/85";
#endif not lint


#include "aed.h"

/*---------------------------------------------------------
 *	Closepl does whatever is necessary to reset the characteristics
 *	of the AED512 after the program is finished.
 *
 *	Results:	None.
 *
 *	Side Effects:
 *	The graphics display modes are reset.
 *---------------------------------------------------------
 */
closepl()
{
    fputs("Q00204\6", stdout);
    (void) fflush(stdout);
    (void) stty(fileno(stdout), &sgttyb);
}
