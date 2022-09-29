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
/* $Header: grnplot.h,v 1.1.1.2 90/05/09 14:35:23 wje Exp $ */
/*
 * Copyright (c) 1980, 1986 Regents of the University of California.
 * All rights reserved.  The Berkeley software License Agreement
 * specifies the terms and conditions for redistribution.
 *
 *	@(#)grnplot.h	6.1 (Berkeley) 8/29/86
 *	modified to grnplot by Brad Rubenstein 8/29/86
 */

/*
 * Given a plot file, produces a grn file
 */

#include <stdio.h>

extern curx, cury;		/* Current screen position */
extern int xbot, ybot;		/* Coordinates of screen lower-left corner */
extern double scale;		/* The number of pixels per 2**12 units
				 * of world coordinates.
				 */
extern int linestyle;
extern int invector, ingrnfile;

#define FONTSIZE 1
#define FONTSTYLE 1
#define DEFAULTLINE 5
#define POINTSTRING "."

/* The following variables describe the screen. */

#define GRXMAX	512	/* Maximum x-coordinate of screen */
#define GRYMAX	512	/* Maximum y-coordinate of screen */
