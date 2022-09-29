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
/* $Header: dumb.h,v 1.1.2.2 90/05/09 14:27:27 wje Exp $ */

/*
 * Copyright (c) 1980 Regents of the University of California.
 * All rights reserved.  The Berkeley software License Agreement
 * specifies the terms and conditions for redistribution.
 *
 *	@(#)dumb.h	5.1 (Berkeley) 5/7/85
 *
 *
 * This accepts plot file formats and produces the appropriate plots
 * for dumb terminals.  It can also be used for printing terminals and
 * lineprinter listings, although there is no way to specify number of
 * lines and columns different from your terminal.  This would be easy
 * to change, and is left as an exercise for the reader.
 */

#include <math.h>

#define scale(x,y) y = LINES-1-(LINES*y/rangeY +minY); x = COLS*x/rangeX + minX

extern int minX, rangeX;	/* min and range of x */
extern int minY, rangeY;	/* min and range of y */
extern int currentx, currenty;
extern int COLS, LINES;

/* A very large screen! (probably should use malloc) */
#define MAXCOLS		132
#define MAXLINES	90

extern char screenmat[MAXCOLS][MAXLINES];
