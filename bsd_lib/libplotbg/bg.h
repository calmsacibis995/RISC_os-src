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
/* $Header: bg.h,v 1.1.2.2 90/05/09 14:21:11 wje Exp $ */

/*
 * Copyright (c) 1980 Regents of the University of California.
 * All rights reserved.  The Berkeley software License Agreement
 * specifies the terms and conditions for redistribution.
 *
 *	@(#)bg.h	5.1 (Berkeley) 4/30/85
 */

/*
 * Displays plot files on a bbn bitgraph terminal.
 */

#include <stdio.h>
#include <math.h>

#define ESC	033
#define PI	3.141592659

/* The graphics address range is 0..XMAX, 0..YMAX. */
#define XMAX	768
#define YMAX	1024
#define scaleX(xi)	((int) ((xi - lowx)*scale +0.5))
#define scaleY(yi)	((int) ((yi - lowy)*scale +0.5))

extern int currentx;
extern int currenty;
extern double lowx;
extern double lowy;
extern double scale;
