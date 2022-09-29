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
#ident	"$Header: open.c,v 1.1.2.2 90/05/07 21:47:38 wje Exp $"

/*
 * Copyright (c) 1980 Regents of the University of California.
 * All rights reserved.  The Berkeley software License Agreement
 * specifies the terms and conditions for redistribution.
 */

#ifndef lint
static char sccsid[] = "@(#)open.c	5.1 (Berkeley) 5/7/85";
#endif not lint

#include <sgtty.h>
#include "hp2648.h"

int shakehands;
int currentx;
int currenty;
int buffcount;
int fildes;
float lowx;
float lowy;
float scalex;
float scaley;
struct sgttyb sarg;

openpl()
{
	if ( isatty(fileno( stdout )) ) {
		shakehands = TRUE;
		fildes = open(TERMINAL, 0);
		gtty(fildes, &sarg);
		sarg.sg_flags = sarg.sg_flags | RAW;
		stty(fildes, &sarg);
		sarg.sg_flags = sarg.sg_flags & ~RAW;
	}
	else {
		shakehands = FALSE;
	}
	buffcount = 0;
	currentx = 0;
	currenty = 0;
	buffready(8);
	putchar(ESC);
	putchar(GRAPHIC);
	putchar(DISPLAY);
	putchar('c');
	putchar(ESC);
	putchar(GRAPHIC);
	putchar(PLOT);
	putchar(BINARY);
	space(0,0,720,360);
}
