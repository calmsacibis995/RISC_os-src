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
#ident	"$Header: cont.c,v 1.1.2.2 90/05/07 21:46:27 wje Exp $"

/*
 * Copyright (c) 1980 Regents of the University of California.
 * All rights reserved.  The Berkeley software License Agreement
 * specifies the terms and conditions for redistribution.
 */

#ifndef lint
static char sccsid[] = "@(#)cont.c	5.1 (Berkeley) 5/7/85";
#endif not lint

#include "hp2648.h"

cont(xi,yi)
int xi,yi;
{
	char xb1,xb2,yb1,yb2;
	itoa(xsc(xi),&xb1,&xb2);
	itoa(ysc(yi),&yb1,&yb2);
	buffready(4);
	putchar(xb2);
	putchar(xb1);
	putchar(yb2);
	putchar(yb1); 
	currentx = xsc(xi);
	currenty = ysc(yi);
}
