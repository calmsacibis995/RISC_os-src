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
#ident	"$Header: open.c,v 1.1.2.2 90/05/07 21:49:42 wje Exp $"

/*
 * Copyright (c) 1980 Regents of the University of California.
 * All rights reserved.  The Berkeley software License Agreement
 * specifies the terms and conditions for redistribution.
 */

#ifndef lint
static char sccsid[] = "@(#)open.c	5.1 (Berkeley) 5/7/85";
#endif not lint

/*
 * Displays plot files on an HP7221 plotter.
 * Cloned from bgplot.c and gigiplot.c by Jim Kleckner
 * Thu Jun 30 13:35:04 PDT 1983
 *  Requires a handshaking program such as hp7221cat to get
 *  the plotter open and ready.
 */

#include <signal.h>
#include "hp7221.h"

int currentx = 0;
int currenty = 0;
double lowx = 0.0;
double lowy = 0.0;
double scale = 1.0;

openpl()
{
	int closepl();

	/* catch interupts */
	signal(SIGINT, closepl);
	currentx = 0;
	currenty = 0;
	printf( "~VR~W" );
	putMBP( 800, 2000 );
	putMBP( 7600, 9600 );
	printf( "~S" );
	putMBP( XMAX, YMAX );
	printf( "vA~*z" );

	space(0,0,XMAX,YMAX);
}
