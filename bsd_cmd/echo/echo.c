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
#ident	"$Header: echo.c,v 1.2.1.2 90/05/07 18:21:37 wje Exp $"

static char *sccsid = "@(#)echo.c	4.2 (Berkeley) 5/27/85";
#include <stdio.h>

main(argc, argv)
int argc;
char *argv[];
{
	register int i, nflg;

	nflg = 0;
	if(argc > 1 && argv[1][0] == '-' && argv[1][1] == 'n' && !argv[1][2]) {
		nflg++;
		argc--;
		argv++;
	}
	for(i=1; i<argc; i++) {
		fputs(argv[i], stdout);
		if (i < argc-1)
			putchar(' ');
	}
	if(nflg == 0)
		putchar('\n');
	exit(0);
}
