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
#ident	"$Header: tc2.c,v 1.1.1.2 90/05/09 14:51:02 wje Exp $"
/*
 * Copyright (c) 1980 Regents of the University of California.
 * All rights reserved.  The Berkeley software License Agreement
 * specifies the terms and conditions for redistribution.
 */

#ifndef lint
static char sccsid[] = "@(#)tc2.c	5.1 (Berkeley) 6/5/85";
#endif not lint

/*
 * tc2 [term]
 * Dummy program to test out termlib.
 * Commands are "tcc\n" where t is type (s for string, f for flag,
 * or n for number) and cc is the name of the capability.
 */
#include <stdio.h>
char buf[1024];
char *getenv(), *tgetstr();

main(argc, argv) char **argv; {
	char *p, *q;
	int rc;
	char b[3], c;
	char area[200];

	if (argc < 2)
		p = getenv("TERM");
	else
		p = argv[1];
	rc = tgetent(buf,p);
	for (;;) {
		c = getchar();
		if (c < 0)
			exit(0);
		b[0] = getchar();
		if (b[0] < ' ')
			exit(0);
		b[1] = getchar();
		b[2] = 0;
		getchar();
		switch(c) {
			case 'f':
				printf("%s: %d\n",b,tgetflag(b));
				break;
			case 'n':
				printf("%s: %d\n",b,tgetnum(b));
				break;
			case 's':
				q = area;
				printf("%s: %s\n",b,tgetstr(b,&q));
				break;
			default:
				exit(0);
		}
	}
}
