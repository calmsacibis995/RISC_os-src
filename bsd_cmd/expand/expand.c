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
#ident	"$Header: expand.c,v 1.2.1.2 90/05/07 18:31:31 wje Exp $"

/*
 * Copyright (c) 1980 Regents of the University of California.
 * All rights reserved.  The Berkeley software License Agreement
 * specifies the terms and conditions for redistribution.
 */

#ifndef lint
char copyright[] =
"@(#) Copyright (c) 1980 Regents of the University of California.\n\
 All rights reserved.\n";
#endif not lint

#ifndef lint
static char sccsid[] = "@(#)expand.c	5.1 (Berkeley) 5/31/85";
#endif not lint

#include <stdio.h>
/*
 * expand - expand tabs to equivalent spaces
 */
int	nstops;
int	tabstops[100];

main(argc, argv)
	int argc;
	char *argv[];
{
	register int c, column;
	register int n;

	argc--, argv++;
	do {
		while (argc > 0 && argv[0][0] == '-') {
			getstops(argv[0]);
			argc--, argv++;
		}
		if (argc > 0) {
			if (freopen(argv[0], "r", stdin) == NULL) {
				perror(argv[0]);
				exit(1);
			}
			argc--, argv++;
		}
		column = 0;
		for (;;) {
			c = getc(stdin);
			if (c == -1)
				break;
			switch (c) {

			case '\t':
				if (nstops == 0) {
					do {
						putchar(' ');
						column++;
					} while (column & 07);
					continue;
				}
				if (nstops == 1) {
					do {
						putchar(' ');
						column++;
					} while (((column - 1) % tabstops[0]) != (tabstops[0] - 1));
					continue;
				}
				for (n = 0; n < nstops; n++)
					if (tabstops[n] > column)
						break;
				if (n == nstops) {
					putchar(' ');
					column++;
					continue;
				}
				while (column < tabstops[n]) {
					putchar(' ');
					column++;
				}
				continue;

			case '\b':
				if (column)
					column--;
				putchar('\b');
				continue;

			default:
				putchar(c);
				column++;
				continue;

			case '\n':
				putchar(c);
				column = 0;
				continue;
			}
		}
	} while (argc > 0);
	exit(0);
}

getstops(cp)
	register char *cp;
{
	register int i;

	nstops = 0;
	cp++;
	for (;;) {
		i = 0;
		while (*cp >= '0' && *cp <= '9')
			i = i * 10 + *cp++ - '0';
		if (i <= 0 || i > 256) {
bad:
			fprintf(stderr, "Bad tab stop spec\n");
			exit(1);
		}
		if (nstops > 0 && i <= tabstops[nstops-1])
			goto bad;
		tabstops[nstops++] = i;
		if (*cp == 0)
			break;
		if (*cp++ != ',')
			goto bad;
	}
}
