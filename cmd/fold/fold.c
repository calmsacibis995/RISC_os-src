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
#ident	"$Header: fold.c,v 1.6.2.2 90/05/09 15:51:25 wje Exp $"

/*	Copyright (c) 1984 AT&T	*/
/*	  All Rights Reserved  	*/

/*	THIS IS UNPUBLISHED PROPRIETARY SOURCE CODE OF AT&T	*/
/*	The copyright notice above does not evidence any   	*/
/*	actual or intended publication of such source code.	*/

#include <stdio.h>
/*
 * fold - fold long lines for finite output devices
 *
 */

int	fold =  80;

main(argc, argv)
	int argc;
	char *argv[];
{
	register c;
	FILE *f;
	char obuf[BUFSIZ];

	argc--, argv++;
	setbuf(stdout, obuf);
	if (argc > 0 && argv[0][0] == '-') {
		fold = 0;
		argv[0]++;
		while (*argv[0] >= '0' && *argv[0] <= '9')
			fold *= 10, fold += *argv[0]++ - '0';
		if (*argv[0]) {
			fprintf(stderr, "Usage: fold [-width] [file...]\n");
			exit(2);
		}
		argc--, argv++;
	}
	do {
		if (argc > 0) {
			if (freopen(argv[0], "r", stdin) == NULL) {
				perror(argv[0]);
				exit(1);
			}
			argc--, argv++;
		}
		for (;;) {
			c = getc(stdin);
			if (c == -1)
				break;
			putch(c);
		}
	} while (argc > 0);
	exit(0);
}

int	col;

putch(c)
	register c;
{
	register ncol;

	switch (c) {
		case '\n':
			ncol = 0;
			break;
		case '\t':
			ncol = (col + 8) &~ 7;
			break;
		case '\b':
			ncol = col ? col - 1 : 0;
			break;
		case '\r':
			ncol = 0;
			break;
		default:
			ncol = col + 1;
	}
	if (ncol > fold)
		putchar('\n'), col = 0;
	putchar(c);
	switch (c) {
		case '\n':
			col = 0;
			break;
		case '\t':
			col += 8;
			col &= ~7;
			break;
		case '\b':
			if (col)
				col--;
			break;
		case '\r':
			col = 0;
			break;
		default:
			col++;
			break;
	}
}
