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
#ident	"$Header: cmp.c,v 1.7.2.2 90/05/09 15:23:17 wje Exp $"

/*	Copyright (c) 1984 AT&T	*/
/*	  All Rights Reserved  	*/

/*	THIS IS UNPUBLISHED PROPRIETARY SOURCE CODE OF AT&T	*/
/*	The copyright notice above does not evidence any   	*/
/*	actual or intended publication of such source code.	*/


/*
 *	compare two files 
 */

#include	<stdio.h>
#include	<ctype.h>

FILE           *file1, *file2;

int             eflg;

#define		L_OFF		0
#define		L_SHORT		1
#define		L_LONG		2
int             lflg = L_SHORT;

long            line = 1;
long            chr = 0;
long            skip1;
long            skip2;

char           *name1;
char           *name2;

main(argc, argv)
	char          **argv;
{
	register        c1, c2;
	extern int      optind;
	int             c;

	while ((c = getopt(argc, argv, "sl")) != EOF) {
		switch (c) {

		case 's':
			lflg = L_OFF;
			break;

		case 'l':
			lflg = L_LONG;
			break;

		default:
			usage();
			break;

		}
	}

	if (argc - optind < 2 || argc - optind > 5) {
		usage();
	}

	name1 = argv[optind];
	if (name1[0] == '-' && name1[1] == 0) {
		file1 = stdin;
		name1 = "(standard input)";
	} else {
		if ((file1 = fopen(name1, "r")) == NULL) {
			badfile(name1);
		}
	}

	name2 = argv[optind + 1];
	if ((file2 = fopen(name2, "r")) == NULL) {
		badfile(name2);
	}
	if (argc - optind >= 3) {
		skip1 = otoi(argv[optind + 2]);
	}
	if (argc - optind == 4) {
		skip2 = otoi(argv[optind + 3]);
	}
	while (skip1) {
		if ((c1 = getc(file1)) == EOF) {
			end_file(name2);
		}
		skip1--;
	}
	while (skip2) {
		if ((c2 = getc(file2)) == EOF) {
			end_file(name2);
		}
		skip2--;
	}

	for (;;) {
		chr++;
		c1 = getc(file1);
		c2 = getc(file2);
		if (c1 == c2) {
			if (c1 == '\n') {
				line++;
			}
			if (c1 == EOF) {
				exit(eflg);
			}
			continue;
		}
		if (lflg == L_OFF) {
			exit(1);
		}
		if (c1 == EOF) {
			end_file(name1);
		}
		if (c2 == EOF) {
			end_file(name2);
		}
		if (lflg == L_SHORT) {
			printf("%s %s differ: char %ld, line %ld\n", name1, name2,
			       chr, line);
			exit(1);
		}
		eflg = 1;
		printf("%6ld %3o %3o\n", chr, c1, c2);
	}
}

otoi(s)
	char           *s;
{
	long            v;
	int             base;

	v = 0;
	base = 10;
	if (*s == '0') {
		base = 8;
	}
	while (isdigit(*s)) {
		v = v * base + *s++ - '0';
	}
	return (v);
}

usage()
{
	fprintf(stderr, "usage: cmp [-l] [-s] file1 file2 [offset1 [offset2]]\n");
	exit(2);
}

badfile(name)
	char           *name;
{
	if (lflg != L_OFF) {
		fprintf(stderr, "cmp: ");
		perror(name);
	}
	exit(2);
}

end_file(name)
	char           *name;
{
	fprintf(stderr, "cmp: EOF on %s\n", name);
	exit(1);
}
