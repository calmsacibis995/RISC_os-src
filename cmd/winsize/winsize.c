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
#ident	"$Header: winsize.c,v 1.3.2.2 90/05/10 00:56:00 wje Exp $"

/*
 * winsize - Get/set window size.
 *
 * In BSD, this is part of stty, but it just doesn't fit into the
 * System V stty paradigm.
 *
 * Usage: winsize [-c] [-r] [value...]
 *
 *	-c says that all printing/setting applies only to the
 *		columns
 *	-r says that all printing/setting applies only to the
 *		rows
 *	No  or both options says that all printing/setting applies to
 *		both, and rows are given/taken before columns.
 *	values are numbers or the word "same".
 *
 * Examples (assume a 24x80 screen)
 *
 *	winsize
 *		prints: 24 80
 *	winsize -c
 *		prints: 80
 *	winsize -r 22
 *		sets number of rows to 22
 *	winsize 30 same
 *		sets number of rows to 30
 *
 */

#include <sys/types.h>
#include <stdio.h>
#include <sys/termio.h>

main(argc, argv)
	int argc;
	char *argv[];
{
	int both = 1;
	int rows = 0;
	int cols = 0;
	extern int optind;
	int c;

	if (argc == 1) {
		printsize(1, 1);
		exit(0);
	}

	while ((c = getopt(argc, argv, "cr")) != EOF) {
		switch (c) {

		case 'c':
			both = 0;
			cols = 1;
			break;

		case 'r':
			both = 0;
			rows = 1;
			break;

		default:
			usage();
			exit(2);
			break;
		}
	}

	if (both == 1) {
		rows = cols = 1;
	}

	if (optind == argc) {
		printsize(rows, cols);
		exit(0);
	}

	if (rows && cols) {
		if (optind != argc - 2) {
			fprintf(stderr, "winsize: Wrong number of values\n");
			exit(2);
		}
		exit(setsize(argv[optind], argv[optind + 1]));
	} else {
		if (optind != argc - 1) {
			fprintf(stderr, "winsize: Too many values\n");
			exit(2);
		}
		if (rows) {
			exit(setsize(argv[optind], "same"));
		} else {
			exit(setsize("same", argv[optind]));
		}
	}

	exit(0);
}

usage()
{
	fprintf(stderr, "winsize: usage: winsize [-c] [-r] [value...]\n");
}

/*
 * Print the current window size.
 */

printsize(rows, cols)
	int rows;
	int cols;
{
	struct winsize win;

	if (ioctl(0, TIOCGWINSZ, &win) < 0) {
		perror("winsize: Can't get window size");
		return;
	}

	if (rows && cols) {
		printf("%d %d\n", win.ws_row, win.ws_col);
	} else {
		printf("%d\n", rows ? win.ws_row : win.ws_col);
	}
}

/*
 * Set the window size.  If a value is "same", leave that item alone.
 *
 * Return value is 0 if all is fine, and 2 if not.
 */

setsize(srows, scols)
	char *srows;
	char *scols;
{
	int rows;
	int cols;
	struct winsize win;

	if (ioctl(0, TIOCGWINSZ, &win) < 0) {
		perror("winsize: Can't get window size");
		return;
	}

	if (strcmp(srows, "same") != 0) {
		rows = atoi(srows);
		if (rows == 0 && strcmp(srows, "0") != 0) {
			fprintf(stderr, "winsize: Non-numeric value %s\n",
				srows);
			return 2;
		}
		win.ws_row = rows;
	}

	if (strcmp(scols, "same") != 0) {
		cols = atoi(scols);
		if (cols == 0 && strcmp(scols, "0") != 0) {
			fprintf(stderr, "winsize: Non-numeric value %s\n",
				scols);
			return 2;
		}
		win.ws_col = cols;
	}

	(void) ioctl(0, TIOCSWINSZ, &win);

	return 0;
}
