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
#ident	"$Header: main.c,v 1.1.2.2 90/05/10 03:08:52 wje Exp $"

# include	"hangman.h"

/*
 * This game written by Ken Arnold.
 */
main()
{
	initscr();
	signal(SIGINT, die);
	setup();
	for (;;) {
		Wordnum++;
		playgame();
		Average = (Average * (Wordnum - 1) + Errors) / Wordnum;
	}
	/* NOTREACHED */
}

/*
 * die:
 *	Die properly.
 */
die()
{
	mvcur(0, COLS - 1, LINES - 1, 0);
	endwin();
	putchar('\n');
	exit(0);
}
