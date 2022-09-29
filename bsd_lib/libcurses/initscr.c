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
#ident	"$Header: initscr.c,v 1.1.1.2 90/05/07 21:36:01 wje Exp $"
/*
 * Copyright (c) 1981 Regents of the University of California.
 * All rights reserved.
 *
 * Redistribution and use in source and binary forms are permitted
 * provided that the above copyright notice and this paragraph are
 * duplicated in all such forms and that any documentation,
 * advertising materials, and other materials related to such
 * distribution and use acknowledge that the software was developed
 * by the University of California, Berkeley.  The name of the
 * University may not be used to endorse or promote products derived
 * from this software without specific prior written permission.
 * THIS SOFTWARE IS PROVIDED ``AS IS'' AND WITHOUT ANY EXPRESS OR
 * IMPLIED WARRANTIES, INCLUDING, WITHOUT LIMITATION, THE IMPLIED
 * WARRANTIES OF MERCHANTIBILITY AND FITNESS FOR A PARTICULAR PURPOSE.
 */

#ifndef lint
static char sccsid[] = "@(#)initscr.c	5.3 (Berkeley) 6/30/88";
#endif /* not lint */

# include	"curses.ext"
# include	<signal.h>

extern char	*getenv();

/*
 *	This routine initializes the current and standard screen.
 *
 */
WINDOW *
initscr() {

	reg char	*sp;
	int		tstp();
	int 		nfd;

# ifdef DEBUG
	fprintf(outf, "INITSCR()\n");
# endif
	if (My_term)
		setterm(Def_term);
	else {
		for (_tty_ch = 0; _tty_ch < nfd; _tty_ch++)
			if (isatty(_tty_ch))
				break;
		gettmode();
		if ((sp = getenv("TERM")) == NULL)
			sp = Def_term;
		setterm(sp);
# ifdef DEBUG
		fprintf(outf, "INITSCR: term = %s\n", sp);
# endif
	}
	_puts(TI);
	_puts(VS);
# ifdef SIGTSTP
	signal(SIGTSTP, tstp);
# endif
	if (curscr != NULL) {
# ifdef DEBUG
		fprintf(outf, "INITSCR: curscr = 0%o\n", curscr);
# endif
		delwin(curscr);
	}
# ifdef DEBUG
	fprintf(outf, "LINES = %d, COLS = %d\n", LINES, COLS);
# endif
	if ((curscr = newwin(LINES, COLS, 0, 0)) == ERR)
		return ERR;
	clearok(curscr, TRUE);
	curscr->_flags &= ~_FULLLINE;
	if (stdscr != NULL) {
# ifdef DEBUG
		fprintf(outf, "INITSCR: stdscr = 0%o\n", stdscr);
# endif
		delwin(stdscr);
	}
	stdscr = newwin(LINES, COLS, 0, 0);
	return stdscr;
}
