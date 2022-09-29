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
#ident	"$Header: keyboard.c,v 1.2.1.2 90/05/07 19:30:59 wje Exp $"
/*
 * Copyright (c) 1980 Regents of the University of California.
 * All rights reserved.  The Berkeley software License Agreement
 * specifies the terms and conditions for redistribution.
 */

#ifndef lint
static char sccsid[] = "@(#)keyboard.c	5.2 (Berkeley) 12/22/87";
#endif not lint

/*
 * Keyboard input support.
 */

#include "systat.h"
#include <ctype.h>

#ifdef RISCOS

#include <sys/termio.h>

#undef CTRL
#define CTRL(x) ((x) & 0x1f)

#endif RISCOS

char	erase_char;
char	kill_char;

keyboard()
{
        char ch, line[80];
	int oldmask;

#ifdef RISCOS
	struct	termio	termio;

	ioctl(0,TCGETA,&termio);
	erase_char = termio.c_cc[VERASE];
	kill_char = termio.c_cc[VKILL];
#else RISCOS
	erase_char = _tty.sg_erase;
	kill_char = _tty.sg_kill;
#endif RISCOS
        for (;;) {
                col = 0;
                move(CMDLINE, 0);
                do {
                        refresh();
                        ch = getch() & 0177;
                        if (ch == 0177 && ferror(stdin)) {
                                clearerr(stdin);
                                continue;
                        }
                        if (ch >= 'A' && ch <= 'Z')
                                ch += 'a' - 'A';
                        if (col == 0) {
#define	mask(s)	(1 << ((s) - 1))
                                if (ch == CTRL('l')) {
#ifdef RISCOS
					sighold(SIGALRM);
#else RISCOS
					oldmask = sigblock(mask(SIGALRM));
#endif RISCOS
					wrefresh(curscr);
#ifdef RISCOS
					sigrelse(SIGALRM);
#else RISCOS
					sigsetmask(oldmask);
#endif RISCOS
                                        continue;
                                }
				if (ch == CTRL('g')) {
#ifdef RISCOS
					sighold(SIGALRM);
#else RISCOS
					oldmask = sigblock(mask(SIGALRM));
#endif RISCOS
					status();
#ifdef RISCOS
					sigrelse(SIGALRM);
#else RISCOS
					sigsetmask(oldmask);
#endif RISCOS
					continue;
				}
                                if (ch != ':')
                                        continue;
                                move(CMDLINE, 0);
                                clrtoeol();
                        }
                        if (ch == erase_char && col > 0) {
                                if (col == 1 && line[0] == ':')
                                        continue;
                                col--;
                                goto doerase;
                        }
                        if (ch == CTRL('w') && col > 0) {
                                while (--col >= 0 && isspace(line[col]))
                                        ;
                                col++;
                                while (--col >= 0 && !isspace(line[col]))
                                        if (col == 0 && line[0] == ':')
                                                break;
                                col++;
                                goto doerase;
                        }
                        if (ch == kill_char && col > 0) {
                                col = 0;
                                if (line[0] == ':')
                                        col++;
                doerase:
                                move(CMDLINE, col);
                                clrtoeol();
                                continue;
                        }
                        if (isprint(ch) || ch == ' ') {
                                line[col] = ch;
                                mvaddch(CMDLINE, col, ch);
                                col++;
                        }
                } while (col == 0 || (ch != '\r' && ch != '\n'));
                line[col] = '\0';
#ifdef RISCOS
		sighold(SIGALRM);
#else RISCOS
		oldmask = sigblock(mask(SIGALRM));
#endif RISCOS
                command(line + 1);
#ifdef RISCOS
		sigrelse(SIGALRM);
#else RISCOS
		sigsetmask(oldmask);
#endif RISCOS
        }
	/*NOTREACHED*/
}
