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
#ident	"$Header: getguess.c,v 1.2.2.2 90/05/10 03:08:32 wje Exp $"

# include	"hangman.h"

/*
 * getguess:
 *	Get another guess
 */
getguess()
{
	register int	i;
	register int	ch;
	register bool	correct;

	leaveok(stdscr, FALSE);
	for (;;) {
		move(PROMPTY, PROMPTX + sizeof "Guess: ");
		refresh();
		ch = readch();
		if (isalpha(ch)) {
			if (isupper(ch))
				ch = tolower(ch);
			if (Guessed[ch - 'a'])
				mvprintw(MESGY, MESGX, "Already guessed '%c'", ch);
			else
				break;
		}
		else if (ch == CTRL('D'))
			die();
		else
			mvprintw(MESGY, MESGX, "Not a valid guess: '%s'",
				unctrl(ch));
	}
	leaveok(stdscr, TRUE);
	move(MESGY, MESGX);
	clrtoeol();

	Guessed[ch - 'a'] = TRUE;
	correct = FALSE;
	for (i = 0; Word[i] != '\0'; i++)
		if (Word[i] == ch) {
			Known[i] = ch;
			correct = TRUE;
		}
	if (!correct)
		Errors++;
}

/*
 * readch;
 *	Read a character from the input
 */
readch()
{
	register int	cnt, r;
	auto char	ch;

	cnt = 0;
	for (;;) {
		if (read(0, &ch, sizeof ch) <= 0)
		{
			if (++cnt > 100)
				die();
		}
		else if (ch == CTRL('L')) {
			wrefresh(curscr);
			mvcur(0, 0, curscr->_cury, curscr->_curx);
		}
		else
			return ch;
	}
}
