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
#ident	"$Header: init_disp.c,v 1.1.2.2 90/05/07 19:33:58 wje Exp $"

/*
 * Initialization code for the display package,
 * as well as the signal handling routines.
 */

#include "talk.h"
#include <signal.h>

/* 
 * Set up curses, catch the appropriate signals,
 * and build the various windows.
 */
init_display()
{
	void sig_sent();
	struct sigvec sigv;

	initscr();
	(void) sigvec(SIGTSTP, (struct sigvec *)0, &sigv);
	sigv.sv_mask |= sigmask(SIGALRM);
	(void) sigvec(SIGTSTP, &sigv, (struct sigvec *)0);
	curses_initialized = 1;
	clear();
	refresh();
	noecho();
	crmode();
	signal(SIGINT, sig_sent);
	signal(SIGPIPE, sig_sent);
	/* curses takes care of ^Z */
	my_win.x_nlines = LINES / 2;
	my_win.x_ncols = COLS;
	my_win.x_win = newwin(my_win.x_nlines, my_win.x_ncols, 0, 0);
	scrollok(my_win.x_win, FALSE);
	wclear(my_win.x_win);

	his_win.x_nlines = LINES / 2 - 1;
	his_win.x_ncols = COLS;
	his_win.x_win = newwin(his_win.x_nlines, his_win.x_ncols,
	    my_win.x_nlines+1, 0);
	scrollok(his_win.x_win, FALSE);
	wclear(his_win.x_win);

	line_win = newwin(1, COLS, my_win.x_nlines, 0);
	box(line_win, '-', '-');
	wrefresh(line_win);
	/* let them know we are working on it */
	current_state = "No connection yet";
}

/*
 * Trade edit characters with the other talk. By agreement
 * the first three characters each talk transmits after
 * connection are the three edit characters.
 */
set_edit_chars()
{
	char buf[3];
	int cc;
	struct sgttyb tty;
	struct ltchars ltc;
	
	ioctl(0, TIOCGETP, &tty);
	ioctl(0, TIOCGLTC, (struct sgttyb *)&ltc);
	my_win.cerase = tty.sg_erase;
	my_win.kill = tty.sg_kill;
	if (ltc.t_werasc == (char) -1)
		my_win.werase = '\027';	 /* control W */
	else
		my_win.werase = ltc.t_werasc;
	buf[0] = my_win.cerase;
	buf[1] = my_win.kill;
	buf[2] = my_win.werase;
	cc = write(sockt, buf, sizeof(buf));
	if (cc != sizeof(buf) )
		p_error("Lost the connection");
	cc = read(sockt, buf, sizeof(buf));
	if (cc != sizeof(buf) )
		p_error("Lost the connection");
	his_win.cerase = buf[0];
	his_win.kill = buf[1];
	his_win.werase = buf[2];
}

void
sig_sent()
{

	message("Connection closing. Exiting");
	quit();
}

/*
 * All done talking...hang up the phone and reset terminal thingy's
 */
quit()
{

	if (curses_initialized) {
		wmove(his_win.x_win, his_win.x_nlines-1, 0);
		wclrtoeol(his_win.x_win);
		wrefresh(his_win.x_win);
		endwin();
	}
	if (invitation_waiting)
		send_delete();
	exit(0);
}
