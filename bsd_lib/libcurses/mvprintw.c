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
#ident	"$Header: mvprintw.c,v 1.1.1.2 90/05/07 21:36:54 wje Exp $"
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
static char sccsid[] = "@(#)mvprintw.c	5.4 (Berkeley) 6/30/88";
#endif /* not lint */

# include	"curses.ext"

/*
 * implement the mvprintw commands.  Due to the variable number of
 * arguments, they cannot be macros.  Sigh....
 *
 */

mvprintw(y, x, fmt, args)
reg int		y, x;
char		*fmt;
int		args; {

	char	buf[512];

	if (move(y, x) != OK)
		return ERR;
	(void) vsprintf(buf, fmt, &args);
	return waddstr(stdscr, buf);
}

mvwprintw(win, y, x, fmt, args)
reg WINDOW	*win;
reg int		y, x;
char		*fmt;
int		args; {

	char	buf[512];

	if (move(y, x) != OK)
		return ERR;
	(void) vsprintf(buf, fmt, &args);
	return waddstr(win, buf);
}
