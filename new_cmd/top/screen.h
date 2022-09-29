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
/* $Header: screen.h,v 1.1.1.2 90/05/10 03:41:51 wje Exp $ */
/*
 *  top - a top users display for Unix 4.2
 *
 *  This file contains all the definitions necessary to use the hand-written
 *  screen package in "screen.c"
 */

#define TCputs(str)	tputs(str, 1, putstdout)
#define putcap(str)	((str) != NULL ? TCputs(str) : 0)
#define Move_to(x, y)	TCputs(tgoto(cursor_motion, x, y))

extern char ch_erase;		/* set to the user's erase character */
extern char ch_kill;		/* set to the user's kill  character */
extern char smart_terminal;     /* set if the terminal has sufficient termcap
				   capabilities for normal operation */

/* These aresome termcap strings for use outside of "screen.c" */
extern char *cursor_motion;
extern char *clear_line;

/* rows and columns on the screen according to termcap */
extern int  screen_length;
extern int  screen_width;

/* a function that puts a single character on stdout */
int putstdout();
