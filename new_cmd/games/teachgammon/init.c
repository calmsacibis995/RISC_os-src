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
#ident	"$Header: init.c,v 1.1.2.2 90/05/10 03:28:25 wje Exp $"

static char sccsid[] = "	init.c	4.1	82/05/11	";

#include <sgtty.h>

/*
 * variable initialization.
 */

				/* name of executable object programs */
char	EXEC[] = "/usr/games/backgammon";
char	TEACH[] = "/usr/games/teachgammon";

int	pnum	= 2;		/* color of player:
					-1 = white
					 1 = red
					 0 = both
					 2 = not yet init'ed */
int	acnt	= 2;		/* length of args */
int	aflag	= 1;		/* flag to ask for rules or instructions */
int	bflag	= 0;		/* flag for automatic board printing */
int	cflag	= 0;		/* case conversion flag */
int	hflag	= 1;		/* flag for cleaning screen */
int	mflag	= 0;		/* backgammon flag */
int	raflag	= 0;		/* 'roll again' flag for recovered game */
int	rflag	= 0;		/* recovered game flag */
int	tflag	= 0;		/* cursor addressing flag */
int	iroll	= 0;		/* special flag for inputting rolls */
int	rfl	= 0;

char	*color[] = {"White","Red","white","red"};
