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
#ident	"$Header: lose.c,v 1.1.2.2 90/05/10 03:34:55 wje Exp $"

/*
 * Copyright (c) 1980 Regents of the University of California.
 * All rights reserved.  The Berkeley software License Agreement
 * specifies the terms and conditions for redistribution.
 */

#ifndef lint
static char sccsid[] = "@(#)lose.c	5.1 (Berkeley) 1/29/86";
#endif not lint

# include	"trek.h"

/*
**  PRINT OUT LOSER MESSAGES
**
**	The messages are printed out, the score is computed and
**	printed, and the game is restarted.  Oh yeh, any special
**	actions which need be taken are taken.
*/

char	*Losemsg[] =
{
	"You ran out of time",
	"You ran out of energy",
	"You have been destroyed",
	"You ran into the negative energy barrier",
	"You destroyed yourself by nova'ing that star",
	"You have been caught in a supernova",
	"You just suffocated in outer space",
	"You could not be rematerialized",
	"\n\032\014 *** Ship's hull has imploded ***",
	"You have burned up in a star",
	"Well, you destroyed yourself, but it didn't do any good",
	"You have been captured by Klingons and mercilessly tortured",
	"Your last crew member died",
};

lose(why)
int	why;
{
	Game.killed = 1;
	sleep(1);
	printf("\n%s\n", Losemsg[why - 1]);
	switch (why)
	{

	  case L_NOTIME:
		Game.killed = 0;
		break;
	}
	Move.endgame = -1;
	score();
	skiptonl(0);
	longjmp(setexit_place, 1);
}
