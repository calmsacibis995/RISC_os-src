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
#ident	"$Header: play.c,v 1.1.2.2 90/05/10 03:35:38 wje Exp $"

/*
 * Copyright (c) 1980 Regents of the University of California.
 * All rights reserved.  The Berkeley software License Agreement
 * specifies the terms and conditions for redistribution.
 */

#ifndef lint
static char sccsid[] = "@(#)play.c	5.1 (Berkeley) 1/29/86";
#endif not lint

# include	"trek.h"
# include	"getpar.h"

/*
**  INSTRUCTION READ AND MAIN PLAY LOOP
**
**	Well folks, this is it.  Here we have the guts of the game.
**	This routine executes moves.  It sets up per-move variables,
**	gets the command, and executes the command.  After the command,
**	it calls events() to use up time, attack() to have Klingons
**	attack if the move was not free, and checkcond() to check up
**	on how we are doing after the move.
*/
extern int	abandon(), capture(), shield(), computer(), dcrept(),
		destruct(), dock(), help(), impulse(), lrscan(),
		warp(), dumpgame(), rest(), shell(), srscan(),
		reset(), torped(), visual(), setwarp(), undock(), phaser();

struct cvntab	Comtab[] =
{
	"abandon",		"",			abandon,	0,
	"ca",			"pture",		capture,	0,
	"cl",			"oak",			shield,	-1,
	"c",			"omputer",		computer,	0,
	"da",			"mages",		dcrept,	0,
	"destruct",		"",			destruct,	0,
	"do",			"ck",			dock,		0,
	"help",			"",			help,		0,
	"i",			"mpulse",		impulse,	0,
	"l",			"rscan",		lrscan,	0,
	"m",			"ove",			warp,		0,
	"p",			"hasers",		phaser,	0,
	"ram",			"",			warp,		1,
	"dump",			"",			dumpgame,	0,
	"r",			"est",			rest,		0,
	"shell",		"",			shell,		0,
	"sh",			"ield",			shield,	0,
	"s",			"rscan",		srscan,	0,
	"st",			"atus",			srscan,	-1,
	"terminate",		"",			reset,		0,
	"t",			"orpedo",		torped,	0,
	"u",			"ndock",		undock,	0,
	"v",			"isual",		visual,	0,
	"w",			"arp",			setwarp,	0,
	0
};

play()
{
	struct cvntab		*r;

	while (1)
	{
		Move.free = 1;
		Move.time = 0.0;
		Move.shldchg = 0;
		Move.newquad = 0;
		Move.resting = 0;
		skiptonl(0);
		r = getcodpar("\nCommand", Comtab);
		(*r->value)(r->value2);
		events(0);
		attack(0);
		checkcond();
	}
}

int
reset()
{
	longjmp(setexit_place, 1);
}
