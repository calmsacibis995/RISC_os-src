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
#ident	"$Header: dumpgame.c,v 1.1.2.2 90/05/10 03:32:11 wje Exp $"

/*
 * Copyright (c) 1980 Regents of the University of California.
 * All rights reserved.  The Berkeley software License Agreement
 * specifies the terms and conditions for redistribution.
 */

#ifndef lint
static char sccsid[] = "@(#)dumpgame.c	4.3 (Berkeley) 1/29/86";
#endif not lint

# include	"trek.h"

/***  THIS CONSTANT MUST CHANGE AS THE DATA SPACES CHANGE ***/
# define	VERSION		2

struct dump
{
	char	*area;
	int	count;
};


struct dump	Dump_template[] =
{
	(char *)&Ship,		sizeof (Ship),
	(char *)&Now,		sizeof (Now),
	(char *)&Param,		sizeof (Param),
	(char *)&Etc,		sizeof (Etc),
	(char *)&Game,		sizeof (Game),
	(char *)Sect,		sizeof (Sect),
	(char *)Quad,		sizeof (Quad),
	(char *)&Move,		sizeof (Move),
	(char *)Event,		sizeof (Event),
	0
};

/*
**  DUMP GAME
**
**	This routine dumps the game onto the file "trek.dump".  The
**	first two bytes of the file are a version number, which
**	reflects whether this image may be used.  Obviously, it must
**	change as the size, content, or order of the data structures
**	output change.
*/

dumpgame()
{
	int			version;
	register int		fd;
	register struct dump	*d;
	register int		i;

	if ((fd = creat("trek.dump", 0644)) < 0)
		return (printf("cannot dump\n"));
	version = VERSION;
	write(fd, &version, sizeof version);

	/* output the main data areas */
	for (d = Dump_template; d->area; d++)
	{
		write(fd, &d->area, sizeof d->area);
		i = d->count;
		write(fd, d->area, i);
	}

	close(fd);
}


/*
**  RESTORE GAME
**
**	The game is restored from the file "trek.dump".  In order for
**	this to succeed, the file must exist and be readable, must
**	have the correct version number, and must have all the appro-
**	priate data areas.
**
**	Return value is zero for success, one for failure.
*/

restartgame()
{
	register int	fd;
	int		version;

	if ((fd = open("trek.dump", 0)) < 0 ||
	    read(fd, &version, sizeof version) != sizeof version ||
	    version != VERSION ||
	    readdump(fd))
	{
		printf("cannot restart\n");
		close(fd);
		return (1);
	}

	close(fd);
	return (0);
}


/*
**  READ DUMP
**
**	This is the business end of restartgame().  It reads in the
**	areas.
**
**	Returns zero for success, one for failure.
*/

readdump(fd1)
int	fd1;
{
	register int		fd;
	register struct dump	*d;
	register int		i;
	int			junk;

	fd = fd1;

	for (d = Dump_template; d->area; d++)
	{
		if (read(fd, &junk, sizeof junk) != (sizeof junk))
			return (1);
		if ((char *)junk != d->area)
			return (1);
		i = d->count;
		if (read(fd, d->area, i) != i)
			return (1);
	}

	/* make quite certain we are at EOF */
	return (read(fd, &junk, 1));
}
