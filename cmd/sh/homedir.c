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
#ident	"$Header: homedir.c,v 1.2.2.2 90/05/09 18:57:56 wje Exp $"

/*
 * homedir.c
 *
 * find a person's login directory, for use by the shell
 *
 * Arnold Robbins (Georgia Tech)
 */

#include "defs.h"

/* validtilde --- indicate whether or not a ~ is valid */

int validtilde (start, argp)
register char *start, *argp;
{
	return ( (flags & tilde_sub_flg) && (
	start == argp - 1 ||			/* ~ at beginning of argument */
	argp[-2] == '=' ||			/* ~ after an assignment */
	(*start == '-' && argp - 3 == start)	/* in middle of an option */
						/* CSH does not do that one */
	));
}

/* homedir --- return the person's login directory */

char *homedir (person)
register char *person;
{
	register int count, i, j, fd;
	static char dir[150];
	char buf[300], name[100], rest[100];

	if (person[0] == '\0')	/* just a plain ~ */
		return (homenod.namval);
	else if (person[0] == '/')	/* e.g. ~/bin */
	{
		/* sprintf (dir, "%s%s", homenod.namval, person); */
		movstr (movstr (homenod.namval, dir), person);
		return (dir);
	}

	if ((fd = open ("/etc/passwd", 0)) < 0)
		return (nullstr);
	
	/*
	 * this stuff is to handle the ~person/bin sort of thing
	 * for catpath()
	 */
	movstr (person, name);
	*rest = '\0';
	for (i = 0; person[i]; i++)
		if (person[i] == '/')
		{
			movstr (& person[i], rest);
			name[i] = '\0';
			break;
		}

	while ((count = read (fd, buf, sizeof(buf))) > 0)
	{
		for (i = 0; i < count; i++)
			if (buf[i] == '\n')
			{
				i++;
				lseek (fd, (long) (- (count - i)), 1);
				break;
			}
		buf[i] = '\0';
		for (j = 0; name[j] && buf[j] == name[j]; j++)
			;
		if (buf[j] == ':' && name[j] == '\0')
			break;	/* found it */
	}
	if (count == 0)
	{
		close (fd);
		return (nullstr);
	}

	j--;
	for (i = 1; i <= 5; i++)
	{
		for (; buf[j] != ':'; j++)
			;
		j++;
	}
	for (i = 0; buf[j] != ':'; i++, j++)
		dir[i] = buf[j];
	if (rest[0])
		for (j = 0; rest[j]; j++)
			dir[i++] = rest[j];
	dir[i] = '\0';
	close (fd);
	return (dir);
}
