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
#ident	"$Header: stab.c,v 1.3.2.3 90/05/09 18:53:47 wje Exp $"

/*
 * Copyright (c) 1983 Eric P. Allman
 * Copyright (c) 1988 Regents of the University of California.
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
static char sccsid[] = "@(#)stab.c	5.6 (Berkeley) 1/1/89";
#endif /* not lint */

# include "sendmail.h"

/*
**  STAB -- manage the symbol table
**
**	Parameters:
**		name -- the name to be looked up or inserted.
**		type -- the type of symbol.
**		op -- what to do:
**			ST_ENTER -- enter the name if not
**				already present.
**			ST_FIND -- find it only.
**
**	Returns:
**		pointer to a STAB entry for this name.
**		NULL if not found and not entered.
**
**	Side Effects:
**		can update the symbol table.
*/

# define STABSIZE	400

static STAB	*SymTab[STABSIZE];

STAB *
stab(name, type, op)
	char *name;
	int type;
	int op;
{
	register STAB *s;
	register STAB **ps;
	register int hfunc;
	register char *p;
	extern char lower();

	if (tTd(36, 5))
		printf("STAB: %s %d ", name, type);

	/*
	**  Compute the hashing function
	**
	**	We could probably do better....
	*/

	hfunc = type;
	for (p = name; *p != '\0'; p++)
		hfunc = (((hfunc << 7) | lower(*p)) & 077777) % STABSIZE;

	if (tTd(36, 9))
		printf("(hfunc=%d) ", hfunc);

	ps = &SymTab[hfunc];
	while ((s = *ps) != NULL && (strcasecmp(name, s->s_name) || s->s_type != type))
		ps = &s->s_next;

	/*
	**  Dispose of the entry.
	*/

	if (s != NULL || op == ST_FIND)
	{
		if (tTd(36, 5))
		{
			if (s == NULL)
				printf("not found\n");
			else
			{
				long *lp = (long *) s->s_class;

				printf("type %d val %lx %lx %lx %lx\n",
					s->s_type, lp[0], lp[1], lp[2], lp[3]);
			}
		}
		return (s);
	}

	/*
	**  Make a new entry and link it in.
	*/

	if (tTd(36, 5))
		printf("entered\n");

	/* make new entry */
	s = (STAB *) xalloc(sizeof *s);
	bzero((char *) s, sizeof *s);
	s->s_name = newstr(name);
	makelower(s->s_name);
	s->s_type = type;

	/* link it in */
	*ps = s;

	return (s);
}
