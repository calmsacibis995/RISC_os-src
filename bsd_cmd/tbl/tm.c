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
#ident	"$Header: tm.c,v 1.1.2.2 90/05/07 19:37:19 wje Exp $"

#ifndef lint
static char sccsid[] = "@(#)tm.c	4.2 8/11/83";
#endif

 /* tm.c: split numerical fields */
# include "t..c"
maknew(str)
	char *str;
{
	/* make two numerical fields */
	int dpoint, c;
	char *p, *q, *ba;
	p = str;
	for (ba= 0; c = *str; str++)
		if (c == '\\' && *(str+1)== '&')
			ba=str;
	str=p;
	if (ba==0)
		{
		for (dpoint=0; *str; str++)
			{
			if (*str=='.' && !ineqn(str,p) &&
				(str>p && digit(*(str-1)) ||
				digit(*(str+1))))
					dpoint=str;
			}
		if (dpoint==0)
			for(; str>p; str--)
			{
			if (digit( * (str-1) ) && !ineqn(str, p))
				break;
			}
		if (!dpoint && p==str) /* not numerical, don't split */
			return(0);
		if (dpoint) str=dpoint;
		}
	else
		str = ba;
	p =str;
	if (exstore ==0 || exstore >exlim)
		{
		exstore = chspace();
		exlim= exstore+MAXCHS;
		}
	q = exstore;
	while (*exstore++ = *str++);
	*p = 0;
	return(q);
	}
ineqn (s, p)
	char *s, *p;
{
/* true if s is in a eqn within p */
int ineq = 0, c;
while (c = *p)
	{
	if (s == p)
		return(ineq);
	p++;
	if ((ineq == 0) && (c == delim1))
		ineq = 1;
	else
	if ((ineq == 1) && (c == delim2))
		ineq = 0;
	}
return(0);
}
