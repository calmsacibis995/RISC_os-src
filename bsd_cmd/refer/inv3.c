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
#ident	"$Header: inv3.c,v 1.1.1.2 90/05/07 19:11:20 wje Exp $"
#ifndef lint
static char *sccsid = "@(#)inv3.c	4.1 (Berkeley) 5/6/83";
#endif

getargs(s, arps)
char *s, *arps[];
{
	int i = 0;

	while (1)
	{
		arps[i++] = s;
		while (*s != 0 && *s!=' '&& *s != '\t')
			s++;
		if (*s == 0)
			break;
		*s++ = 0;
		while (*s==' ' || *s=='\t')
			s++;
		if (*s==0)
			break;
	}
	return(i);
}
