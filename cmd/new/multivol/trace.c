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
#ident	"$Header: trace.c,v 1.2.2.2 90/05/09 18:08:48 wje Exp $"
#ifdef DEBUG	/* Only used if DEBUG is defined */
#include <stdio.h>
	int	tron;	/* global = trace on */
	char	tr[160];

strace(file, line, step)
	char	*file, *step;
	int	line;
{
	static int	indent = 0;
	register	i;

	if (*step == '-')
		indent--; 
	if (tron) {
		fprintf(stderr, "%14s: %5d: ", file, line);
		for (i = indent; i--; )
			fprintf(stderr, ".  ");
		fprintf(stderr, "%s\n", step);
	}
	if (*step == '+')
		indent++; 
}
#endif
