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
#ident	"$Header: stdhosts.c,v 1.1.1.2 90/05/09 19:31:47 wje Exp $"
#ifndef lint
static char sccsid[] = 	"@(#)stdhosts.c	1.1 88/03/07 4.0NFSSRC Copyr 1988 Sun Micro";
#endif

/*
 * Copyright (c) 1985 by Sun Microsystems, Inc.
 */

#include <stdio.h>
#include <sys/types.h>
#include <netinet/in.h>

/* 
 * Filter to convert addresses in /etc/hosts file to standard form
 */

main(argc, argv)
	char **argv;
{
	char line[256];
	char adr[256];
	char *any(), *trailer;
	extern char *inet_ntoa();
	extern u_long inet_addr();
	FILE *fp;
	
	if (argc > 1) {
		fp = fopen(argv[1], "r");
		if (fp == NULL) {
			fprintf(stderr, "stdhosts: can't open %s\n", argv[1]);
			exit(1);
		}
	}
	else
		fp = stdin;
	while (fgets(line, sizeof(line), fp)) {
		struct in_addr in;

		if (line[0] == '#')
			continue;
		if ((trailer = any(line, " \t")) == NULL)
			continue;
		sscanf(line, "%s", adr);
		in.s_addr = inet_addr(adr);
		fputs(inet_ntoa(in), stdout);
		fputs(trailer, stdout);
	}
}

/* 
 * scans cp, looking for a match with any character
 * in match.  Returns pointer to place in cp that matched
 * (or NULL if no match)
 */
static char *
any(cp, match)
	register char *cp;
	char *match;
{
	register char *mp, c;

	while (c = *cp) {
		for (mp = match; *mp; mp++)
			if (*mp == c)
				return (cp);
		cp++;
	}
	return (NULL);
}
