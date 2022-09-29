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
#ident	"$Header: where_main.c,v 1.1.1.2 90/05/09 19:16:44 wje Exp $"
#ifndef lint
static char sccsid[] = 	"@(#)where_main.c	1.1 88/03/07 4.0NFSSRC Copyr 1988 Sun Micro";
#endif

#include <stdio.h>
#include <sys/param.h>

main(argc, argv)
	char **argv;
{
	char host[MAXPATHLEN];
	char fsname[MAXPATHLEN];
	char within[MAXPATHLEN];
	char *pn;
	int many;

	many = argc > 2;
	while (--argc > 0) {
		pn = *++argv;
		where(pn, host, fsname, within);
		if (many)
			printf("%s:\t", pn);
		printf("%s:%s%s\n", host, fsname, within);
	}
}

