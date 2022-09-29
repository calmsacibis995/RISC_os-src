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
#ident	"$Header: comp.c,v 1.1.2.2 90/05/10 03:02:45 wje Exp $"

/*
 * Copyright (c) 1980 Regents of the University of California.
 * All rights reserved.  The Berkeley software License Agreement
 * specifies the terms and conditions for redistribution.
 */

#ifndef lint
char copyright[] =
"@(#) Copyright (c) 1980 Regents of the University of California.\n\
 All rights reserved.\n";
#endif not lint

#ifndef lint
static char sccsid[] = "@(#)comp.c	5.1 (Berkeley) 5/30/85";
#endif not lint

#include <stdio.h>
#define MAX ' '

char new[MAX], old[MAX];

main ()
{
	register int i, j;
	old[0] = '\0';
	while (fgets(&new[0], MAX, stdin) != NULL) {
		for (i=0; i<MAX && old[i]==new[i]; i++);
		if (i >= MAX) {
			fprintf(stderr, "long word\n");
			exit(1);
		}
		putc(i, stdout);
		for (j=0; (old[j]=new[j]) != '\n'; j++);
		old[j] = '\0';
		fputs(&old[i], stdout);
	}
}
