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
#ident	"$Header: retest.c,v 1.1.1.2 90/05/07 19:48:30 wje Exp $"

/*
 * Copyright (c) 1980 Regents of the University of California.
 * All rights reserved.  The Berkeley software License Agreement
 * specifies the terms and conditions for redistribution.
 */

#ifndef lint
static char sccsid[] = "@(#)retest.c	5.1 (Berkeley) 6/5/85";
#endif not lint

#include <ctype.h>

int l_onecase = 0;
char * _start;
char * _escaped;
char * convexp();
char * expmatch();
main()
{
    char reg[132];
    char *ireg;
    char str[132];
    char *match;
    char matstr[132];
    char c;

    while (1) {
	printf ("\nexpr: ");
	scanf ("%s", reg);
	ireg = convexp(reg);
	match = ireg;
	while(*match) {
	    switch (*match) {

	    case '\\':
	    case '(':
	    case ')':
	    case '|':
		printf ("%c", *match);
		break;

	    default:
		if (isalnum(*match))
		    printf("%c", *match);
		else
		    printf ("<%03o>", *match);
		break;
	    }
	    match++;
	}
	printf("\n");
	getchar();
	while(1) {
	    printf ("string: ");
	    match = str;
	    while ((c = getchar()) != '\n')
		*match++ = c;
	    *match = 0;
	    if (str[0] == '#')
		break;
	    matstr[0] = 0;
	    _start = str;
	    _escaped = 0;
	    match = expmatch (str, ireg, matstr);
	    if (match == 0)
		printf ("FAILED\n");
	    else
		printf ("match\nmatstr = %s\n", matstr);
	}

    }
}
