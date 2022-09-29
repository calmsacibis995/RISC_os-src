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
#ident	"$Header: mesg.c,v 1.3.1.2 90/05/07 18:52:52 wje Exp $"

/*
 * Copyright (c) 1987 Regents of the University of California.
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
char copyright[] =
"@(#) Copyright (c) 1987 Regents of the University of California.\n\
 All rights reserved.\n";
#endif /* not lint */

#ifndef lint
static char sccsid[] = "@(#)mesg.c	4.5 (Berkeley) 6/29/88";
#endif /* not lint */

/*
 * mesg -- set current tty to accept or
 *	forbid write permission.
 *
 *	mesg [y] [n]
 *		y allow messages
 *		n forbid messages
 */

#include <sys/types.h>
#include <sys/stat.h>
#include <stdio.h>

static char *tty;

main(argc, argv)
	int argc;
	char **argv;
{
	struct stat sbuf;
	char *ttyname();

	if (!(tty = ttyname(2))) {
		fputs("mesg: not a device in /dev.\n", stderr);
		exit(2);
	}
	if (stat(tty, &sbuf) < 0) {
		perror("mesg");
		exit(2);
	}
	if (argc < 2) {
		if (sbuf.st_mode & 020) {
			fputs("is y\n", stderr);
			exit(0);
		}
		fputs("is n\n", stderr);
		exit(1);
	}
#define	OTHER_WRITE	020
	switch(*argv[1]) {
	case 'y':
		newmode(sbuf.st_mode | OTHER_WRITE);
		exit(0);
	case 'n':
		newmode(sbuf.st_mode &~ OTHER_WRITE);
		exit(1);
	default:
		fputs("usage: mesg [y] [n]\n", stderr);
		exit(2);
	}
	/*NOTREACHED*/
}

static
newmode(m)
	u_short m;
{
	if (chmod(tty, m) < 0) {
		perror("mesg");
		exit(2);
	}
}
