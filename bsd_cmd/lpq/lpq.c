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
#ident	"$Header: lpq.c,v 1.1.2.3 90/05/07 18:50:04 wje Exp $"
/*
 * Copyright (c) 1983 Regents of the University of California.
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
"@(#) Copyright (c) 1983 Regents of the University of California.\n\
 All rights reserved.\n";
#endif /* not lint */

#ifndef lint
static char sccsid[] = "@(#)lpq.c	5.5 (Berkeley) 6/30/88";
#endif /* not lint */

/*
 * Spool Queue examination program
 *
 * lpq [+[n]] [-l] [-Pprinter] [user...] [job...]
 *
 *  + means continually scan queue until empty
 * -l long output
 * -P used to identify printer as per lpr/lprm
 */

#include "lp.h"

char	*user[MAXUSERS];	/* users to process */
int	users;			/* # of users in user array */
int	requ[MAXREQUESTS];	/* job number of spool entries */
int	requests;		/* # of spool requests */

static int	repeat = 0;	/* + flag indicator */
static int	slptime = 30;	/* pause between screen refereshes */
static int	lflag = 0;	/* long output option */

/*
 * Termcap stuff for fancy display
 */
#ifdef TERMCAP
struct sgttyb sbuf;
static unsigned ospeed;
static int	dumb;		/* whether to use capabilities */
static char	PC;		/* pad character for output */
static char	*UP;		/* up one line */
static char	*BC;		/* backspace character, other than \b */
static char	*CM;		/* cursor motion */
static char	*CL;		/* clear display */
static char	*TI;		/* terminal init for CM */
static char	*TE;		/* terminal clear for CM */
static char	*SO;		/* stand out start */
static char	*SE;		/* stand out end */

char	*tgetstr();
int	putch();		/* for tputs' */
#endif

main(argc, argv)
	register int	argc;
	register char	**argv;
{
	register char *arg;
	register int n;

	name = *argv;
	if (gethostname(host, sizeof(host))) {
		perror("lpq: gethostname");
		exit(1);
	}
	openlog("lpd", 0, LOG_LPR);

	while (--argc) {
		if ((arg = *++argv)[0] == '+') {
			if (arg[1] != '\0')
				if ((n = atoi(&arg[1])) > 0)
					slptime = n;
			repeat++;
		} else if (arg[0] == '-')
			switch (arg[1]) {
			case 'P':		/* printer name */
				if (arg[2])
					printer = &arg[2];
				else if (argc > 1) {
					argc--;
					printer = *++argv;
				}
				break;

			case 'l':		/* long output */
				lflag++;
				break;

			default:
				usage();
		} else {
			if (isdigit(arg[0])) {
				if (requests >= MAXREQUESTS)
					fatal("too many requests");
				requ[requests++] = atoi(arg);
			} else {
				if (users >= MAXUSERS)
					fatal("too many users");
				user[users++] = arg;
			}
		}
	}

	if (printer == NULL && (printer = getenv("PRINTER")) == NULL)
		printer = DEFLP;
#ifdef TERMCAP
	dumb = termcap();
#endif
	if (repeat) {
#ifdef TERMCAP
		if (TI)
			tputs(TI, 0, putch);
#endif
		do {
#ifdef TERMCAP
			if (!dumb) {
				tputs(CL, 0, putch);
				tputs(tgoto(CM, 0, 0), 0, putch);
			}
#endif
			if ((n = displayq(lflag)) > 0)
				sleep(slptime);
		} while (n > 0);
#ifdef TERMCAP
		if (!dumb) {
			standout(stdout, "Hit return to continue");
			while (getchar() != '\n');
			if (TE)
				tputs(TE, 0, putch);
		}
#endif
	} else
		displayq(lflag);
	exit(0);
}

static
usage()
{
	puts("usage: lpq [-Pprinter] [-l] [+[n]] [user...] [job...]\n");
	exit(1);
}

/*
 * If we have the capability, print this in standout mode
 */
static
standout(f, s, a1, a2)
	FILE *f;
	char *s;
{
#ifdef TERMCAP
	if (SO)
		tputs(SO, 0, putch);
	fprintf(f, s, a1, a2);
	if (SO && SE)
		tputs(SE, 0, putch);
#else
	fprintf(f, s, a1, a2);
#endif
}

#ifdef TERMCAP
static char *
capstrings[] = {
	"bc", "cl", "cm", "so", "se", "ti", "te", "up",
	0
};

static char **
caps[] = {
	&BC, &CL, &CM, &SO, &SE, &TI, &TE, &UP,
};

/*
 * All we need from termcap is to clear screen and
 *   position cursor at the top; if these aren't available
 *   we say the terminal is dumb and let things scroll
 */
static
termcap()
{
	char *term, tbuf[BUFSIZ];
	static char buf[BUFSIZ/2];
	register short columns;
	char *bp = buf;
	register char **p, ***q, *cp;

	ioctl(0, TIOCGETP, (char *)&sbuf);
	ospeed = sbuf.sg_ospeed;
	if ((term = getenv("TERM")) != NULL && tgetent(tbuf, term) > 0) {
		for (p = capstrings, q = caps; *p != NULL; p++, q++)
			**q = tgetstr(*p, &bp);
		if ((cp = tgetstr("pc", &bp)) != NULL)
			PC = *cp;
	}
	return(CL == NULL || CM == NULL);
}

/*
 * Putchar writearound for tputs
 */
static
putch(c)
	char c;
{
	putchar(c);
}
#endif
