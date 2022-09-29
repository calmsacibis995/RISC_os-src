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
#ident	"$Header: sh.print.c,v 1.3.1.2 90/05/07 18:17:06 wje Exp $"

/*
 * Copyright (c) 1980 Regents of the University of California.
 * All rights reserved.  The Berkeley Software License Agreement
 * specifies the terms and conditions for redistribution.
 */

#ifndef lint
static char *sccsid = "@(#)sh.print.c	5.4 (Berkeley) 6/6/88";
#endif

#include "sh.h"
#ifdef TERMIO
#ifdef RISCOS
#include <sysv/sys/termio.h>
#else RISCOS
#include <sysv/termio.h>
#endif RISCOS
#else TERMIO
#include <sys/ioctl.h>
#endif TERMIO

/*
 * C Shell
 */

psecs(l)
	long l;
{
	register int i;

	i = l / 3600;
	if (i) {
		printf("%d:", i);
		i = l % 3600;
		p2dig(i / 60);
		goto minsec;
	}
	i = l;
	printf("%d", i / 60);
minsec:
	i %= 60;
	printf(":");
	p2dig(i);
}

p2dig(i)
	register int i;
{

	printf("%d%d", i / 10, i % 10);
}

char	linbuf[LINELEN];
char	*linp = linbuf;

cshputchar(ch)
	register int ch;
{
	CSHPUTCHAR;
}

draino()
{

	linp = linbuf;
}

flush()
{
	register int unit;

	if (linp == linbuf)
		return;
	if (haderr)
		unit = didfds ? 2 : SHDIAG;
	else
		unit = didfds ? 1 : SHOUT;
#if defined(TERMIO) && defined(LNEW_FLUSHO)
	{
		struct termio termio;

		if (didfds == 0 &&
		    ioctl(unit, TCGETA, (char *)&termio) == 0 &&
		    termio.c_lflag & LNEW_FLUSHO) {
			termio.c_lflag &= LNEW_FLUSHO;
			(void) ioctl(unit, TCSETA, (char *)&termio);
			(void) write(unit, "\n", 1);
		};
	}
#else defined(TERMIO) && defined(LNEW_FLUSHO)
#ifdef TIOCLGET
	{
	  	int lmode;

		if (didfds == 0 && ioctl(unit, TIOCLGET, (char *)&lmode) == 0 &&
		    lmode&LFLUSHO) {
			lmode = LFLUSHO;
			(void) ioctl(unit, TIOCLBIC, (char *)&lmode);
			(void) write(unit, "\n", 1);
		};
	}
#endif
#endif defined(TERMIO) && defined(LNEW_FLUSHO)
	(void) write(unit, linbuf, linp - linbuf);
	linp = linbuf;
}
