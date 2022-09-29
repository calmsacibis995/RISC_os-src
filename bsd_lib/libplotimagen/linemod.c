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
#ident	"$Header: linemod.c,v 1.1.2.2 90/05/09 14:40:32 wje Exp $"

/*
 * Copyright (c) 1985 Regents of the University of California.
 * All rights reserved.  The Berkeley software License Agreement
 * specifies the terms and conditions for redistribution.
 */

#ifndef lint
static char sccsid[] = "@(#)linemod.c	5.3 (Berkeley) 9/21/85";
#endif not lint

#include "imp.h"

/*
 * Hack to set font.
 */
linemod(s)
char *s;
{
	register char *tit;
	register char *nam;
	int siz = 0;
	nam = s;
	for(tit = "charset="; *tit; )
		if (*tit++ != *nam++)
			return;
	s = nam;
	while(*nam) 
		switch(*nam++) {
		case ',':
		case '\n':
			*--nam = 0;
		}
	siz = atoi(++nam);
	if (siz == 0) {
		while (*--nam >= '0' && *nam <= '9')
			;
		siz = (atoi(++nam)*4)/3;
	}
	if (siz == 0)
		siz = imPcsize;
	setfont(s, siz);
}
