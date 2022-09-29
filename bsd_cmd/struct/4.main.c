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
#ident	"$Header: 4.main.c,v 1.1.1.2 90/05/07 19:24:50 wje Exp $"

#ifndef lint
static char sccsid[] = "@(#)4.main.c	4.1	(Berkeley)	2/11/83";
#endif not lint

#include <stdio.h>
#include "def.h"
#include "4.def.h"

LOGICAL *brace;
output()
	{
	VERT w;
	int i;
	brace = challoc(nodenum * sizeof(*brace));
	for (i = 0; i < nodenum; ++i)
		brace[i] = FALSE;
	if (progress) fprintf(stderr,"ndbrace:\n");
	for (w = START; DEFINED(w); w = RSIB(w))
		ndbrace(w);
	if (progress) fprintf(stderr,"outrat:\n");
	for (w = START; DEFINED(w); w = RSIB(w))
		outrat(w,0,YESTAB);
	OUTSTR("END\n");
	chfree(brace,nodenum * sizeof(*brace));
	brace = 0;
	}
