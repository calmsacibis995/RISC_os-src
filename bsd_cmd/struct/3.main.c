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
#ident	"$Header: 3.main.c,v 1.1.1.2 90/05/07 19:24:15 wje Exp $"

#ifndef lint
static char sccsid[] = "@(#)3.main.c	4.1	(Berkeley)	2/11/83";
#endif not lint

#include <stdio.h>
#include "def.h"

structure()
	{
	VERT v, *head;

	if (progress)
		fprintf(stderr,"	getreach:\n");
	getreach();
	if (routerr) return;
	if (progress)
		fprintf(stderr,"	getflow:\n");
	getflow();
	if (progress)
		fprintf(stderr,"	getthen:\n");
	getthen(START);
	head = challoc(nodenum * sizeof(*head));
	for (v = 0; v < nodenum; ++v)
		head[v] = UNDEFINED;
	for (v = START; DEFINED(v); v = RSIB(v))
		fixhd(v,UNDEFINED,head);
			/* fixhd must be called before getloop so that
				it gets applied to IFVX which becomes NXT(w) for UNTVX w */
	if (progress)
		fprintf(stderr,"	getloop:\n");
	getloop();
	if (progress)
		fprintf(stderr,"	getbranch:\n");
	getbranch(head);
	chfree(head,nodenum * sizeof(*head));
	head = 0;
	}
