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
#ident	"$Header: 0.list.c,v 1.1.1.2 90/05/07 19:20:08 wje Exp $"

#ifndef lint
static char sccsid[] = "@(#)0.list.c	4.1	(Berkeley)	2/11/83";
#endif not lint

#include <stdio.h>
#include "def.h"

struct list *consls(v,ls)		/* make list */
VERT v;
struct list *ls;
	{
	struct list *temp;
	temp = challoc(sizeof(*temp));
	temp->elt = v;
	temp->nxtlist = ls;
	return(temp);
	}

struct list *append(v,ls)		/* return ls . v */
VERT v;
struct list *ls;
	{
	struct list *temp;
	if (!ls) return(consls(v,0));
	for (temp = ls; temp -> nxtlist; temp = temp->nxtlist)
		;
	temp->nxtlist = consls(v,0);
	return(ls);
	}


freelst(ls)
struct list *ls;
	{
	if (!ls) return;
	if (ls->nxtlist)
		freelst(ls->nxtlist);
	chfree(ls,sizeof(*ls));
	}


oneelt(ls)		/* return w if w is only elt of ls, UNDEFINED otherwise */
struct list *ls;
	{
	if (!ls) return(UNDEFINED);
	if (ls->nxtlist) return(UNDEFINED);
	return(ls->elt);
	}


lslen(ls)		/* return number of elements in list ls */
struct list *ls;
	{
	int count;
	struct list *lp;
	count = 0;
	for (lp = ls; lp; lp = lp->nxtlist)
		++count;
	return(count);
	}


prlst(ls)
struct list *ls;
	{
	struct list *lp;
	for (lp = ls; lp; lp = lp->nxtlist)
		printf("%d,",lp->elt);
	fprintf(stderr,"\n");
	}
