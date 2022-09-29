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
#ident	"$Header: 0.extr.c,v 1.1.1.2 90/05/07 19:19:57 wje Exp $"

#ifndef lint
static char sccsid[] = "@(#)0.extr.c	4.1	(Berkeley)	2/11/83";
#endif not lint

#include <stdio.h>
#include "def.h"
struct lablist	{long labelt;  struct lablist *nxtlab; };
struct lablist *endlab, *errlab, *reflab, *linelabs, *newlab;

int nameline;			/* line number of function/subroutine st., if any */
int stflag;		/* determines whether at beginning or middle of block of straight line code */



int   nlabs, lswnum, swptr, flag,
	 counter, p1, p3, begline, endline, r1,r2, endcom;
long begchar, endchar, comchar;


char *pred, *inc, *prerw, *postrw, *exp, *stcode;

#define maxdo	20	/* max nesting of do loops */
long dostack[maxdo];		/* labels of do nodes */
int doloc[maxdo];		/* loc of do node */
int doptr;


struct list *FMTLST;		/* list of FMTVX's generated */
struct list *ENTLST;		/* list of STLNVX nodes corresponding to entry statements */
long rtnbeg;	/* number of chars up to beginning of current routine */
