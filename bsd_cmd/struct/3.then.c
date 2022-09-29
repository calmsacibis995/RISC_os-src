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
#ident	"$Header: 3.then.c,v 1.1.1.2 90/05/07 19:24:27 wje Exp $"

#ifndef lint
static char sccsid[] = "@(#)3.then.c	4.1	(Berkeley)	2/11/83";
#endif not lint

#include <stdio.h>
#include "def.h"
#include "3.def.h"

#define BRANCHTYPE(t)	(t == STOPVX || t == RETVX || t == BRKVX || t == NXTVX || t == GOVX)
#define MAXCHUNK	20
		/* if else clause smaller than MAXCHUNK and smaller than then clause,
			and there is no reason not to negate the if, negate the if */

getthen(v)		/* turn IFVX into THEN when appropriate, create else ifs where possible  */
VERT v;
	{
	VERT tch, fch;
	int tn,fn;
	int recvar;

	if (NTYPE(v) == IFVX)
		{
		tch = LCHILD(v,THEN);
		fch = LCHILD(v,ELSE);
		if (!DEFINED(fch))
			mkthen(v);
		else if (!DEFINED(tch))
			{
			negate(v);
			mkthen(v);
			}
		else if (BRANCHTYPE(NTYPE(tch)))
			mkthen(v);
		else if (BRANCHTYPE(NTYPE(fch)))
			{
			negate(v);
			mkthen(v);
			}
		else if (NTYPE(fch) != IFVX || DEFINED(RSIB(fch)))	/* not an else if */
			if ( NTYPE(tch) == IFVX && !DEFINED(RSIB(tch)))
					/* invert into else if */
				negate(v);
			else
				{
				/* asoc(v,n) returns number of statements associated with v
					if <= n, -1 otherwise */
				tn = asoc(tch,MAXCHUNK);
				fn = asoc(fch,MAXCHUNK);
				if (fn >= 0 && (tn < 0 || fn < tn))
					/* else clause smaller */
					negate(v);
				}
		}
	RECURSE(getthen,v,recvar);
	}

mkthen(v)
VERT v;
	{
	VERT w,tc;
	w = LCHILD(v,ELSE);
	tc = LCHILD(v,THEN);
	ASSERT(!DEFINED(w) || (DEFINED(tc) && BRANCHTYPE(NTYPE(tc)) ),mkthen);
	if (DEFINED(w))
		{
		insib(v,w);
		LCHILD(v,ELSE) = UNDEFINED;
		}
	ASSERT(IFTHEN(v),mkthen);
	}


negate(v)
VERT v;
	{
	ASSERT(NTYPE(v) == IFVX,negate);
	exchange(&LCHILD(v,THEN), &LCHILD(v,ELSE));
	NEG(v) = !NEG(v);
	}
