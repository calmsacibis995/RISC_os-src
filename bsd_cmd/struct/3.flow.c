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
#ident	"$Header: 3.flow.c,v 1.1.1.2 90/05/07 19:23:36 wje Exp $"

#ifndef lint
static char sccsid[] = "@(#)3.flow.c	4.1	(Berkeley)	2/11/83";
#endif not lint

#include <stdio.h>
#
/*
correct the flow of control in the new program - use GOTO's which may
be changed later to NEXT, BREAK, etc.
*/
#include "def.h"
#include "3.def.h"

#define BRANCHTYPE(v)	(NTYPE(v) == GOVX )
#define HASLEX(t)	(t != GOVX && t != COMPVX && t != ASGOVX  && t != ITERVX )
			/* for these, control never flows directly to following statement */


getflow()
	{
	fixflow(START,UNDEFINED);
	}


fixflow(v,autolex)
VERT v;
VERT autolex;		/* lexical successor of v */
	{
	VERT lex,chlex,z,x,w;
	int i;
	lex = lexval(v,autolex);
	if (HASLEX(NTYPE(v)) && NTYPE(v) != ICASVX)
		if (DEFINED(REACH(v)) && REACH(v) != lex)
			insib(v,makebr(REACH(v)));
		else if (NTYPE(v) == DOVX && ARC(v,1) != lex)
			insib(v,makebr(ARC(v,1)));
	if (NTYPE(v) == ITERVX)
		{
		BRK(v) = autolex;
		chlex = v;
		}
	else
		chlex = lexval(v,autolex);

	for (i = 0; i < CHILDNUM(v); ++i)
		{
		w = LCHILD(v,i);
		if (DEFINED(w))
			fixflow(w,chlex);
		else
			{
			ASSERT(i < ARCNUM(v),fixflow);
			z = ARC(v,i);
			ASSERT(DEFINED(z), fixflow);
			if (z != chlex)
				{
				x = makebr(z);
				LCHILD(v,i) = x;
				RSIB(x) = UNDEFINED;
				}
			}
		}
	if (DEFINED(RSIB(v)))
		fixflow(RSIB(v),autolex);
	}


lexval(v,lastlex)
VERT v,lastlex;
	{
	VERT sib;
	if (!HASLEX(NTYPE(v))) return(UNDEFINED);
	sib = RSIB(v);
	if (NTYPE(v) == ICASVX || NTYPE(v) == ACASVX)
		return(lastlex);
	else if (!DEFINED(sib))
		return(lastlex);
	else if (BRANCHTYPE(sib))
		return(ARC(sib,0));
	else return(sib);
	}


makebr(w)		/* make branching node leading to w */
VERT w;
	{
	VERT new;
	new = create(GOVX,1);
	ARC(new,0) = w;
	RSIB(new) = UNDEFINED;
	REACH(new) = UNDEFINED;
	return(new);
	}
