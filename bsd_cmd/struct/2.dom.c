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
#ident	"$Header: 2.dom.c,v 1.1.1.2 90/05/07 19:22:57 wje Exp $"

#ifndef lint
static char sccsid[] = "@(#)2.dom.c	4.1	(Berkeley)	2/11/83";
#endif not lint

#include <stdio.h>
#
/*
set dom[v] to immediate dominator of v, based on arcs as stored in inarcs
(i.e. pretending the graph is reducible if it isn't).
Algorithm is from Hecht and Ullman, Analysis of a simple algorithm for global
flow analysis problems, except bit vector operations replaced by search
through DOM to save quadratic blowup in space 
*/
#include "def.h"
#include "2.def.h"


getdom(inarc,dom)
struct list **inarc;
VERT *dom;
	{
	VERT v;
	int i;
	struct list *ls;
	for (v = 0; v < nodenum; ++v)
		dom[v] = UNDEFINED;
	for (i = 1; i < accessnum; ++i)
		{
		v = after[i];
		for (ls = inarc[v]; ls; ls = ls->nxtlist)
			{
			ASSERT(ntoaft[ls->elt] < i,getdom);
			dom[v] = comdom(dom[v],ls->elt,dom);
			}

		}
	}


comdom(u,v,dom)			/* find closest common dominator of u,v */
VERT u,v, *dom;
	{
	if (u == UNDEFINED) return(v);
	if (v == UNDEFINED) return(u);
	while(u != v)
		{
		ASSERT(u != UNDEFINED && v != UNDEFINED, comdom);
		if (ntoaft[u] < ntoaft[v])	
			v = dom[v];
		else
			u = dom[u];
		}
	return(u);
	}
