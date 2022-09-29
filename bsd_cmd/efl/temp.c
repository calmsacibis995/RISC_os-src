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
#ident	"$Header: temp.c,v 1.1.1.2 90/05/07 18:25:43 wje Exp $"

#include "defs"

ptr gentemp(t)
ptr t;
{
register ptr oldp;
register ptr p;
register ptr q;
int ttype;
ptr ttypep, tdim;

/* search the temporary list for a matching type */

ttype = t->vtype;
ttypep = t->vtypep;
tdim = t->vdim;

for(oldp = &tempvarlist ; p = oldp->nextp ; oldp = p)
	if( (q = p->datap) && (q->vtype == ttype) &&
	  (q->vtypep == ttypep) && eqdim(q->vdim,tdim) )
		{
		oldp->nextp = p->nextp;
		break;
		}

if(p == PNULL)
	{
	q = allexpblock();
	q->tag = TTEMP;
	q->subtype = t->subtype;
	q->vtype = ttype;
	q->vclass = t->vclass;
	q->vtypep = ( ttypep ? cpexpr(ttypep) : PNULL);
	q->vdim = tdim;
	mkftnp(q);	/* assign fortran types */

	p = mkchain(q, CHNULL);
	p->datap = q;
	}

p->nextp = thisexec->temps;
thisexec->temps = p;

return( cpexpr(q) );
/* need a copy of the block for the temporary list and another for use */
}


ptr gent(t,tp)  /* make a temporary of type t, typepointer tp */
int t;
ptr tp;
{
static struct varblock model;

model.vtype = t;
model.vtypep = tp;

return( gentemp(&model) );
}
