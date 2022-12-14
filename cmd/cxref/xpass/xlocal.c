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
#ident	"$Header: xlocal.c,v 1.5.2.3 90/05/09 15:39:34 wje Exp $"

/*	Copyright (c) 1984 AT&T	*/
/*	  All Rights Reserved  	*/

/*	THIS IS UNPUBLISHED PROPRIETARY SOURCE CODE OF AT&T	*/
/*	The copyright notice above does not evidence any   	*/
/*	actual or intended publication of such source code.	*/

#include <stdio.h>
#include "mfile1"
#ifndef FLEXNAMES
#	define LCHNM	8
#	define LFNM	15
#endif
#include "lmanifest"
#include "messages.h"
/*
 * this file contains the functions local to CXREF
 * they put their output to outfp
 * the others contain lint's 1st pass with output thrown away
 * cgram.c has calls to these functions whenever a NAME is seen
 */

char infile[120];
FILE *outfp;

ref( i, line)
	int i, line;
{
#ifdef FLEXNAMES
	fprintf(outfp, "R%s\t%05d\n", stab[i].sname, line);
#else
	fprintf(outfp, "R%.8s\t%05d\n",stab[i].sname,line);
#endif
}

def( i, line)
	int i, line;
{
	if (stab[i].sclass == EXTERN)
		ref(i, line);
	else
#ifdef FLEXNAMES
		fprintf(outfp, "D%s\t%05d\n", stab[i].sname, line);
#else
		fprintf(outfp,"D%.8s\t%05d\n",stab[i].sname,line);
#endif
}


newf(i, line)
	int i, line;
{
#ifdef FLEXNAMES
	fprintf(outfp, "F%s\t%05d\n", stab[i].sname, line);
#else
	fprintf(outfp,"F%.8s\t%05d\n",stab[i].sname, line);
#endif
}

efcode() {}

bfcode(a,n) int a[]; {}

defnam(p) struct symtab *p;
{
	fprintf(outfp,"D%.8s\t%05d\n",p->sname,p->suse);
}

fldty(p) struct symtap *p; {}

commdec(i) int i; {}

ejobcode(f) {}

char *
exname(p) char *p; {return (p);}

ecode(p) NODE *p; {}

extern char msgtxt[];
lwerror(msgndx, arg1, arg2) int msgndx;
{
    where('u');
    fprintf(stderr, msgtext[msgndx], arg1, arg2);
    fprintf(stderr, "\n");
}

luerror(msgndx, arg1) short msgndx;
{
    where('w');
    fprintf(stderr, "warning: ");
    fprintf(stderr, msgtext[msgndx], arg1);
    fprintf(stderr, "\n");
}
