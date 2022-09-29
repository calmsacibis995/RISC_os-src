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
#ident	"$Header: callout.c,v 1.4.1.3 90/05/09 15:25:42 wje Exp $"

/*	Copyright (c) 1984 AT&T	*/
/*	  All Rights Reserved  	*/

/*	THIS IS UNPUBLISHED PROPRIETARY SOURCE CODE OF AT&T	*/
/*	The copyright notice above does not evidence any   	*/
/*	actual or intended publication of such source code.	*/

/*
 * This file contains code for the crash function callout.
 */

#include "crash.h"
#include "sys/callo.h"

struct syment *Callout;                 /* namelist symbol pointer */

/* get arguments for callout function */
int
getcallout()
{
	int c;

	if(!Callout)
		if(!(Callout = symsrch("calltodo")))
			error("calltodo not found in symbol table\n");

	optind = 1;
	while((c = getopt(argcnt,args,"w:")) !=EOF) {
		switch(c) {
			case 'w' :	redirect();
					break;
			default  :	longjmp(syn,0);
		}
	}
	fprintf(fp,"FUNCTION        ARGUMENT   TIME  ID\n");
	if(args[optind]) 
		longjmp(syn,0);
	else prcallout();
}

/* print callout table */
int
prcallout()
{
	struct syment *sp;
	extern struct syment *findsym();
	struct callout callbuf;

	seekmem((long)Callout->n_value,1,-1);
	if(read(mem,(char *)&callbuf,sizeof callbuf)!=sizeof callbuf)
		error("read error on callout table\n");
	if (callbuf.c_next == 0)
		return;
	seekmem((long)callbuf.c_next,1,-1);
	for(;;) {
		if(read(mem,(char *)&callbuf,sizeof callbuf)!=sizeof callbuf) 
			error("read error on callout table\n");
		if(!callbuf.c_next)
			return;
		seekmem((long)callbuf.c_next,1,-1);
		if(!(sp = findsym((unsigned long)callbuf.c_func)))
			error("%8x does not match in symbol table\n",
				callbuf.c_func);
		fprintf(fp,"%-15s",sp->n_name);
		fprintf(fp," %08lx %6u  %5u\n", 
			callbuf.c_arg,
			callbuf.c_time,
			callbuf.c_id);
	}
}
