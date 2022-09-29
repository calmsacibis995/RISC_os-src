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
#ident	"$Header: search.c,v 1.4.1.4 90/05/09 17:20:58 wje Locked $"

/*	Copyright (c) 1984 AT&T	*/
/*	  All Rights Reserved  	*/

/*	THIS IS UNPUBLISHED PROPRIETARY SOURCE CODE OF AT&T	*/
/*	The copyright notice above does not evidence any   	*/
/*	actual or intended publication of such source code.	*/

/*
 * This file contains code for the crash function search.
 */

#include "crash.h"

#define pageround(x) ((x + NBPP) & ~(NBPP -1))
#define BUFSIZE (NBPP/sizeof(int))

extern int ignore_memerr;

/* get arguments for search function */
int
getsearch()
{
	long mask = 0xffffffff;
	int phys = 0;
	int proc = Procslot;
	int c;
	unsigned long pat,start,len;
	struct syment *sp;

	optind = 1;
	while((c = getopt(argcnt,args,"ips:w:m:")) !=EOF) {
		switch(c) {
			case 'w' :	redirect();
					break;
			case 'p' :	phys = 1;
					break;
			case 's' :	proc = setproc();
					break;
			case 'm' :	if((mask = strcon(optarg,'h')) == -1)
						error("\n");
					break;
			case 'i':	ignore_memerr = 1;
					break;
			default  :	longjmp(syn,0);
		}
	}
	if(args[optind]) {
		pat = strcon(args[optind++],'h');
		if(pat == -1)
			error("\n");
		if(args[optind]) {
			if(*args[optind] == '(') {
				if((start = eval(++args[optind])) == -1)
					error("\n");
			}
			else if(sp = symsrch(args[optind]))
				start = (long)sp->n_value;
			else if(isasymbol(args[optind]))
				error("%s not found in symbol table\n",
					args[optind]);
				else if((start = strcon(args[optind],'h')) == -1)
						error("\n");
			if(args[++optind]) {
				if((len = strcon(args[optind++],'h')) == -1)
					error("\n");
				prsearch(mask,pat,start,len,phys,proc);
			}
			else longjmp(syn,0);
		}
		else longjmp(syn,0);
	}
	else longjmp(syn,0);
}

/* print results of search */
int
prsearch(mask,pat,start,len,phys,proc)
long mask;
unsigned long pat,start,len;
int phys,proc;
{
	unsigned long buf[BUFSIZE];
	int i;
	unsigned n;
	long remainder;

	fprintf(fp,"MASK = %x, PATTERN = %x, START = %x, LENGTH = %x\n\n",
		mask,pat,start,len);
	while(len > 0)  {
		remainder = pageround(start) - start;
		n = min(remainder,len); 
		if (readbuf((long)start,(long)start,phys,proc,
			(char *)buf,n,"buffer") >= 0)
		    for(i = 0; i<n/sizeof (int); i++)  
			if((buf[i] & mask) == (pat & mask)) {
				fprintf(fp,"MATCH AT %8x: %8x\n",start+
					i*sizeof (int),buf[i]);
			}
		start +=n;
		len -=n;
	}
}
