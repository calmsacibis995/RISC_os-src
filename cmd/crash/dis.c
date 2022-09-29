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
#ident	"$Header: dis.c,v 1.4.1.2 90/05/09 15:25:54 wje Exp $"

/*	Copyright (c) 1984 AT&T	*/
/*	  All Rights Reserved  	*/

/*	THIS IS UNPUBLISHED PROPRIETARY SOURCE CODE OF AT&T	*/
/*	The copyright notice above does not evidence any   	*/
/*	actual or intended publication of such source code.	*/

/*
 * This file contains code for the crash function dis.
 */
#include	"crash.h"


/* get arguments for dis function */
int
getdis()
{
	int c;
	int absflg = 0;
	int regflg = 0;
	long addr;
	int count = 1;
	struct syment *sp;

	optind = 1;
	while((c = getopt(argcnt,args,"ahw:")) != EOF) {
		switch(c) {
			case 'w' :	redirect();
					break;
			case 'a' :	absflg = 1;
					break;
			case 'h' :      regflg = 1;
					break;
			default  :	longjmp(syn,0);
		}
	}
	if(!args[optind])
		longjmp(syn,0);
	if(*args[optind] == '(') {
		if((addr = eval(++args[optind])) == -1)
			error("\n");
	}
	else if(sp = symsrch(args[optind]))
		addr = sp->n_value;
	else if(isasymbol(args[optind]))
		error("%s not found in symbol table\n",args[optind]);
	else if((addr = strcon(args[optind],'h')) == -1)
			error("\n");
	if(args[++optind]) 
		if((count = strcon(args[optind],'d')) == -1)
			error("\n");
	prdis(addr,count,absflg,regflg);
}

static long dis_addr;

char *
dis_name(addr)
long addr;
{
	struct syment *sp;
	extern struct syment *findsym();

	sp = findsym(addr);
	if (addr == sp->n_value)
		return(sp->n_name);
	else return(NULL);
}

dis_bytes()
{
	long v;

	readmem(dis_addr,1, -1, &v, sizeof(v), "text");
	dis_addr+=4;
	return(v);
}

dis_hdr(addr)
long addr;
{
	struct syment *sp;

	sp = findsym(addr);
	prlineno(addr,sp);
	fprintf(fp,"\t");
}

prdis(addr, count, absflg, regflg)
long addr;
int count, absflg;
{
	long lastadr = addr + count*4;

	while (addr < lastadr) {
		dis_addr = addr;
		addr += disassembler(addr, regflg, absflg? NULL : dis_name, NULL, dis_bytes, dis_hdr);
	}
}

/* disassembler calls puts to print on stdout.. */
puts(s)
char *s;
{
	fprintf(fp,"%s\n",s);
}
