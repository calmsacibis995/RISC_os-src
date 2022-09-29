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
#ident	"$Header: size.c,v 1.4.1.2 90/05/09 15:27:40 wje Exp $"

/*	Copyright (c) 1984 AT&T	*/
/*	  All Rights Reserved  	*/

/*	THIS IS UNPUBLISHED PROPRIETARY SOURCE CODE OF AT&T	*/
/*	The copyright notice above does not evidence any   	*/
/*	actual or intended publication of such source code.	*/

/*
 * This file contains code for the crash functions:  size, findslot, and
 * findaddr.
 */

#include "crash.h"
#include "sys/callo.h"
#include "sys/fcntl.h"
#include "sys/flock.h"
#include "sys/termio.h"
#include "sys/stty_ld.h"

struct sizetable {
	char *name;
	char *symbol;
	unsigned size;
};

struct sizetable siztab[] = {
	"buf","buf",sizeof (struct buf),
	"callo","callout",sizeof (struct callout),
	"callout","callout",sizeof (struct callout),
	"file","file",sizeof (struct file),
	"inode","inode",sizeof (struct inode),
	"pfdat","pfdat",sizeof (struct pfdat),
	"proc","proc",sizeof (struct proc),
	"region","region",sizeof (struct region),
	"stty_ld","stty_ld",sizeof (struct stty_ld),
	NULL,NULL,NULL
};	


/* get size from size tables */
unsigned
getsizetab(name)
char *name;
{
	unsigned size = 0;
	struct sizetable *st;
	extern unsigned getsizenetab();

	for(st = siztab; st->name; st++) {
		if(!(strcmp(st->name,name))) {
			size = st->size;
			break;
		}
	if(!size)
		size = getsizenetab(name);
	}
	return(size);
}

/* get arguments for size function */
int
getsize()
{
	int c;
	char *all = "";
	int hex = 0;

	optind = 1;
	while((c = getopt(argcnt,args,"xw:")) !=EOF) {
		switch(c) {
			case 'w' :	redirect();
					break;
			case 'x' : 	hex = 1;
					break;
			default  :	longjmp(syn,0);
		}
	}
	if(args[optind]) {
		do{
			prsize(args[optind++],hex);
		}while(args[optind]);
	}
	else prsize(all,hex);
}

/* print size */
int
prsize(name,hex)
char *name;
int hex;
{
	unsigned size;
	struct sizetable *st;
	int i;

	if(!strcmp("",name)) {
		for(st = siztab,i = 0; st->name; st++,i++) {
			if(!(i & 3))
				fprintf(fp,"\n");
			fprintf(fp,"%-15s",st->name);
		}
		prsizenet(name);
	}
	else {
		size = getsizetab(name);
		if(size) {
			if(hex)
				fprintf(fp,"0x%x\n",size);
			else fprintf(fp,"%d\n",size);
		}
		else error("%s does not match in sizetable\n",name);
	}
}
	

/* get arguments for findaddr function */
int
getfindaddr()
{
	int c;
	int slot;
	char *name;

	optind = 1;
	while((c = getopt(argcnt,args,"w:")) !=EOF) {
		switch(c) {
			case 'w' :	redirect();
					break;
			default  :	longjmp(syn,0);
		}
	}
	if(args[optind]) {
		name = args[optind++];
		if(args[optind]) {
			if((slot = (int)strcon(args[optind],'d')) == -1)
				error("\n");
			prfindaddr(name,slot);
		}
		else longjmp(syn,0);
	}
	else longjmp(syn,0);
}

/* print address */
int
prfindaddr(name,slot)
char *name;
int slot;
{
	unsigned size = 0;
	struct syment *sp;
	struct sizetable *st;
	char symbol[10];

	symbol[0] = '\0';
	for(st = siztab; st->name; st++) 
		if(!(strcmp(st->name,name))) {
			strcpy(symbol,st->symbol);
			size = st->size;
			break;
		}
	if(symbol[0] == '\0') 
		getnetsym(name,symbol,&size);
	if(symbol[0] == '\0')
		error("no match for %s in sizetable\n",name);
	if(!(sp = symsrch(symbol)))
		error("no match for %s in symbol table\n",name);
	fprintf(fp,"%8x\n",sp->n_value + size * slot);
}

/* get arguments for findslot function */
int
getfindslot()
{
	int c;
	long addr;

	optind = 1;
	while((c = getopt(argcnt,args,"w:")) !=EOF) {
		switch(c) {
			case 'w' :	redirect();
					break;
			default  :	longjmp(syn,0);
		}
	}
	if(args[optind]) {
		do{
			if((addr = strcon(args[optind++],'h')) == -1)
				continue;
			prfindslot(addr);
		}while(args[optind]);
	}
	else longjmp(syn,0);
}

/* print table and slot */
int
prfindslot(addr)
long addr;
{
	struct syment *sp;
	int slot,offset;
	unsigned size;
	extern struct syment *findsym();
	char *name;

	if(!(sp = findsym((unsigned long)addr)))
		error("no symbol match for %8x\n",addr);
	name = sp->n_name;
	size = getsizetab(name);
	if(!size)
		error("%s does not match in sizetable\n",name);
	slot = (addr - sp->n_value)/size;
	offset = (addr - sp->n_value)%size;
	fprintf(fp,"%s",name);
	fprintf(fp,", slot %d, offset %d\n",slot,offset);
}
