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
#ident	"$Header: sizenet.c,v 1.4.1.2 90/05/09 15:27:46 wje Exp $"

/*	Copyright (c) 1984 AT&T	*/
/*	  All Rights Reserved  	*/

/*	THIS IS UNPUBLISHED PROPRIETARY SOURCE CODE OF AT&T	*/
/*	The copyright notice above does not evidence any   	*/
/*	actual or intended publication of such source code.	*/

/*
 * This file contains code for the crash function size.  The 
 * Streams tables and sizes are listed here to allow growth and not
 * overrun the compiler symbol table.
 */

#include "crash.h"


struct sizenetable {
	char *name;
	char *symbol;
	unsigned size;
};
struct sizenetable sizntab[] = {
	"datab","dblock",sizeof (struct datab),
	"dblk","dblock",sizeof (struct datab),
	"dblock","dblock",sizeof (struct datab),
	"dbalcst","dballoc",sizeof (struct dbalcst),
	"dballoc","dballoc",sizeof (struct dbalcst),
	"linkblk","linkblk",sizeof (struct linkblk),
	"mblk","mblock",sizeof (struct msgb),
	"mblock","mblock",sizeof (struct msgb),
	"msgb","mblock",sizeof (struct msgb),
	"queue","queue",sizeof (struct queue),
	NULL,NULL,NULL
};	


/* get size from size table */
unsigned
getsizenetab(name)
char *name;
{
	unsigned size = 0;
	struct sizenetable *st;

	for(st = sizntab; st->name; st++) {
		if(!(strcmp(st->name,name))) {
			size = st->size;
			break;
		}
	}
	return(size);
}

/* print size */
int
prsizenet(name)
char *name;
{
	struct sizenetable *st;
	int i;

	if(!strcmp("",name)) {
		for(st = sizntab,i = 0; st->name; st++,i++) {
			if(!(i & 3))
				fprintf(fp,"\n");
			fprintf(fp,"%-15s",st->name);
		}
		fprintf(fp,"\n");
	}
}

/* get symbol name and size */
int
getnetsym(name,symbol,size)
char *name;
char *symbol;
unsigned *size;
{
	struct sizenetable *st;

	for(st = sizntab; st->name; st++) 
		if(!(strcmp(st->name,name))) {
			strcpy(symbol,st->symbol);
			*size = st->size;
			break;
		}
}
