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
#ident	"$Header: major.c,v 1.4.1.2 90/05/09 15:26:35 wje Exp $"

/*	Copyright (c) 1984 AT&T	*/
/*	  All Rights Reserved  	*/

/*	THIS IS UNPUBLISHED PROPRIETARY SOURCE CODE OF AT&T	*/
/*	The copyright notice above does not evidence any   	*/
/*	actual or intended publication of such source code.	*/

/*
 * This file contains code for the crash function major.
 */

#include "crash.h"

#define MAJSIZE 128		/* size of MAJOR table */

char majbuf[MAJSIZE];		/* buffer for MAJOR table */
static struct syment *Major;	/* namelist symbol pointer */

/* get arguments for major function */
int
getmajor()
{
	int slot = -1;
	int c;

	if(!Major)
		if((Major = symsrch("MAJOR")) == NULL)
			error("MAJOR not found in symbol table\n");
	optind = 1;
	while((c = getopt(argcnt,args,"w:")) !=EOF) {
		switch(c) {
			case 'w' :	redirect();
					break;
			default  :	longjmp(syn,0);
		}
	}
	fprintf(fp,"MAJOR TABLE SIZE = %d\n",MAJSIZE);
	readmem((long)Major->n_value,1,-1,(char *)majbuf,
		MAJSIZE,"MAJOR Table");

	if(args[optind]) {
		do {
			if((slot = strcon(args[optind++],'d')) == -1)
				continue;
			if((slot < 0) || (slot >= MAJSIZE))
				error("%d is out of range\n",slot);
			prmajor(slot);
		}while(args[optind]);
	}
	else prmajor(slot);
}

/* print MAJOR table */
int
prmajor(slot)
int slot;
{
	int i;

	if(slot == -1) {
		for(i = 0; i < MAJSIZE; i++) {
			if(!(i & 3))
				fprintf(fp,"\n");
			fprintf(fp,"[%3d]: %3d\t",i,majbuf[i]);
		}
		fprintf(fp,"\n");
	}
	else fprintf(fp,"[%3d]: %3d\n",slot,majbuf[slot]);
}
