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
#ident	"$Header: base.c,v 1.4.1.2 90/05/09 15:24:59 wje Exp $"

/*	Copyright (c) 1984 AT&T	*/
/*	  All Rights Reserved  	*/

/*	THIS IS UNPUBLISHED PROPRIETARY SOURCE CODE OF AT&T	*/
/*	The copyright notice above does not evidence any   	*/
/*	actual or intended publication of such source code.	*/

/*
 * This file contains code for the crash function base.
 */

#include "crash.h"

/* get arguments for function base */
int
getbase()
{
	int c;

	optind = 1;
	while((c = getopt(argcnt,args,"w:")) !=EOF) {
		switch(c) {
			case 'w' :	redirect();
					break;
			default  :	longjmp(syn,0);
		}
	}
	if(args[optind]) {
		do {
			prnum(args[optind++]);
		}while(args[optind]);
	}
	else longjmp(syn,0);
}


/* print results of function base */
int
prnum(string)
char *string;
{
	int i;
	long num;

	if(*string == '(') 
		num = eval(++string);
	else num = strcon(string,NULL);
	if(num == -1)
		return;
	fprintf(fp,"hex: %x\n",num);
	fprintf(fp,"decimal: %d\n",num);
	fprintf(fp,"octal: %o\n",num);
	fprintf(fp,"binary: ");
	for(i=0;num >= 0 && i < 32;i++,num<<=1);
	for(;i<32;i++,num<<=1)
		num < 0 ? fprintf(fp,"1") : fprintf(fp,"0");
	fprintf(fp,"\n");
}
