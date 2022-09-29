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
#ident	"$Header: fs.c,v 1.4.1.2 90/05/09 15:26:00 wje Exp $"

/*	Copyright (c) 1984 AT&T	*/
/*	  All Rights Reserved  	*/

/*	THIS IS UNPUBLISHED PROPRIETARY SOURCE CODE OF AT&T	*/
/*	The copyright notice above does not evidence any   	*/
/*	actual or intended publication of such source code.	*/

/*
 * This file contains code for the crash function fs.
 */

#include "crash.h"

/* get arguments for fs function */
int
getfs()
{
	int slot = -1;
	int all = 0;
	int phys = 0;
	long addr = -1;
	long arg1 = -1;
	long arg2 = -1;
	int c;
	struct	vfssw *vfsNVFS;
	struct	vfssw *vfssw;
	short nvfstypes;

	if(!SYM_vfssw)
		if((SYM_vfssw = symsrch("vfssw")) == NULL)
			error("vfssw not found in symbol table\n");
	if(!SYM_vfsNVFS)
		if((SYM_vfsNVFS = symsrch("vfsNVFS")) == NULL)
			error("vfsNVFS not found in symbol table\n");

	optind = 1;
	while((c = getopt(argcnt,args,"pw:")) !=EOF) {
		switch(c) {
			case 'p' :	phys = 1;
					break;
			case 'w' :	redirect();
					break;
			default  :	longjmp(syn,0);
		}
	}
	vfssw = (struct vfssw *) SYM_vfssw->n_value;
	readmem((long)SYM_vfsNVFS->n_value,1,-1,(char *)&vfsNVFS,
		sizeof (int), "end of vfssw table");
	nvfstypes = (vfsNVFS - vfssw);
	fprintf(fp,"FILE SYSTEM INFORMATION TABLE SIZE = %d\n",nvfstypes-1);
	fprintf(fp,"SLOT NAME\n");
	if(args[optind]) {
		all = 1;
		do {
			getargs(- nvfstypes,&arg1,&arg2);
			if(arg1 == -1) 
				continue;
			if(arg1 == 0) {
				fprintf(fp,"0 is out of range\n");
				continue;
			}
			if(arg2 != -1) {
			  	if (arg1 >= 0 &&
				    arg1 < nvfstypes) {
					for(slot = arg1; slot <= arg2; slot++)
						prfs(all,slot,phys,addr,nvfstypes);
				} else {
					for (addr = arg1; addr <= arg2;
					     addr = (long) (((struct vfssw *) addr) + 1))
						prfs(all,-1,phys,addr,nvfstypes);
				};
			} else {
				if (arg1 >= 0 &&
				    arg1 < nvfstypes)
					slot = arg1;
				else addr = arg1;
				prfs(all,slot,phys,addr,nvfstypes);
			}
			slot = addr = arg1 = arg2 = -1;
		}while(args[++optind]);
	}
	else for(slot = 1; slot < nvfstypes; slot++)
		prfs(all,slot,phys,addr,nvfstypes);
}

/* print vfssw table */
int
prfs(all,slot,phys,addr,max)
int all,slot,phys,max;
long addr;
{
	struct vfssw fsbuf;
	char name[MAXPATHLEN+1];

	readbuf(addr,(long)(SYM_vfssw->n_value+slot*sizeof fsbuf),phys,-1,
		(char *)&fsbuf,sizeof fsbuf,"virtual file system switch table");
	if(!fsbuf.vsw_name && !all)
		return; 
	if(addr != -1)
		slot = getslot(addr,(long)SYM_vfssw->n_value,sizeof fsbuf,phys,max);
	if(slot == -1)
		fprintf(fp,"  - ");
	else
		fprintf(fp, "%4d", slot);
	readmem((long)fsbuf.vsw_name,1,-1,name,sizeof name,"vsw_name");
	fprintf(fp," %-20s\n", name); 
}
