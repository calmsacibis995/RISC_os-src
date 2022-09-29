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
#ident	"$Header: var.c,v 1.4.1.2 90/05/09 15:28:55 wje Exp $"

/*	Copyright (c) 1984 AT&T	*/
/*	  All Rights Reserved  	*/

/*	THIS IS UNPUBLISHED PROPRIETARY SOURCE CODE OF AT&T	*/
/*	The copyright notice above does not evidence any   	*/
/*	actual or intended publication of such source code.	*/

/*
 * This file contains code for the crash function var.
 */

#include "crash.h"
#include "sys/fs/ufs_mount.h"

/* get arguments for var function */
int
getvar()
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
	if(args[optind]) 
		longjmp(syn,0);
	else {
		prvar();
		prtune();
	}
}

struct	syment *Mount;

/* print var structure */
int
prvar()
{
	extern struct syment *V;
	unsigned index,fndex,mndex,pndex;

	readmem((long)V->n_value,1,-1,(char *)&vbuf,
		sizeof vbuf,"var structure");
	if(!SYM_inode)
		SYM_inode = symsrch("inode");
	if (SYM_inode)
		index = (unsigned)(vbuf.ve_inode - SYM_inode->n_value) /
			sizeof (struct inode);
	fndex = (unsigned)(vbuf.ve_file - File->n_value) /sizeof (struct file);
	if (!Mount)
		Mount = symsrch("mount");
	if (Mount)
		mndex = (unsigned)(vbuf.ve_mount - Mount->n_value) /sizeof (struct mount);
	pndex = (unsigned)(vbuf.ve_proc - Proc->n_value) /sizeof (struct proc);

	fprintf(fp,"VAR STRUCTURE:\n");
	fprintf(fp,"v_buf: %3d\tv_call: %3d\nv_clist: %3d\tv_maxup: %3d\n",
		vbuf.v_buf,
		vbuf.v_call,
		vbuf.v_clist,
		vbuf.v_maxup);
	fprintf(fp,"v_hbuf: %3d\tv_hmask: %3d\nv_pbuf: %3d\tv_nofiles: %3d\n",
		vbuf.v_hbuf,
		vbuf.v_hmask,
		vbuf.v_pbuf, 
		vbuf.v_nofiles);
	fprintf(fp,"v_inode: %3d\t\tve_inode: ",
		vbuf.v_inode);
	if (SYM_inode)
		fprintf(fp,"%3d\n",index);
	else
		fprintf(fp," - \n");
	fprintf(fp,"v_file: %3d\t\tve_file: %3d\n",
		vbuf.v_file,
		fndex);
	fprintf(fp,"v_mount: %3d\t\tve_mount: ",
		vbuf.v_mount);
	if (Mount)
		fprintf(fp,"%3d",mndex);
	else
		fprintf(fp," - ");
	fprintf(fp,"\nv_proc: %3d\t\tve_proc: %3d\n",
		vbuf.v_proc,
		pndex);
	fprintf(fp,"v_region: %3d\tv_sptmap: %3d\nv_nstreams: %3d\tv_nqueue: %3d\n",
		vbuf.v_region,
		vbuf.v_sptmap,
		vbuf.v_nstream,
		vbuf.v_nqueue);
	fprintf(fp,"v_vhndfrac: %d\tv_maxpmem: %d\nv_autoup: %d\n",
		vbuf.v_vhndfrac,
		vbuf.v_maxpmem,
		vbuf.v_autoup);
	fprintf(fp,"v_nblk4096: %d\tv_nblk2048: %d\tv_nblk1024: %d\tv_nblk512: %d\n",
		vbuf.v_nblk4096,
		vbuf.v_nblk2048,
		vbuf.v_nblk1024,
		vbuf.v_nblk512); 
	fprintf(fp,"v_nblk256: %d\tv_nblk128: %d\tv_nblk64: %d\tv_nblk16: %d\n",
		vbuf.v_nblk256,
		vbuf.v_nblk128,
		vbuf.v_nblk64,
		vbuf.v_nblk16);
	fprintf(fp,"v_nblk4: %d\n",
		vbuf.v_nblk4);
#ifdef V5.3.1
	fprintf(fp,"v_ulimit: %d\n",
		vbuf.v_ulimit);
#endif
}

/* print tune structure */
int
prtune()
{
	struct syment *T;
	struct tune tune;

	if(!(T = symsrch("tune")))
		fatal("tune structure not found in symbol table\n");
	readmem((long)T->n_value,1,-1,(char *)&tune,
		sizeof tune,"tune structure");
	fprintf(fp,"TUNE STRUCTURE:\n");
	fprintf(fp,"t_gpgslo: %d\tt_gpgshi: %d\n", tune.t_gpgslo, tune.t_gpgshi);
	fprintf(fp,"t_vhandr: %d\tt_vhandl: %d\n", tune.t_vhandr, tune.t_vhandl);
	fprintf(fp,"t_gpgslmsk: %x\tt_gpgshmsk: %x\n", tune.t_gpgslmsk, tune.t_gpgshmsk);
	fprintf(fp,"t_maxsc: %d\tt_maxfc: %d\n", tune.t_maxsc, tune.t_maxfc);
	fprintf(fp,"t_bdflushr: %d\tt_maxumem: %d\n", tune.t_bdflushr, tune.t_maxumem);
	fprintf(fp,"t_minarmem: %d\tt_minasmem: %d\n", tune.t_minarmem, tune.t_minasmem);
}
