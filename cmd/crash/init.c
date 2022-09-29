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
#ident	"$Header: init.c,v 1.4.1.3.1.1.1.2 90/12/20 19:14:06 beacker Exp $"

/*	Copyright (c) 1984 AT&T	*/
/*	  All Rights Reserved  	*/

/*	THIS IS UNPUBLISHED PROPRIETARY SOURCE CODE OF AT&T	*/
/*	The copyright notice above does not evidence any   	*/
/*	actual or intended publication of such source code.	*/

/*
 * This file contains code for the crash initialization.
 */

#include "crash.h"

extern char *dumpfile;		
extern int active;		/* flag for active system */
extern char *malloc();
extern int target_pagesize;	/* pagesize of target system */
extern int pte_to_pfn_shift;
extern int pfn_to_byte_shift;
extern int Usize;
extern uint Pde_pg_vr,		/* pte "valid" mask */
            Pde_pg_g,		/* pte "global" mask */
            Pde_pg_m,		/* pte "modified" mask */
            Pde_pg_n,		/* pte "noncached" mask */
            Pde_pg_lock,	/* pte "page locked" mask */
            Pde_pg_sv,		/* pte "page valid" mask */
            Pde_pg_cw;		/* pte "copy-on-write" mask */
       int Npgpt;
struct user *ubp;		/* pointer to ublock buffer */
struct ucred *ucredp;


/* initialize buffers, symbols, and global variables for crash session */
int
init()
{
	extern void sigint();
	
	if((mem = open(dumpfile, 0)) < 0)	/* open dump file, if error print */
		fatal("cannot open dump file %s\n",dumpfile);
	/*
	 * Set a flag if the dumpfile is of an active system.
	 */
	if((strcmp(dumpfile,"/dev/mem")==0) || (strcmp(dumpfile,"/dev/kmem")==0))
		active = 1;

	rdsymtab();			/* open and read the symbol table */


	if(!(V = symsrch("v")))
		fatal("var structure not found in symbol table\n");
	if(!(Proc = symsrch("proc")))
		fatal("proc not found in symbol table\n");
	if(!(File = symsrch("file")))
		fatal("file not found in symbol table\n");
	if(!(Panic = symsrch("panicstr")))
		fatal("panicstr not found in symbol table\n");
	if(!(Curproc = symsrch("curproc")))
		fatal("curproc not found in symbol table\n");

	if (Pagesize = symsrch("pagesize")) {
		readmem((long)Pagesize->n_value,1,-1,(char *)&target_pagesize,
			sizeof(int),"pagesize");
	} else {
		target_pagesize = NBPP;		/* assume Mips1 */
	}
	switch (target_pagesize) {
	case 4096:	pte_to_pfn_shift = 12;
			pfn_to_byte_shift = 12;
			Usize = USIZE;
			Pde_pg_vr = PG_VR;
			Pde_pg_g  = PG_G;
			Pde_pg_m  = PG_M;
			Pde_pg_n  = PG_N;
			Pde_pg_lock = PG_LOCK;
			Pde_pg_cw = PG_CW;
			Pde_pg_sv = PG_SV;
			break;
	case 16384:	pte_to_pfn_shift = 10;
			pfn_to_byte_shift = 14;
			Usize = 1;
			Pde_pg_g  = 0x1;
			Pde_pg_vr = 0x2;
			Pde_pg_m  = 0x4;
			Pde_pg_n  = 0x8;
			Pde_pg_cw = 0x80;
			Pde_pg_sv = 0x100;
			Pde_pg_lock = 0x200;
			break;
	default:	fatal("cannot handle pagesize other than 4096 or 16384");
	} /* switch */
	Npgpt = NBPS/target_pagesize;		/* num ptes in "segment" */

	readmem((long)V->n_value,1,-1,(char *)&vbuf,
		sizeof vbuf,"var structure");

	/* Allocate ublock buffer */
	ubp = (user_t*)malloc(target_pagesize*Usize);
	ucredp = (struct ucred *) malloc(sizeof(*ucredp));
	Procslot = getcurproc();
	/* setup break signal handling */
	if(signal(SIGINT,sigint) == SIG_IGN)
		signal(SIGINT,SIG_IGN);
}
