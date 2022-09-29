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
#ident	"$Header: vtop.c,v 1.4.1.3 90/05/09 15:29:01 wje Exp $"

/*	Copyright (c) 1984 AT&T	*/
/*	  All Rights Reserved  	*/

/*	THIS IS UNPUBLISHED PROPRIETARY SOURCE CODE OF AT&T	*/
/*	The copyright notice above does not evidence any   	*/
/*	actual or intended publication of such source code.	*/

/*
 * This file contains code for the crash functions:  vtop and mode, as well as
 * the virtual to physical offset conversion routine vtop.
 */

#include "crash.h"

struct syment *Kptbl;

int abortflag = 0;			/* flag to abort or continue */
preg_t *foundprp;                       /* set by findreg */
reg_t *foundrp;                         /* set by findreg */
pde_t *kptbl;
struct proc prbuf;			/* process entry buffer */

extern int Usize;
extern int pte_to_pfn_shift;
extern int pfn_to_byte_shift;
extern uint Pde_pg_vr;
extern int Npgpt;
extern int target_pagesize;

/* virtual to physical offset address translation */
paddr_t
vtop(vaddr,slot)
unsigned long vaddr;
int slot;
{

	pde_t **ppt, *pt, pde;

	if (IS_KSEG0(vaddr))
		return(K0_TO_PHYS(vaddr));
	if (IS_KSEG1(vaddr))
		return(K1_TO_PHYS(vaddr));
/*      if (IS_UAREA(vaddr)) {  UH: not usable */
	if (vaddr >= UADDR && vaddr <= UADDR-1+Ctob(Usize)) {
		int pg;
		pg = Btotp(vaddr - UADDR);
		procntry(slot, &prbuf);
		pt = &prbuf.p_ubptbl[pg];
		return(Ctob(pte_to_pfn(pt->pgi.pg_pde))
			 + (vaddr & (target_pagesize-1)));
	}
	if (IS_KSEG2(vaddr)) {
		if(!Kptbl)
			if(!(Kptbl = symsrch("kptbl")))
				error("kptbl not found in symbol table\n");
		if (kptbl == 0)
			readmem((long)Kptbl->n_value,1,-1,(char *)&kptbl,
				sizeof kptbl,"kptbl");
		readmem((long)Kvtokptbl(vaddr),1,-1,(char *)&pde,
			sizeof pde,"pde in kptbl");
		if ((pde.pgi.pg_pde & Pde_pg_vr) == 0) {
			if(abortflag) {
				abortflag = 0;
				error("KSEG2 address %x invalid\n",vaddr);
			}
			return(-1);
		}
		return(Ctob(pte_to_pfn(pde.pgi.pg_pde))
			 + (vaddr & (target_pagesize-1)));
	}
	if (IS_KPTESEG(vaddr)) {
		error("cannot map KPTESEG addr %x\n",vaddr);
	}
	if (IS_KUSEG(vaddr)) {
		preg_t *prp;
		reg_t *rp;
		int tmp;

		procntry(slot, &prbuf);
		if (findreg(&prbuf, vaddr)==0) {
			if(abortflag) {
				abortflag = 0;
				error("KUSEG address %x not in address space\n",vaddr);
			}
			return(-1);
		}
		prp = foundprp;
		rp = foundrp;
		tmp = Btotp(vaddr - (int)prp->p_regva);
		ppt = (pde_t **)&rp->r_list[tmp/Npgpt];
		readmem((long)ppt,1,-1,
			(char *)&pt,sizeof pt,"r_list");
		readmem((long)&pt[tmp%Npgpt],1,-1,
			(char *)&pde,sizeof pde,"r_list");
		if ((pde.pgi.pg_pde & Pde_pg_vr) == 0) {
			if(abortflag) {
				abortflag = 0;
				error("KUSEG address %x not mapped\n",vaddr);
			}
			return(-1);
		}
		return(Ctob(pte_to_pfn(pde.pgi.pg_pde))
			 + (vaddr & (target_pagesize-1)));
	}
	/* impossible to get here */
	error("vtop: can't translate address %x\n",vaddr);
}

/* get arguments for vtop function */
int
getvtop()
{
	int proc = Procslot;
	struct syment *sp;
	unsigned long addr;
	int c;


	optind = 1;
	while((c = getopt(argcnt,args,"w:s:")) !=EOF) {
		switch(c) {
			case 'w' :	redirect();
					break;
			case 's' :	proc = setproc();
					break;
			default  :	longjmp(syn,0);
		}
	}
	if(args[optind]) {
		fprintf(fp,"VIRTUAL  PHYSICAL\n");
		do {
			if(*args[optind] == '(') {
				if((addr = eval(++args[optind])) == -1)
					continue;
				prvtop(addr,proc);
			}
			else if(sp = symsrch(args[optind])) 
				prvtop((long)sp->n_value,proc);
			else if(isasymbol(args[optind]))
				error("%s not found in symbol table\n",
					args[optind]);
			else {
				if((addr = strcon(args[optind],'h')) == -1)
					continue;
				prvtop(addr,proc);
			}
		}while(args[++optind]);
	}
	else longjmp(syn,0);
}

/* print vtop information */
int
prvtop(addr,proc)
unsigned long addr;
int proc;
{
	unsigned long paddr;

	abortflag = 1;
	paddr = vtop(addr,proc);
	fprintf(fp,"%8x %8x\n",
		addr,
		paddr);
	abortflag = 0;
}


/* get arguments for mode function */
int
getmode()
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
		prmode(args[optind]);
	else prmode("s");
}

/* print mode information */
int
prmode(mode)
char *mode;
{

	switch(*mode) {
		case 'p' :  Virtmode = 0;
			    break;
		case 'v' :  Virtmode = 1;
			    break;
		case 's' :  break;
		default  :  longjmp(syn,0);
	}
	if(Virtmode)
		fprintf(fp,"Mode = virtual\n");
	else fprintf(fp,"Mode = physical\n");
}
	
preg_t *
findreg(p, vaddr)
register struct proc	*p;
register caddr_t	vaddr;
{
	register preg_t *prp,*oprp;
	static preg_t pr,opr;
	static reg_t oprp_reg;
	int i;

	if(!Pregpp)
		if(!(Pregpp = symsrch("pregpp")))
			error("pregpp not found in symbol table\n");
	if (pregpp == 0)
		readmem((long)Pregpp->n_value,1,-1,(char *)&pregpp,
			sizeof pregpp,"pregpp");

	oprp = p->p_region;
	readmem((long)oprp,1,-1,
		(char *)&opr,sizeof opr,"pregion[0]");
	for(i = 1; i < pregpp; i++) {
		prp = &p->p_region[i];
		readmem((long)prp,1,-1,
			(char *)&pr,sizeof pr,"pregion[i]");
		if(!pr.p_reg)
			break;
		if (vaddr >= pr.p_regva  &&
		   pr.p_regva > opr.p_regva) {
			oprp = prp;
			opr = pr;
		}
	}
	if (opr.p_reg)
		readmem((long)opr.p_reg,1,-1,
			(char *)&oprp_reg,sizeof oprp_reg,"oprp->p_reg");
	if (opr.p_reg  &&
	   vaddr >= opr.p_regva /* + Ctob(oprp_reg.r_pgoff) */ &&
	   vaddr < (opr.p_regva +
	   Ctob(oprp_reg.r_pgsz + oprp_reg.r_pgoff))) {
		foundprp = &opr;
		foundrp = &oprp_reg;
		return(prp);
	}
	return(NULL);
}
