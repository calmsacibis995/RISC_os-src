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
#ident	"$Header: u.c,v 1.5.1.3.1.1.1.3 91/01/04 17:48:50 beacker Exp $"

/*	Copyright (c) 1984 AT&T	*/
/*	  All Rights Reserved  	*/

/*	THIS IS UNPUBLISHED PROPRIETARY SOURCE CODE OF AT&T	*/
/*	The copyright notice above does not evidence any   	*/
/*	actual or intended publication of such source code.	*/

/*
 * This file contains code for the crash functions:  user, pcb, stack,
 * trace, and kfp.
 */

#include "crash.h"
#include "sys/acct.h"
#include "sys/lock.h"
#include "sys/reg.h"

extern struct user *ubp;		/* ublock pointer */
extern struct ucred *ucredp;
extern int active;			/* active system flag */
struct proc procbuf;			/* proc entry buffer */
int	*stk_bptr;			/* stack pointer */
extern char *ctime();
extern struct	syment	*findsym();
extern long vtop();
extern long lseek();
extern char *malloc();
void free();
int *temp;
extern int target_pagesize;
extern int Usize;
extern int pte_to_pfn_shift;
extern int pfn_to_byte_shift;

/* read ublock into buffer */
int
getublock(slot)
int slot;
{
	int 	i,cnt;
	long	pslot_va;

	if(slot == -1) 
		slot = getcurproc();
	if(slot >=  vbuf.v_proc || slot < 0) {
		prerrmes("%d out of range\n",slot);
		return(-1);
	}

	pslot_va = (long)(Proc->n_value+slot*sizeof(struct proc));
	readmem(pslot_va,1,slot,(char *)&procbuf,sizeof procbuf,
		"process table");
	if (!procbuf.p_stat) {
		prerrmes("%d is not a valid process\n",slot);
		return(-1);
	}
	if (procbuf.p_stat == SZOMB) {
		prerrmes("%d is a zombie process\n",slot);
		return(-1);
	}
	for(cnt=0; cnt < Usize; cnt ++) {
		/* seek from beginning of memory to ith page of uarea */
		if(lseek(mem,
			 (long)(pte_to_byte(procbuf.p_ubptbl[cnt].pgi.pg_pde)),
			 0) == -1) {
			prerrmes("seek error on ublock address\n");
			return(-1);
		}
		if(read(mem,(char *)ubp+cnt*target_pagesize,target_pagesize)
		    != target_pagesize) {
				prerrmes("read error on ublock\n");
				return(-1);
		}
	}
	if (ubp->u_cred != NULL) {
		readmem((long) ubp->u_cred,1,-1,(char *) ucredp,
			sizeof(*ucredp),"u.u_cred");
		ubp->u_cred = ucredp;
	};
#ifdef U_BSD43_EXTENSION
	if (U_BSD43_EXTENSION(ubp)) {
		ubp->u_bsd43_extension_p = (struct u_bsd43_extension *)
			((((char *) ubp->u_bsd43_extension_p) - 
				((char *) UADDR)) + ((char *) ubp));
	};
#endif U_BSD43_EXTENSION
	return(0);
}

/* allocate buffer for stack */
unsigned
setbf(pbottom, ptop, slot)
long *ptop;
long *pbottom;
int slot;
{
	unsigned range;
	char *bptr;
	long remainder;
	long nbyte;
	long paddr;
	long bottom = *pbottom;
	long top = *ptop;


	if (bottom > top) 
		error("Top of stack value less than bottom of stack\n");
	range = (unsigned)(top - bottom);
	/*UH: skip first invalid pages */
	do {
		remainder = ((bottom + NBPP) & ~((long)NBPP -1)) - bottom;
		nbyte = min(remainder, range);
		if((paddr = vtop(bottom,slot)) != -1)
			break;
		bottom += nbyte;
	} while (bottom < top);
	*pbottom = bottom;
	range = (unsigned)(top - bottom);

	if((stk_bptr = (int *)malloc(range)) == NULL)
	{
		prerrmes("Insufficient memory available for stack buffering.\n"); 
		prerrmes("Only the most recently pushed 4K bytes will be dumped from the stack.\n");
		range = 4096;
		top = bottom + range;
		*ptop = top;
		prerrmes("New stack upper bound: %8.8x\n",top);
		if((stk_bptr = (int *)malloc(range)) == 0)
			error("Second attempt to allocate memory for stack buffering failed, try again later\n");
	}
	memset(stk_bptr,0,range);
	bptr = (char *)stk_bptr;
	do {
		remainder = ((bottom + NBPP) & ~((long)NBPP -1)) - bottom;
		nbyte = min(remainder, range);
		if((paddr = vtop(bottom,slot)) != -1) {
			if(lseek(mem,paddr,0) == -1) {
				free((char *)stk_bptr);
				error("seek error on stack\n");
			}
			if(read(mem,bptr,(unsigned)nbyte) != (unsigned)nbyte) {
				free((char *)stk_bptr);
				stk_bptr = NULL;
				error("read error on stack\n");
			}
		}
		bptr += nbyte;
		bottom += nbyte;
	} while (bottom < top);
	return(range);
}

/* get arguments for user function */
int
getuser()
{
	int slot = Procslot;
	int full = 0;
	int all = 0;
	long arg1 = -1;
	long arg2 = -1;
	unsigned lastproc;
	int c;

	optind = 1;
	while((c = getopt(argcnt,args,"efw:")) !=EOF) {
		switch(c) {
			case 'f' :	full = 1;
					break;
			case 'e' :	all = 1;
					break;
			case 'w' :	redirect();
					break;
			default  :	longjmp(syn,0);
		}
	}
	if(args[optind]) {
		do {
			getargs(vbuf.v_proc,&arg1,&arg2);
			if(arg1 == -1) 
				continue;
			if(arg2 != -1)
				for(slot = arg1; slot <= arg2; slot++)
					pruser(full,slot);
			else pruser(full,arg1);
			slot = arg1 = arg2 = -1;
		}while(args[++optind]);
	}
	else if(all) {
		readmem((long)V->n_value,1,-1,(char *)&vbuf,
			sizeof vbuf,"var structure");
		lastproc = (unsigned)(vbuf.ve_proc - Proc->n_value) /
			sizeof (struct proc);
		for(slot =0; slot < lastproc; slot++) 
			pruser(full,slot);
	}
	else pruser(full,slot);
}

/* print ublock */
int
pruser(full,slot)
int full,slot;
{
	register  int  i,j;
	unsigned offset;

	if(getublock(slot) == -1)
		return;
	if(slot == -1)
		slot = getcurproc();
	fprintf(fp,"PER PROCESS USER AREA FOR PROCESS %d\n",slot);
	fprintf(fp,"USER ID's:\t");
	fprintf(fp,"uid: %u, gid: %u, real uid: %u, real gid: %u\n",
		ubp->u_uid,
		ubp->u_gid,
		ubp->u_ruid,
		ubp->u_rgid);
	fprintf(fp,"PROCESS TIMES:\t");
	fprintf(fp,"user: %ld, sys: %ld, child user: %ld, child sys: %ld\n",
		ubp->u_utime,
		ubp->u_stime,
		ubp->u_cutime,
		ubp->u_cstime);
	fprintf(fp,"PROCESS MISC:\n");
	fprintf(fp,"\tcommand: %s,", ubp->u_comm);
	fprintf(fp," psargs: %s\n", ubp->u_psargs);
	fprintf(fp,"\tproc slot: %d", ((unsigned)ubp->u_procp - Proc->n_value)
		/sizeof (struct proc));
	if(ubp->u_ttyp != 0)
		fprintf(fp,", cntrl tty: %3u,%-3u\n",
			major(ubp->u_ttyd),
			minor(ubp->u_ttyd));
	else fprintf(fp,", cntrl tty: u_ttyd = %x\n",ubp->u_ttyd);
	fprintf(fp,"\tstart: %s", ctime(&ubp->u_start));
	fprintf(fp,"\tmem: %x, type: %s%s\n",
		ubp->u_mem,
		ubp->u_acflag & AFORK ? "fork" : "exec",
		ubp->u_acflag & ASU ? " su-user" : "");
	fprintf(fp,"\tproc/text lock:%s%s%s%s\n",
		ubp->u_lock & TXTLOCK ? " txtlock" : "",
		ubp->u_lock & DATLOCK ? " datlock" : "",
		ubp->u_lock & PROCLOCK ? " proclock" : "",
		ubp->u_lock & (PROCLOCK|TXTLOCK|DATLOCK) ? "" : " none");
/*      fprintf(fp,"\tstack: %8x,", ubp->u_stack);   UH: always 0 */
	fprintf(fp,"\tvnode of current directory: ");
	fprintf(fp,"%8x",(long)ubp->u_cdir);
	if(ubp->u_rdir) {
		fprintf(fp,", vnode of root directory: ");
		fprintf(fp,"%8x",(long)ubp->u_rdir);
	}
	fprintf(fp,"\nOPEN FILES AND POFILE FLAGS:\n");
	for(i = 0, j = 0; i < vbuf.v_nofiles; i++){
		if(ubp->u_ofile[i] != 0) {
			if(j == 3) {
				fprintf(fp,"\n");
				j = 0;
			}
			j++;
			fprintf(fp,"\t[%d]: F#%d,",
				i,
				((unsigned)ubp->u_ofile[i] -
				File->n_value)/sizeof (struct file));
			offset = ((char *)(ubp->u_pofile+i) - (char *)UADDR);
			fprintf(fp," %x\t",((char *)ubp)[offset]);
		}
	}
	fprintf(fp,"\n");
	fprintf(fp,"FILE I/O:\n\tu_base: %8x,",ubp->u_base);
	fprintf(fp," file offset: %d, bytes: %d,\n",
		ubp->u_offset,
		ubp->u_count);
	fprintf(fp,"\tsegment: %s,", ubp->u_segflg == 0 ? "data" :
		(ubp->u_segflg == 1 ? "sys" : "text"));
	fprintf(fp," cmask: %4.4o, ulimit: %d\n",
		ubp->u_cmask,
		ubp->u_limit);
	fprintf(fp,"\tfile mode(s):");	
	fprintf(fp,"%s%s%s%s%s%s%s%s%s\n",
		ubp->u_fmode & FREAD ? " read" : "",
		ubp->u_fmode & FWRITE ? " write" : "",
		ubp->u_fmode & FAPPEND ? " append" : "",
		ubp->u_fmode & FSYNC ? " sync" : "",
		ubp->u_fmode & FNET ? " net" : "",
		ubp->u_fmode & FCREAT ? " creat" : "",
		ubp->u_fmode & FTRUNC ? " trunc" : "",
		ubp->u_fmode & FEXCL ? " excl" : "",
		ubp->u_fmode & FNDELAY ? " ndelay" : "");
	fprintf(fp,"SIGNAL DISPOSITION:");
	for (i = 0; i < NSIG; i++) {
		if(!(i & 3))
			fprintf(fp,"\n\t");
		fprintf(fp,"%4d: ", i+1);
		if((int)ubp->u_signal[i] == 0 || (int)ubp->u_signal[i] == 1)
			fprintf(fp,"%8s",(int)ubp->u_signal[i] ? "ignore" : "default");
		else fprintf(fp,"%8x",(int)ubp->u_signal[i]);
	}
	if(full) {
		fprintf(fp,"\nUAREA MISC:");
		fprintf(fp,"\n\tnshmseg: %d, error: %d\n",
			ubp->u_nshmseg,
			ubp->u_error);
		fprintf(fp,"\tap: %x, rval1: %x\n",
			ubp->u_ap,
			ubp->u_rval1);
		fprintf(fp,"\terrcnt: %d\n",
			ubp->u_errcnt);
		fprintf(fp,"\ttsize: %x, dsize: %x, ssize: %x\n",
			ubp->u_tsize,
			ubp->u_dsize,
			ubp->u_ssize);
		fprintf(fp,"\targ[0]: %x, arg[1]: %x, arg[2]: %x\n",
			ubp->u_arg[0],
			ubp->u_arg[1],
			ubp->u_arg[2]);
		fprintf(fp,"\targ[3]: %x, arg[4]: %x, arg[5]: %x\n",
			ubp->u_arg[3],
			ubp->u_arg[4],
			ubp->u_arg[5]);	
		fprintf(fp,"\tar0: %x, ttyp: %x, ticks: %x\n",
			ubp->u_ar0,
			ubp->u_ttyp,
			ubp->u_ticks);
		fprintf(fp,"\tpr_base: %x, pr_size: %d, pr_off: %x, pr_scale: %d\n",
			ubp->u_prof.pr_base,
			ubp->u_prof.pr_size,
			ubp->u_prof.pr_off,
			ubp->u_prof.pr_scale);
		fprintf(fp,"\tior: %x, iow: %x, iosw: %x, ioch: %x\n",
			ubp->u_ior,
			ubp->u_iow,
			ubp->u_iosw,
			ubp->u_ioch);
#ifdef V5.3.1
		fprintf(fp, "\tsysabort: %d, systrap: %d\n",
			ubp->u_sysabort,
			ubp->u_systrap);
		fprintf(fp, "\tentrymask:");
		for (i = 0; i < SYSMASKLEN; i++)
			fprintf(fp, " %08x", ubp->u_entrymask[i]);
		fprintf(fp, "\n");
		fprintf(fp, "\texitmask:");
		for (i = 0; i < SYSMASKLEN; i++)
			fprintf(fp, " %08x", ubp->u_exitmask[i]);
		fprintf(fp, "\n");
#endif
		/* now the mips stuff */
		fprintf(fp,"\tsigtramp: %x, code: %x, trapcause: %x, trapinfo: %x\n",
			ubp->u_sigtramp,
			ubp->u_code,
			ubp->u_trapcause,
			ubp->u_trapinfo);
		fprintf(fp,"\tEXDATA:\n");
		fprintf(fp,"\tvp: ");
		fprintf(fp,"%8x, ",ubp->u_exdata.vp);
		fprintf(fp,"tsize: %x, dsize: %x, bsize: %x, lsize: %x\n",
			ubp->u_exdata.ux_tsize,
			ubp->u_exdata.ux_dsize,
			ubp->u_exdata.ux_bsize,
			ubp->u_exdata.ux_lsize);
		fprintf(fp,"\tmagic#: %o, toffset: %x, doffset: %x, loffset: %x\n",
			ubp->u_exdata.ux_mag,
			ubp->u_exdata.ux_toffset,
			ubp->u_exdata.ux_doffset,
			ubp->u_exdata.ux_loffset);
		fprintf(fp,"\ttxtorg: %x, datorg: %x, entloc: %x, nshlibs: %d\n",
			ubp->u_exdata.ux_txtorg,
			ubp->u_exdata.ux_datorg,
			ubp->u_exdata.ux_entloc,
			ubp->u_exdata.ux_nshlibs);
		fprintf(fp,"\texecsz: %x\n",ubp->u_execsz);
		fprintf(fp,"\ttracepc: %x\n",ubp->u_tracepc);
		fprintf(fp,"\tsyscall: %d\n",
			ubp->u_syscall);
	}
	fprintf(fp,"\n");
}

/* get arguments for pcb function */
int
getpcb()
{
	int proc = Procslot;
	int phys = 0;
	long addr = -1;
	int c;
	struct syment *sp;

	optind = 1;
	while((c = getopt(argcnt,args,"pw:")) !=EOF) {
		switch(c) {
			case 'w' :	redirect();
					break;
			case 'p' :	phys = 1;
					break;
			default  :	longjmp(syn,0);
		}
	}
	if(args[optind]) {
		if((proc = strcon(args[optind],'d')) == -1)
			error("\n");
		if((proc > vbuf.v_proc) || (proc < 0))
			error("%d out of range\n",proc);
		prpcb(proc);
	}
	else prpcb(proc);
}


/* print user, kernel, or active pcb */
int
prpcb(proc)
int proc;
{
	int	i, j;
	pcb_t *pcbp;

	if(getublock(proc) == -1)
		return;
	pcbp = &ubp->u_pcb;
	fprintf(fp,"resched: %x, bd_epc: %x, bd_cause: %x, bd_ra: %x, bd_instr: %x\n",
		pcbp->pcb_resched,
		pcbp->pcb_bd_epc,
		pcbp->pcb_bd_cause,
		pcbp->pcb_bd_ra,
		pcbp->pcb_bd_instr);
	fprintf(fp,"softfp_pc: %x, fpc_csr: %x, fpc_eir: %x, ownedfp: %x\n",
		pcbp->pcb_softfp_pc,
		pcbp->pcb_fpc_csr,
		pcbp->pcb_fpc_eir,
		pcbp->pcb_ownedfp);
	fprintf(fp,"sstep: %x, ssi_cnt: %d:, bp[0] = %x,%x, bp[1] =  %x,%x\n",
		pcbp->pcb_sstep,
		pcbp->pcb_ssi.ssi_cnt,
		pcbp->pcb_ssi.ssi_bp[0].bp_addr,
		pcbp->pcb_ssi.ssi_bp[0].bp_inst,
		pcbp->pcb_ssi.ssi_bp[1].bp_addr,
		pcbp->pcb_ssi.ssi_bp[1].bp_inst);
	fprintf(fp,"regs: (S0-S7, SP, FP, PC, SR)\n");
	for (i=0; i<NPCBREGS; i++) {
		fprintf(fp,"  %08x",pcbp->pcb_regs[i]);
		if ((i & 3) == 3)
			fprintf(fp,"\n");
	}
}

/* get arguments for stack function */
int
getstack()
{
	int proc = Procslot;
	int phys = 0;
	char type = 'k';
	long addr = -1;
	int c;
	struct syment *sp;

	optind = 1;
	while((c = getopt(argcnt,args,"ukpw:")) !=EOF) {
		switch(c) {
			case 'w' :	redirect();
					break;
			case 'p' :	phys = 1;
					break;
			case 'u' :	type = 'u';
					break;
			case 'k' :	type = 'k';
					break;
			default  :	longjmp(syn,0);
		}
	}
	if(args[optind]) {
		if((proc = strcon(args[optind],'d')) == -1)
			error("\n");
		if((proc > vbuf.v_proc) || (proc < 0))
			error("%d out of range\n",proc);
		if(type == 'u')
			prustk(proc);
		else prkstk(proc);
	}
	else if(type == 'u')
		prustk(proc);
	else prkstk(proc);
}

/* print kernel stack */
int
prkstk(proc)
int proc;
{
	long stkhi ;
	long stklo ;

	if(getublock(proc) == -1)
		return;
	stklo = UADDR + (unsigned)&ubp->u_ofile[vbuf.v_nofiles] - (unsigned)ubp + vbuf.v_nofiles;
	stklo = (stklo + 15) & ~15;
	stkhi = KERNELSTACK;
	prstack(stklo,stkhi,proc);
}


/* print user stack */
int
prustk(proc)
int proc;
{
	long                    stkhi;
	long                    stklo;

	if(getublock(proc) == -1)
		return;
	stklo = USERSTACK-Ctob(ubp->u_ssize);
	stkhi = USERSTACK;
	prstack(stklo,stkhi,proc);
}

/* dump stack */
int
prstack(stklo,stkhi,slot)
long stkhi,stklo;
int slot;
{
	unsigned dmpcnt;
	register int *stkptr, *stkend;
	int prcnt;

	if ( stkhi < stklo)
		error("upper bound < lower bound, unable to process stack\n") ;
	
	dmpcnt = setbf(&stklo, &stkhi, slot);
	stkptr = stk_bptr;
	stkend = (int *)((unsigned)stkptr + dmpcnt);

	for( ; stkptr < (int *)stkend; stkptr++)
		if(*stkptr)
			break;

	while(((unsigned) stkptr - (unsigned) stk_bptr) % 16)
		stkptr--;	/* back up to even boundry */
	stkptr -= 4;            /* back up 4 words more */

	dmpcnt -= (unsigned) stkptr - (unsigned) stk_bptr;

	prcnt = 0;
	for(; dmpcnt != 0; stkptr++, dmpcnt -= sizeof(*stkptr))
	{
		if((prcnt++ % 4) == 0){
			if (prcnt != 1)
				dumpascii(stkptr-4);
			fprintf(fp,"\n%8.8x: ",
				(int)(((long)stkptr - (long)stk_bptr) + stklo));
		}
		fprintf(fp,"  %8.8x", *stkptr);
	}
	if(prcnt != 0)
		dumpascii(stkptr-4);
	fprintf(fp,"\n");
	free((char *)stk_bptr);
	stk_bptr = NULL;
}

dumpascii(ptr)
register char *ptr;
{
	register i;
	fprintf(fp,"  |");
	for (i=0; i<16;i++,ptr++) {
		if (*ptr >= ' ' && *ptr < 0x7f)
			fputc(*ptr,fp);
		else fputc(' ',fp);
	}
	fputc('|',fp);
}

/* get arguments for trace function */
int
gettrace()
{
	int proc = Procslot;
	int phys = 0;
	int all = 0;
	int flag = 0;
	long arg1 = -1;
	long arg2 = -1;
	int c;
	unsigned lastproc;
	struct syment *sp;

	optind = 1;
	while((c = getopt(argcnt,args,"ersapw:")) !=EOF) {
		switch(c) {
			case 'w' :	redirect();
					break;
			case 'p' :	phys = 1;
					break;
			case 'e' :	all = 1;
					break;
			case 'r' :      flag = 1;
					break;
			case 's' :      flag = 2;
					break;
			case 'a' :      flag = 3;
					break;
			default  :	longjmp(syn,0);
		}
	}
	if(args[optind]) {
		do {
			getargs(vbuf.v_proc,&arg1,&arg2);
			if(arg1 == -1)
				continue;
			if(arg2 != -1)
				for(proc = arg1; proc <= arg2; proc++)
					prktrace(proc,flag);
			else prktrace(arg1,flag);
			proc = arg1 = arg2 = -1;
		}while(args[++optind]);
	}
	else if(all) {
		readmem((long)V->n_value,1,-1,(char *)&vbuf,
			sizeof vbuf,"var structure");
		lastproc = (unsigned)(vbuf.ve_proc - Proc->n_value) /
			sizeof (struct proc);
		for(proc =0; proc < lastproc; proc++)
			prktrace(proc,flag);
	}
	else prktrace(proc,flag);

}

prktrace(slot,flag)
	int     slot;
	int     flag;
{
	extern unsigned long startoftext, endoftext;
	register struct  syment  *sp;
	register struct  procent  *pp;
	unsigned long kpc, ksp;
	static lastslot;
	register long *ep = 0;
	register char *name;

	if(getublock(slot) == -1)
		return;

	/* flag = 0 means -s if proc sleeping, -r otherwise */
	if (flag == 0 && (procbuf.p_stat == SSLEEP ||
			 procbuf.p_stat == SXBRK))
		flag = 2;

	/* -a on new slot means -r */
	if (flag == 3 && lastslot != slot)
		flag = 1;
	lastslot = slot;

	fprintf(fp,"STACK TRACE FOR PROCESS %d:\n",slot);
	if (flag == 2) {
		/* can take sp, pc from pcb */
		ksp = ubp->u_pcb.pcb_regs[JB_SP];
		kpc = ubp->u_pcb.pcb_regs[JB_PC];
	}
	else {
		/* must use heuristics for a possible trace */
		if (findksp(&ksp, &kpc, flag) < 0) {
			prerrmes("can't find a good stack trace\n");
			return;
		}
	}
	if (ksp < UADDR || ksp > (UADDR-1+Ctob(Usize))) {
		fprintf(fp,"\tSP=%lx invalid\n",ksp);
		return;
	}
	do {
		if (kpc < startoftext || kpc > endoftext || (kpc & 3)) {
			fprintf(fp,"trace stops because bad pc %x found\n", kpc);
			return;
		}
		sp = findsym(kpc);
		name = sp->n_name;
		prlineno(kpc,sp);
		fputc('\n', fp);
		/* if name starts with VEC, we have an exception frame */
		if (name[0] == 'V' && name[1] == 'E' && name[2] == 'C') {
			ep = (long *)(ksp - UADDR + (long)ubp);
			if (badep(ep))
				return;
			printep(ep);
			ksp = ep[EF_SP];
			kpc = ep[EF_RA];
		} else {
			pp = sp->n_proc;
			if (pp == 0) {
				fprintf(fp,"trace stops because no procedure description for %x (%s + %x)\n",
					kpc, name, kpc - sp->n_value);
				return;
			}
			/* this cannot happen, would mean a leafnode called someone.. */
			if ((pp->regmask & 0x80000000) == 0 ||
			    pp->frameoffset == 0 && pp->regoffset == 0)
				return;
			ksp += pp->frameoffset;
			kpc = ksp + pp->regoffset - UADDR + (long)ubp;
			kpc = *(long *)kpc;
		}
	} while(ksp >= UADDR && ksp <= UADDR-1+Ctob(Usize));
}

findksp(psp, ppc, flag)
unsigned long *psp, *ppc;
{
	register unsigned long pc, pce, sp;
	static unsigned long lastpc;
#define MAXREGOFF 28     /* maximum difference of pc and sp */

	if (flag != 3 || lastpc == 0)
		/* start at end of struct user u */
		pc = (unsigned)&ubp->u_ofile[vbuf.v_nofiles] + vbuf.v_nofiles;
	else pc = lastpc + sizeof(long);
	pc &= ~(sizeof(long) - 1);
	pce = (unsigned)ubp + Ctob(Usize) - EF_SIZE;

	/* try to find a combination (pc,sp) that can be traced
	   through the whole stack.. quite expensive
	   look first for a pc value on the stack, then use the next
	   few (MAXREGOFF/4) words as possible sp values
	*/

	for ( ; pc < pce; pc += sizeof(long)) {
		if (*(unsigned *)pc < startoftext || *(unsigned *)pc >= endoftext)
			continue;
/*printf("pc %x\n",*(unsigned *)pc);    */
		for (sp = pc + sizeof(long); sp < pc + MAXREGOFF; sp += sizeof(long)) {
/*printf("sp %x\n",sp-(unsigned)ubp+UADDR);     */
			if (successful(*(unsigned *)pc,sp)) {
				lastpc = pc;
				*psp = sp-(unsigned)ubp+UADDR;
				*ppc = *(unsigned *)pc;
				return(0);
			}
		}
	}
	lastpc = 0;
	return(-1);
}

/* for a given pair (pc,sp), see if this leads to a stacktrace that
   can be traced up to the end of the stack
*/
successful(pc, sp)
register unsigned long pc, sp;
{
	register struct syment *sy;
	register struct procent *pp;
	register unsigned long spe = (unsigned)ubp + (KERNELSTACK-UADDR);
	register long *ep = 0;
	register char *name;

	for(;;) {
		if (pc < startoftext || pc > endoftext || (pc & 3))
			return(0);
		sy = findsym(pc);
		pp = sy->n_proc;
		if (pp == 0)
			return(0);
		name = sy->n_name;
		if (name[0] == 'V' && name[1] == 'E' && name[2] == 'C') {
			ep = (long *)sp;
			if (badep(ep))
				return(0);
			sp += pp->frameoffset;
			if (sp == spe)
				return(1);
			sp = ep[EF_SP] - UADDR + (unsigned)ubp;
			if (sp == spe)
				return(1);
			if (sp > spe)
				return(0);
			pc = ep[EF_RA];
		} else {
			sp += pp->frameoffset;
			if (sp == spe)
				return(1);
			if ((pp->regmask & 0x80000000) == 0 ||
			     pp->frameoffset == 0 && pp->regoffset == 0)
				return(0);
			if (sp > spe)
				return(0);
			pc = sp + pp->regoffset;
			pc = *(long *)pc;
		}
	}
}

static char *regnames[] = {
		"r0/zero",      "r1/at ",       "r2/v0 ",       "r3/v1 ",
		"r4/a0  ",      "r5/a1 ",       "r6/a2 ",       "r7/a3 ",
		"r8/t0  ",      "r9/t1 ",       "r10/t2",       "r11/t3",
		"r12/t4 ",      "r13/t5",       "r14/t6",       "r15/t7",
		"r16/s0 ",      "r17/s1",       "r18/s2",       "r19/s3",
		"r20/s4 ",      "r21/s5",       "r22/s6",       "r23/s7",
		"r24/t8 ",      "r25/t9",       "r26/k0",       "r27/k1",
		"r28/gp ",      "r29/sp",       "r30/fp",       "r31/ra",
};

printep(ep)
register long *ep;
{
	register char *s;
	register i;
	long epc;

	fprintf(fp,"\t%s:%08x ",regnames[0],0);
	for (i = 1; i < 32; i++) {
		fprintf(fp,"%s:%08x ",regnames[i],ep[i+EF_AT-1]);
		if ((i & 3) == 3)
			fprintf(fp,"\n\t");
	}

	switch(ep[EF_CAUSE] & CAUSE_EXCMASK) {
	case EXC_INT:        s="INT"; break;
	case EXC_MOD:        s="MOD"; break;
	case EXC_RMISS:      s="RMISS"; break;
	case EXC_WMISS:      s="WMISS"; break;
	case EXC_RADE:       s="RADE"; break;
	case EXC_WADE:       s="WADE"; break;
	case EXC_IBE:        s="IBE"; break;
	case EXC_DBE:        s="DBE"; break;
	case EXC_SYSCALL:    s="SYSCALL"; break;
	case EXC_BREAK:      s="BREAK"; break;
	case EXC_II:         s="II"; break;
	case EXC_CPU:        s="CPU"; break;
	case EXC_OV:         s="OV"; break;
	case EXC_TRAP:	     s="TRAP"; break;
	case EXC_DBL_NC:     s="DBL_NC"; break;
	case EXC_CHECK:	     s="CHECK"; break;
	default:             s="???"; break;
	}
	epc = ep[EF_EPC];
	fprintf(fp,"EPC=%x, CAUSE=%x (%s), SR=%x, BADVADDR=%x\n",
		epc,
		ep[EF_CAUSE],
		s,
		ep[EF_SR],
		ep[EF_BADVADDR]);
	if (epc >= startoftext && epc < endoftext) {
		fprintf(fp,"\tEPC=");
		prlineno(epc, findsym(epc));
		fputc('\n', fp);
	}
}

/* plausibility test for an ep-vector, return nonnull if incorrect */
/* if returns 0, doesnt mean that frame is ok however! */
badep(ep)
register unsigned long *ep;
{
	register unsigned long v;
#define MAXTEXT 0x600000        /* assume user program < 2M code (normally starts at 0x400000) */
#define MAXSTACK 0x7f800000     /* assume user program < 8M stack */
	v = ep[EF_RA];
	if (v&3)
		return(1);
	if (v > K0SEG && (v < startoftext || v >= endoftext))
		return(2);
	if (v < K0SEG && v > MAXTEXT)
		return(3);
	v = ep[EF_EPC];
	if (v&3)
		return(4);
	/* EPC may point to utlbmiss */
	if (v > (K0SEG+0x100) && (v < startoftext || v >= endoftext))
		return(5);
	if (v < K0SEG && v > MAXTEXT)
		return(6);
	v = ep[EF_SP];
	if (v&3)
		return(7);
	if (v > K0SEG && (v < UADDR || v > UADDR-1+Ctob(Usize)))
		return(8);
	if (v < MAXSTACK)
		return(9);
	v = ep[EF_CAUSE];
	if (v & 0x4fff00c3)     /* bits that should be zero */
		return(10);
	v = ep[EF_SR];
	if (v & 0x0fc000c0)     /* bits that should be zero */
		return(11);
	return(0);
}
