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
#ident	"$Header: proc.c,v 1.5.1.2.1.1.1.2 90/12/20 19:14:54 beacker Exp $"

/*	Copyright (c) 1984 AT&T	*/
/*	  All Rights Reserved  	*/

/*	THIS IS UNPUBLISHED PROPRIETARY SOURCE CODE OF AT&T	*/
/*	The copyright notice above does not evidence any   	*/
/*	actual or intended publication of such source code.	*/

/*
 * This file contains code for the crash functions:  proc, defproc.
 */

#include "crash.h"
#include "time.h"

extern uint Pde_pg_vr,
	    Pde_pg_g,
	    Pde_pg_n,
	    Pde_pg_m,
	    Pde_pg_lock,
	    Pde_pg_cw,
	    Pde_pg_sv;
extern int  pte_to_pfn_shift;

extern int active;
extern struct user *ubp;		/* pointer to the ublock */
int pregpp;

/* get arguments for proc function */
int
getproc()
{
	int slot = Procslot;
	int all = 0;
	int full = 0;
	int phys = 0;
	int run = 0;
	long addr = -1;
	long arg1 = -1;
	long arg2 = -1;
	int id = -1;
	int c;
	char *heading = "SLOT ST PID   PPID  PGRP   UID PRI CPU   EVENT     NAME        FLAGS\n";

	if(!Pregpp)
		if(!(Pregpp = symsrch("pregpp")))
			error("pregpp not found in symbol table\n");
	if(!Region)
		if(!(Region = symsrch("region")))
			error("region not found in symbol table\n");

	optind = 1;
	while((c = getopt(argcnt,args,"efprw:")) !=EOF) {
		switch(c) {
			case 'e' :	all = 1;
					break;
			case 'f' :	full = 1;
					break;
			case 'w' :	redirect();
					break;
			case 'r' :	run = 1;
					break;
			case 'p' :	phys = 1;
					break;
			default  :	longjmp(syn,0);
		}
	}
	fprintf(fp,"PROC TABLE SIZE = %d\n",vbuf.v_proc);
	if(!full)
		fprintf(fp,"%s",heading);
	if(args[optind]) {
		all = 1;
		do {
			if(*args[optind] == '#') {
				if((id = strcon(++args[optind],'d')) == -1)
					error("\n");
				prproc(all,full,slot,id,phys,run,addr,heading);
			}
			else {
				getargs(- vbuf.v_proc,&arg1,&arg2);
				if(arg1 == -1) 
					continue;
				if(arg2 != -1) {
					if (arg1 >= 0 &&
					    arg1 < vbuf.v_proc) {
						for(slot = arg1; slot <= arg2; slot++)
							prproc(all,full,slot,id,phys,
								run,addr,heading);
					} else {
						for (addr = arg1; addr <= arg2;
						     addr = (long) (((struct proc *) addr) + 1))
							prproc(all,full,-1,id,phys,run,addr,heading);
					}
				} else {
					if(arg1 >= 0 &&
					   arg1 < vbuf.v_proc)
						slot = arg1;
					else addr = arg1;
					prproc(all,full,slot,id,phys,run,addr,
						heading);
				}
			}
			id = slot = addr = arg1 = arg2 = -1;
		}while(args[++optind]);
	}
	else for(slot = 0; slot < vbuf.v_proc; slot++)
		prproc(all,full,slot,id,phys,run,addr,heading);
}


/* print proc table */
int
prproc(all,full,slot,id,phys,run,addr,heading)
int all,full,slot,id,phys,run;
long addr;
char *heading;
{
	char ch,*typ;
	char cp[PSCOMSIZ+1];
	long pslot_va;
	struct proc procbuf;
	struct pregion pregbuf;
	int i,j,cnt,regslot;
	extern long lseek();

	if(id != -1) {
		for(slot = 0; slot < vbuf.v_proc; slot++) {
			readmem((long)(Proc->n_value+slot*sizeof procbuf),1,
				slot,(char *)&procbuf,sizeof procbuf,
					"proc table");
				if(procbuf.p_pid == id) 
					break;
		}
		if(slot == vbuf.v_proc) {
			fprintf(fp,"%d not valid process id\n",id);
			return;
		}
	}
	else readbuf(addr,(long)(Proc->n_value+slot*sizeof procbuf),phys,-1,
		(char *)&procbuf,sizeof procbuf,"proc table");
	if(!procbuf.p_stat && !all)
		return;
	if(run)
		if(!(procbuf.p_stat == SRUN || procbuf.p_stat == SONPROC))
			return;
	if(addr > -1) 
		slot = getslot(addr,(long)Proc->n_value,sizeof procbuf,phys);
	if(full)
		fprintf(fp,"%s",heading);
	switch(procbuf.p_stat) {
	case NULL:   ch = ' '; break;
	case SSLEEP: ch = 's'; break;
	case SRUN:   ch = 'r'; break;
	case SIDL:   ch = 'i'; break;
	case SZOMB:  ch = 'z'; break;
	case SSTOP:  ch = 't'; break;
	case SONPROC:  ch = 'p'; break;
	case SXBRK:  ch = 'x'; break;
	default:     ch = '?'; break;
	}
	if(slot == -1)
		fprintf(fp,"  - ");
	else fprintf(fp,"%4d",slot);
	fprintf(fp," %c %5u %5u %5u %5u %3u %3u",
		ch,
		procbuf.p_pid,
		procbuf.p_ppid,
		procbuf.p_pgrp,
		procbuf.p_uid,
		procbuf.p_pri,
		procbuf.p_cpu);
	if(procbuf.p_stat == SONPROC)
		fprintf(fp,"          ");
	else fprintf(fp," %08lx ",procbuf.p_wchan);
	for(i = 0; i < PSCOMSIZ+2; i++)
		cp[i] = '\0';
	pslot_va = (long)(Proc->n_value+slot*sizeof(struct proc));
	if(procbuf.p_stat == SZOMB)
		strcpy(cp,"zombie");
	else if(getublock(slot) == -1)
		strcpy(cp,"read err on uarea");
	else
		strcpy(cp, ubp->u_comm);
	for(i = 0; i < 8 && cp[i]; i++) {
		if(cp[i] < 040 || cp[i] > 0176) {
			strcpy(cp,"unprint");
			break;
		}
	}
	fprintf(fp,"%-14s", cp);
	fprintf(fp,"%s%s%s%s%s%s%s%s%s%s%s%s%s%s%s%s%s%s%s%s%s%s\n",
		procbuf.p_flag & SLOAD ? " load" : "",
		procbuf.p_flag & SSYS ? " sys" : "",
		procbuf.p_flag & SLOCK ? " lock" : "",
		procbuf.p_flag & STRC ? " trc" : "",
 		procbuf.p_flag & SWTED ? " wted" : "",
 		procbuf.p_flag & SNWAKE ? " nwak" : "",
 		procbuf.p_flag & SRSIG ? " rsig" : "",
 		procbuf.p_flag & SPOLL ? " poll" : "",
 		procbuf.p_flag & SPRSTOP ? " prst" : "",
 		procbuf.p_flag & SPROCTR ? " prtr" : "",
 		procbuf.p_flag & SPROCIO ? " prio" : "",
 		procbuf.p_flag & SSEXEC ? " sx" : "",
 		procbuf.p_flag & SPROPEN ? " prop" : "",
		procbuf.p_flag & SOWEUPC ? " oweupc" : "",
		procbuf.p_flag & SFIXADE ? " fixade" : "",
#ifdef V5.3.1
		(procbuf.p_flag & (SLOAD|SULOAD)) == SULOAD ? " uload" : "",
		procbuf.p_flag & SRUNLCL ? " rlcl" : "",
		procbuf.p_flag & SNOSTOP ? " nstp" : "",
		procbuf.p_flag & SPTRX ? " ptrx" : "",
		procbuf.p_flag & SASLEEP ? " aslp" : "",
		procbuf.p_flag & SUSWAP ? " uswp" : "",
		procbuf.p_flag & SUWANT ? " uwnt" : "");
#else
		"","","","","","","");
#endif
	if(!full)
		return;
 	fprintf(fp,"\ttime: %d, nice: %d, exit %d\n",
		procbuf.p_time,
		procbuf.p_nice,  
		procbuf.p_xstat);  
#ifdef V5.3.1
	fprintf(fp,"\tsig: %x, cursig: %d.%06d, clktim: %d, suid: %d, sgid: %d, size: %d\n",
#else
	fprintf(fp,"\tsig: %x, clktim: %d.%06d, suid: %d, sgid: %d, size: %d\n",
#endif
		procbuf.p_sig,
#ifdef V5.3.1
		procbuf.p_cursig,
#endif
		procbuf.p_realtimer.it_value.tv_sec,
		procbuf.p_realtimer.it_value.tv_usec,
		procbuf.p_suid,
		procbuf.p_sgid,
		procbuf.p_size);
	fprintf(fp,"\tflink: %x\tblink: %x\n",
		procbuf.p_flink,
		procbuf.p_blink);
	fprintf(fp,"\tparent: %x\tchild: %x\tsibling: %x\n",
		procbuf.p_parent,
		procbuf.p_child,
		procbuf.p_sibling);
	if(procbuf.p_flink)
		fprintf(fp,"\tflink: %d",
		((unsigned)procbuf.p_flink - Proc->n_value) /sizeof (procbuf));
	if(procbuf.p_blink)
		fprintf(fp,"\tblink: %d",
		((unsigned)procbuf.p_blink - Proc->n_value) /sizeof (procbuf));
	if(procbuf.p_mlink)
		fprintf(fp,"\tmlink: %d\n",
		((unsigned)procbuf.p_mlink - Proc->n_value) /sizeof (procbuf));
	fprintf(fp,"\tutime: %ld\tstime: %ld\n",
		procbuf.p_utime,procbuf.p_stime);
	fprintf(fp,"\n\tepid: %d, sysid: %x, minwd: %x, rlink: %x\n",
		procbuf.p_epid,
		procbuf.p_sysid,
		procbuf.p_minwd,
		procbuf.p_rlink);
	fprintf(fp,"\ttrlock: %d, trace: %x, sigmask: %x,",
		procbuf.p_trlock,
		procbuf.p_trace,
		procbuf.p_sigmask);
	fprintf(fp," mpgneed: %d\n",procbuf.p_mpgneed);
	fprintf(fp,"\thold: %x, chold: %x\n",
		procbuf.p_hold,
		procbuf.p_chold);
#ifdef V5.3.1
	fprintf(fp, "\twhystop: %d, whatstop: %d\n",
		procbuf.p_whystop,
		procbuf.p_whatstop);
#endif
	/* now the mips stuff: */
	fprintf(fp,"\tnexttlb: %d, tlbpid: %d, fp: %d\n",
		procbuf.p_nexttlb,
		procbuf.p_tlbpid,
		procbuf.p_fp);
	fprintf(fp,"\twired entries:\n\tTLBHI       PFN FLAGS\n");
	for (i=0; i<procbuf.p_nexttlb; i++)
		fprintf(fp,"\t%8x %6x %s%s%s%s%s%s%s\n",
			procbuf.p_tlbhi_tbl[i],
			procbuf.p_ubptbl[i].pgi.pg_pde >> pte_to_pfn_shift,
			(procbuf.p_ubptbl[i].pgi.pg_pde & Pde_pg_n)
				? "nocache " : "",
			(procbuf.p_ubptbl[i].pgi.pg_pde & Pde_pg_m)
				? "mod " : "",
			(procbuf.p_ubptbl[i].pgi.pg_pde & Pde_pg_vr)
				? "rvalid " : "",
			(procbuf.p_ubptbl[i].pgi.pg_pde & Pde_pg_g)
				? "global " : "",
			(procbuf.p_ubptbl[i].pgi.pg_pde & Pde_pg_lock)
				? "lock " : "",
			(procbuf.p_ubptbl[i].pgi.pg_pde & Pde_pg_sv)
				? "svalid " : "",
			(procbuf.p_ubptbl[i].pgi.pg_pde & Pde_pg_cw)
				? "cw " : "");

	/* locate and print per process region table */

	if (pregpp == 0)
		readmem((long)Pregpp->n_value,1,slot,(char *)&pregpp,
			sizeof pregpp,"pregpp");

	fprintf(fp,"\tpreg reg#   regva  type  flags\n");
	for( i = 0; i < pregpp; i++) {
	readmem((long)(procbuf.p_region+i),1,slot,
		(char *)&pregbuf,sizeof pregbuf,"pregion table");

		if(!pregbuf.p_reg) 
			break;
		fprintf(fp,"\t%4d ",i);
		regslot = ((long)pregbuf.p_reg-Region->n_value)/
			sizeof(struct region);
		if((regslot >= 0) && (regslot < vbuf.v_region))
			fprintf(fp,"%4d ",regslot);
		else fprintf(fp,"  -  ");
		fprintf(fp,"%8x ",pregbuf.p_regva);
		switch(pregbuf.p_type) {
			case PT_UNUSED: typ = "unusd"; break;
			case PT_TEXT: typ = "text"; break;
			case PT_DATA: typ = "data"; break;
			case PT_STACK: typ = "stack"; break;
			case PT_SHMEM: typ = "shmem"; break;
			case PT_GR: typ = "gr"; break;
			case PT_LIBTXT: typ = "lbtxt"; break;
			case PT_LIBDAT: typ = "lbdat"; break;
			default: typ = ""; break;
		}
		fprintf(fp,"%5s ",typ);
		fprintf(fp,"%s\n",
			pregbuf.p_flags & PF_RDONLY ? "rdonly" : "");
	}
	fprintf(fp,"\n");
}


/* get arguments for defproc function */
int
getdefproc()
{
	int c;
	int proc = -1;
	int reset = 0;

	optind = 1;
	while((c = getopt(argcnt,args,"cw:")) !=EOF) {
		switch(c) {
			case 'w' :	redirect();
					break;
			case 'c' :	reset = 1;
					break;
			default  :	longjmp(syn,0);
		}
	}
	if(args[optind]) 
		if((proc = strcon(args[optind],'d')) == -1)
			error("\n");
	prdefproc(proc,reset);
}

/* print results of defproc function */
int
prdefproc(proc,reset)
int proc,reset;
{

	if(reset)
		Procslot = getcurproc();
	else if(proc > -1) {
		if((proc > vbuf.v_proc) || (proc < 0))
			error("%d out of range\n",proc);
		Procslot = proc;
	}
	fprintf(fp,"Procslot = %d\n",Procslot);
}
