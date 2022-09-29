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
#ident	"$Header: page.c,v 1.4.1.4 90/05/09 15:27:21 wje Exp $"

/*	Copyright (c) 1984 AT&T	*/
/*	  All Rights Reserved  	*/

/*	THIS IS UNPUBLISHED PROPRIETARY SOURCE CODE OF AT&T	*/
/*	The copyright notice above does not evidence any   	*/
/*	actual or intended publication of such source code.	*/

/*
 * This file contains code for the crash functions: pfdat, region, sdt, and pdt.
 */

#include "crash.h"

extern int  target_pagesize;
extern int  pfn_to_byte_shift;
extern long vtop();
extern long lseek();

/* get arguments for pfdat function */
int
getpfdat()
{
	int slot = -1;
	int all = 0;
	int phys = 0;
	long addr = -1;
	long arg1 = -1;
	long arg2 = -1;
	int c;
	struct syment *maxc, *kpb;
	unsigned  size, maxclick, kpbase;

	if(!Pfdat)
		if(!(Pfdat = symsrch("pfdat")))
			error("pfdat not found in symbol table\n");


	optind = 1;
	while((c = getopt(argcnt,args,"epw:")) !=EOF) {
		switch(c) {
			case 'e' :	all = 1;
					break;
			case 'w' :	redirect();
					break;
			case 'p' :	phys = 1;
					break;
			default  :	longjmp(syn,0);
		}
	}
	/* collect system variables to calculate size */
	if(!(maxc = symsrch("maxclick"))) 
		error("maxclick not found in symbol table\n");
	readmem((long)maxc->n_value,1,-1,(char *)&maxclick,
		sizeof maxclick,"maxclick");
	
	if(!(kpb = symsrch("kpbase"))) 
		error("kpbase not found in symbol table\n");
	readmem((long)kpb->n_value,1,-1,(char *)&kpbase,
		sizeof kpbase,"kpbase");

	size = (maxclick*target_pagesize - kpbase)/target_pagesize;

	fprintf(fp,"PFDATA TABLE SIZE: %d\n", size);
	fprintf(fp,"SLOT   BLKNO  USE DEVID    NODEID SWPI RCNT WCNT NEXT PREV HASH FLAGS\n");
	if(args[optind]) {
		all = 1;
		do {
			getargs(- ((int)size),&arg1,&arg2);
			if(arg1 == -1) 
				continue;
			if(arg2 != -1) {
				if (arg1 >= 0 &&
				    arg1 < (int) size) {
					for(slot = arg1; slot <= arg2; slot++)
						prpfdat(all,slot,phys,addr,size);
				} else {
					for (addr = arg1; addr <= arg2;
					     addr = (long) (((pfd_t *) addr) + 1))
						prpfdat(all,-1,phys,addr,size);
				};
			} else {
				if(arg1 >= 0 &&
				   arg1 < (int) size)
					slot = arg1;
				else addr = arg1;
				prpfdat(all,slot,phys,addr,size);
			}
			slot = addr = arg1 = arg2 = -1;
		}while(args[++optind]);
	}
	else for(slot = 0; slot < size; slot++)
		prpfdat(all,slot,phys,addr,size);
}


/* print pfdata table */
int
prpfdat(all,slot,phys,addr,size)
int all,slot,phys;
long addr;
unsigned size;
{

	long pfdat;
	pfd_t pfbuf;
	long next, prev, hash;

	readmem((long)Pfdat->n_value,1,-1,
		(char *)&pfdat,sizeof pfdat,"pfdat pointer");

	readbuf(addr,(long)(pfdat+slot*sizeof (struct pfdat)),phys,-1,
		(char *)&pfbuf,sizeof pfbuf,"pfdata table");
	if(!pfbuf.pf_flags && !all)
		return;
	/* calculate pfdata entry number of pointers */
	next = ((long)pfbuf.pf_next - pfdat)/sizeof pfbuf;
	if ((next < 0) || (next > size)) next = -1;
	prev = ((long)pfbuf.pf_prev - pfdat)/sizeof pfbuf;
	if ((prev < 0) || (prev > size)) prev = -1;
	hash = ((long)pfbuf.pf_hchain - pfdat)/sizeof pfbuf;
	if ((hash < 0) || (hash > size)) hash = -1;
	if(addr != -1) 
		slot = getslot(addr,(long)pfdat,sizeof pfbuf,phys,size);
	if(slot == -1)
		fprintf(fp,"  - ");
	else fprintf(fp,"%4d",slot);
	fprintf(fp," %8d %3d %8x %6d  %3d  %3d  %3d",
		pfbuf.pf_blkno,
		pfbuf.pf_use,
		pfbuf.pf_devid,
		pfbuf.pf_nodeid,
		pfbuf.pf_swpi,
		pfbuf.pf_rawcnt,
		pfbuf.pf_waitcnt);
	if(next == -1)
		fprintf(fp,"     ");
	else fprintf(fp," %4d",next);
	if(prev == -1)
		fprintf(fp,"     ");
	else fprintf(fp," %4d",prev);
	if(hash == -1)
		fprintf(fp,"     ");
	else fprintf(fp," %4d",hash);
	fprintf(fp,"%s%s%s%s%s%s%s%s%s\n",
		pfbuf.pf_flags & P_QUEUE ? " que" : "",
		pfbuf.pf_flags & P_BAD ? " bad" : "",
		pfbuf.pf_flags & P_HASH ? " hsh" : "",
		pfbuf.pf_flags & P_DONE ? " don" : "",
		pfbuf.pf_flags & P_SWAP ? " swp" : "",
		pfbuf.pf_flags & P_DUMPED ? " dmp" : "",
		pfbuf.pf_flags & P_LOCKED ? " lck" : "",
		pfbuf.pf_flags & P_CLEARED ? " clr" : "",
		pfbuf.pf_flags & P_WANTED ? " wnt" : "");
}

/* get arguments for pdt function */
int
getpdt()
{
	int proc = Procslot;
	int all = 0;
	int phys = 0;
	unsigned long addr = -1;
	int c;
	struct proc prbuf;
	unsigned long segment;
	int count = NPGPT;

	optind = 1;
	while((c = getopt(argcnt,args,"epw:s:")) !=EOF) {
		switch(c) {
			case 'e' :	all = 1;
					break;
			case 'p' :	phys = 1;
					break;
			case 's' :	proc = setproc();
					break;
			case 'w' :	redirect();
					break;
			default  :	longjmp(syn,0);
		}
	}
	if(!args[optind])
		longjmp(syn,0);
	if((segment = strcon(args[optind++],'d')) == -1)
		error("\n");
	if(!IS_KUSEG(segment)) {
		addr = (long)segment;
		if (addr & (NBPC-1))
			error("address must be on a 4k boundary\n");
		segment = -1;
	}
	if(args[optind])
		if((count = strcon(args[optind],'d')) == -1)
			error("invalid count\n");
	if (count > NPGPT)
		error("count must be <= %d\n",NPGPT);
	procntry(proc,&prbuf);
	fprintf(fp,"SLOT    PFN NR SWPI DBDBLKNO FLAGS\n");
	prpdt(all,phys,segment,addr,count,proc,&prbuf);
}

/* print page descriptor table */
int
prpdt(all,phys,segment,addr,count,proc,pp)
int all,phys,segment,count,proc;
long addr;
struct proc *pp;
{
	long plen,paddr;
	pde_t pgbuf;
	int i;
	char *s;
	char	dbd_str_buf[50];

	if(addr == -1) {
		/* find page table which maps user address 'segment' */
		extern preg_t *foundprp;        /* set by findreg */
		extern reg_t *foundrp;          /* set by findreg */
		preg_t *prp;
		reg_t *rp;
		pde_t **ppd;
		pde_t *pd;
		int tmp;

		/* determine address and length by segment */
		if (findreg(pp, segment)==0)
			error("Not a valid user address\n");
		prp = foundprp;
		rp = foundrp;
		tmp = Btotp(segment - (int)prp->p_regva);
		ppd = (pde_t **)&rp->r_list[tmp/NPGPT];
		readmem((long)ppd,1,-1,
			(char *)&pd,sizeof pd,"r_list");
		paddr = vtop(pd, proc);
		plen = count;
	}
	else {
		if(phys || !Virtmode) {
		paddr = addr;
		plen = count;
		}
		else {
			paddr = vtop(addr,proc);
			if(paddr == -1)
				error("%x is an invalid address\n",addr);
			plen = count;
		}
	}

	fprintf(fp,"Page table at physical address %x\n",paddr);
	if(lseek(mem,paddr, 0) == -1)
		error("seek error on page descriptor table address\n");
	for(i = 0; i < plen; i++) {
		if(read(mem,&pgbuf, sizeof pgbuf) != sizeof pgbuf) 
			error("read error on page descriptor table\n");
		if(!pgbuf.pgm.pg_pfn && !all)
			continue;
		fprintf(fp,"%4d %6x %2d %4d %8d %s%s%s%s%s%s%s",
			i,pgbuf.pgm.pg_pfn,pgbuf.pgm.pg_nr,
			pgbuf.pgi.dbd.dbd_swpi,
			pgbuf.pgi.dbd.dbd_blkno,
			pgbuf.pgm.pg_n ? "nocache" : "",
			pgbuf.pgm.pg_m ? "mod " : "",
			pgbuf.pgm.pg_vr ? "rvalid " : "",
			pgbuf.pgm.pg_g ? "global " : "",
			pgbuf.pgm.pg_lock ? "lock " : "",	
			pgbuf.pgm.pg_sv ? "svalid " : "",
			pgbuf.pgm.pg_cw ? "cw " : "");
		switch(pgbuf.pgi.dbd.dbd_type) {
			case DBD_NONE: s="DBD_NONE"; break;
			case DBD_SWAP: s="DBD_SWAP"; break;
			case DBD_FILE: s="DBD_FILE"; break;
			case DBD_LSTFILE: s="DBD_LSTFILE"; break;
			case DBD_DZERO: s="DBD_DZERO"; break;
			case DBD_DFILL: s="DBD_DFILL"; break;
			case DBD_MMAP: s="DBD_MMAP"; break;
			case DBD_PGUSE: s="DBD_PGUSE"; break;
			case DBD_SYS: s="DBD_SYS"; break;
			case DBD_PDE: s="DBD_PDE"; break;
			default:
				sprintf(dbd_str_buf,"(%d)",
					pgbuf.pgi.dbd.dbd_type);
				s = dbd_str_buf;
				break;
		}
		fprintf(fp,"%s\n",s);
	}
}

/* get arguments for region function */
int
getregion()
{
	int slot = -1;
	int full = 0;
	int all = 0;
	int phys = 0;
	long addr = -1;
	long arg1 = -1;
	long arg2 = -1;
	int c;
	char * heading = "SLOT NSG PSZ OFF #VL RCNT WAT   FSIZE  TYPE FOR BCK VNODE    LISTADDR FLAGS\n";

	if(!Region)
		if(!(Region = symsrch("region")))
			error("region not found in symbol table\n");

	optind = 1;
	while((c = getopt(argcnt,args,"efpw:")) !=EOF) {
		switch(c) {
			case 'e' :	all = 1;
					break;
			case 'f' :	full = 1;
					break;
			case 'w' :	redirect();
					break;
			case 'p' :	phys = 1;
					break;
			default  :	longjmp(syn,0);
		}
	}
	fprintf(fp,"REGION TABLE SIZE = %d\n",vbuf.v_region);
	if(!full)
		fprintf(fp,"%s",heading);
	if(args[optind]) {
		all = 1;
		do {
			getargs(- vbuf.v_region,&arg1,&arg2);
			if(arg1 == -1) 
				continue;
			if(arg2 != -1) {
				if (arg1 >= 0 &&
				    arg1 < vbuf.v_region) {
					for(slot = arg1; slot <= arg2; slot++)
						prregion(all,full,slot,phys,addr,
							heading);
				} else {
					for (addr = arg1; addr <= arg2;
					     addr = (long) (((struct region *) addr) + 1))
						prregion(all,full,-1,phys,addr,
							 heading);
				};
			} else {
				if(arg1 >= 0 &&
				   arg1 < vbuf.v_region)
					slot = arg1;
				else addr = arg1;
				prregion(all,full,slot,phys,addr,heading);
			}
			slot = addr = arg1 = arg2 = -1;
		}while(args[++optind]);
	}
	else for(slot = 0; slot < vbuf.v_region; slot++)
		prregion(all,full,slot,phys,addr,heading);
}


/* print region table */
int
prregion(all,full,slot,phys,addr,heading)
int all,full,slot,phys;
long addr;
char *heading;
{
	struct region rbuf;
	char *typ;
	long forw,back;

	readbuf(addr,(long)(Region->n_value+slot*sizeof rbuf),phys,-1,
		(char *)&rbuf,sizeof rbuf,"region table");
	if(!all && !rbuf.r_refcnt)
		return;
	if(addr != -1) 
		slot = getslot(addr,(long)Region->n_value,sizeof rbuf,phys,
			vbuf.v_region);
	if(full)
		fprintf(fp,"%s",heading);
	if(slot == -1)
		fprintf(fp,"  - ");
	else fprintf(fp,"%4d",slot);
	fprintf(fp," %3d %3d %3d %3d  %3d %3d %8x ",
		rbuf.r_listsz,
		rbuf.r_pgsz,
		rbuf.r_pgoff,
		rbuf.r_nvalid,
		rbuf.r_refcnt,
		rbuf.r_waitcnt,
		rbuf.r_filesz);
	switch(rbuf.r_type) {
		case RT_UNUSED:  typ = "nuse"; break;
		case RT_PRIVATE:  typ = "priv"; break;
		case RT_STEXT:  typ = "stxt"; break;
		case RT_SHMEM:  typ = "shmm"; break;
		default:  typ = "?"; break;
	}
	fprintf(fp,"%4s ",typ);
	if((forw = ((long)rbuf.r_forw-Region->n_value)/sizeof(struct region))
		>= vbuf.v_region)
		fprintf(fp,"    ");
	else fprintf(fp,"%3d ",forw);
	if((back = ((long)rbuf.r_back-Region->n_value)/sizeof(struct region))
		>= vbuf.v_region)
		fprintf(fp,"    ");
	else fprintf(fp,"%3d ",back);
	if(rbuf.r_vptr)
		fprintf(fp,"%8x ",(long)rbuf.r_vptr);
	else fprintf(fp,"         ");
	if(rbuf.r_pgsz)
		fprintf(fp,"%8x",rbuf.r_list);
	else fprintf(fp,"        ");
	fprintf(fp,"%s%s%s%s%s%s%s%s",
			rbuf.r_flags & RG_NOFREE ? " nofree" : "",
			rbuf.r_flags & RG_DONE ? " done" : "",
			rbuf.r_flags & RG_NOSHARE ? " noshare" : "",
			rbuf.r_flags & RG_LOCK ? " lock" : "",
			rbuf.r_flags & RG_WANTED ? " want" : "",
			rbuf.r_flags & RG_WAITING ? " wait" : "",
#ifdef V5.3.1
			rbuf.r_flags & RG_WASTEXT ? " wastext" : "",
#else
			"",
#endif
			rbuf.r_flags & XREMOTE ? " xremote" : "");
	if(full) { 
		if(rbuf.r_pgsz) 
			prlist(rbuf.r_list,(unsigned)(rbuf.r_pgsz + (NPGPT - 1))/NPGPT);
		fprintf(fp,"\tnoswapcnt: %d\n",rbuf.r_noswapcnt);
	}
	else fprintf(fp,"\n");
}


/* print list of pointers to page tables */
int
prlist(addr,nopg)
long addr;
unsigned nopg;
{

	int list, i;

	fprintf(fp,"\n\tPDT: ");
	seekmem(addr,1,-1);
	for(i = 0; i < nopg; i++) {
		if(read(mem,(char *)&list,sizeof list) != sizeof list) 
			error("read error on rlist\n");
		if(!list)
			break;
		fprintf(fp,"%8x ", list);
		if(((i+1) % 4) == 0)
			fprintf(fp,"\n\t");
	}
}
