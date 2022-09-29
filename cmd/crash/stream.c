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
#ident	"$Header: stream.c,v 1.5.1.3 90/05/09 15:27:57 wje Exp $"

/*	Copyright (c) 1984 AT&T	*/
/*	  All Rights Reserved  	*/

/*	THIS IS UNPUBLISHED PROPRIETARY SOURCE CODE OF AT&T	*/
/*	The copyright notice above does not evidence any   	*/
/*	actual or intended publication of such source code.	*/

/*
 * This file contains code for the crash functions:  stream, queue, mblock,
 * mbfree, dblock, dbfree, strstat, linkblk, dballoc, qrun.
 */

#include "crash.h"
#include "sys/poll.h"
#include "sys/stropts.h"
#if RISCOS
#include "sys/strstat.h"
#endif

static struct syment *Dblock, *Qhead, *Mbfree, *Dbfree,
#if RISCOS
	*Strst,		/* used for strstat command */
#endif
	*Linkblk, *Nmuxlink, *Dballoc;		/* namelist symbol pointers */
struct syment *Mblock;
struct syment *SYM_spec_vnodeops;
struct syment *SYM_cdevcnt;
struct syment *SYM_cdevsw;
struct cdevsw *cdevsw_copy;
int	cdevcnt;
struct	syment *SYM_MAJOR;
char	MAJOR[128];
extern char *malloc(), *realloc();

int ndblock = 0;	 /* number of data blocks */
int nmblock = 0;	 /* number of message blocks */


int
blockinit()
{
	/* Initialize streams data block and message block counts - these  */
	/* formulas should match those in space.h                          */

	if((ndblock == 0) || (nmblock == 0)) {
		ndblock = vbuf.v_nblk4096 + vbuf.v_nblk2048 + vbuf.v_nblk1024 +
			vbuf.v_nblk512 + vbuf.v_nblk256 + vbuf.v_nblk128 +
			vbuf.v_nblk64 + vbuf.v_nblk16 + vbuf.v_nblk4;   
		nmblock = ndblock + ndblock/4;
	}
}


/* get arguments for stream function */
int
getstream()
{
	int full = 0;
	int all = 0;
	int phys = 0;
	long addr = -1;
	long arg1 = -1;
	long arg2 = -1;
	int list = 0;
	int c;
	char *heading = "ADDR      WRQ     IOCB VNODE     PGRP    IOCID   IOCWT WOFF ERR FLAG\n";

	if(!Mblock)
		if(!(Mblock = symsrch("mblock")))
			error("mblock not found in symbol table\n");
	if (!SYM_spec_vnodeops)
		if (! (SYM_spec_vnodeops = symsrch("spec_vnodeops")))
			error("spec_vnodeops not found in symbol table\n");
	read_cdevsw();
	blockinit();
	optind = 1;
	while((c = getopt(argcnt,args,"efnpw:")) !=EOF) {
		switch(c) {
			case 'e' :	all = 1;
					break;
			case 'f' :	full = 1;
					break;
			case 'n' : 	list = 1;
					break;
			case 'w' :	redirect();
					break;
			case 'p' :	phys = 1;
					break;
			default  :	longjmp(syn,0);
		}
	}
	if(!full)
		fprintf(fp,"%s",heading);
	if(args[optind]) {
		all = 1;
		do {
			getargs(GETARGS_NO_SLOT,&arg1,&arg2);
			if(arg1 == -1) 
				continue;
			if(arg2 != -1)
				for(addr = arg1; addr <= arg2;
				    addr = (long)
					     (((struct stdata *) addr) + 1))
					prstream(all,full,phys,addr,
						heading,list);
			else {
				addr = arg1;
				prstream(all,full,phys,addr,heading,list);
			}
			addr = arg1 = arg2 = -1;
		}while(args[++optind]);
	}
	else {
		struct findvnodeargs fva;
		struct	vnode vnode_buf;
		int	imajor;

		fva = null_findvnodeargs;
		fva.fva_type = VFS_TYPE_spec;
		fva.fva_flags |= FVA_SINGLE_TYPE;
		while (find_next_vnode(&fva,&vnode_buf)) {
			if (vnode_buf.v_type != VCHR || 
			    (imajor = MAJOR[major(vnode_buf.v_rdev)])
				>= cdevcnt ||
			    cdevsw_copy[imajor].d_str == NULL ||
			    (long) vnode_buf.v_stream == NULL)
				continue;
			prstream(all,full,phys,(long)vnode_buf.v_stream,
				 heading,list);
		};
	}

}

/* print streams table */
int
prstream(all,full,phys,addr,heading,list)
int all,full,phys;
long addr;
char *heading;
int list;
{
	struct stdata strm;
	struct strevent evbuf;
	struct strevent *next;
	int ioc_slot; 

	readbuf(addr,0,phys,-1,
		(char *)&strm, sizeof strm,"streams data block");
	if(full)
		fprintf(fp,"%s",heading);
	fprintf(fp,"%8x ",addr);
	fprintf(fp,"%8x ",((long)strm.sd_wrq));
	ioc_slot = ((long)strm.sd_iocblk - Mblock->n_value)/
		(sizeof(struct msgb));
	if ( (ioc_slot>=0)&&(ioc_slot<nmblock) )
		fprintf(fp,"%4d ",ioc_slot);
	else fprintf(fp,"   - ");
	fprintf(fp,"%8x ",((long)strm.sd_vnode));
	fprintf(fp,"%5d %10d %5d %4d %3o ", strm.sd_pgrp, strm.sd_iocid,
		strm.sd_iocwait, strm.sd_wroff, strm.sd_error);
	fprintf(fp,"%s%s%s%s%s%s%s%s%s%s%s%s%s%s%s%s%s%s%s\n",
		((strm.sd_flag & IOCWAIT) ? "iocw " : ""),
		((strm.sd_flag & RSLEEP) ? "rslp " : ""),
		((strm.sd_flag & WSLEEP) ? "wslp " : ""),
		((strm.sd_flag & STRPRI) ? "pri " : ""),
		((strm.sd_flag & STRHUP) ? "hup " : ""),
		((strm.sd_flag & STWOPEN) ? "stwo " : ""),
		((strm.sd_flag & STPLEX) ? "plex " : ""),
		((strm.sd_flag & RMSGDIS) ? "mdis " : ""),
		((strm.sd_flag & RMSGNODIS) ? "mnds " : ""),
		((strm.sd_flag & STRERR) ? "err " : ""),
		((strm.sd_flag & STRTIME) ? "sttm " : ""),
		((strm.sd_flag & STR2TIME) ? "s2tm " : ""),
		((strm.sd_flag & STR3TIME) ? "s3tm " : ""),
		((strm.sd_flag & STFIONBIO) ? "nbio " : ""),
		((strm.sd_flag & STRISTTY) ? "tty " : ""),
		((strm.sd_flag & STFIOASYNC) ? "asyn " : ""),
		((strm.sd_flag & STFJOBCTRL) ? "jctl " : ""),
		((strm.sd_flag & STFTOSTOP) ? "tost " : ""),
		((strm.sd_flag & STFPENDIN) ? "pend " : ""));
	if(full) {
		fprintf(fp,"\t STRTAB  RCNT\n");
		fprintf(fp,"\t%8x   %2d\n",
			strm.sd_strtab,
			strm.sd_pushcnt);
		fprintf(fp,"\tSIGFLAGS:  %s%s%s%s\n",
			((strm.sd_sigflags & S_INPUT) ? " input" : ""),
			((strm.sd_sigflags & S_HIPRI) ? " hipri" : ""),
			((strm.sd_sigflags & S_OUTPUT) ? " output" : ""),
			((strm.sd_sigflags & S_MSG) ? " msg" : ""));
		fprintf(fp,"\tSIGLIST:\n");
		next = strm.sd_siglist;
		while(next) {
			readmem((long)next,1,-1,(char *)&evbuf,
				sizeof evbuf,"stream event buffer");
			fprintf(fp,"\t\tPROC:  %3d   %s%s%s%s\n",
				((long)evbuf.se_procp-Proc->n_value)/
					sizeof(struct proc),
				((evbuf.se_events & S_INPUT) ? " input" : ""),
				((evbuf.se_events & S_HIPRI) ? " hipri" : ""),
				((evbuf.se_events & S_OUTPUT) ? " output" : ""),
				((evbuf.se_events & S_MSG) ? " msg" : ""));
			next = evbuf.se_next;	
		}
		fprintf(fp,"\tPOLLFLAGS:  %s%s%s\n",
			((strm.sd_pollflags & POLLIN) ? " in" : ""),
			((strm.sd_pollflags & POLLPRI) ? " pri" : ""),
			((strm.sd_pollflags & POLLOUT) ? " out" : ""));
		fprintf(fp,"\tPOLLIST:\n");
		next = strm.sd_pollist;
		while(next) {
			readmem((long)next,1,-1,(char *)&evbuf,
				sizeof evbuf,"stream event buffer");
			fprintf(fp,"\t\tPROC:  %3d   %s%s%s\n",
				((long)evbuf.se_procp-Proc->n_value)/
					sizeof(struct proc),
				((evbuf.se_events & POLLIN) ? " in" : ""),
				((evbuf.se_events & POLLPRI) ? " pri" : ""),
				((evbuf.se_events & POLLOUT) ? " out" : ""));
			next = evbuf.se_next;	
		}
		fprintf(fp,"\n");
	}
	if (list &&
	    strm.sd_wrq) {
		struct	queue que;

		fprintf(fp,"QUEUES FOR STREAM %8x:\n",addr);
		fprintf(fp,"ADDR     LINK       PTR     RCNT HEAD TAIL MINP MAXP HIWT LOWT FLAG\n");
		prqueuelist(all,phys,(long)strm.sd_wrq,&que,full,1,1,list);
	};

}

/* get arguments for queue function */
int
getqueue()
{
	int all = 0;
	int phys = 0;
	long addr = -1;
	long arg1 = -1;
	long arg2 = -1;
	int	do_rd = 0;
	int	do_wr = 0;
	int	do_list = 0;
	int	full = 0;
	int c;
	queue_t	que;

	if(!Mblock)
		if(!(Mblock = symsrch("mblock")))
			error("mblock not found in symbol table\n");
	read_cdevsw();
	optind = 1;
	while((c = getopt(argcnt,args,"fRWnpw:")) !=EOF) {
		switch(c) {
			case 'f' :	full = 1;
					break;
			case 'R' :	do_rd = 1;
					break;
			case 'W' :	do_wr = 1;
					break;
			case 'n' :	do_list = 1;
					break;
			case 'e' :	all = 1;
					break;
			case 'w' :	redirect();
					break;
			case 'p' :	phys = 1;
					break;
			default  :	longjmp(syn,0);
		}
	}
	fprintf(fp,"ADDR     LINK       PTR     RCNT HEAD TAIL MINP MAXP HIWT LOWT FLAG\n");
	if(args[optind]) {
		all = 1;
		do {
			getargs(GETARGS_NO_SLOT,&arg1,&arg2);
			if(arg1 == -1) 
				continue;
			if(arg2 != -1)
				for(addr = arg1; addr <= arg2; 
				    addr = (long) (((queue_t *) addr) + 1))
					prqueuelist(all,phys,addr,&que,full,do_rd,do_wr,do_list);
			else {
				prqueuelist(all,phys,arg1,&que,do_rd,do_wr,do_list);
			}
			addr = arg1 = arg2 = -1;
		}while(args[++optind]);
	}
	else {
		struct findvnodeargs fva;
		struct	vnode vnode_buf;
		int	imajor;
		struct stdata strm;

		fva = null_findvnodeargs;
		fva.fva_type = VFS_TYPE_spec;
		fva.fva_flags |= FVA_SINGLE_TYPE;
		while (find_next_vnode(&fva,&vnode_buf)) {
			if (vnode_buf.v_type != VCHR || 
			    (imajor = MAJOR[major(vnode_buf.v_rdev)])
				>= cdevcnt ||
			    cdevsw_copy[imajor].d_str == NULL ||
			    (long) vnode_buf.v_stream == NULL)
				continue;
			readbuf((long)vnode_buf.v_stream,0,phys,-1,
				(char *)&strm, sizeof strm,
				"streams data block");
			prqueuelist(all,phys,(long)strm.sd_wrq,&que,full,1,1,1);
		};
	}
}

/* print possible list of queues */
int
prqueuelist(all,phys,addr,que_p,full,do_rd,do_wr,list)
int all,phys;
long addr;
queue_t	*que_p;
{
	queue_t *np;
	long	wr_addr;
	long	rd_addr;

	while (addr != (long) NULL) {
		if (list || do_wr || do_rd) {
			rd_addr = addr;
			wr_addr = addr;
			readbuf(addr,0,phys,-1,
				(char *)que_p, sizeof(*que_p),"queue");
			if ((que_p->q_flag & QREADR)) {
				wr_addr = (long) (WR((struct queue *) addr));
				readbuf(wr_addr,0,phys,-1,
					(char *)que_p,
					sizeof(*que_p),"queue");
			} else 
				rd_addr = (long) (RD((struct queue *) addr));
			np = que_p->q_next;
		};

		if (! do_rd &&
		    ! do_wr) 
			prqueue(all,phys,addr,que_p,full);
		else {
			if (do_wr)
				prqueue(all,phys,wr_addr,que_p,full);
			if (do_rd) 
				prqueue(all,phys,rd_addr,que_p,full);
		};

		if (! list)
			return;

		addr = (long) np;
	};
}

/* print queue table */
int
prqueue(all,phys,addr,que_p,full)
int all,phys;
long addr;
queue_t	*que_p;
{
	mblk_t *m;
	long qn, ql;

	readbuf(addr,0,phys,-1,
		(char *)que_p, sizeof(*que_p),"queue");
        fprintf(fp,"%8x ",addr);
	ql = (long)que_p->q_link;
	if (ql != (long) NULL)
		fprintf(fp,"%8x ",ql);
	else 
		fprintf(fp,"   -     ");
	fprintf(fp,"%8x ",que_p->q_ptr);
	fprintf(fp," %4d ",que_p->q_count);
	m = que_p->q_first;
	if (m) {
		fprintf(fp,"%4d ",
			((long)m - Mblock->n_value)/(sizeof(struct msgb)));
	}
	else fprintf(fp,"   - ");
	m = que_p->q_last;
	if (m) {
		fprintf(fp,"%4d ",
			((long)m - Mblock->n_value)/(sizeof(struct msgb)));
	}
	else fprintf(fp,"   - ");
	fprintf(fp,"%4d %4d %4d %4d ",
		que_p->q_minpsz,
		que_p->q_maxpsz,
		que_p->q_hiwat,
		que_p->q_lowat);
	fprintf(fp,"%s%s%s%s%s%s\n",
		((que_p->q_flag & QENAB) ? "en " : ""),
		((que_p->q_flag & QWANTR) ? "wr " : ""),
		((que_p->q_flag & QWANTW) ? "ww " : ""),
		((que_p->q_flag & QFULL) ? "fl " : ""),
		((que_p->q_flag & QREADR) ? "rr " : ""),
		((que_p->q_flag & QNOENB) ? "ne " : ""));
	if (! full)
		return;
	fprintf(fp,"\tINFO = %8x; NEXT = ",que_p->q_qinfo);
	qn = (long)que_p->q_next;
	if (qn != (long) NULL)
		fprintf(fp,"%8x\n",qn);
	else 
		fprintf(fp,"   -    \n");
}

/* get arguments for mblock function */
int
getmess()
{
	int slot = -1;
	int all = 0;
	int phys = 0;
	long addr = -1;
	long arg1 = -1;
	long arg2 = -1;
	int c;

	if(!Mblock)
		if(!(Mblock = symsrch("mblock")))
			error("mblock not found in symbol table\n");
	if(!Dblock)
		if(!(Dblock = symsrch("dblock")))
			error("dblock not found in symbol table\n");
	blockinit();
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
	fprintf(fp,"MESSAGE BLOCK TABLE SIZE = %d\n",nmblock);
	fprintf(fp,"SLOT NEXT CONT PREV   RPTR     WPTR   DATAB\n");
	if(args[optind]) {
		all = 1;
		do {
			getargs(- nmblock,&arg1,&arg2);
			if(arg1 == -1) 
				continue;
			if(arg2 != -1) {
				if (arg1 >= 0 &&
				    arg1 < nmblock) {
					for(slot = arg1; slot <= arg2; slot++)
						prmess(all,slot,phys,addr);
				} else {
					for (addr = arg1; addr <= arg2; 
					     addr = (long) (((mblk_t *) addr) + 1))
						prmess(all,-1,phys,addr);
				};
			} else {
				if(arg1 >= 0 &&
				   arg1 < nmblock)
					slot = arg1;
				else addr = arg1;
				prmess(all,slot,phys,addr);
			}
			slot = addr = arg1 = arg2 = -1;
		}while(args[++optind]);
	}
	else for(slot = 0; slot < nmblock; slot++)
		prmess(all,slot,phys,addr);
}

/* print mblock table */
int
prmess(all,slot,phys,addr)
int all,slot,phys;
long addr;
{
	mblk_t mblk;
	int mnext, mcont, datab, mprev;

	readbuf(addr,(long)(Mblock->n_value+slot*sizeof mblk),phys,-1,
		(char *)&mblk,sizeof mblk,"message block");
	if (!mblk.b_datap && !all) 
		return(-1);
	if(addr != -1) 
		slot = getslot(addr,(long)Mblock->n_value,sizeof mblk,phys,
			nmblock);
	if(slot == -1)
		fprintf(fp,"  -  ");
	else fprintf(fp,"%4d ",slot);
	mnext = ((long)mblk.b_next - Mblock->n_value)/sizeof(struct msgb);
	mcont = ((long)mblk.b_cont - Mblock->n_value)/sizeof(struct msgb);
	mprev = ((long)mblk.b_prev - Mblock->n_value)/sizeof(struct msgb);
	datab = ((long)mblk.b_datap - Dblock->n_value)/sizeof(struct datab);
	if ((mnext >= 0) && (mnext < nmblock))
		fprintf(fp,"%4d ",mnext);
	else fprintf(fp,"   - ");
	if ((mcont >= 0) && (mcont < nmblock))
		fprintf(fp,"%4d ",mcont);
	else fprintf(fp,"   - ");
	if ((mprev >= 0) && (mprev < nmblock))
		fprintf(fp,"%4d ",mprev);
	else fprintf(fp,"   - ");
	fprintf(fp,"%8x %8x ",
		mblk.b_rptr,
		mblk.b_wptr);
	if ((datab >= 0) && (datab < ndblock))
		fprintf(fp,"%4d\n",datab);
	else fprintf(fp,"   -\n");
	return(mnext);
	
}

/* get arguments for mbfree function */
int
getmbfree()
{
	int c;

	if(!Mbfree)
		if(!(Mbfree = symsrch("mbfreelist")))
			error("mbfreelist not found in symbol table\n");
	if(!Mblock)
		if(!(Mblock = symsrch("mblock")))
			error("mblock not found in symbol table\n");
	if(!Dblock)
		if(!(Dblock = symsrch("dblock")))
			error("dblock not found in symbol table\n");
	blockinit();
	optind = 1;
	while((c = getopt(argcnt,args,"w:")) !=EOF) {
		switch(c) {
			case 'w' :	redirect();
					break;
			default  :	longjmp(syn,0);
		}
	}
	fprintf(fp,"SLOT NEXT CONT PREV   RPTR     WPTR   DATAB\n");
	if(args[optind]) 
		longjmp(syn,0);
	prmbfree();
}

/* print mblock free list */
int
prmbfree()
{
	mblk_t *m;
	int  mnext;

	readmem((long)Mbfree->n_value,1,-1,(char *)&m,
			sizeof m,"mbfreelist");
	mnext = ((long)m - Mblock->n_value)/sizeof(struct msgb);
	while ((mnext >=0) && (mnext < nmblock))
		mnext = prmess(1,mnext,0,-1);
}


/* get arguments for dblock function */
int
getdblk()
{
	int slot = -1;
	int all = 0;
	int phys = 0;
	int class = 0;
	long addr = -1;
	long arg2 = -1;
	long arg1 = -1;
	int c;
	int klass;

	if(!Dblock)
		if(!(Dblock = symsrch("dblock")))
			error("dblock not found in symbol table\n");
	blockinit();
	optind = 1;
	while((c = getopt(argcnt,args,"epcw:")) !=EOF) {
		switch(c) {
			case 'e' :	all = 1;
					break;
			case 'p' :	phys = 1;
					break;
			case 'c' :	class = 1;
					break;
			case 'w' :	redirect();
					break;
			default  :	longjmp(syn,0);
		}
	}
	fprintf(fp,"DATA BLOCK TABLE SIZE = %d\n",ndblock);
	fprintf(fp,"SLOT CLASS SIZE  RCNT   TYPE     BASE     LIMIT  FREEP\n");
	if(args[optind]) {
		if(class) 
			do {
				if((klass = (int)strcon(args[optind++],'d'))
					== -1)
					continue;
				prclass(klass);
			}while(args[optind]);
		else {
			all = 1;
			do {
				getargs(- ndblock,&arg1,&arg2);
				if(arg1 == -1) 
					continue;
				if(arg2 != -1) {
					if (arg1 >= 0 &&
					    arg1 <= ndblock) {
						for(slot = arg1; slot <= arg2; slot++)
							prdblk(all,slot,phys,addr);
					} else {
						for (addr = arg1; addr <= arg2;
						     addr = (long) (((dblk_t *) addr) + 1))
							prdblk(all,-1,phys,addr);
					};
				} else {
					if(arg1 >= 0 &&
					   arg1 < ndblock)
						slot = arg1;
					else addr = arg1;
					prdblk(all,slot,phys,addr);
				}
				slot = addr = arg1 = arg2 = -1;
			}while(args[++optind]);
		}
	}
	else for(slot = 0; slot < ndblock; slot++)
		prdblk(all,slot,phys,addr);
}


/* print dblock table */
int
prdblk(all,slot,phys,addr)
int all,slot,phys;
long addr;
{
	dblk_t dblk;
	int dfree;
	static int lastcls;

	readbuf(addr,(long)(Dblock->n_value+slot*sizeof dblk),phys,-1,
		(char *)&dblk, sizeof dblk,"data block");
	if (!dblk.db_ref && !all)
		return(-1);
	if(addr != -1) 
		slot = getslot(addr,(long)Dblock->n_value,sizeof dblk,phys,
			ndblock);
	if (dblk.db_class != lastcls) {
		fprintf(fp,"\n");
		lastcls=dblk.db_class;
	}
	if(slot == -1)
		fprintf(fp,"  - ");
	else fprintf(fp,"%4d",slot);
	fprintf(fp," %5d ",
		dblk.db_class);
	switch (dblk.db_class) {
		case 0: fprintf(fp,"   4 "); break;
		case 1: fprintf(fp,"  16 "); break;
		case 2: fprintf(fp,"  64 "); break;
		case 3: fprintf(fp," 128 "); break;
		case 4: fprintf(fp," 256 "); break;
		case 5: fprintf(fp," 512 "); break;
		case 6: fprintf(fp,"1024 "); break;
		case 7: fprintf(fp,"2048 "); break;
		case 8: fprintf(fp,"4096 "); break;
		default: fprintf(fp,"   - ");
	}
	fprintf(fp," %4d ", dblk.db_ref); 
	switch (dblk.db_type) {
		case M_DATA: fprintf(fp,"data     "); break;
		case M_PROTO: fprintf(fp,"proto    "); break;
		case M_BREAK: fprintf(fp,"break    "); break;
		case M_PASSFP: fprintf(fp,"passfs   "); break;
		case M_SIG: fprintf(fp,"sig      "); break;
		case M_DELAY: fprintf(fp,"delay    "); break;
		case M_CTL: fprintf(fp,"ctl      "); break;
		case M_IOCTL: fprintf(fp,"ioctl    "); break;
		case M_SETOPTS: fprintf(fp,"setopts  "); break;
		case M_IOCACK: fprintf(fp,"iocack   "); break;
		case M_IOCNAK: fprintf(fp,"iocnak   "); break;
		case M_PCPROTO: fprintf(fp,"pcproto  "); break;
		case M_PCSIG: fprintf(fp,"pcsig    "); break;
		case M_FLUSH: fprintf(fp,"flush    "); break;
		case M_STOP: fprintf(fp,"stop     "); break;
		case M_START: fprintf(fp,"start    "); break;
		case M_HANGUP: fprintf(fp,"hangup   "); break;
		case M_ERROR: fprintf(fp,"error    "); break;
		default: fprintf(fp,"       - ");
	}
	fprintf(fp,"%8x %8x ",
		dblk.db_base,
		dblk.db_lim);
	dfree = ((long)dblk.db_freep - Dblock->n_value)/sizeof(struct datab);
	if ((dfree >= 0) && (dfree < ndblock))
		fprintf(fp," %4d\n",dfree);
	else fprintf(fp,"   -\n");
	return(dfree);
	
}

/* get class for dblock */
int
prclass(class)
int class;
{
	int i,n, *p;
	int clasize[NCLASS];

	if ((class < 0) || (class > NCLASS-1))
		error("%d is out of range\n",class);
	p = &(vbuf.v_nblk4096);
	for (i=NCLASS-1; i>=0; i--) clasize[i] = *p++;  /* load size of each block class */
	n=0;
	for (i=NCLASS-1; i>class; i--) n+=clasize[i];	/* compute slot of first block */
	for (i=0; i<clasize[class]; i++)
		prdblk(0,n++,0,-1);
}

/* get arguments for dbfree function */
int
getdbfree()
{
	int c;
	int class;
	int klass;

	if(!Dbfree)
		if(!(Dbfree = symsrch("dbfreelist")))
			error("dbfreelist not found in symbol table\n");
	if(!Dblock)
		if(!(Dblock = symsrch("dblock")))
			error("dblock not found in symbol table\n");
	blockinit();
	optind = 1;
	while((c = getopt(argcnt,args,"w:")) !=EOF) {
		switch(c) {
			case 'w' :	redirect();
					break;
			default  :	longjmp(syn,0);
		}
	}
	fprintf(fp,"SLOT CLASS SIZE  RCNT   TYPE     BASE     LIMIT  FREEP\n");
	if(args[optind]) 
		do {
			if((klass = (int)strcon(args[optind++],'d')) == -1)
				continue;
			prdbfree(klass);
		} while(args[optind]);
	else for(class = 0; class < NCLASS; class++)
		prdbfree(class);
}

/* print dblock free list */
int
prdbfree(class)
int class;
{
	dblk_t *d;
	int  dnext;

	if ((class < 0) || (class > NCLASS-1))
		error("%d is out of class range\n",class);
	readmem((long)(Dbfree->n_value+class*sizeof d),1,-1,(char *)&d,
		sizeof d,"dbfreelist");
	dnext = ((long)d - Dblock->n_value)/sizeof(struct datab);
	while ((dnext >=0) && (dnext < ndblock))
		dnext = prdblk(1,dnext,0,-1);
}

/* get arguments for qrun function */
int
getqrun()
{
	int c;

	if(!Qhead)
		if(!(Qhead = symsrch("qhead")))
			error("qhead not found in symbol table\n");
	optind = 1;
	while((c = getopt(argcnt,args,"w:")) !=EOF) {
		switch(c) {
			case 'w' :	redirect();
					break;
			default  :	longjmp(syn,0);
		}
	}
	prqrun();
}

/* print qrun information */
int
prqrun()
{
	queue_t que, *q;

	readmem((long)Qhead->n_value,1,-1,(char *)&q,
		sizeof q,"qhead");
	fprintf(fp,"Queue addresses scheduled for service: ");
	while (q) {
		fprintf(fp,"%8x ",(long) q);
		readmem((long)q,1,-1,(char *)&que,
			sizeof que,"scanning queue list");
		q = que.q_link;
	}
	fprintf(fp,"\n");
}


/* initialization for namelist symbols */
int
streaminit()
{
	static int strinit = 0;

	if(strinit)
		return;
	if(!Mblock)
		if(!(Mblock = symsrch("mblock")))
			error("mblock not found in symbol table\n");
	if(!Dblock)
		if(!(Dblock = symsrch("dblock")))
			error("dblock not found in symbol table\n");
	if(!Qhead)
		if(!(Qhead = symsrch("qhead")))
			error("qhead not found in symbol table\n");
	if(!Dbfree)
		if(!(Dbfree = symsrch("dbfreelist")))
			error("dbfreelist not found in symbol table\n");
	if(!Mbfree)
		if(!(Mbfree = symsrch("mbfreelist")))
			error("mbfreelist not found in symbol table\n");
#if RISCOS
	if(!Strst)
		if(!(Strst = symsrch("strst")))
			error("strst not found in symbol table\n");
#endif
	blockinit();

	strinit = 1;
}


/* get arguments for strstat function */
int
getstrstat()
{
	int c;

	streaminit();
	optind = 1;
	while((c = getopt(argcnt,args,"w:")) !=EOF) {
		switch(c) {
			case 'w' :	redirect();
					break;
			default  :	longjmp(syn,0);
		}
	}
	prstrstat();
}

/* print stream statistics */
int
prstrstat()
{
	queue_t que, *q;
	struct stdata str;
	mblk_t mes, *m;
	dblk_t dbk, *d;
	int qusecnt, susecnt, mfreecnt, musecnt,
	    dfreecnt, dusecnt, dfc[NCLASS], duc[NCLASS], dcc[NCLASS], qruncnt;
	int i,j, *p;
#if RISCOS
	struct strstat strs;
#endif

	qusecnt = susecnt = mfreecnt = 0;
	musecnt = dfreecnt = dusecnt = qruncnt = 0;
	for (i=0; i<NCLASS; i++) dfc[i] = duc[i] =0;
	p = &(vbuf.v_nblk4096);
	for (i=NCLASS-1; i>=0; i--) dcc[i] = *p++;

#if RISCOS
	readmem((long)Strst->n_value,1,-1,
		&strs,sizeof strs,"streams status block");
	fprintf(fp,"ITEM               CONFIGURED   ALLOCATED     FREE        MAX      FAILED\n");
#else
	fprintf(fp,"ITEM               CONFIGURED   ALLOCATED     FREE\n");
#endif

	readmem((long)Qhead->n_value,1,-1,(char *)&q,
		sizeof q,"qhead");
	while (q) {
		qruncnt++;
		readmem((long)q,1,-1,(char *)&que,sizeof que,"queue run list");
		q = que.q_link;
	}
	
	qusecnt = strs.queue.use;
	susecnt = strs.stream.use;

	seekmem((long)Mbfree->n_value,1,-1);
	if (read(mem, &m, sizeof m) != sizeof m) 
		error(fp,"read error for mbfreelist\n");
	while (m) {
		mfreecnt++;
		readmem((long)m,1,-1,(char *)&mes,sizeof mes,"message list");
		m = mes.b_next;
	}

	seekmem((long)Mblock->n_value,1,-1);
	for (i=0; i<nmblock; i++) {
		if (read(mem, &mes, sizeof mes) != sizeof mes) 
			error(fp,"Read error for message table\n");
		if (mes.b_datap) musecnt++;
	}

	for (j=0; j<NCLASS; j++){
		readmem((long)(Dbfree->n_value + j*(sizeof d)),1,-1,
			(char *)&d,sizeof d,"data block table");
		while (d) {
			dfc[j]++;
			dfreecnt++;
			readmem((long)d,1,-1,(char *)&dbk,sizeof dbk,
				"data block table");
			d = dbk.db_freep;
		}
	}

	seekmem((long)Dblock->n_value,1,-1);
	for (i=0; i<ndblock; i++) {
		if (read(mem, &dbk, sizeof dbk) != sizeof dbk) 
			error(fp,"read error in data block table\n");
		if (dbk.db_ref) {
			dusecnt++;
			duc[dbk.db_class]++;
		}
	}

	fprintf(fp,"streams                -           %4d        -         %4d       %4d\n",
		susecnt,
		strs.stream.max, strs.stream.fail);
	fprintf(fp,"queues                 -           %4d        -         %4d       %4d\n",
		qusecnt, 
		strs.queue.max, strs.queue.fail);
	fprintf(fp,"message blocks        %4d         %4d       %4d       %4d       %4d\n",
		nmblock, musecnt, mfreecnt, strs.mblock.max, strs.mblock.fail);
	fprintf(fp,"data block totals     %4d         %4d       %4d       %4d       %4d\n",
		ndblock, dusecnt, dfreecnt, strs.dblock.max, strs.dblock.fail);
	for (i=0; i<NCLASS; i++) 
		{
		fprintf(fp,"data block size ");
		switch (i) {
			case 0: fprintf(fp,"   4  "); break;
			case 1: fprintf(fp,"  16  "); break;
			case 2: fprintf(fp,"  64  "); break;
			case 3: fprintf(fp," 128  "); break;
			case 4: fprintf(fp," 256  "); break;
			case 5: fprintf(fp," 512  "); break;
			case 6: fprintf(fp,"1024  "); break;
			case 7: fprintf(fp,"2048  "); break;
			case 8: fprintf(fp,"4096  "); break;
			default: fprintf(fp,"   -  ");
			}
		fprintf(fp,"%4d         %4d       %4d       %4d       %4d\n",
			dcc[i], duc[i], dfc[i],
			strs.dblk[i].max, strs.dblk[i].fail);
		}
	fprintf(fp,"\nCount of scheduled queues:%4d\n", qruncnt);


}


/* get arguments for linkblk function */
int
getlinkblk()
{
	int c;
	int slot = -1;
	int all = 0;
	int phys = 0;
	long addr = -1;
	long arg2 = -1;
	long arg1 = -1;
	int nmuxlink;

	if(!Linkblk)
		if(!(Linkblk = symsrch("linkblk")))
			error("linkblk not found in symbol table\n");
	if(!Nmuxlink)
		if(!(Nmuxlink = symsrch("nmuxlink")))
			error("nmuxlink not found in symbol table\n");
	optind = 1;
	while((c = getopt(argcnt,args,"epw:")) !=EOF) {
		switch(c) {
			case 'e' :	all = 1;
					break;
			case 'p' :	phys = 1;
					break;
			case 'w' :	redirect();
					break;
			default  :	longjmp(syn,0);
		}
	}
	readmem((long)Nmuxlink->n_value,1,-1,(char *)&nmuxlink,
		sizeof nmuxlink,"linkblk table size");
	fprintf(fp,"LINKBLK TABLE SIZE = %d\n",nmuxlink);
	fprintf(fp,"SLOT   QTOP     QBOT   INDEX\n");
	if(args[optind]) {
		all = 1;
		do {
			getargs(- nmuxlink,&arg1,&arg2);
			if(arg1 == -1) 
				continue;
			if(arg2 != -1) {
				if (arg1 >= 0 &&
				    arg1 < nmuxlink) {
					for(slot = arg1; slot <= arg2; slot++)
						prlinkblk(all,slot,phys,addr,nmuxlink);
				} else {
					for (addr = arg1; addr <= arg2;
					     addr = (long) (((struct linkblk *) addr) + 1))
						prlinkblk(all,-1,phys,addr,nmuxlink);
				};
			} else {
				if(arg1 >= 0 &&
				   arg1 < nmuxlink)
					slot = arg1;
				else addr = arg1;
				prlinkblk(all,slot,phys,addr,nmuxlink);
			}
			slot = addr = arg1 = arg2 = -1;
		}while(args[++optind]);
	}
	else for(slot = 0; slot < nmuxlink; slot++)
		prlinkblk(all,slot,phys,addr,nmuxlink);	
}

/* print linkblk table */
int
prlinkblk(all,slot,phys,addr,max)
int all,slot,phys,max;
long addr;
{
	struct linkblk linkbuf;

	readbuf(addr,(long)(Linkblk->n_value+slot*sizeof linkbuf),phys,-1,
		(char *)&linkbuf,sizeof linkbuf,"linkblk table");
	if(!linkbuf.l_qtop && !all)
		return;
	if(addr != -1)
		slot = getslot(addr,(long)Linkblk->n_value,sizeof linkbuf,phys,
			max);
	if(slot == -1)
		fprintf(fp,"  - ");
	else fprintf(fp,"%4d", slot);
		fprintf(fp," %8x",linkbuf.l_qtop);
		fprintf(fp," %8x",linkbuf.l_qbot);
		fprintf(fp,"   %3d\n",linkbuf.l_index);
}


/* get arguments for dballoc function */
int
getdballoc()
{
	int c,class;

	if(!Dballoc)
		if(!(Dballoc = symsrch("dballoc")))
			error("dballoc not found in symbol table\n");
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
			if((class = strcon(args[optind++],'d')) == -1)
				continue;
			if((class >= 0) && (class < NCLASS))
				prdballoc(class);
			else {
				fprintf(fp,"%d is out of class range\n",
					class);
				continue;
			}
		} while(args[optind]);
	}
	else for(class = 0; class < NCLASS; class++)
		prdballoc(class);
}

/* print dballoc table */
int
prdballoc(class)
int class;
{
	struct dbalcst dbalbuf;
	struct strevent *next;

	readmem((long)(Dballoc->n_value+class*sizeof dbalbuf),1,-1,
		(char *)&dbalbuf,sizeof dbalbuf,"dballoc table");
	fprintf(fp,"CLASS   RCNT    LO   MED\n");
	fprintf(fp,"  %d    %5d %5d %5d\n",
		class,
		dbalbuf.dba_cnt,
		dbalbuf.dba_lo,
		dbalbuf.dba_med);
	fprintf(fp,"\tLOP:\n");
	next = dbalbuf.dba_lop;
	while(next)
		prfuncname(next);
	fprintf(fp,"\tMEDP:\n");
	next = dbalbuf.dba_medp;
	while(next)
		prfuncname(next);
	fprintf(fp,"\tHIP:\n");
	next = dbalbuf.dba_hip;
	while(next)
		prfuncname(next);
}

/* print function name for dballoc */
int
prfuncname(addr)
long(addr);
{
	struct syment *sp;
	extern struct syment *findsym();
	struct strevent evbuf;
	char *name;
	
	readmem((long)addr,1,-1,(char *)&evbuf,
		sizeof evbuf,"stream event buffer");
	if(!(sp = findsym((unsigned long)evbuf.se_func))) {
		prerrmes("%8x does not match in symbol table\n",evbuf.se_func);
		return;
	}
	name = sp->n_name;
	fprintf(fp,"\t\t%-15s",name);
	fprintf(fp,"(%8x)\n",evbuf.se_arg);
}


/* read in cdevsw and MAJOR arrays */
int
read_cdevsw()
{
	if (!SYM_cdevcnt)
		if (! (SYM_cdevcnt = symsrch("cdevcnt")))
			error("cdevcnt not found in symbol table\n");
	if (!SYM_cdevsw)
		if (! (SYM_cdevsw = symsrch("cdevsw")))
			error("cdevsw not found in symbol table\n");
	if (!SYM_MAJOR)
		if (! (SYM_MAJOR = symsrch("MAJOR")))
			error("MAJOR not found in symbol table\n");

	readmem(SYM_cdevcnt->n_value,1,-1,(char *) &cdevcnt,
		sizeof(cdevcnt),"cdevcnt");
	if (cdevcnt > 0) {
		if(!(cdevsw_copy=(struct cdevsw *)malloc((unsigned)
				(cdevcnt * sizeof(struct cdevsw)))))
			fatal("cannot allocate space for cdevsw\n");
		readmem(SYM_cdevsw->n_value,1,-1,(char *) cdevsw_copy,
			sizeof(struct cdevsw) * cdevcnt,"cdevsw");
	};
	readmem(SYM_MAJOR->n_value,1,-1, MAJOR,
		128,"MAJOR");
}
		
