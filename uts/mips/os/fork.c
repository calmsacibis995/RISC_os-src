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
 * |         950 DeGuigne Drive                                |
 * |         Sunnyvale, CA 94086                               |
 * |-----------------------------------------------------------|
 */
#ident	"$Header: fork.c,v 1.33.1.9.1.4.1.2 90/10/16 10:02:32 beacker Exp $"

/*	Copyright (c) 1984 AT&T	*/
/*	  All Rights Reserved  	*/

/*	THIS IS UNPUBLISHED PROPRIETARY SOURCE CODE OF AT&T	*/
/*	The copyright notice above does not evidence any   	*/
/*	actual or intended publication of such source code.	*/

#include "sys/types.h"
#include "sys/param.h"
#include "sys/sysmacros.h"
#include "sys/signal.h"
#include "sys/psw.h"
#include "sys/pcb.h"
#include "sys/immu.h"
#include "sys/user.h"
#include "sys/systm.h"
#include "sys/sysinfo.h"
#include "sys/pfdat.h"
#include "sys/vnode.h"
#include "sys/file.h"
#include "sys/region.h"
#include "sys/sbd.h"
#include "sys/proc.h"
#include "sys/var.h"
#include "sys/acct.h"
#include "sys/errno.h"
#include "sys/debug.h"
#include "sys/cmn_err.h"
#include "sys/tuneable.h"
#include "sys/stream.h"
#include "sys/conf.h"
#include "sys/cpu_board.h"
#include "sys/reg.h"
#include "sys/numips.h"

extern dbd_t null_dbd;

#define NOFORCE	0

/*	The following is used to lock the kernel window which
**	is used to address a u-block other than the u-block of
**	the current process.  This lock is needed since
**	procdup can sleep while using the window.
*/

int	*win_ublk;
int	win_ublk_lock;

/*
 * The following three variables control how the child is run.
 * My current belief is that it is reasonable for the child to
 * inherit the CPU badness of it's parent, but it needs a slightly
 * lower priority to let it exec and sever copy-on-write pages.
 */
int fork_child_pri = -1;	/* Child gets parent p_pri + child_pri */
int fork_child_cpu = 0;		/* Child gets parent p_cpu + child_cpu */
int fork_do_runrun = 1;		/* Do runrun after fork? */

#define BOUND(x, min, max) \
	( (x)<(min) ? (min):  ((x)>(max)?(max):(x))  )

winublock()
{
	while (win_ublk_lock)
		sleep(&win_ublk_lock, PZERO);
	win_ublk_lock = 1;
}

winubunlock()
{
	win_ublk_lock = 0;
	wakeup(&win_ublk_lock);
}

winublocked()
{
	return(win_ublk_lock);
}

/*
 * fork system call.
 */

fork()
{
	fork1(0);
}

vfork()
{
	fork1(1);
}

fork1(isvfork)
	int	isvfork;
{
	sysinfo.sysfork++;

	/*
	 * Disallow if
	 *  No processes at all; or
	 *  not su and too many procs owned; or
	 *  not su and would take last slot; or
	 * Check done in newproc().
	 */

	switch (newproc(1, isvfork)) {
		case 1:  /* child -- successful newproc */
			u.u_rval1 = u.u_procp->p_ppid;
			u.u_rval2 = 1; /* child */
			u.u_start = time.tv_sec;
			u.u_ticks = lbolt;
			u.u_mem = u.u_procp->p_size;
			u.u_ior = u.u_iow = u.u_ioch = 0;
			u.u_cstime = 0;
			u.u_stime = 0;
			u.u_cutime = 0;
			u.u_utime = 0;
			u.u_acflag = AFORK;
			u.u_lock = 0;
		  	u.u_bsd43_start = time;
			bzero((caddr_t)&up->u_ru,
				sizeof (struct bsd43_rusage));
			bzero((caddr_t)&up->u_cru,
				sizeof (struct bsd43_rusage));
			return;
		case 0: /* parent, rval1 setup by newproc */
			/* u.u_rval1 = pid_of_child; */
			break;
		default:	/* couldn't fork */
			u.u_error = EAGAIN;
			break;
	}

	u.u_rval2 = 0;	/* parent */
}


/*
 * Create a new process-- the internal version of
 * sys fork.
 *
 * This changes the new proc structure and
 * alters only u.u_procp kf the uarea.
 *
 * It returns 1 in the new process, 0 in the old.
 */

int	mpid;
#if	DEBUG
int	procdup_failcnt;
#endif

newproc(failok, isvfork)
	int failok;
	int isvfork;
{
	register struct proc *cp, *pp, *pend;
	register n, a;
	extern struct kpcb kpcb_pdup;

	/*
	 * First, just locate a slot for a process
	 * and copy the useful info from this process into it.
	 * The panic "cannot happen" because fork has already
	 * checked for the existence of a slot.
	 */

retry:
	mpid++;
	if (mpid >= MAXPID) {
		mpid = 0;
		goto retry;
	}
	pp = &proc[0];
	cp = NULL;
	n = (struct proc *)v.ve_proc - pp;
	a = 0;
	do {
		if (pp->p_stat == NULL) {
			if (cp == NULL)
				cp = pp;
			continue;
		}
		if (pp->p_pid == mpid ||
		    pp->p_pgrp == mpid ||
		    pp->p_jcpgrp == mpid)
			goto retry;
		if (pp->p_uid == u.u_ruid)
			a++;
		pend = pp;
	} while (pp++, --n);
	if (cp == NULL) {
		if ((struct proc *)v.ve_proc >= &proc[v.v_proc]) {
			if (failok) {
				syserr.procovf++;
				u.u_error = EAGAIN;
				return(-1);
			} else
				cmn_err(CE_PANIC, "newproc - no procs");
		}
		cp = (struct proc *)v.ve_proc;
	}
	if (cp > pend)
		pend = cp;
	pend++;
	v.ve_proc = (char *)pend;
	if (u.u_uid && u.u_ruid) {
		if (cp == &proc[v.v_proc-1] || a > v.v_maxup) {
			u.u_error = EAGAIN;
			return(-1);
		}
	}
	/*
	 * make proc entry for new proc
	 */
	pp = u.u_procp;
	cp->p_uid = pp->p_uid;
	cp->p_suid = pp->p_suid;
	cp->p_sgid = pp->p_sgid;
	cp->p_pgrp = pp->p_pgrp;
	cp->p_jcpgrp = pp->p_jcpgrp;
	cp->p_nice = pp->p_nice;
	cp->p_chold = pp->p_chold;
	if (POSIX_SYSCALL)
		cp->p_sig = 0;
	else
		cp->p_sig = pp->p_sig;
	cp->p_hold = pp->p_hold;
	cp->p_stat = SIDL;
#ifdef BSD_ITIMER_BUG_FIXED_HERE
	/*
	 * In BSD, all of the timers propogate across fork(2) except the 
	 * real itimer.  This seems wrong, but probably can't be fixed.
	 */
	cp->p_realtimer = pp->p_realtimer;
	if (timerisset(&cp->p_realtimer.it_value)) {
		timeout(realitexpire, (caddr_t)cp, 
			hzto(&cp->p_realtimer.it_value));
	}
#else
	timerclear(&cp->p_realtimer.it_interval);
	timerclear(&cp->p_realtimer.it_value);
#endif
	cp->p_flag = SLOAD | (pp->p_flag & (SSEXEC|SPROCTR)); 
	if (isvfork)
		cp->p_flag |= SVFORK;
	cp->p_pid = mpid;
	cp->p_epid = mpid;
	cp->p_ppid = pp->p_pid;
	cp->p_time = 0;
	cp->p_cpu = BOUND(pp->p_cpu + fork_child_cpu, 0, 255);
	cp->p_pri = BOUND(calcppri(pp) + fork_child_pri,
				PUSER + pp->p_nice - NZERO, 127);
	cp->p_sigmask = pp->p_sigmask;
	cp->p_sigcatch = pp->p_sigcatch;
	cp->p_sigignore = pp->p_sigignore;
	cp->p_tlbpid = -1;
	cp->p_fp = 0;
	cp->p_slptime = 0;
	cp->p_pctcpu = 0;
	cp->p_cpticks = 0;
	cp->p_maxrss = pp->p_maxrss;	/* inherit limits on maxrss */
	if (pp->p_ttyvp != NULL) {
		cp->p_ttyfp = pp->p_ttyfp;
		FP_HOLD(pp->p_ttyfp);
		cp->p_ttyvp = pp->p_ttyvp;
	};

	/* link up to parent-child-sibling chain---
	 * no need to lock generally since only a free proc call
	 * (done by same parent as newproc) diddles with child chain.
	 */

	cp->p_sibling = pp->p_child;
	cp->p_parent = pp;
	pp->p_child = cp;
	cp->p_sysid = pp->p_sysid;	/* REMOTE */

	/*
	 * make duplicate entries
	 * where needed
	 */

	for (n=0; n<v.v_nofiles; n++)
		if (u.u_ofile[n] != NULL) 
			u.u_ofile[n]->f_count++;

	/*
	 * Bump other ref counts, as needed.
	 */
	VN_HOLD(u.u_cdir);
	if (u.u_rdir)
		VN_HOLD(u.u_rdir);
	crhold(u.u_cred);
	
	/*
	 * Copy process.
	 */
	switch (procdup(cp, pp)) {
	case 0:
		/* Successful copy */
		break;
	case -1:
		if (!failok)
			cmn_err(CE_PANIC, "newproc - fork failed\n");

#if DEBUG
		procdup_failcnt++;
#endif

		/* reset all incremented counts */
		pexit();

		if (cp->p_ttyvp != NULL) {
		    struct file *fp;

		    fp = cp->p_ttyfp;
		    cp->p_ttyvp = NULL;
		    cp->p_ttyfp = NULL;
		    FP_RELE(fp);
		};
		/* clean up parent-child-sibling pointers--
		* No lock necessary since nobody else could
		* be diddling with them here.
		*/

		pp->p_child = cp->p_sibling;
		cp->p_parent = NULL;
		cp->p_sibling = NULL;
		cp->p_stat = NULL;
		cp->p_ppid = 0;
		u.u_error = EAGAIN;
		return(-1);
	case 1:
		/* Child resumes here */
		u.u_sig_flag &= ~(BSD43_U_SF_OMASK);
		return(1);
	}

	u.u_rval1 = cp->p_pid;		/* parent returns pid of child */

	/* have parent give up processor after
	 * its priority is recalculated so that
	 * the child runs first (its already on
	 * the run queue at sufficiently good
	 * priority to accomplish this).  This
	 * allows the dominant path of the child
	 * immediately execing to break the multiple
	 * use of copy on write pages with no disk home.
	 * The parent will get to steal them back
	 * rather than uselessly copying them.
	 */
	if (fork_do_runrun)
		runrun = 1;

	/*
	 * If vfork, wait for child to finish
	 */
	if (isvfork) {
		while (cp->p_flag & SVFORK)
			sleep((caddr_t)cp, PZERO - 1);
	}
	return(0);
}

/*
 * Create a duplicate copy of a process, everything but stack.
 */

procdup(cp, pp)
struct proc	*cp;
struct proc	*pp;
{
	register preg_t		*p_prp;
	register preg_t		*c_prp;
	register user_t		*uservad;
	register struct vnode	*vp;
	user_t			*uballoc();
	int			s;


	winublock();
	if ((uservad = uballoc(cp)) == NULL) {
		winubunlock();
		return(-1);
	}

	/*
	 * Map the child's u block at uservad.
	 */

	uaccess(cp, uservad);

	/*
	 * Save here so rsav area will be copied to child 
	 * before we start memory allocation to copy regions
	 */

	checkfp(u.u_procp,0);	/* save current fp values into pcb */
	s = splall();		/* put things into a known state */
	if(save())  {
		extern struct proc	*lastproc;
		extern int fpowner;
#ifdef	XPR_DEBUG
	XPR3(XPR_SWTCH, "procdup: resuming procp=0x%x, fpowner=0x%x", 
		u.u_procp, fpowner);
#endif	XPR_DEBUG
		if (fpowner == (int)u.u_procp) {
			USER_REG(EF_SR) |= SR_CU1;
		}
		else {
			USER_REG(EF_SR) &= ~SR_CU1;
		}
		
		/*
		 * RESUME:
		 * When we resume a new process, get rid of previous proc
		 * if it was a zombie
		 */
		switch (lastproc->p_stat) {
			case SZOMB:

				/*	Free up remaining memory used by the
			 	 *	zombie process here.  This is just
			 	 *	the u-block and the sdt's.
			 	 */

				ubfree(lastproc);
				break;
		}
		splx(s);
		return 1;
	}
	splx(s);

	/*	Setup child u-block
	 */
	
	setuctxt(cp, uservad);

	/*	Duplicate all the regions of the process.
	 */

	p_prp = pp->p_region;
	c_prp = cp->p_region;

	for (; p_prp->p_reg ; p_prp++, c_prp++) {
		register int		prot;
		register reg_t		*rp;

		if ( p_prp->p_type == PT_GR )
			continue;
		prot = (p_prp->p_flags & PF_RDONLY ? SEG_RO : SEG_RW);
		reglock(p_prp->p_reg);
		if ((rp = dupreg(p_prp->p_reg, NOSLEEP, NOFORCE)) == NULL) {
			regrele(p_prp->p_reg);
			if (c_prp > cp->p_region)
				do {
					c_prp--;
					reglock(c_prp->p_reg);
					detachreg(c_prp, uservad, 0 /* don't invalidate TLBs */);
				} while (c_prp > cp->p_region);
			udeaccess(cp);
			winubunlock();
			ubfree(cp);
			return(-1);
		}
		if (attachreg(rp, uservad, p_prp->p_regva,
			     p_prp->p_type, prot) == NULL) {
			if (rp->r_refcnt)
			{
				regrele(rp);
			}
			else
				freereg(rp);
			if (rp != p_prp->p_reg) {
				regrele(p_prp->p_reg);

				/* Note that we don't want to
				** do a prele(ip) here since
				** rp will have had the same
				** ip value and the freereg
				** will have unlocked it.
				*/
			}
			if (c_prp > cp->p_region)
				do {
					c_prp--;
					reglock(c_prp->p_reg);
					detachreg(c_prp, uservad, 0 /* don't invalidate TLBs */);
				} while (c_prp > cp->p_region);
			udeaccess(cp);
			winubunlock();
			ubfree(cp);
			return(-1);
		}
		regrele(p_prp->p_reg);
		if (rp != p_prp->p_reg) {
			regrele(rp);
		}
	}

	/*	Flush the parents regions so that any
	**	pages which were made copy-on-write
	**	get flushed from cache.
	**	unmod all tlb entries for the parent's regions.
	**	Do this by giving the parent a new tlbpid. (We're flushing
	**	a lot of data pages that we need to and text pages that
	**	we don't need to.)
	**	Don't screw kernel processes.
	*/

	if(pp->p_tlbpid > SYS_TLBPID_UPB)
		new_tlbpid(pp);


	/*	Put the child on the run queue.
	*/

	udeaccess(cp);
	winubunlock();
	cp->p_stat = SRUN;
	setrq(cp);
	return(0);
}

/*	Setup context of child process
*/

setuctxt(p, up)
register struct proc *p;	/* child proc pointer */
register struct user *up;	/* child u-block pointer */
{
	ASSERT(winublocked());

	/*	Copy u-block 
	 */

	bcopy(&u, up, KERNELSTACK-UADDR);


	/*	Reset u_procp.
	*/

	up->u_procp = p;

	/* Initialize wired tlb entries for new process */
	setup_wired_tlb(p);
#ifdef R6000
	/* zero the segment table */
	bzero((caddr_t)utostbl(up), KSTESIZE);
#endif
}

/*
 * uballoc(p) 
 *	Allocates ublock for proc p.
 *	Returns address of win_ublk or zero.
 *	Tlb wired entries use:
 *	<USIZE>	map u block at &u
 *	<USIZE> map second u block at &win_ublk for procdup
 *	<rest> ? map kpdeseg entries for user to reduce double tlb misses
 */

struct user *
uballoc(p)
register proc_t *p;
{
	register struct user *addr;
	register pde_t *pdptr;
	register int ii;
	register caddr_t *q;
	register pfd_t *pfd;
	register int s;

	ASSERT(winublocked());

	/* 	Allocate pages for ublock.
	*/

	/* avail[rs]mem can be modified at interrupt level */
	s = splhi();
	if (availrmem - USIZE < tune.t_minarmem ||
	   availsmem - USIZE < tune.t_minasmem) {
		splx(s);
		nomemmsg("uballoc", USIZE, 0, 0);
		return(NULL);
	} else {
		availrmem -= USIZE;
		availsmem -= USIZE;
	}
	splx(s);

	memlock();
	if (ptmemall(&sysreg, ubptbl(p), USIZE, REGION_NOT_LOCKED, 0,
		     btoct(&u))) {
		cmn_err(CE_PANIC,
			"uballoc - ptmemall failed for u-block");
	}
	memunlock();
	addr = (struct user *)win_ublk;

	/* 
	 * Don't want tlbmod faults on uarea;
	 * Also, set up virtual addresses for u area in p_tlbhi_tbl.
	 */

	for(ii = 0, pdptr = ubptbl(p), q = p->p_tlbhi_tbl;
		ii < USIZE;
		ii++, pdptr++, q++) {
		pg_setmod(pdptr);
		pg_setvalid(pdptr);
		pfd = pdetopfdat(pdptr);
		pfd->pf_dbd.dbd_type = DBD_SYS;
		pfd->pf_rawcnt = 1;
		ASSERT(pfd->pf_use == 1);
		*q = ((char *) &u) + ii * NBPC;
	}

	return(addr);
}

/*
 *	ubfree(P) frees up proc p's SDT tables and ublock
 */

ubfree(p)
register struct proc *p;
{
	int	i;
	pde_t	*pde;
	pfd_t	*pfd;
	/* free up u_block */

	memlock();
	for (i = 0, pde = ubptbl(p); i < USIZE; i++, pde++) 
		if (pg_isvalid(pde)) {
			pfd = pdetopfdat(pde);
			ASSERT(pfd->pf_dbd.dbd_type == DBD_SYS && \
			       pfd->pf_rawcnt == 1 && \
			       pfd->pf_use == 1);
			pfd->pf_dbd = null_dbd;
			pfree(&sysreg,pde,&(pfd->pf_dbd),1);
			/* avail[rs]mem can be modified at interrupt level */
			{   register int s;
			    s = splhi();
			    availrmem++;
			    availsmem++;
			    splx(s);
			}
		};
	memunlock();
}

/* 
 * Set up tlb wired entries to allow access to process p's ublock.
 */

uaccess(p, addr)
struct proc *p;
register char * addr;
{
	register int i;
	register pde_t pte;
#ifdef PERFECT_COLORING
	extern unsigned int scachemask;
#endif PERFECT_COLORING	

	for(i = 0; i < USIZE; i++) {
		pte = p->p_ubptbl[i];		/* u-area pte */
		pte.pgi.pg_pde |= PG_G;		/* add Global */
#ifdef PERFECT_COLORING
		if ((pte.pgm.pg_pfn ^ btoct(addr)) & scachemask)  {
		  pte.pgi.pg_pde |= PG_N;	/* make noncached */
		  invalidate_scache( ptosv(pte.pgm.pg_pfn), NBPC );
		  cmn_err(CE_WARN,
			  "uaccess page non-cached due to bad coloring");
		}
#endif PERFECT_COLORING	
		*kvtokptbl(ctob(btoc(addr) + i)) = pte;
		/* tlbwired(TLBWIREDBASE + USIZE + i, 0,
		 *	ctob(btoc(addr) + i), p->p_ubptbl[i].pgi.pg_pde);
		 */
	}
}

/*
 *	Invalidate tlb entries used to map winublk
 */

udeaccess(p) 
struct proc *p;
{

	kvirt_inval(pnum(win_ublk), USIZE);
}

/* 
 * Initialize the wired tlb entries used for mapping page tables.
 * Called on fork, exec, region shrink or detach.
 * (Generally, when page tables are removed.)
 * Page table for wired entries is cleared.
 * Virtual address table is set to an invalid virtual address.
 * Wired tlb is cleared if current process.
 * Index of next open entry is reset.
 * Note: Wired tlb entries are "faulted in" by tfault on double tlb miss.
 */

setup_wired_tlb(p)
struct proc *p;
{
	register int i;
	register pde_t *pd;
	register caddr_t *q;

	q = p->p_tlbhi_tbl + USIZE;
	pd = ubptbl(p) + USIZE;
	for(i = USIZE + TLBWIREDBASE; i < NWIREDENTRIES; i++, pd++, q++) {
		if(p == u.u_procp)
			invaltlb(i);
		*q = (caddr_t)PHYS_TO_K0(0);
		pd->pgi.pg_pde = 0;
	}
	p->p_nexttlb = USIZE;
}


#define gpds_putc(cp,c) (*((*(cp))++) = (c))

gpds_puts(cp,s,w)
	char	**cp;
	char	*s;
	int w;
{
	for (;*s && w > 0 ;s++, w--)
		*((*cp)++) = *s;
	for (;w > 0; w--)
		*((*cp)++) = ' ';
}


gpds_putn(cp,d,w)
	char	**cp;
	unsigned int	d;
	int w;
{
	char	dbuf[10];
	char	*dp;

	dp = dbuf + sizeof(dbuf) - 1;
	*dp = 0;
	while (1) {
		*(--dp) = (d % 10) + '0';
		d = d / 10;
		if (d == 0)
			break;
	};

	if (w > 0) {
		if (w > 9)
			w = 9;
		if (dp < dbuf + sizeof(dbuf) - 1 - w)
			dp = dbuf + sizeof(dbuf) - 1 - w;
		else 
			while (dp > dbuf + sizeof(dbuf) - 1 - w)
				*(--dp) = ' ';
	};
	gpds_puts(cp,dp,w);
};
	
	
get_process_display_status(sbuf,slen,wq)
     	char	*sbuf;
	int	slen;
        queue_t *wq;
{
	struct	proc *cp;
	struct	proc *p;
	struct	user *cu;
	char	*sp;	
	struct	stdata *stp;
	queue_t *qp;

	if (slen < 0)
		return(1);

	for (cp = NULL, p = proc; p < (struct proc *) v.ve_proc; p++)
		if (p->p_stat != 0 &&
		    p->p_ttyvp != NULL) {
			if (p->p_ttyvp->v_type != VCHR)
				continue;
			if (! cdevsw[major(p->p_ttyvp->v_rdev)].d_str)
				continue;
			stp = p->p_ttyvp->v_stream;
			if (stp == NULL)
				continue;			
			for (qp = stp->sd_wrq; qp != NULL ; qp = qp->q_next)
				if (qp == wq)
					break;
			if (qp == NULL)
				continue;
			if (p->p_jcpgrp != 0 &&
			    stp->sd_pgrp != p->p_jcpgrp)
			  	continue;
			if (cp == NULL ||
			    p->p_stat == SRUN ||
			    (p->p_stat == SSLEEP &&
			     cp->p_stat != SRUN)) {
				cp = p;
				if (cp->p_stat == SRUN)
					break;
			};
		};

	if (cp == NULL)
		return(1);

	cu = (((cp->p_stat != SZOMB) &&
	       cp->p_ubptbl[0].pgm.pg_vr)
		? (struct user *)(mkpde(K0SEG,cp->p_ubptbl[0].pgm.pg_pfn))
		: NULL);

	/*	      Name Pid Sta U Cpu  S CPu Read Write Mem */
	sp = sbuf;
	gpds_puts(&sp,(cu ? cu->u_psargs 
			: ((cp->p_stat == SZOMB) ? "(zombie)" 
					: "(swapped)")),20);
	gpds_putc(&sp,' ');
	gpds_putn(&sp,cp->p_pid,5);
	gpds_putc(&sp,' ');
	gpds_puts(&sp,
		((cp->p_stat == SRUN) ? "run" :
		 ((cp->p_stat == SZOMB) ? "zomb" :
		 ((cp->p_stat == SSLEEP) ? "slp" :
	     	 ((cp->p_stat == SSTOP) ? "stop" :
		 ((cp->p_stat == SIDL) ? "idle" :
		 ((cp->p_stat == SONPROC) ? "proc" :
		 ((cp->p_stat == SXBRK) ? "xbrk" :
			"?"))))))),
		  4);
	gpds_putc(&sp,' ');
	if (cp->p_stat == SZOMB) {
		gpds_putn(&sp,
			cp->p_utime / HZ,
			  5);
		gpds_putc(&sp,'.');
		gpds_putn(&sp,
			((cp->p_utime % HZ) * 10) / HZ,
			  1);
		gpds_putc(&sp,' ');
		gpds_putn(&sp,
			cp->p_stime / HZ,
			  5);
		gpds_putc(&sp,'.');
		gpds_putn(&sp,
			((cp->p_stime % HZ) * 10) / HZ,
			  1);
	} else if (cu != NULL) {
		gpds_putn(&sp,
			cu->u_utime / HZ,
			  5);
		gpds_putc(&sp,'.');
		gpds_putn(&sp,
			((cu->u_utime % HZ) * 10) / HZ,
			  1);
		gpds_putc(&sp,' ');
		gpds_putn(&sp,
			cu->u_stime / HZ,
			  5);
		gpds_putc(&sp,'.');
		gpds_putn(&sp,
			((cu->u_stime % HZ) * 10) / HZ,
			  1);
	} else 
		gpds_puts(&sp,"     ?     ?",12);
	gpds_putc(&sp,' ');
	gpds_putn(&sp,
		(cu ? cu->u_ior :
		 (cp->p_ru ? cp->p_ru->ru_inblock : 0)),
		  4);
	gpds_puts(&sp,"R ",2);
	gpds_putn(&sp,
		(cu ? cu->u_iow :
		 (cp->p_ru ? cp->p_ru->ru_oublock : 0)),
		  4);
	gpds_puts(&sp,"W ",2);
	gpds_putn(&sp,
		cp->p_size,
		  4);
	gpds_putc(&sp,'P');
	*sp = 0;

	return(0);
}
