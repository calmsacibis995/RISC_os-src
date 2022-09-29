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
#ident	"$Header: exit.c,v 1.29.1.6.1.6 90/08/13 17:17:26 hawkes Exp $"

/*	Copyright (c) 1984 AT&T	*/
/*	  All Rights Reserved  	*/

/*	THIS IS UNPUBLISHED PROPRIETARY SOURCE CODE OF AT&T	*/
/*	The copyright notice above does not evidence any   	*/
/*	actual or intended publication of such source code.	*/

#include "sys/types.h"
#include "sys/signal.h"		/* needed by user.h */
#include "sys/pcb.h"		/* needed by user.h */
#include "sys/user.h"
#include "sys/immu.h"		/* needed by proc.h */
#include "sys/sbd.h"		/* needed by proc.h */
#include "sys/region.h"		/* needed by proc.h */
#include "sys/proc.h"
#include "sys/var.h"
#include "sys/errno.h"
#include "sys/cmn_err.h"
#include "sys/debug.h"
#include "sys/numips.h"
#include "sys/vnode.h"
#include "sys/file.h"
#include "sys/wait.h"
#include "sys/fs/proc_fs.h"
#include "sys/termio.h"
#include "sys/kmem.h"
#include "sys/sysmacros.h"

extern int _riscos_kill_stopped_orphans;

extern caddr_t kmemzalloc();
extern void kmemfree();

/*
 * exit system call:
 * pass back caller's arg
 */

rexit()
{
	register struct a {
		int	rval;
	} *uap;

	uap = (struct a *)u.u_ap;
	exit((uap->rval & 0377) << 8);
}

/*
 * Release resources.
 * Enter zombie state.
 * Wake up parent and init processes,
 * and dispose of children.
 */

exit(rv)
{
	register int i;
	register struct proc *p, *q, *nq;
	register preg_t	*prp;
	register reg_t	*rp;
	struct vnode	*vp;
	int		flag;
	extern int realitexpire();
	int		s;
	struct bsd43_rusage *m;

  	m = (struct bsd43_rusage *) kmemzalloc(sizeof(struct bsd43_rusage),
					       M_ZOMBIE,M_WAITOK);
	p = u.u_procp;
#ifdef mips
	checkfp(p, 1);
#endif
	p->p_flag &= ~(STRC);
 	if (p->p_flag & SPROPEN) {		/* being traced by /proc */
 		VTOP(p->p_trace)->pi_xstat = rv;
 		wakeup(p->p_trace);
 	}
	p->p_flag &= ~(SPRSTOP); /* we don't stop now */

	/* Turn off real time timer. */
	untimeout_func(realitexpire, (caddr_t)p);
	timerclear(&p->p_realtimer.it_interval);
	timerclear(&p->p_realtimer.it_value);

	for (i = 0; i < NSIG; i++) {
		u.u_signal[i] = SIG_IGN;
		u.u_procp->p_sigignore |= sigmask(i);
		u.u_procp->p_sigcatch &= ~sigmask(i);
	};
	p->p_cursig = 0; /* keep sleep() from stopping if we were called */ 
			 /* from psig()					 */

	if ((p->p_pid == p->p_pgrp)
	 && (u.u_procp->p_ttyvp != NULL)) {
	        int	cur_pgrp;
		int	null_pgrp = 0;
		int 	rdev;

		/*
		 *	This is a session leader exiting
		 */
		if (iioctl(u.u_procp->p_ttyvp,TIOCGPGRP,(char *) &cur_pgrp,
			      FWRITE | FREAD)) {
			u.u_error = 0;
			cur_pgrp = 0;
		};
		if (p->p_pgrp != p->p_jcpgrp &&
		    p->p_jcpgrp != 0) {
			cmn_err(CE_WARN,"exit: fixed process %d pgrp (%d) != jcpgrp (%d)",
				p->p_pid,p->p_pgrp,p->p_jcpgrp);
			if (cur_pgrp == p->p_jcpgrp) {
				cur_pgrp = p->p_pgrp;
				if (u.u_procp->p_ttyvp != NULL)
					(void) iioctl(u.u_procp->p_ttyvp,
						    TIOCSPGRP,
						    (char *) &cur_pgrp,
					      	    FWRITE | FREAD);
			};
			p->p_jcpgrp = p->p_pgrp;
		};
		if (p->p_jcpgrp != 0 &&
		    cur_pgrp == p->p_jcpgrp) {
			signal(p->p_jcpgrp, SIGHUP);
		} else if (cur_pgrp != 0) {
			struct proc *ap;

			/*
			 * If the terminal's foreground process group is in the
			 * same session (same p_pgrp) as the current
			 * process (session leader) which is exiting,
			 * send SIGHUP to the foreground process group.
			 */
			for (ap = proc; ap < (struct proc *)v.ve_proc; ap++)
				if (ap->p_jcpgrp == cur_pgrp && 
				    ap->p_stat != SZOMB &&
				    ap->p_pgrp == p->p_pgrp) {
					signal(ap->p_jcpgrp,SIGHUP);
					break;
				};
			if (ap >= (struct proc *) v.ve_proc)
				cur_pgrp = 0;
					/* foreground process group is	*/
					/* in a different session	*/
		};
		/*
		 * Relinquish controlling terminal
		 */
		if (u.u_procp->p_ttyvp != NULL) {
			rdev = u.u_procp->p_ttyvp->v_rdev;
			if (cur_pgrp != 0)
				(void) iioctl(u.u_procp->p_ttyvp,
					TIOCSPGRP,(char *) &null_pgrp,
				      	FWRITE | FREAD);
			forceclose_pgrp(rdev,p->p_pgrp);
		};
	}

	for (i = 0; i < v.v_nofiles; i++) {
		if (u.u_ofile[i] != NULL) {
			if (u.u_pofile[i] & UF_MAPPED)
	  			munmapfd(i);
			/* Release all System-V style record locks, if any */
			(void) vno_lockrelease(u.u_ofile[i]);
			closef(u.u_ofile[i]);
			u.u_ofile[i] = NULL;
			u.u_pofile[i] = 0;
		};
	}
	punlock();
	if (u.u_cdir) {
		VN_RELE(u.u_cdir);
		u.u_cdir = NULL;
	};
	if (u.u_rdir) {
		VN_RELE(u.u_rdir);
		u.u_rdir = NULL;
	}
  	u.u_rlimit[BSD43_RLIMIT_FSIZE].rlim_cur = BSD43_RLIM_INFINITY;

	/*
	 * insert calls to "exitfunc" functions.
	 */
	semexit();
	shmexit();

	update_rusage();
	acct(rv);

	if (u.u_procp->p_ttyvp != NULL) {
		clr_controlling_tty();
	};

	INODE_LOCK_TEST(p->p_pid);

	if (p->p_pid == 1) {
		if (p->p_region[0].p_reg == NULL ||
		    p->p_region[1].p_reg == NULL) {
			cmn_err(CE_PANIC,"Can't exec /etc/init");
		} else
			cmn_err(CE_PANIC,"init died");
	}

	/*
	 * if we are vfork'ed, wakeup parent
	 */
	vrelvm();

	/* free data, text; free U block and stack switch */
	prp = p->p_region;
	while (rp = prp->p_reg) {
		reglock(rp);
		detachreg(prp, &u, 0 /* don't invalidate TLBs */);
	}

	p->p_xstat = rv;
	p->p_utime = u.u_cutime + u.u_utime;
	p->p_stime = u.u_cstime + u.u_stime;
	p->p_ru = m;
	if (m != NULL) {
		*p->p_ru = u.u_ru;
		ruadd(p->p_ru, &u.u_cru);
	};

	flag = 0;
	if ((q = p->p_child) != NULL)
	{
		for (; q != NULL  ;   q = nq) {
			s = splhi();
		        nq = q->p_sibling;
			p->p_child = nq;
			q->p_ppid = 1;
			q->p_parent = &proc[1];
			q->p_sibling = proc[1].p_child;
			proc[1].p_child = q;
			splx(s);

			if (q->p_stat == SZOMB) {
				flag = 1;
			}
			/*
			 * Traced processes are killed
			 * since their existence means someone is screwing up.
			 * Stopped processes are sent a hangup and a continue.
			 * This is designed to be ``safe'' for setuid
			 * processes since they must be willing to tolerate
			 * hangups anyways.
			 */
			  else if (q->p_flag&STRC) {
				q->p_flag &= ~STRC;
				if (_riscos_kill_stopped_orphans == 0) {
					psignal(q, SIGKILL);
				}
				if (q->p_stat == SSTOP) {
					q->p_cursig = 0;
					setrun(q);
				};
			} else if (q->p_stat == SSTOP) {
				psignal(q, SIGHUP);
				psignal(q, SIGCONT);
			}
			/*
			 * Protect this process from future
			 * tty signals, clear TSTP/TTIN/TTOU if pending.
			 */
			(void) spgrp(q);

		}	/* end for loop */

		/* only send 1 death of child sig to proc 1--also delay
		 * sending sig until reasonably sure not to sleep on
		 * proc 1 parent-child-sib lock.
		 */

		if (flag) {
			psignal(&proc[1], SIGCLD);
		}
	}	/* end if children */

	/* Now take care of all descendants in session group.
	 * We do not use parent-child-sibling chain here
	 * since such use would require elaborate locking
	 * of those chains all down the line, and this
	 * case happens infrequently to warrant that
	 * overhead everywhere.
	 */
	for (q = &proc[1]; q < (struct proc *) v.ve_proc; q++) {
		/* if proc is in same session as the exiting proc */
		if (p->p_pgrp == q->p_pgrp) { 
			/* and the exiting proc is a session leader,
			 * then remove this proc from the session group
			 */
			if (p->p_pid == p->p_pgrp)
				q->p_pgrp = 0;	
			
			/* if any member of a newly-orphaned process group
			 * is stopped send a SIGHUP followed by a SIGCONT,
			 * to every member of the process group
			 */
			if (POSIX_SYSCALL && p->p_jcpgrp == p->p_pid && 
			    p->p_jcpgrp == q->p_jcpgrp && q->p_stat == SSTOP) {
				signal(p->p_jcpgrp, SIGHUP);
				signal(p->p_jcpgrp, SIGCONT);
			}
		}
	}

	crfree(u.u_cred);
	u.u_cred = NULL;

	p->p_pgrp = 0;
	p->p_stat = SZOMB;
	psignal(p->p_parent, SIGCLD);
	wakeup((caddr_t)p->p_parent);

	/* pswtch() frees up stack and ublock */

	swtch();

	/* no deposit, no return */
}

waitpid()
{
	register struct a {
		int	pid;
		int	*stat_loc;
		int	options;
	} *uap = (struct a *)u.u_ap;

	if (uap->options & ~(WNOHANG|WUNTRACED)) {
		u.u_error = EINVAL;
		return;
	}
	u.u_error = wait1(uap->stat_loc,uap->options,(struct bsd43_rusage *)0,
			  1, 0, uap->pid);
}

wait3()
{
	register struct a {
		int	*sp;
		int	options;
		struct	bsd43_rusage *ru;
	} *uap = (struct a *)u.u_ap;

	u.u_error = wait1(uap->sp,uap->options,uap->ru,1,
			  sizeof(struct bsd43_rusage),-1);
}

/*
 * Generic interface for wait(), wait2(), and wait3().
 * (Assumes that wait() and wait2() libc code sets unused arguments to 0.)
 *
 * Search for terminated (zombie) children.
 * Collect their status, and for zombies clean up.
 *
 * Look also for stopped (traced) children,
 * and pass back status from them.
 *
 * Note: 
 *	Wait3() passes the status back to user mode in u.u_rval2 for
 *      compatibility with old wait() libc code which relied on this.
 *	Since we do the copyout() ourselves, the libc code no longer 
 *	needs to do this.
 */

wait1(uap_sp,uap_options,uap_ru,uap_copyflag,uap_ru_size,pid)
	int	*uap_sp;
	int	uap_options;
	struct	bsd43_rusage *uap_ru;	
	int	uap_copyflag;
	int	uap_ru_size;
        int     pid;
{
	struct proc *p;
	int	found;

loop:
	found = 0;
	for (p = u.u_procp->p_child ; p ; p = p->p_sibling) {
		if (!((pid == -1) ||
		      (pid == p->p_pid) ||
		      ((pid < -1) && (p->p_jcpgrp == -pid)) ||
		      ((pid == 0) && (p->p_jcpgrp == u.u_procp->p_jcpgrp)))) {
			continue; /* not the process we are looking for */
		} 
		found++;
		if (p->p_stat == SZOMB) {
			freeproc(p, 1, uap_ru, uap_copyflag, uap_ru_size);
			goto done;
		}
		if (p->p_stat == SSTOP && 
		    (p->p_flag&SWTED) == 0 &&
		    ((p->p_flag & STRC) ||
		     (uap_options & WUNTRACED))) {
			p->p_flag |= SWTED;
			u.u_rval1 = p->p_pid;
			u.u_rval2 = (p->p_cursig<<8) | 0177; /* WSTOPPED */
			goto done;
		}
	}
	if (!found) {
		u.u_error = ECHILD;
		return(u.u_error);
	}
	if (uap_options & WNOHANG) {
		u.u_rval1 = 0;
		return(u.u_error);
	}
	/*
	 * Handle restart after interrupt.
	 */
	if (BSD_SYSCALL && setjmp(u.u_qsav)) {
		if ((u.u_sigintr & bsd43_sigmask(u.u_procp->p_cursig)) != 0)
			u.u_error = EINTR;
	        else {
		  	u.u_eosys = RESTARTSYS;
			u.u_error = 0;
		};
		return(u.u_error);
	}

	sleep((caddr_t)u.u_procp, PWAIT);
	goto loop;

done:
	/*
	 * Copy the status to user space.
	 */
	if ( uap_sp
	&& (copyout((caddr_t)&u.u_rval2, (caddr_t)uap_sp, sizeof(int)) < 0)) {
		u.u_error = EFAULT;
	}
	return(u.u_error);
}

/*
 * The old wait(2) system call has no arguments to the kernel.
 */
wait()
{
	u.u_ap[0] = 0;
	u.u_ap[1] = 0;
	u.u_ap[2] = 0;

	wait3();
}

#ifdef mips
/*
 * mips_wait() provides the same functionality as wait3() above but allows
 * the rusage structure to grow so that mips specific stats can be returned
 * in the future.
 */
mips_wait(status, options, ru, rusage_size)
union bsd43_wait *status; /* user supplied pointer */
int options;		/* user supplied options */
struct bsd43_rusage *ru; /* user supplied pointer */
int rusage_size;	/* user supplied size    */
{
	if(rusage_size < 0 || rusage_size > sizeof(struct bsd43_rusage)){
		return(EINVAL);
	}
	return(wait1((int *) status, options, ru, 1, rusage_size, -1));
}
#endif mips

/*
 * Remove zombie children from the process table.
 */
freeproc(p, flag, ru, ru_copyflag, ru_size)
register struct proc *p;
	struct bsd43_rusage *ru;
	int	ru_copyflag;
	int	ru_size;
{

	register proc_t	*q;

	if (flag) {
		register n;

		n = u.u_procp->p_cpu + (p->p_cpu - (p->p_nice - NZERO));
		if ((n - (u.u_procp->p_nice - NZERO)) > 80)	/* LOVE THEM MAGIC NUMBERS!!!  Why 80? */
			n = 80 + (u.u_procp->p_nice - NZERO);
		n = BOUND(n,0,255);
		if (n > u.u_procp->p_cpu)
			u.u_procp->p_cpu = n;
		u.u_rval1 = p->p_pid;
		u.u_rval2 = ((struct proc *)p)->p_xstat;
	}
	u.u_cutime += ((struct proc *)p)->p_utime;
	u.u_cstime += ((struct proc *)p)->p_stime;
	if (ru && p->p_ru && ru_size > 0) {
		if (ru_size > sizeof(struct bsd43_rusage))
			ru_size = sizeof(struct bsd43_rusage);
		if (ru_copyflag)
			u.u_error = copyout((caddr_t)p->p_ru,
					    (caddr_t)ru, ru_size);
		else
			bcopy((caddr_t)p->p_ru,(caddr_t)ru,ru_size);
	};
	if (p->p_ru) {
		ruadd(&u.u_cru, p->p_ru);
		kmemfree((caddr_t) p->p_ru,M_ZOMBIE,M_WAITOK);
		p->p_ru = NULL;
	}
	p->p_stat = NULL;
	p->p_pid = 0;
	p->p_ppid = 0;
	p->p_sig = 0L;
	p->p_flag = 0;
	p->p_wchan = 0;
	p->p_cursig = 0;
	p->p_trace = 0;
	p->p_pgrp = 0;

	q = p->p_parent;
	if (q->p_child == p)
		q->p_child = p->p_sibling;
	else {
		for (q = q->p_child ; q ; q = q->p_sibling)
			if (q->p_sibling == p)
				break;
	
		ASSERT(q  &&  q->p_sibling == p);
		q->p_sibling = p->p_sibling;
	}

	p->p_parent = NULL;
	p->p_sigcatch = 0;
	p->p_sigignore = 0;
	p->p_sigmask = 0;
	p->p_jcpgrp = 0;
	p->p_ttyvp = 0;
	p->p_ttyfp = NULL;
}

/* clean up common process stuff -- called from newproc()
 * on error in fork() due to no swap space
 *
 * NB:  This is usually called because procdup failed to getcpages
 *      or dupreg, and should just decrement the stuff in the
 *      forking procs state that was incremented prior to the
 *      call to procdup.  Free's occur in the same order that
 *      incs occured in newproc.
 */
pexit()
{
	register int	i;

	/* don't include punlock() since not needed for newproc() clean */

	for (i=0; i<v.v_nofiles; i++)
		if (u.u_ofile[i] != NULL) {
			u.u_ofile[i]->f_count--;
		};

	crfree(u.u_cred);

	if (u.u_cdir) {
		VN_RELE(u.u_cdir);
	};

	if (u.u_rdir) {
		VN_RELE(u.u_rdir);
	}
}
