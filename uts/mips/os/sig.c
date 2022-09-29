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
#ident	"$Header: sig.c,v 1.39.1.7 90/05/10 05:53:06 wje Exp $"

/*	Copyright (c) 1984 AT&T	*/
/*	  All Rights Reserved  	*/

/*	THIS IS UNPUBLISHED PROPRIETARY SOURCE CODE OF AT&T	*/
/*	The copyright notice above does not evidence any   	*/
/*	actual or intended publication of such source code.	*/

#include "sys/param.h"
#include "sys/types.h"
#include "sys/sysmacros.h"
#include "sys/signal.h"		/* needed by user.h */
#include "sys/pcb.h"		/* needed by user.h */
#include "sys/user.h"
#include "sys/immu.h"		/* needed by proc.h */
#include "sys/sbd.h"		/* needed by proc.h */
#include "sys/region.h"		/* needed by proc.h */
#include "sys/proc.h"
#include "sys/uio.h"
#include "sys/var.h"
#include "sys/errno.h"
#include "sys/cmn_err.h"
#include "sys/debug.h"
#include "sys/reg.h"
#include "sys/acct.h"
#include "sys/message.h"
#include "sys/vnode.h"
#include "sys/ptrace.h"
#include "sys/numips.h"
#include "sys/cpu_board.h"

extern int _riscos_kill_stopped_orphans;

/*
 * Priority for tracing
 */

#define	IPCPRI	PZERO

/*
 * Tracing variables.
 * Used to pass trace command from
 * parent to child being traced.
 * This data base cannot be
 * shared and is locked
 * per user.
 */

struct {
	int	ip_lock;
	int	ip_req;
	int	*ip_addr;
	int	ip_data;
	struct	proc *ip_tptr;
	int	ip_uid;
} ipc;

/*
 * Send the specified signal to
 * all processes with 'pgrp' as
 * job control process group.
 * Called by streamio.c for quits and
 * interrupts.
 */

signal(pgrp, sig)
register pgrp;
{
	register struct proc *p;

	if (pgrp == 0)
		return;
	for (p = &proc[1]; p < (struct proc *)v.ve_proc; p++)
		if (p->p_jcpgrp == pgrp && p->p_stat != SZOMB)
			psignal(p, sig);
}

/*
 * Send the specified signal to
 * the specified process.
 */

#define	stopsigmask	(sigmask(SIGSTOP)|sigmask(SIGTSTP)| \
			sigmask(SIGTTIN)|sigmask(SIGTTOU))

#ifndef SIG_CATCH
#define SIG_CATCH (void(*)())3
#endif SIG_CATCH

psignal(p, sig)
register struct proc *p;
int sig;
{
	int s;
	register void (*action)();
	int	mask;

	if (sig <= 0 || sig >= NSIG)
		return;
	mask = sigmask(sig);

	/*
	 * If proc is traced, always give parent a chance.
	 */
	if (p->p_flag & STRC)
		action = SIG_DFL;
	else {
		/* When SIGCONT is generated for a process that is stopped, 
		 * the process shall be continued, even if the SIGCONT
		 * signal is blocked or ignored.  P1003.1, section 3.3.1.2
		 */
		if (sig == SIGCONT && p->p_stat == SSTOP && 
		    ((u.u_sig_flag & BSD43_U_SF_SIGCONTEXT_SELECT) == 
		     BSD43_U_SF_POSIX_SIGCONTEXT)) {
			if (p->p_sigcatch & mask)
				action = SIG_CATCH;
			else
				action = SIG_DFL;
		} else
		/*
		 * If the signal is being ignored,
		 * then we forget about it immediately.
		 */
		if (p->p_sigignore & mask) {
		  	if (sig != SIGCLD)
				return;
			action = SIG_IGN;
		} else if (p->p_hold & mask)
			action = SIG_HOLD;
		else if (p->p_sigcatch & mask)
			action = SIG_CATCH;
		else
			action = SIG_DFL;
	}

	s = splclock();
	p->p_sig |= mask;
	splx(s);
	switch (sig) {

	case SIGTERM:
		if ((p->p_flag&STRC) || action != SIG_DFL)
			break;
		/* fall into ... */

	case SIGKILL:
		if (p->p_nice > NZERO)
			p->p_nice = NZERO;
		break;

	case SIGCONT:
		s = splclock();
		p->p_sig &= ~stopsigmask;
		splx(s);
			
		break;

	case SIGSTOP:
	case SIGTSTP:
	case SIGTTIN:
	case SIGTTOU:
		s = splclock();
		p->p_sig &= ~sigmask(SIGCONT);
		splx(s);
		break;

	default:
		break;
	}

	if (action == SIG_HOLD)
		return;

	s = splhi();
	switch (p->p_stat) {

	case SSLEEP:
		/*
		 * If process is sleeping uninterruptably
		 * we can't interrupt the sleep... the signal will
		 * be noticed when the process returns through
		 * trap() or syscall().
		 */
		if (p->p_flag & SNWAKE)
			goto out;
		/*
		 * Process is sleeping and traced... make it runnable
		 * so it can discover the signal in issig() and stop
		 * for the parent.
		 */
		if (p->p_flag&STRC)
			goto run;
		switch (sig) {

		case SIGSTOP:
		case SIGTSTP:
		case SIGTTIN:
		case SIGTTOU:
			/*
			 * These are the signals which by default
			 * stop a process.
			 */
			if (action != SIG_DFL)
				goto run;
			/*
			 * Don't clog system with children of init
			 * stopped from the keyboard.
			 */
			if (sig != SIGSTOP && p->p_ppid == 1) {
				if (_riscos_kill_stopped_orphans == 0)
					psignal(p, SIGKILL);
				p->p_sig &= ~mask;
				goto out;
			}
			/*
			 * If a child in vfork(), stopping could
			 * cause deadlock.
			 */
			if (p->p_flag&SVFORK)
				goto out;
			p->p_sig &= ~mask;
			p->p_cursig = sig;
			if (!(p->p_parent->p_flag & SNOCLDSTOP))
				psignal(p->p_parent, SIGCLD);
			stop(p);
			goto out;

		case SIGIO:
		case SIGURG:
		case SIGWINCH:
			/*
			 * These signals are special in that they
			 * don't get propogated... if the process
			 * isn't interested, forget it.
			 */
			if (action != SIG_DFL)
				goto run;
			p->p_sig &= ~mask;		/* take it away */
			goto out;

		case SIGCONT:
			if (sigmask(p->p_cursig) & stopsigmask)
				p->p_cursig = 0;
			/* fall through */

		default:
			/*
			 * All other signals cause the process to run
			 */
			goto run;
		}
		/*NOTREACHED*/

	case SSTOP:
		/*
		 * If traced process is already stopped,
		 * then no further action is necessary.
		 */
		if (p->p_flag&STRC)
			goto out;

		switch (sig) {

		case SIGKILL:
			/*
			 * Kill signal always sets processes running.
			 */
			goto run;

		case SIGCONT:
			/*
			 * If the process catches SIGCONT, let it handle
			 * the signal itself.  If it isn't waiting on
			 * an event, then it goes back to run state.
			 * Otherwise, process goes back to sleep state.
			 */
			if (sigmask(p->p_cursig) & stopsigmask)
				p->p_cursig = 0;
			if (action != SIG_DFL || p->p_wchan == 0)
				goto run;
			p->p_stat = SSLEEP;
			goto out;

		case SIGSTOP:
		case SIGTSTP:
		case SIGTTIN:
		case SIGTTOU:
			/*
			 * Already stopped, don't need to stop again.
			 * (If we did the shell could get confused.)
			 */
			p->p_sig &= ~mask;		/* take it away */
			goto out;

		default:
			/*
			 * If process is sleeping interruptibly, then
			 * unstick it so that when it is continued
			 * it can look at the signal.
			 * But don't setrun the process as its not to
			 * be unstopped by the signal alone.
			 */
			if (p->p_wchan && ((p->p_flag & SNWAKE) == 0))
				unsleep(p);
			goto out;
		}
		/*NOTREACHED*/

	default:
		/*
		 * SRUN, SIDL, SZOMB do nothing with the signal.
		 * It will either never be noticed, or noticed very soon.
		 */
		goto out;
	}
	/*NOTREACHED*/
run:
#ifdef NOTDEF
	/*
	 * Raise priority to at least PUSER.
	 */
	if (p->p_pri > PUSER)
		p->p_pri = PUSER;
#endif NOTDEF
	setrun(p);
out:
	splx(s);
}

/*
 * Returns true if the current
 * process has a signal to process,
 * and the signal is not held.
 * The signal to process is put in p_cursig.
 * This is asked at least once
 * each time a process enters the
 * system.
 * A signal does not do anything
 * directly to a process; it sets
 * a flag that asks the process to
 * do something to itself.
 */

issig()
{
	register n;
	register struct proc *p, *q;
	int	mask;
	int s;

	p = u.u_procp;
	if (p->p_cursig)
		return(p->p_cursig);
	while (n=fsig(p)) {
		mask = sigmask(n);
		s = splclock();
		p->p_sig &= ~mask;
		splx(s);
		p->p_cursig = n;

		if ((p->p_flag & STRC) &&
		    (mask & p->p_sigmask) &&
		    ((p->p_flag & SVFORK) == 0)
		    )
			return(n);
#ifdef RISCOS
		/* We have to do this because SIGXFSZ overloaded with SIGPWR */
		if (BSD_SYSCALL && n == SIGXFSZ) {
			if ((u.u_signal[n-1] != SIG_IGN) || 
				(p->p_flag&STRC))
				return(n);
			break;
		}
#endif

		switch (n) {
		case SIGTSTP:
		case SIGTTIN:
		case SIGTTOU:
			if (u.u_signal[n-1] == SIG_DFL) {
				/*
				 * Children of init aren't allowed to stop
				 * on signals from the keyboard.
				 */
				if (p->p_parent == &proc[1]) {
					psignal(p, SIGKILL);
					break;
				}
				/* fall into ... */
			};

		case SIGSTOP:
			if (u.u_signal[n-1] == SIG_DFL) {
				if (p->p_flag&STRC)
					break;
				if (!(p->p_parent->p_flag & SNOCLDSTOP))
					psignal(p->p_parent, SIGCLD);

				stop(p);
				swtch();
				continue;
			};

		case SIGCONT:
		case SIGURG:
		case SIGIO:
		case SIGWINCH:
		case SIGPWR:
				/*
				 * These signals are normally not
				 * sent if the action is the default.
				 */
			if (! (u.u_signal[n-1] == SIG_DFL ||
			       u.u_signal[n-1] == SIG_IGN))
				/* signal is being caught */
				return (n);
			break;

		case SIGCLD:
			if (u.u_signal[SIGCLD-1] == SIG_IGN && 
 			    ((u.u_sig_flag & BSD43_U_SF_SIGCONTEXT_SELECT)
                                        == BSD43_U_SF_SYSV_SIGCONTEXT)) {
				for (q = p->p_child; q; q = q->p_sibling)
					if (q->p_stat == SZOMB)
						freeproc(q, 0, NULL, 0, 0);
			} else if (u.u_signal[SIGCLD-1] != SIG_DFL)
				return(n);
			break;
		default:
			if ((u.u_signal[n-1] != SIG_IGN) || 
				(p->p_flag&STRC))
				return(n);
			break;
		};
	}
	p->p_cursig = 0;
	return(0);
}

/*
 * Put the argument process into the stopped state and notify the
 * parent and other interested parties via wakeup and/or signal.
 * 
 */

stop(p)
struct proc *p;
{
	ASSERT(p->p_parent->p_pid == p->p_ppid);
	p->p_stat = SSTOP;
	p->p_flag &= ~SWTED;
	wakeup((caddr_t) p->p_parent);
	if (p->p_trace)
		wakeup((caddr_t)p->p_trace);
}

/*
 * Perform the action specified by
 * the current signal.
 * The usual sequence is:
 *	if (issig())
 *		psig();
 */

psig(sig_type)
	int sig_type;	/* Interface from which signal was recieved */
			/* Currently 2 choices: SIG_TRAP & SIG_SYSCALL */
{
	register n, mask;
	register struct proc *rp = u.u_procp;
	void(*p)();
	int	is_sigset;
	int s;
	int	sent_signal;

restart:
	if (rp->p_flag & SPRSTOP) {
		rp->p_flag &= ~SPRSTOP;
		if (rp->p_ppid == 1)
			exit(rp->p_cursig);
		stop(rp);
		swtch();
		if (rp->p_cursig == 0)
			goto look_for_signals;
	}
	mask = sigmask(rp->p_cursig);
	if ((rp->p_flag & STRC)
	  || ((rp->p_flag & SPROCTR)
	    && ((rp->p_flag & SVFORK) == 0)
	    && (mask & rp->p_sigmask))) {
		/*
		 * If traced, always stop, and stay
		 * stopped until released by parent or tracer.
		 * If tracing via /proc, do not call procxmt.
		 */
		if ((rp->p_parent->p_sig_flag & BSD43_U_SF_SIGCONTEXT_SELECT)
				== BSD43_U_SF_BSD43_SIGCONTEXT)
			psignal(rp->p_parent, SIGCLD);
		do {
			stop(rp);
			swtch();
		} while ((rp->p_flag&SPROCTR) == 0
		  && !procxmt() && (rp->p_flag & STRC));

		/*
		 * If the traced bit got turned off,
		 * then put the signal taken above back into p_sig
		 * and rescan signals.
		 * This ensures that p_sig* and u_signal are consistent.
		 */
		if ((rp->p_flag&STRC) == 0) {
			s = splclock();
			rp->p_sig |= mask;
			splx(s);
			rp->p_cursig = 0;
look_for_signals:
			if (issig())
				goto restart;
			else
				return;
		}

		/*
		 * If parent wants us to take the signal,
		 * then it will leave it in p->p_cursig;
		 * otherwise we just look for signals again.
		 */
		if (rp->p_cursig == 0)
			goto look_for_signals;
		mask = sigmask(rp->p_cursig);

		/*
		 * If signal is being masked put it back
		 * into p_sig and look for other signals.
		 */
		if (rp->p_hold & mask) {
			s = splclock();
			rp->p_sig |= mask;
			splx(s);
			goto look_for_signals;
		}

	} 
	n = rp->p_cursig;
	if (n == 0)
		goto look_for_signals;
	if ((p = u.u_signal[n-1]) != SIG_DFL) {
		int 	returnmask;

		if ((p == SIG_IGN) || (rp->p_hold & mask)) {
			if ((u.u_sig_flag & BSD43_U_SF_SIGCONTEXT_SELECT)
                                        == BSD43_U_SF_SYSV_SIGCONTEXT)
				cmn_err(CE_WARN,"psig action (%d)",rp->p_cursig);
			rp->p_cursig = 0;
			return;
		};
		u.u_error = 0;

		/* if it is sigset, turn on p_hold bit */
		if(rp->p_chold & mask) {
			/* Mark that this is a sigset() by putting the
			   params to sigrelse() in it. This is what we
			   have to call sigrelse() with after the signal
			   has been serviced */
			is_sigset = n | SIGRELSE;
		} else {
			if(n != SIGILL && n != SIGTRAP && n != SIGPWR) {
				u.u_signal[n-1] = SIG_DFL;
				rp->p_sigignore &= ~mask;
				rp->p_sigcatch &= ~mask;
			};
			is_sigset = 0;
			mask = 0;
		}

		switch(u.u_sig_flag & BSD43_U_SF_SIGCONTEXT_SELECT) {
		case BSD43_U_SF_BSD43_SIGCONTEXT:
		case BSD43_U_SF_POSIX_SIGCONTEXT:
			if (u.u_sig_flag & BSD43_U_SF_OMASK) {
				returnmask = u.u_oldmask;
				u.u_sig_flag &= ~BSD43_U_SF_OMASK;
			} else 
				returnmask = rp->p_hold;
			rp->p_hold |= u.u_sigmask[n-1] | mask;
			break;

		default:
			rp->p_hold |= mask;
			break;
		};
			
		u.u_ru.ru_nsignals++;

		switch(u.u_sig_flag & BSD43_U_SF_SIGCONTEXT_SELECT) {
		case BSD43_U_SF_BSD43_SIGCONTEXT:
			sent_signal = 
				bsd_sendsig((int (*)()) p,n,returnmask);
			break;

		case BSD43_U_SF_POSIX_SIGCONTEXT:
			sent_signal = 
				posix_sendsig((void (*)())p, n,
					   returnmask, sig_type);
			break;

		default:
			sent_signal =
				sendsig(p, n, is_sigset, sig_type);
			break;
		};
		
		if (! sent_signal) {
		    /* Can't dispatch the user signal handler;
		     * force a core dump.
		     */
		    u.u_arg[0] = n; 
		    if (core())
			n += 0200;
		    exit(n);
		}
		rp->p_cursig = 0;
		return;
	}

	if (n == SIGWINCH) {
		/* default behaviour is to ignore this signal */
		rp->p_cursig = 0;
		return;
	};

	u.u_acflag |= AXSIG;
	switch (n) {
	case SIGQUIT:
	case SIGILL:
	case SIGTRAP:
	case SIGIOT:
	case SIGEMT:
	case SIGFPE:
	case SIGBUS:
	case SIGSEGV:
	case SIGSYS:
		u.u_arg[0] = n; /* save in core for debuggers */
		if (core())
			n += 0200;
	}
	exit(n);
}

/*
 * find the unhold signal in bit-position
 * representation in p_sig.
 */

fsig(p)
struct proc *p;
{
	register i;
	register n;
	register m;

	n = p->p_sig & ~p->p_hold;
	if ((p->p_flag & STRC) == 0)
	  	n &= ~(p->p_sigignore & ~sigmask(SIGCLD));
	if (p->p_flag&SVFORK)
		n &= ~stopsigmask;
	if (n == 0)
		return(0);

	m = n & stopsigmask;
	if (m) 
		for (i=1; i<NSIG; i++) {
			if (m & 1L)
				return(i);
			m >>= 1;
		}
	for (i=1; i<NSIG; i++) {
		if (n & 1L)
			return(i);
		n >>= 1;
	}
	return(0);
}

/*
 * Create a core image on the file "core"
 *
 * It writes USIZE block of the
 * user.h area followed by the entire
 * data+stack segments.
 */


core()
{
	struct vnode *vp;
	struct	 vattr	vattr;
	register preg_t	*prp;
	register proc_t *pp;
	register int	gap;
	extern int	userstack[];
	caddr_t stack_base;		/* Low address of user's stack. */
	int stack_size;			/* Size of user's stack in bytes. */
	int	fd;
	unsigned int	offset;		/* Stores position into file */

#ifdef MIPS_LOCAL
	printf("core: %s (%d) pc=0x%x vaddr=0x%x\n", 
		u.u_comm, u.u_procp->p_pid, 
		u.u_ar0[EF_EPC], u.u_ar0[EF_BADVADDR]);
#endif

	if (u.u_uid != u.u_ruid ||
	    u.u_gid != u.u_rgid)
		return(0);

	/* 
	 * unmap funky devices in the user address space 
	 */
  	for (fd = 0; fd < v.v_nofiles; fd++) 
		if (u.u_ofile[fd] &&
		    (u.u_pofile[fd] & UF_MAPPED)) {
			munmapfd(fd);
			u.u_pofile[fd] &= ~UF_MAPPED;
		};

	/*	Put the region sizes into the u-block for the
	 *	dump.
	 */
	pp = u.u_procp;
	if (prp = findpreg(pp, PT_TEXT))
		u.u_tsize = prp->p_reg->r_pgsz;
	else
		u.u_tsize = 0;
		
	/*	In the following, we do not want to write
	**	out the gap but just the actual data.  The
	**	caluclation mirrors that in loadreg and
	**	mapreg which allocates the gap and the
	**	actual space separately.  We have to watch
	**	out for the case where the entire data region
	**	was given away by a brk(0).
	*/

	if (prp = findpreg(pp, PT_DATA)) {
		u.u_dsize = prp->p_reg->r_pgsz;
	gap = btoct((caddr_t)u.u_exdata.ux_datorg
			- prp->p_regva);
	if (u.u_dsize > gap)
		u.u_dsize -= gap;
	else
		u.u_dsize = 0;
	} else {
		u.u_dsize = 0;
	}

	if (prp = findpreg(pp, PT_STACK)) {
		stack_base = prp->p_regva + 
			ctob(prp->p_reg->r_pgoff);
		stack_size = ((caddr_t)userstack - stack_base);
	} else {
		stack_base = 0;
		stack_size = 0;
	}

	/*	Check the sizes against the current ulimit and
	**	don't write a file bigger than ulimit.  If we
	**	can't write everything, we would prefer to
	**	write the stack and not the data rather than
	**	the other way around.
	*/

	if (USIZE + u.u_dsize + btoc(stack_size) > 
					(uint)dtop(u.u_limit)) {
		u.u_dsize = 0;
		if (USIZE + btoc(stack_size) > 
			   (uint)dtop(u.u_limit))
			stack_size = 0;
	}

	u.u_error = 0;
	if (USIZE + u.u_dsize + btoc(stack_size) >
			btoc(u.u_rlimit[BSD43_RLIMIT_CORE].rlim_cur)) {
		return(0);
	};
	
	u.u_error = 0;
	u.u_syscall = DUCOREDUMP;
	vattr_null(&vattr);
	vattr.va_type = VREG;
	vattr.va_mode = 0666;
	u.u_error =
	    vn_create("core", UIO_SYSSPACE, &vattr, NONEXCL, VWRITE, &vp);
	if (u.u_error)
		return (0);

	/* ??? SysV verified that we could WRITE the file, and that it was
	 * an IFREG (VREG) file.  It error-returned an EACCESS if it was
	 * not the case.  I think these are unnecessary given the
	 * semantics of the vn_create above.
	 */
#ifdef NOTDEF
	if (!FS_ACCESS(ip, IWRITE) && ip->i_ftype == IFREG)
		/* do io */
	else {
		printf("sig access error\n");
		u.u_error = EACCES;
	}
#endif
	u.u_error = vn_rdwr(UIO_WRITE, vp,
	    (caddr_t)&u,
	    ctob(USIZE),
	    (off_t)0, UIO_SYSSPACE, IO_UNIT, (int *)0);
	offset = ctob(USIZE);
	u.u_acflag |= ACORE;		/* SysV does this after first write */
	if (u.u_error == 0 && u.u_dsize) {
		u.u_error = vn_rdwr(UIO_WRITE, vp,
		    (caddr_t)u.u_exdata.ux_datorg,
		    (int)ctob(u.u_dsize) - poff(u.u_exdata.ux_datorg),
		    offset, UIO_USERSPACE, IO_UNIT, (int *)0);
		offset += ctob(u.u_dsize) - poff(u.u_exdata.ux_datorg);
		offset = ctob(btoc(offset));
	}
	if (u.u_error == 0 && stack_size)
		u.u_error = vn_rdwr(UIO_WRITE, vp,
		    stack_base,
		    stack_size,
		    offset, UIO_USERSPACE, IO_UNIT, (int *)0);


	VN_RELE(vp);
	return(u.u_error==0);
}

/*
 * sys-trace system call.
 */
ptrace()
{
	register struct proc *p;
	register struct a {
		int	req;
		int	pid;
		int	*addr;
		int	data;
	} *uap;

	uap = (struct a *)u.u_ap;
	if (uap->req <= 0) {
#ifdef TODO
		/*
		 * Return error if already being traced.
		 */
		if (u.u_procp->p_tptr)
			u.u_error = EPERM;
		else {
			u.u_procp->p_tptr = u.u_procp->p_parent;
#endif TODO
			u.u_procp->p_flag |= STRC;
#ifdef TODO
		}
#endif TODO
		return;
	}

#ifdef TODO
	if ((p = procfind(uap->pid)) == 0) {
		u.u_error = ESRCH;
		return;
	}
	if (uap->req == PTRC_TRACEON) {
		if (p->p_tptr || p == u.u_procp) {
			/*
			 * already being traced
			 * or trying to trace ourself
			 */
			u.u_error = EPERM;
			return;
		}
		/*
		 * Temporarily set ourselves as the tracing process
		 * so we can stop the process to be traced long enough
		 * to look at our request.  If he doesn't want to be
		 * traced by us, he will clear these and ignore the
		 * signal.
		 */
		p->p_tptr = u.u_procp;
		p->p_flag |= STRC;
		psignal(p, SIGTRAP);
		while (p->p_stat != SSTOP && p->p_stat != SZOMB)
			sleep((caddr_t)u.u_procp, PWAIT);
		if (p->p_stat == SZOMB) {
			/*
			 * The little bugger died before we could
			 * catch him!
			 */
			p->p_tptr = NULL;
			p->p_flag &= ~STRC;
			wakeup((caddr_t)p->p_pptr);
			u.u_error = ESRCH;
			return;
		}
	} else if ((p->p_flag & STRC) == 0 || p->p_tptr != u.u_procp
	    || p->p_stat != SSTOP) {
		u.u_error = EPERM;
		return;
	}
#else
	for (p = u.u_procp->p_child; p; p = p->p_sibling)
		if (p->p_stat == SSTOP
		 && p->p_pid == uap->pid)
			goto found;
	u.u_error = ESRCH;
	return;

    found:
#endif TODO

	while (ipc.ip_lock)
		sleep((caddr_t)&ipc, IPCPRI);
	ipc.ip_lock = p->p_pid;
	ipc.ip_tptr = u.u_procp;
	ipc.ip_uid = u.u_uid;
	ipc.ip_data = uap->data;
	ipc.ip_addr = uap->addr;
	ipc.ip_req = uap->req;
#ifdef TODO
	p->p_flag &= ~(SWTED|SWTED2);
#else
	p->p_flag &= ~SWTED;
#endif TODO
	while (ipc.ip_req > 0) {
		if (p->p_stat==SSTOP)
			setrun(p);
		sleep((caddr_t)&ipc, IPCPRI);
	}
	if (ipc.ip_req < 0) {
		u.u_error
		   = (uap->req == PTRC_TRACEON || uap->req == PTRC_TRACEOFF)
		   ? EPERM : EIO;
	} else
		u.u_rval1 = ipc.ip_data;
	ipc.ip_lock = 0;
	wakeup((caddr_t)&ipc);
}


#define	ADDR(x)	((char *)&(x))
#define	PCB(x)	(u.u_pcb.x)

struct regmap {
	char *rm_addr;
	unsigned rm_width;
	unsigned rm_wrprot;
} regmap[NSPEC_REGS] = {
	{ ADDR(USER_REG(EF_EPC)),	sizeof(USER_REG(EF_EPC)),	0 },
	{ ADDR(USER_REG(EF_CAUSE)),	sizeof(USER_REG(EF_CAUSE)),	1 },
	{ ADDR(USER_REG(EF_MDHI)),	sizeof(USER_REG(EF_MDHI)),	0 },
	{ ADDR(USER_REG(EF_MDLO)),	sizeof(USER_REG(EF_MDLO)),	0 },
	{ ADDR(PCB(pcb_fpc_csr)),	sizeof(PCB(pcb_fpc_csr)),	0 },
	{ ADDR(PCB(pcb_fpc_eir)),	sizeof(PCB(pcb_fpc_eir)),	1 },
	{ ADDR(u.u_trapcause),		sizeof(u.u_trapcause),		1 },
	{ ADDR(u.u_trapinfo),		sizeof(u.u_trapinfo),		1 }
};

/*
 * Code that the child process
 * executes to implement the command
 * of the parent process in tracing.
 *
 * Returning 0 implies stay in SSTOP state, returning non-zero 
 * means resume user mode execution.
 */
procxmt()
{
	register int i;
	register char *p;
	register int olddata;
	register unsigned regno;
	int width;
	int	s;

	if (ipc.ip_lock != u.u_procp->p_pid)
		return (0);

	u.u_procp->p_slptime = 0;
	i = ipc.ip_req;
	ipc.ip_req = 0;
#ifdef TODO
	if (ipc.ip_tptr != u.u_procp->p_tptr)
		goto error;
#endif TODO

	if (u.u_pcb.pcb_ssi.ssi_cnt)
		remove_bp();

	switch (i) {
	/* 
	 * read user I 
	 */
	case PTRC_RD_I:
		if ((int)ipc.ip_addr & (NBPW-1) ||
		    copyin((caddr_t)ipc.ip_addr, &ipc.ip_data, NBPW))
			goto error;
		break;

	/* 
	 * read user D 
	 */
	case PTRC_RD_D:
		if ((int)ipc.ip_addr & (NBPW-1) ||
		    copyin((caddr_t)ipc.ip_addr, &ipc.ip_data, NBPW))
			goto error;
		break;

	/* 
	 * read user registers and u area values
	 */
	case PTRC_RD_REG:
		i = (int)ipc.ip_addr;
		if ((unsigned)(regno = i - GPR_BASE) < NGP_REGS)
			ipc.ip_data = (regno==0) ? 0 : USER_REG(EF_AT+regno-1);
		else if ((unsigned)(regno = i - FPR_BASE) < NFP_REGS) {
			checkfp(u.u_procp, 0);
			ipc.ip_data = u.u_pcb.pcb_fpregs[regno];
		} else if ((unsigned)(regno = i - SIG_BASE) < NSIG_HNDLRS)
			ipc.ip_data = (int) u.u_signal[regno];
		else if ((unsigned)(regno = i - SPEC_BASE) < NSPEC_REGS) {
			if(i == FPC_CSR || i == FPC_EIR)
				checkfp(u.u_procp, 0);
			switch (regmap[regno].rm_width) {
			case sizeof(char):
				ipc.ip_data = *(char *)(regmap[regno].rm_addr);
				break;
			case sizeof(short):
				ipc.ip_data = *(short *)(regmap[regno].rm_addr);
				break;
			case sizeof(int):
				ipc.ip_data = *(int *)(regmap[regno].rm_addr);
				break;
			default:
				cmn_err(CE_PANIC, "ptrace regmap botch");
			}
		} else
			goto error;
		break;

	/* 
	 * write user I 
	 * Must set text up to allow writing
	 */
	case PTRC_WR_I:
		if (!write_utext(ipc.ip_addr, ipc.ip_data))
			goto error;
		if (IS_R6300) {
		    invalidate_virt_icache( ipc.ip_addr, sizeof(int) );
		} else {
		    vflush(ipc.ip_addr, sizeof(int));
		}
		break;

	/* 
	 * write user D 
	 */
	case PTRC_WR_D:
		if (suword((caddr_t)ipc.ip_addr, 0) < 0)
			goto error;
		(void) suword((caddr_t)ipc.ip_addr, ipc.ip_data);
		if (IS_R6300) {
		    invalidate_virt_icache( ipc.ip_addr, sizeof(int) );
		} else {
		    vflush(ipc.ip_addr, sizeof(int));
		}
		break;

	/* 
	 * write u 
	 */
	case PTRC_WR_REG:
		i = (int)ipc.ip_addr;
		width = sizeof(int);
		if ((unsigned)(regno = i - GPR_BASE) < NGP_REGS) {
			if (regno == 0)
				goto error;
			if (IS_M6000 && ((EF_AT+regno-1) == EF_IVECTMASK)) {
				/* can't allow any mod of SBC IVectMask */
				goto error;
			}
			p = (char *)&USER_REG(EF_AT+regno-1);
		} else if ((unsigned)(regno = i - FPR_BASE) < NFP_REGS) {
			checkfp(u.u_procp, 0);
			p = (char *)&u.u_pcb.pcb_fpregs[regno];
		} else if ((unsigned)(regno = i - SIG_BASE) < NSIG_HNDLRS)
			p = (char *)&u.u_signal[regno];
		else if ((unsigned)(regno = i - SPEC_BASE) < NSPEC_REGS) {
			if(i == FPC_CSR || i == FPC_EIR)
				checkfp(u.u_procp, 0);
			if (regmap[regno].rm_wrprot)
				goto error;
			/*
			 * complain if pc unaligned
			 */
			if (i == PC && (ipc.ip_data & sizeof(int)-1))
				goto error;
			p = (char *)(regmap[regno].rm_addr);
			width = regmap[regno].rm_width;
		} else
			goto error;
		switch (width) {
		case sizeof(char):
			/*
			 * User may have to and off sign extend
			 */
			olddata = *(char *)p;
			*(char *)p = ipc.ip_data;
			ipc.ip_data = olddata;
			break;
		case sizeof(short):
			olddata = *(short *)p;
			*(short *)p = ipc.ip_data;
			ipc.ip_data = olddata;
			break;
		case sizeof(int):
			olddata = *(int *)p;
			*(int *)p = ipc.ip_data;
			ipc.ip_data = olddata;
			break;
		default:
			cmn_err(CE_PANIC, "ptrace regmap botch");
		}
		break;

	/* 
	 * set signal and continue 
	 * one version causes a trace-trap
	 */
	case PTRC_CONTINUE:
	case PTRC_STEP:
		if ((int)ipc.ip_addr != 1) {
			/*
			 * complain if pc unaligned
			 */
			if ((int)ipc.ip_addr & (sizeof(int)-1))
				goto error;
			u.u_ar0[EF_EPC] = (int)ipc.ip_addr;
		}
		if ((unsigned)ipc.ip_data > NSIG)
			goto error;
#ifdef NOTDEF
		s = splhi();
		u.u_procp->p_sig = 0L;
		splx(s);
#endif NOTDEF
		u.u_procp->p_cursig = ipc.ip_data; /* see issig() */
		u.u_pcb.pcb_sstep = (i == PTRC_STEP);
		wakeup((caddr_t)&ipc);
		return (1);

#ifdef TODO
	/*
	 * turn on tracing 
	 */
	case PTRC_TRACEON:
		/*
		 * ptrace() has set-up our proc entry for tracing,
		 * if we don't mind being traced by the uid passed
		 * in ipc.ip_data, we're done; if we refuse to be
		 * traced by the uid passed, we clear the tracing
		 * set-up and continue on our way
		 */
		if (ipc.ip_uid != u.u_uid && ipc.ip_uid != 0) {
			u.u_procp->p_flag &= ~STRC;
			u.u_procp->p_tptr = NULL;
			u.u_procp->p_cursig = 0;
			ipc.ip_req = -1;
			wakeup((caddr_t)&ipc);
			return(1);
		}
		u.u_trapcause = CAUSETRACEON;
		break;

	/*
	 * turn off tracing 
	 */
	case PTRC_TRACEOFF:
		if ((unsigned)ipc.ip_data > NSIG)
			goto error;
		u.u_procp->p_flag &= ~STRC;
		u.u_procp->p_tptr = NULL;
		u.u_procp->p_cursig = ipc.ip_data;
		wakeup((caddr_t)&ipc);
		return(1);
#endif TODO

	/*
	 * read all regular registers
	 */
	case PTRC_RD_GPRS:
		(void) suword((caddr_t)(ipc.ip_addr), 0);
		for (i=1; i < NGP_REGS; i++) {
			(void) suword((caddr_t)(ipc.ip_addr+i), 
				u.u_ar0[i+EF_AT-1]);
		}
		break;

	/*
	 * read all floating point registers
	 * Note: this currently does not include the fpc_csr or fpc_eir and
	 * may want to be extended to include them when this is turned on.
	 */
	case PTRC_RD_FPRS:
		checkfp(u.u_procp, 0);
		for (i=0; i < NFP_REGS; i++) {
			(void) suword((caddr_t)(ipc.ip_addr+i), 
				u.u_pcb.pcb_fpregs[i]);
		}
		break;

	/*
	 * write all regular registers
	 */
	case PTRC_WR_GPRS:
		for (i=1; i < NGP_REGS; i++)
			u.u_ar0[i+EF_AT-1] = fuword((caddr_t)(ipc.ip_addr+i));
		break;

	/*
	 * write all floating point registers
	 * Note: this currently does not include the fpc_csr or fpc_eir and
	 * may want to be extended to include them when this is turned on.
	 */
	case PTRC_WR_FPRS:
		checkfp(u.u_procp, 0);
		for (i=0; i < NFP_REGS; i++) {
			u.u_pcb.pcb_fpregs[i] = 
					fuword((caddr_t)(ipc.ip_addr+i));
		}
		break;

#ifdef TODO
	/*
	 * specify a watchpoint in the data or stack segment 
	 */
	case PTRC_SET_WP:
		for (i=0; i < MAXWIDS; i++) {
			if (((1<<i) & u.u_wpmask) == 0) {
				u.u_wp[i].wp_addr = ipc.ip_addr;
				u.u_wp[i].wp_bcnt = ipc.ip_data;
				u.u_wpmask |= 1<<i;
				break;
			}
		}
		if (i == MAXWIDS)
			goto error;
		ipc.ip_data = i;
		break;

	/*
	 * delete a watchpoint
	 */
	case PTRC_DEL_WP:
		if ((i >= MAXWIDS) || (((1<<i) & u.u_wpmask) == 0))
			goto error;
		u.u_wpmask &= ~(1<<i);
		break;

	/*
	 * TODO:
	 * return an open file descriptor for process' a.out file 
	 */
	case PTRC_AOUTFD:
		goto error;
#endif TODO

	/* force exit */
	case PTRC_TERMINATE:
		wakeup((caddr_t)&ipc);
		exit(u.u_procp->p_cursig);

	default:
	error:
		ipc.ip_req = -1;
	}
	wakeup((caddr_t)&ipc);
	return (0);
}


/*
 * Find the proc struct associated with process id (pid). Return 0 if none.
 */
struct proc *
procfind(pid)
register int pid;
{
	register struct proc *p;

	for (p=proc; p < (struct proc *)v.ve_proc; p++) {
		if ((p->p_stat != NULL) && (p->p_pid == pid))
			return(p);
	}
	return((struct proc *)0);
}

/*
 *
 */
write_utext(addr, data)
caddr_t addr;
unsigned data;
{
	register preg_t *prp;
	register reg_t *rp;
	int rdonly = 0, i;
	preg_t *vtopreg(), *xdup();
	unsigned tmp;
	pde_t	*pte;

	if ((prp = vtopreg(u.u_procp, addr)) == NULL)
		return(0);
	rp = prp->p_reg;
	if (rp->r_type == RT_STEXT) {
		reglock(rp);
		if ((prp = xdup(u.u_procp, prp)) == NULL)
			return(0);
		rp = prp->p_reg;
		regrele(rp);
	}
	if (prp->p_flags & PF_RDONLY) {
		rdonly++;
		reglock(rp);
		/* Make the region writeable so we can take a copy on
		   write hit on the text write */
		prp->p_flags &= ~PF_RDONLY;
		regrele(rp);
	}
	/*
	 * Zapped address must be on a word boundary; no explicit
	 * check here because the hardware enforces it and suiword()
	 * will fail on a non-word address.
	 */
	i = suiword(addr, data);
	if (rdonly) {
		reglock(rp);
		prp->p_flags |= PF_RDONLY;
		/* Change the protection of the page we just wrote on to
		   read only */
		tmp = btotp((int)addr - (int)prp->p_regva);
		pte = (pde_t *)&(rp->r_list[tmp/NPGPT][tmp%NPGPT]);
		pg_clrmod(pte);
		regrele(rp);
	}
	if (i < 0)
		return(0);
	else
		return(1);
}
