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
#ident	"$Header: kern_sig.c,v 1.4.1.5 90/05/10 04:40:32 wje Exp $"

/*
 * Copyright (c) 1982, 1986 Regents of the University of California.
 * All rights reserved.  The Berkeley software License Agreement
 * specifies the terms and conditions for redistribution.
 *
 *	@(#)kern_sig.c	7.1 (Berkeley) 6/5/86
 */

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

extern struct proc *bsd43_pfind();


#define	cantmask	(bsd43_sigmask(BSD43_SIGKILL)| \
			 bsd43_sigmask(BSD43_SIGCONT)| \
			 bsd43_sigmask(BSD43_SIGSTOP))
#define	stopsigmask	(bsd43_sigmask(BSD43_SIGSTOP)| \
			 bsd43_sigmask(BSD43_SIGTSTP)| \
			 bsd43_sigmask(BSD43_SIGTTIN)| \
			 bsd43_sigmask(BSD43_SIGTTOU))

/*
 * Generalized interface signal handler.
 */
sigvec()
{
	register struct a {
		int	signo;
		struct	bsd43_sigvec *nsv;
		struct	bsd43_sigvec *osv;
#ifdef mips
		int	(*sigtramp)();
#endif mips
	} *uap = (struct a  *)u.u_ap;
	struct bsd43_sigvec vec;
	register struct bsd43_sigvec *sv;
	register int sig;
	int bit;

	sig = uap->signo;
	if (sig <= 0 ||
	    sig >= NSIG ||
	    sig == BSD43_SIGKILL ||
	    sig == BSD43_SIGSTOP) {
		u.u_error = EINVAL;
		return;
	}

	switch (u.u_sig_flag & BSD43_U_SF_SIGCONTEXT_SELECT) {
	case BSD43_U_SF_SIGCONTEXT_NOT_SET:
		u.u_sig_flag |= BSD43_U_SF_BSD43_SIGCONTEXT;
		u.u_procp->p_sig_flag |= BSD43_U_SF_BSD43_SIGCONTEXT;
	case BSD43_U_SF_BSD43_SIGCONTEXT:
		break;
	default:
		u.u_error = EINVAL;
		return;
	};
	sv = &vec;
	if (uap->osv) {
		sv->sv_handler = (int (*)())u.u_signal[sig-1];
		sv->sv_mask = u.u_sigmask[sig-1];
		bit = bsd43_sigmask(sig);
		sv->sv_flags = 0;
		if ((u.u_sigonstack & bit) != 0)
			sv->sv_flags |= BSD43_SV_ONSTACK;
		if ((u.u_sigintr & bit) != 0)
			sv->sv_flags |= BSD43_SV_INTERRUPT;
		u.u_error =
		    bsd_copyout((caddr_t)sv, (caddr_t)uap->osv, sizeof (vec));
		if (u.u_error)
			return;
	}
	if (uap->nsv) {
		u.u_error =
		    bsd_copyin((caddr_t)uap->nsv, (caddr_t)sv, sizeof (vec));
		if (u.u_error)
			return;
		if (sig == BSD43_SIGCONT && 
		    ((void (*)())sv->sv_handler == SIG_IGN)) {
			u.u_error = EINVAL;
			return;
		}
#ifdef mips
		/*
		 * check for unaligned pc on sighandler
		 */
		if (((void (*)()) sv->sv_handler) != SIG_IGN
		    && ((int)sv->sv_handler & (sizeof(int)-1))) {
			u.u_error = EINVAL;
			return;
		}
#endif mips
		setsigvec(sig, sv);
	}
#ifdef mips
	u.u_sigtramp = uap->sigtramp;
#endif mips
}

setsigvec(sig, sv)
	int sig;
	register struct bsd43_sigvec *sv;
{
	register struct proc *p;
	register int bit;

	bit = bsd43_sigmask(sig);
	p = u.u_procp;
	/*
	 * Change setting atomically.
	 */
	(void) splhigh();
	u.u_signal[sig-1] = (void (*)())sv->sv_handler;
	u.u_sigmask[sig-1] = sv->sv_mask &~ cantmask;
	if (sv->sv_flags & BSD43_SV_INTERRUPT)
		u.u_sigintr |= bit;
	else
		u.u_sigintr &= ~bit;
	if (sv->sv_flags & BSD43_SV_ONSTACK)
		u.u_sigonstack |= bit;
	else
		u.u_sigonstack &= ~bit;
	if (((void (*)()) sv->sv_handler) == SIG_IGN) {
		p->p_sig &= ~bit;		/* never to be seen again */
		p->p_chold &= ~bit;
		p->p_sigignore |= bit;
		p->p_sigcatch &= ~bit;
	} else {
		p->p_sigignore &= ~bit;
		if (((void (*)()) sv->sv_handler) == SIG_DFL) {
			p->p_chold &= ~bit;
			p->p_sigcatch &= ~bit;
		} else {
			p->p_chold |= bit;
			p->p_sigcatch |= bit;
		};
	}
	(void) spl0();
}


sigblock()
{
	struct a {
		int	mask;
	} *uap = (struct a *)u.u_ap;
	register struct proc *p = u.u_procp;

	switch (u.u_sig_flag & BSD43_U_SF_SIGCONTEXT_SELECT) {
	case BSD43_U_SF_SIGCONTEXT_NOT_SET:
		u.u_sig_flag |= BSD43_U_SF_BSD43_SIGCONTEXT;
		p->p_sig_flag |= BSD43_U_SF_BSD43_SIGCONTEXT;
	case BSD43_U_SF_BSD43_SIGCONTEXT:
		break;
	default:
		u.u_error = EINVAL;
		return;
	};

	(void) splhigh();
	u.u_rval1 = p->p_hold;
	p->p_hold |= uap->mask &~ cantmask;
	(void) spl0();
}

sigsetmask()
{
	struct a {
		int	mask;
	} *uap = (struct a *)u.u_ap;
	register struct proc *p = u.u_procp;

	switch (u.u_sig_flag & BSD43_U_SF_SIGCONTEXT_SELECT) {
	case BSD43_U_SF_SIGCONTEXT_NOT_SET:
		u.u_sig_flag |= BSD43_U_SF_BSD43_SIGCONTEXT;
		p->p_sig_flag |= BSD43_U_SF_BSD43_SIGCONTEXT;
	case BSD43_U_SF_BSD43_SIGCONTEXT:
		break;
	default:
		u.u_error = EINVAL;
		return;
	};

	(void) splhigh();
	u.u_rval1 = p->p_hold;
	p->p_hold = uap->mask &~ cantmask;
	(void) spl0();
}

sigpause()
{
	struct a {
		int	mask;
	} *uap = (struct a *)u.u_ap;
	register struct proc *p = u.u_procp;

	switch (u.u_sig_flag & BSD43_U_SF_SIGCONTEXT_SELECT) {
	case BSD43_U_SF_SIGCONTEXT_NOT_SET:
		u.u_sig_flag |= BSD43_U_SF_BSD43_SIGCONTEXT;
		p->p_sig_flag |= BSD43_U_SF_BSD43_SIGCONTEXT;
	case BSD43_U_SF_BSD43_SIGCONTEXT:
		break;
	default:
		u.u_error = EINVAL;
		return;
	};

	/*
	 * When returning from sigpause, we want
	 * the old mask to be restored after the
	 * signal handler has finished.  Thus, we
	 * save it here and mark the proc structure
	 * to indicate this (should be in u.).
	 */
	u.u_oldmask = p->p_hold;
	u.u_sig_flag |= BSD43_U_SF_OMASK;
	p->p_hold = uap->mask &~ cantmask;
	pause();
	/*NOTREACHED*/
}
#undef cantmask

sigstack()
{
	register struct a {
		struct	bsd43_sigstack *nss;
		struct	bsd43_sigstack *oss;
	} *uap = (struct a *)u.u_ap;
	struct bsd43_sigstack ss;

	switch (u.u_sig_flag & BSD43_U_SF_SIGCONTEXT_SELECT) {
	case BSD43_U_SF_SIGCONTEXT_NOT_SET:
		u.u_sig_flag |= BSD43_U_SF_BSD43_SIGCONTEXT;
		u.u_procp->p_sig_flag |= BSD43_U_SF_BSD43_SIGCONTEXT;
	case BSD43_U_SF_BSD43_SIGCONTEXT:
		break;
	default:
		u.u_error = EINVAL;
		return;
	};

	if (uap->oss) {
		u.u_error = bsd_copyout((caddr_t)&u.u_sigstack, (caddr_t)uap->oss, 
		    sizeof (struct bsd43_sigstack));
		if (u.u_error)
			return;
	}
	if (uap->nss) {
		u.u_error =
		    bsd_copyin((caddr_t)uap->nss, (caddr_t)&ss, sizeof (ss));
		if (u.u_error == 0)
			u.u_sigstack = ss;
	}
}

bsd_kill()
{
	register struct a {
		int	pid;
		int	signo;
	} *uap = (struct a *)u.u_ap;
	register struct proc *p;

	if (uap->signo < 0 || uap->signo > NSIG) {
		u.u_error = EINVAL;
		return;
	}
	if (uap->pid > 0) {
		/* kill single process */
		p = bsd43_pfind(uap->pid);
		if (p == 0) {
			u.u_error = ESRCH;
			return;
		}
		if (u.u_uid && u.u_uid != p->p_uid)
			u.u_error = EPERM;
		else if (uap->signo)
			psignal(p, uap->signo);
		return;
	}
	switch (uap->pid) {
	case -1:		/* broadcast signal */
		u.u_error = killpg1(uap->signo, 0, 1);
		break;
	case 0:			/* signal own process group */
		u.u_error = killpg1(uap->signo, 0, 0);
		break;
	default:		/* negative explicit process group */
		u.u_error = killpg1(uap->signo, -uap->pid, 0);
		break;
	}
	return;
}

killpg()
{
	register struct a {
		int	pgrp;
		int	signo;
	} *uap = (struct a *)u.u_ap;

	if (uap->signo < 0 || uap->signo > NSIG) {
		u.u_error = EINVAL;
		return;
	}
	u.u_error = killpg1(uap->signo, uap->pgrp, 0);
}

/* KILL CODE SHOULDNT KNOW ABOUT PROCESS INTERNALS !?! */

killpg1(signo, pgrp, all)
	int signo, pgrp, all;
{
	register struct proc *p;
	int f, error = 0;

	if (!all && pgrp == 0) {
		/*
		 * Zero process id means send to my process group.
		 */
		pgrp = u.u_procp->p_jcpgrp;
		if (pgrp == 0)
			return (ESRCH);
	}
	for (f = 0, p = proc; p < (struct proc *) v.ve_proc; p++) {
		if (p->p_stat == SZOMB)
			continue;
		if (((p->p_jcpgrp != pgrp) && 
		         !all) || 
		    (p->p_ppid == 0) ||
		    (p->p_flag&SSYS) || 
		    (all && p == u.u_procp))
			continue;
		if (u.u_uid != 0 && u.u_uid != p->p_uid &&
		    (signo != BSD43_SIGCONT || !inferior(p))) {
			if (!all)
				error = EPERM;
			continue;
		}
		f++;
		if (signo)
			psignal(p, signo);
	}
	return (error ? error : (f == 0 ? ESRCH : 0));
}

