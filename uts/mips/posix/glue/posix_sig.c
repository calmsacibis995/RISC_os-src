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
#ident	"$Header: posix_sig.c,v 1.5.1.6.1.2.1.4 90/11/05 17:47:55 beacker Exp $"

/*
 * Copyright (c) 1982, 1986 Regents of the University of California.
 * All rights reserved.  The Berkeley software License Agreement
 * specifies the terms and conditions for redistribution.
 *
 *	@(#)kern_sig.c	7.1 (Berkeley) 6/5/86
 */

#include "sys/types.h"
#include "sys/param.h"
#include "sys/errno.h"
#include "sys/pcb.h"
#include "sys/dir.h"
#include "posix/sys/signal.h"
#include "sys/user.h"
#include "sys/immu.h"
#include "sys/region.h"
#include "sys/sbd.h"
#include "sys/proc.h"
#include "sys/sema.h"
#include "sys/comm.h"
#include "sys/ipc.h"
#include "sys/msg.h"
#include "sys/inode.h"
#include "sys/stream.h"
#include "sys/termio.h"
#include "sys/stty_ld.h"
#include "sys/strstat.h"
#include "sys/file.h"
#include "sys/conf.h"
#include "sys/mount.h"
#include "sys/message.h"
#include "sys/var.h"
#include "sys/debug.h"
#include "sys/limits.h"


extern struct proc *bsd43_pfind();

/* sigmask defined in "sys/signal.h from sys/user.h" */

#define	posix_cantmask	(sigmask(SIGKILL)|sigmask(SIGSTOP))
#define	posix_stopmask	(sigmask(SIGSTOP)|sigmask(SIGTSTP)|sigmask(SIGTTIN)| \
			 sigmask(SIGTTOU))

void
utoksigset(sigset_t *uset, ksigset_t *kset)
{
	*kset = uset->sig_bits[0];
}

void
ktousigset(ksigset_t *kset, sigset_t *uset)
{
	uset->sig_bits[0] = *kset;
	uset->sig_bits[1] = 0;
	uset->sig_bits[2] = 0;
	uset->sig_bits[3] = 0;
}

/*
 * Generalized interface signal handler.
 */
sigaction()
{
	register struct a {
		int	signo;
		struct	sigaction *nact;
		struct	sigaction *oact;
#ifdef mips
		int	(*sigtramp)();
#endif mips
	} *uap = (struct a  *)u.u_ap;
	struct sigaction act;
	register int sig;
	int mask;

	sig = uap->signo;
	if (sig <= 0 || sig >= NSIG || 
		(uap->nact && (sig == SIGKILL || sig == SIGSTOP))) {
		u.u_error = EINVAL;
		return;
	}

	switch (u.u_sig_flag & BSD43_U_SF_SIGCONTEXT_SELECT) {
	case BSD43_U_SF_SIGCONTEXT_NOT_SET:
		u.u_sig_flag |= BSD43_U_SF_POSIX_SIGCONTEXT;
		break;
	case BSD43_U_SF_POSIX_SIGCONTEXT:
		break;
	default:
		u.u_error = EINVAL;
		return;
	};
	if (uap->oact) {
		act.sa_handler = (void (*)())u.u_signal[sig-1];
		ktousigset((ksigset_t *)&u.u_sigmask[sig-1], &act.sa_mask);
		act.sa_flags = 0;
		if (u.u_procp->p_flag & SNOCLDSTOP)
			act.sa_flags |= SA_NOCLDSTOP;
		u.u_error = bsd_copyout((caddr_t)&act, (caddr_t)uap->oact, 
					sizeof (act));
		if (u.u_error)
			return;
	}
	if (uap->nact) {
		u.u_error = bsd_copyin((caddr_t)uap->nact, (caddr_t)&act, 
					sizeof (act));
		if (u.u_error)
			return;
#ifdef mips
		/*
		 * check for unaligned pc on sighandler
		 */
		if (((void (*)()) act.sa_handler) != SIG_IGN
		    && ((int)act.sa_handler & (sizeof(int)-1))) {
			u.u_error = EINVAL;
			return;
		}
#endif mips
		setsigaction(sig, &act);
	}
#ifdef mips
	u.u_sigtramp = uap->sigtramp;
#endif mips
}

setsigaction(sig, sa)
	int sig;
	register struct sigaction *sa;
{
	register struct proc *p;
	int bit = sigmask(sig);

	p = u.u_procp;
	/*
	 * Change setting atomically.
	 */
	(void) splhigh();
	u.u_signal[sig-1] = (void (*)())sa->sa_handler;
	utoksigset(&sa->sa_mask, (ksigset_t *)&u.u_sigmask[sig-1]);
	u.u_sigmask[sig-1] &= ~posix_cantmask;
	/*
	 * do not restart system call on interrupt 
 	 */
	u.u_sigintr |= bit;
	/*
	 * can never be on user specified signal stack 
	 */
	u.u_sigonstack &= ~bit;
	if ((sa->sa_flags & SA_NOCLDSTOP) && (sig == SIGCLD)) {
		u.u_procp->p_flag |= SNOCLDSTOP;
	} else {
		u.u_procp->p_flag &= ~SNOCLDSTOP;
	}
	if (((void (*)()) sa->sa_handler) == SIG_IGN) {
		p->p_sig &= ~bit;	/* never to be seen again */
		p->p_chold &= ~bit;
		/* posix allows SIGCONT to be set to SIG_IGN, but 
		 * it requires that SIGCONT always continue a stopped
		 * process, so we don't really set it to be ignored.
		 * In this way, it gets treated as thought it were
		 * really set to SIG_DFL 	--Kris.
		 */
		if (sig != SIGCONT)
			p->p_sigignore |= bit;
		p->p_sigcatch &= ~bit;
	} else {
		if (((void (*)()) sa->sa_handler) == SIG_DFL) {
			p->p_chold &= ~bit;
			p->p_sigcatch &= ~bit;
			if (sig == SIGCLD) {
				/*
				 * SIG_DFL for SIGCLD is SIG_IGN, so ignore it
				 */
				p->p_sig &= ~bit;  /* never to be seen again */
				p->p_sigignore |= bit;
			} else {
				p->p_sigignore &= ~bit;
			}
		} else {
			p->p_chold |= bit;
			p->p_sigcatch |= bit;
			p->p_sigignore &= ~bit;
		};
	}
	(void) spl0();
}


sigprocmask()
{
	struct a {
		int		how;
		sigset_t	*mask;
		sigset_t	*omask;
	} *uap = (struct a *)u.u_ap;
	register struct proc *p = u.u_procp;
	ksigset_t	mask;

	switch (u.u_sig_flag & BSD43_U_SF_SIGCONTEXT_SELECT) {
	case BSD43_U_SF_SIGCONTEXT_NOT_SET:
		u.u_sig_flag |= BSD43_U_SF_POSIX_SIGCONTEXT;
		break;
	case BSD43_U_SF_POSIX_SIGCONTEXT:
		break;
	default:
		u.u_error = EINVAL;
		return;
	};
	
	if (uap->omask) {
		u.u_error = bsd_copyout((caddr_t)&p->p_hold, 
					(caddr_t)uap->omask, sizeof (int));
		if (u.u_error)
			return;
	}

	if (uap->mask) {
		u.u_error = bsd_copyin((caddr_t)uap->mask, (caddr_t)&mask,
					sizeof (ksigset_t));
		if (u.u_error)
			return;
	} else {
		/* no new mask, so just return */
		u.u_rval1 = 0;
		return;
	}
	
	mask &= ~posix_cantmask;
	switch (uap->how) {
	case SIG_BLOCK:
		(void) splhigh();
		p->p_hold |= mask;
		u.u_rval1 = 0;
		(void) spl0();
		return;
	case SIG_UNBLOCK:
		(void) splhigh();
		p->p_hold &= ~mask;
		u.u_rval1 = 0;
		(void) spl0();
		return;
	case SIG_SETMASK:
		(void) splhigh();
		p->p_hold = mask;
		u.u_rval1 = 0;
		(void) spl0();
		return;
	default:
		u.u_error = EINVAL;
		return;
	}
}

sigpending()
{
	struct a {
		sigset_t	*mask;
	} *uap = (struct a *)u.u_ap;
	register struct proc *p = u.u_procp;

	switch (u.u_sig_flag & BSD43_U_SF_SIGCONTEXT_SELECT) {
	case BSD43_U_SF_SIGCONTEXT_NOT_SET:
		u.u_sig_flag |= BSD43_U_SF_POSIX_SIGCONTEXT;
		break;
	case BSD43_U_SF_POSIX_SIGCONTEXT:
		break;
	default:
		u.u_error = EINVAL;
		return;
	};
	
	u.u_error = bsd_copyout((caddr_t)&p->p_sig, (caddr_t)uap->mask, 
				sizeof (ksigset_t));
	if (u.u_error)
		return;
	u.u_rval1 = 0;
}

sigsuspend()
{
	struct a {
		sigset_t	*mask;
	} *uap = (struct a *)u.u_ap;
	register struct proc *p = u.u_procp;
	ksigset_t mask;

	switch (u.u_sig_flag & BSD43_U_SF_SIGCONTEXT_SELECT) {
	case BSD43_U_SF_SIGCONTEXT_NOT_SET:
		u.u_sig_flag |= BSD43_U_SF_POSIX_SIGCONTEXT;
		break;
	case BSD43_U_SF_POSIX_SIGCONTEXT:
		break;
	default:
		u.u_error = EINVAL;
		return;
	};

	u.u_error = bsd_copyin((caddr_t)uap->mask, (caddr_t)&mask, 
				sizeof (ksigset_t));
	if (u.u_error)
		return;
	/*
	 * When returning from sigpause, we want
	 * the old mask to be restored after the
	 * signal handler has finished.  Thus, we
	 * save it here and mark the proc structure
	 * to indicate this (should be in u.).
	 */
	u.u_oldmask = p->p_hold;
	u.u_sig_flag |= BSD43_U_SF_OMASK;
	p->p_hold = mask & ~posix_cantmask;
	pause();
	/*NOTREACHED*/
}

int 
sigemptyset(sigset_t *set)
{
	int	i;
	
	for (i = 0; i < _SIGSETS; set->sig_bits[i++] = 0);
}

int 
sigfillset(sigset_t *set)
{
	int 	i;

	for (i = 0; i < _SIGSETS; set->sig_bits[i++] = UINT_MAX);
}

int 
sigaddset(ksigset_t *set, int signo)
{
	*set |= (1 << (signo-1));
}

int 
sigdelset(ksigset_t *set, int signo)
{
	*set &= ~(1 << (signo-1));
}

int 
sigismember(ksigset_t *set, int signo)
{
	return((*set & (1<<(signo-1))) != 0);
}

int 
sigsetand(ksigset_t *set1, ksigset_t *set2)
{

	*set1 &= *set2;
}

int 
sigsetor(ksigset_t *set1, ksigset_t *set2)
{

	*set1 |= *set2;
}

posix_kill()
{
	register struct a {
		int	pid;
		int	signo;
	} *uap;
	register struct proc *p, *q;
	register arg;
	int f;

	uap = (struct a *)u.u_ap;
	if (uap->signo < 0 || uap->signo >= NSIG) {
		u.u_error = EINVAL;
		return;
	}
	f = 0;
	arg = uap->pid;
	if (arg > 0)
		p = &proc[1];
	else
		p = &proc[2];
	q = u.u_procp;
	if (arg == 0 && q->p_pgrp == 0 && q->p_jcpgrp == 0) {
		u.u_error = ESRCH;
		return;
	}
	for (; p < (struct proc *)v.ve_proc; p++) {
		if (p->p_stat == NULL || p->p_stat == SZOMB)
			continue;
		if (arg > 0 && p->p_pid != arg)
			continue;
		if (arg == 0 && p->p_jcpgrp != q->p_jcpgrp)
			continue;
		if (arg < -1 && p->p_jcpgrp != -arg)
			continue;
		if (uap->signo == SIGCONT && p->p_pgrp == u.u_procp->p_pgrp) {
			/* skip uid tests */
		} else {
		   	 if ((!(u.u_uid == 0 ||
			        u.u_uid == p->p_uid ||
			        u.u_ruid == p->p_uid ||
			        u.u_uid == p->p_suid ||
			        u.u_ruid == p->p_suid)) ||
			     ((p == &proc[1]) && (uap->signo == SIGKILL))) {
		                if (arg > 0) {
			                u.u_error = EPERM;
				        return;
		                } else
			     	   	continue;
			}
		}
		f++;
		if (uap->signo)
			psignal(p, uap->signo);
		if (arg > 0)
			break;
	}
	if (f == 0)
		u.u_error = ESRCH;
}
