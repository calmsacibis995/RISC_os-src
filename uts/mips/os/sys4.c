/*
 * |-----------------------------------------------------------|
 * | Copyright (c) 1990 MIPS Computer Systems, Inc.            |
 * | All Rights Reserved                                       |
 * |-----------------------------------------------------------|
 * |          Restrictive Rights Legend                        |
 * | Use, duplication, or disclosure by the Government is      |
 * | subject to restrictions as set forth in                   |
 * | subparagraph (c)(1)(ii) of the Rights in Technical        |
 * | Data and Computer Software Clause of DFARS 52.227-7013.   |
 * |         MIPS Computer Systems, Inc.                       |
 * |         928 Arques Avenue                                 |
 * |         Sunnyvale, CA 94086                               |
 * |-----------------------------------------------------------|
 */
#ident	"$Header: sys4.c,v 1.27.1.7.1.1.1.3 91/01/09 19:22:14 beacker Exp $"

/*	Copyright (c) 1984 AT&T	*/
/*	  All Rights Reserved  	*/

/*	THIS IS UNPUBLISHED PROPRIETARY SOURCE CODE OF AT&T	*/
/*	The copyright notice above does not evidence any   	*/
/*	actual or intended publication of such source code.	*/

#include "sys/param.h"
#include "sys/types.h"
#include "sys/numips.h"
#include "sys/sysmacros.h"
#include "bsd/sys/time.h"
#include "sys/systm.h"
#include "sys/tuneable.h"
#include "sys/signal.h"
#include "sys/sbd.h"
#include "sys/immu.h"
#include "sys/psw.h"
#include "sys/pcb.h"
#include "sys/user.h"
#include "sys/errno.h"
#include "sys/file.h"
#include "sys/vnode.h"
#include "sys/region.h"
#include "sys/proc.h"
#include "sys/var.h"
#include "sys/clock.h"
#include "sys/conf.h"

/*
 * Everything in this file is a routine implementing a system call.
 */


gtime()
{
	u.u_rtime = time.tv_sec;
}

/*
 * Set the time of day.
 *
 * XXX: WHAT DO WE DO ABOUT PENDING REAL-TIME TIMEOUTS????
 *	-- adjust their times!!! [wje]
 * (Same problem in bsd_settimeofday().)
 */
stime()
{
	int s;
	register struct a {
		time_t	time;
	} *uap;
	extern 	timedelta;
	extern resettodr();

	uap = (struct a *)u.u_ap;
	if (suser()) {
		s = splhi();
		boottime.tv_sec += (uap->time - time.tv_sec);
		time.tv_sec = uap->time;
		time.tv_usec = 0;
		timedelta = 0;
		splx(s);
		resettodr();
		rem_date();
	}
}

setuid()
{
	register unsigned uid;
	register struct a {
		int	uid;
	} *uap;

	uap = (struct a *)u.u_ap;
	uid = uap->uid;
	if (uid >= MAXUID) {
		u.u_error = EINVAL;
		return;
	}
	if (u.u_uid  && (uid == u.u_ruid || uid == u.u_procp->p_suid)) {
		u.u_cred = crcopy(u.u_cred);
		u.u_uid = uid;
	} else if (suser()) {
		u.u_cred = crcopy(u.u_cred);
		u.u_uid = uid;
		u.u_procp->p_uid = uid;
		u.u_procp->p_suid = uid;
		u.u_ruid = uid;
	}
}

getuid()
{

	u.u_rval1 = (int) u.u_ruid;
	u.u_rval2 = (int) u.u_uid;
}

setgid()
{
	register unsigned gid;
	register struct a {
		int	gid;
	} *uap;

	uap = (struct a *)u.u_ap;
	gid = uap->gid;
	if (gid >= MAXUID) {
		u.u_error = EINVAL;
		return;
	}

	if (u.u_uid && (gid == u.u_rgid || gid == u.u_procp->p_sgid)) {
		u.u_cred = crcopy(u.u_cred);
		u.u_gid = gid;
	} else if (suser()) {
		u.u_cred = crcopy(u.u_cred);
		u.u_gid = gid;
		u.u_procp->p_sgid = gid;
		if (u.u_rgid != gid) {
			leavegroup(u.u_rgid);
			(void) entergroup((ushort)gid);
			u.u_rgid = gid;
		}
	}
}

getgid()
{

	u.u_rval1 = (int) u.u_rgid;
	u.u_rval2 = (int) u.u_gid;
}

getpid()
{
	u.u_rval1 = u.u_procp->p_pid;
	u.u_rval2 = u.u_procp->p_ppid;
}

setpgrp()
{
	register struct proc *p = u.u_procp;
	register struct a {
		int	flag;
	} *uap;
	struct	vnode *vp;

	uap = (struct a *)u.u_ap;
	if (uap->flag) {
		if (p->p_pgrp != p->p_pid) {
			u.u_ttyd = 0;		  	
			u.u_ttyp = NULL;
			u.u_ttyvp = NULL;
			clr_controlling_tty();
		}
		p->p_pgrp = p->p_pid;
		p->p_jcpgrp = p->p_pgrp;
	}
	u.u_rval1 = p->p_jcpgrp;
}


extern struct proc *bsd43_pfind();

setpgid()
{
	register struct proc *p, *q;
	register struct a {
	  	int	pid;
		int	pgid;
	} *uap = (struct a *)u.u_ap;

	if (uap->pid < 0 || uap->pgid < 0) {
		u.u_error = EINVAL;
		return;
	}

	if (uap->pid == 0) {
		uap->pid = u.u_procp->p_pid;
		p = u.u_procp;
	} else {
		p = bsd43_pfind(uap->pid);

		/* if pid not equal to the current process or one of it's 
		 * children, return ESRCH
		 */
		if (p == NULL || (p != u.u_procp && p->p_parent != u.u_procp)) {
			u.u_error = ESRCH;
			return;
		}
	}

	if (POSIX_SYSCALL 
		    && (p->p_parent == u.u_procp) && (p->p_flag & SEXECED)) {
		u.u_error = EACCES;
		return;
	}


	/* A session leader may not change its job control process group */
	if (p->p_pid == p->p_pgrp) {
		u.u_error = EPERM;
		return;
	};

	/* A process may not change the job control process group id of a child
	 * in another session 
	 */
	if (p != u.u_procp && p->p_pgrp != u.u_procp->p_pgrp) {
		u.u_error = EPERM;
		return;
	};

	/* if the pgid argument is null, use the process id of the 
	 * current process as the new job control process group id
	 */
	if (uap->pgid == 0)
		uap->pgid = uap->pid;

	/* A process may not use a pid as a job control process 
 	 * group id if there is no such process in the caller's session 
	 */
	q = bsd43_pfind(uap->pgid);
	if (q == NULL || q->p_pgrp != u.u_procp->p_pgrp) {
		u.u_error = EPERM;
		return;
	};

	if (p->p_pgrp == 0) {
		if (p->p_pid != uap->pgid) {
			u.u_error = EPERM;
			return;
		};
		p->p_pgrp = uap->pgid;
	};
	p->p_jcpgrp = uap->pgid;
}


nice()
{
	register n;
	register struct a {
		int	niceness;
	} *uap;
	int chgpri();

	uap = (struct a *)u.u_ap;
	n = uap->niceness;
	u.u_procp->p_nice = chgpri(u.u_procp->p_nice, n);
	u.u_rval1 = u.u_procp->p_nice - NZERO;
}


/* 
 * ssig() is the common entry for signal, sigset, sighold, sigrelse,
 * sigignore and sigpause 
 */
ssig()
{
	register a;
	register struct proc *p;
	struct a {
		int	signo;
		void	(*fun)();
		int	(*sigtramp)();	/* user's signal trampoline code */
	} *uap;
	int mask;

	uap = (struct a *)u.u_ap;
	a = uap->signo & SIGNO_MASK;
	if (a <= 0 || a >= NSIG || a == SIGKILL) {
		u.u_error = EINVAL;
		return;
	}

	p = u.u_procp;
	switch (u.u_sig_flag & BSD43_U_SF_SIGCONTEXT_SELECT) {
		case BSD43_U_SF_SIGCONTEXT_NOT_SET:
			u.u_sig_flag |= BSD43_U_SF_SYSV_SIGCONTEXT;
			/* fall through */
		case BSD43_U_SF_SYSV_SIGCONTEXT:
			u.u_sigintr = 0xFFFFFFFF; /* all signals interrupt */
			break;
		default:
			u.u_error = EINVAL;
			return;
	};

	mask = sigmask(a);
	switch (uap->signo & ~SIGNO_MASK) {

	case SIGHOLD:	/* sighold */
		p->p_hold |= mask;
		u.u_rval1 = 0;
		return;

	case SIGRELSE:	/* sigrelse */
		p->p_hold &= ~mask;
		u.u_rval1 = 0;
		return;

	case SIGIGNORE:	/* signore */
		u.u_signal[a-1] = SIG_IGN;
		p->p_sig &= ~mask;
		p->p_hold &= ~mask;
		p->p_sigignore |= mask;
		p->p_sigcatch &= ~mask;
		u.u_rval1 = 0;
		return;

	case SIGPAUSE:	/* sigpause */
		p->p_hold &= ~mask;
		u.u_rval1 = 0;
		pause();
		/* not reached */

	case SIGDEFER:		/* sigset */
		if (uap->fun != SIG_DFL && uap->fun != SIG_IGN
			&& uap->fun != SIG_HOLD) 
			p->p_chold |= mask;

		if (uap->fun == SIG_IGN) {
			p->p_sigignore |= mask;
			p->p_sigcatch &= ~mask;
		} else {
			p->p_sigignore &= ~mask;
			if (uap->fun == SIG_DFL)
				p->p_sigcatch &= ~mask;
			else
				p->p_sigcatch |= mask;
		};

		if (p->p_hold & mask)
			u.u_rval1 = (int) SIG_HOLD;
		else
			u.u_rval1 = (int) u.u_signal[a-1];
		u.u_sigtramp = uap->sigtramp;
		if (uap->fun == SIG_HOLD) {
			p->p_hold |= mask;
		} else {
			u.u_signal[a-1] = uap->fun;
			p->p_hold &= ~mask;
		}
		break;

	case 0:	/* signal */
		p->p_chold &= ~mask;
		u.u_rval1 = (int) u.u_signal[a-1];
		u.u_sigtramp = uap->sigtramp;
		u.u_signal[a-1] = uap->fun;
		/* p_sig only cleared in signal, not in sigset */
		p->p_sig &= ~mask;

		if (uap->fun == SIG_IGN) {
			p->p_sigignore |= mask;
			p->p_sigcatch &= ~mask;
		} else {
			p->p_sigignore &= ~mask;
			if (uap->fun == SIG_DFL)
				p->p_sigcatch &= ~mask;
			else
				p->p_sigcatch |= mask;
		};

		break;

	default:		/* error */
		u.u_error = EINVAL;
		return;
	}
	
	if (a == SIGCLD) {
		for (p = p->p_child ; p ; p = p->p_sibling) {
			if (p->p_stat == SZOMB)
				psignal(u.u_procp, SIGCLD);
		}
	}
}

kill()
{
	register struct proc *p, *q;
	register arg;
	register struct a {
		int	pid;
		int	signo;
	} *uap;
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
		if (arg == 0 && p->p_pgrp != q->p_pgrp &&
		    p->p_jcpgrp != q->p_jcpgrp)
			continue;
		if (arg < -1 && p->p_pgrp != -arg &&
		    p->p_jcpgrp != -arg)
			continue;
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
		f++;
		if (uap->signo)
			psignal(p, uap->signo);
		if (arg > 0)
			break;
	}
	if (f == 0)
		u.u_error = ESRCH;
}

times()
{
	register struct a {
		time_t	(*times)[4];
	} *uap;
	int ospl;

	uap = (struct a *)u.u_ap;
	if (copyout((caddr_t)&u.u_utime, (caddr_t)uap->times, sizeof(*uap->times)))
		u.u_error = EFAULT;
	ospl = splall();
	u.u_rtime = lbolt;
	splx(ospl);
}

/*
 * Profiling
 */

profil()
{
	register struct a {
		short	*bufbase;
		unsigned bufsize;
		unsigned pcoffset;
		unsigned pcscale;
	} *uap;

	uap = (struct a *)u.u_ap;
	u.u_prof.pr_base = uap->bufbase;
	u.u_prof.pr_size = uap->bufsize;
	u.u_prof.pr_off = uap->pcoffset;
	if (uap->pcscale == 1)
		u.u_prof.pr_scale = 0;
	else
		u.u_prof.pr_scale = uap->pcscale;
}

/*
 * alarm clock signal - implemented using BSD style timer stuff.
 */
alarm()
{
	register struct a {
		int	deltat;
	} *uap = (struct a *)u.u_ap;
	register struct proc *p = u.u_procp;
	int s = splhigh();
	extern int realitexpire();

	untimeout_func(realitexpire, (caddr_t)p);
	timerclear(&p->p_realtimer.it_interval);
	u.u_rval1 = 0;
	if (timerisset(&p->p_realtimer.it_value) 
	&& timercmp(&p->p_realtimer.it_value, &time, >)) {
		u.u_rval1 = p->p_realtimer.it_value.tv_sec - time.tv_sec;
		if (u.u_rval1 == 0)
			/* Timer is less than one second away. */
			u.u_rval1 = 1;
	}
	if (uap->deltat == 0) {
		timerclear(&p->p_realtimer.it_value);
		splx(s);
		return;
	}
	p->p_realtimer.it_value = time;
	p->p_realtimer.it_value.tv_sec += uap->deltat;
	timeout(realitexpire, (caddr_t)p, hzto(&p->p_realtimer.it_value));
	splx(s);
}

/*
 * indefinite wait.
 * no one should wakeup(&u)
 */
pause()
{

	for (;;)
		sleep((caddr_t)&u, PSLEP);
}


/*
 * Set IUPD and IACC times on file.
 */
utime()
{
	register struct a {
		char	*fname;
		time_t	*tptr;
	} *uap;
	struct vattr vattr;
	time_t tv[2];

	uap = (struct a *)u.u_ap;
	if (uap->tptr != NULL) {
		if (server())	/* server already copied time in */
			bcopy((caddr_t)uap->tptr, (caddr_t)tv, sizeof(tv));
		else
			if (copyin((caddr_t)uap->tptr,(caddr_t)tv, sizeof(tv))) {
				u.u_error = EFAULT;
				return;
			}
	} else {
		tv[0] = time.tv_sec;
		tv[1] = time.tv_sec;
	}
	vattr_null(&vattr);
	vattr.va_atime.tv_sec = tv[0];
	vattr.va_atime.tv_usec = 0;
	vattr.va_mtime.tv_sec = tv[1];
	vattr.va_mtime.tv_usec = 0;
	u.u_error = namesetattr(uap->fname, FOLLOW_LINK, &vattr);
}

ulimit()
{
	register struct a {
		int	cmd;
		long	arg;
	} *uap;

	uap = (struct a *)u.u_ap;
	switch (uap->cmd) {

	case 2:	/* Set new file size limit. */
		if (uap->arg > u.u_limit && !suser())
			return;
		u.u_limit = uap->arg;
		/*
		 * This is somewhat more complicated for us.  The ulimit()
		 * can be set here, or using the BSD setrlimit() call.  We
		 * maintain u.u_limit equals u.u_rlimit[RLIMIT_FSIZE].rlim_cur
		 * rounded up to the next 512-byte boundary.
		 * In order to preserve SYSV semantics, however, we retain
		 * the test against the old u_limit, rather than the old
		 * rlim_max.  If you used setrlimit() to reduce u_limit below
		 * rlim_max, then you must use setrlimit() to increase it.
		 * You can *never* raise u_limit using this system call.
		 * BUG ??? XXX ~~~ cannot limit a bsd43_write to a value
		 * larger than (2GB-2), but *can* limit a sysv_write to such
		 * a value.
		 * Also see code in bsd43/glue/kern_resource.c:setrlimit().
		 */

		u.u_rlimit[BSD43_RLIMIT_FSIZE].rlim_cur =
		  u.u_rlimit[BSD43_RLIMIT_FSIZE].rlim_max =
			(((uint)(u.u_limit << SCTRSHFT) > BSD43_RLIM_INFINITY)
			  ? (BSD43_RLIM_INFINITY)
			  : (u.u_limit << SCTRSHFT));

	case 1:	/* Return current file size limit. */
		u.u_roff = u.u_limit;
		break;

	case 3:	/* Return maximum possible break value. */
		{
		register preg_t	*prp, *dprp, *prp2;

		/*	Find the data region
		 */

		if ((dprp = findpreg(u.u_procp, PT_DATA)) == NULL)
			u.u_roff = 0;
		else {
			/*	Now find the region with a virtual
			 *	address greater than the data region
			 *	but lower than any other region
			 */
			register int size;
			register reg_t *rp;
	
			prp = u.u_procp->p_region;
	
			size = 0;
			while (rp = prp->p_reg) {
				size += rp->r_pgsz;
				prp++;
			}

			size = ctob(tune.t_maxumem - size);

			prp2 = NULL;
			for (prp = u.u_procp->p_region; prp->p_reg; prp++) {
				if (prp->p_regva <= dprp->p_regva)
					continue;
				if (prp2==NULL || prp->p_regva < prp2->p_regva)
					prp2 = prp;
			}
			u.u_roff =(off_t)(dprp->p_regva+ctob(dprp->p_reg->r_pgsz)+size);
			if (prp2 != NULL)
				u.u_roff = min(u.u_roff, (off_t)prp2->p_regva);
		}
		break;
	}
	case 4:	/* Return configured value of NOFILES. */
		u.u_roff = v.v_nofiles;
		break;

	default:
		u.u_error = EINVAL;
	}
}
