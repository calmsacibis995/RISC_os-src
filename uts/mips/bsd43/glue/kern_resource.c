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
#ident	"$Header: kern_resource.c,v 1.5.1.4 90/05/10 04:40:24 wje Exp $"

/*
 * Copyright (c) 1982, 1986 Regents of the University of California.
 * All rights reserved.  The Berkeley software License Agreement
 * specifies the terms and conditions for redistribution.
 *
 *	@(#)kern_resource.c	7.1 (Berkeley) 6/5/86
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

/*
 * Resource controls and accounting.
 */

getpriority()
{
	register struct a {
		int	which;
		int	who;
	} *uap = (struct a *)u.u_ap;
	register struct proc *p;
	int low = BSD43_PRIO_MAX + 1;

	switch (uap->which) {

	case BSD43_PRIO_PROCESS:
		if (uap->who == 0)
			p = u.u_procp;
		else
			p = bsd43_pfind(uap->who);
		if (p == 0)
			break;
		low = (p->p_nice - NZERO);
		break;

	case BSD43_PRIO_PGRP:
		if (uap->who == 0)
			uap->who = u.u_procp->p_jcpgrp;
		for (p = proc; p < (struct proc *) v.ve_proc; p++) {
			if (p->p_jcpgrp == uap->who && p->p_stat != SZOMB &&
			    (p->p_nice - NZERO) < low)
				low = (p->p_nice - NZERO);
		}
		break;

	case BSD43_PRIO_USER:
		if (uap->who == 0)
			uap->who = (int)u.u_uid;
		for (p = proc; p < (struct proc *) v.ve_proc; p++) {
			if (p->p_uid == uap->who &&
			    (p->p_nice - NZERO) < low)
				low = (p->p_nice - NZERO);
		}
		break;

	default:
		u.u_error = EINVAL;
		return;
	}
	if (low == BSD43_PRIO_MAX + 1) {
		u.u_error = ESRCH;
		return;
	}
	u.u_rval1 = low;
}

setpriority()
{
	register struct a {
		int	which;
		int	who;
		int	prio;
	} *uap = (struct a *)u.u_ap;
	register struct proc *p;
	int found = 0;

	switch (uap->which) {

	case BSD43_PRIO_PROCESS:
		if (uap->who == 0)
			p = u.u_procp;
		else
			p = bsd43_pfind(uap->who);
		if (p == 0)
			break;
		donice(p, uap->prio);
		found++;
		break;

	case BSD43_PRIO_PGRP:
		if (uap->who == 0)
			uap->who = u.u_procp->p_jcpgrp;
		for (p = proc; p < (struct proc *) v.ve_proc; p++)
			if (p->p_jcpgrp == uap->who) {
				donice(p, uap->prio);
				found++;
			}
		break;

	case BSD43_PRIO_USER:
		if (uap->who == 0)
			uap->who = (int)u.u_uid;
		for (p = proc; p < (struct proc *) v.ve_proc; p++)
			if (p->p_uid == uap->who) {
				donice(p, uap->prio);
				found++;
			}
		break;

	default:
		u.u_error = EINVAL;
		return;
	}
	if (found == 0)
		u.u_error = ESRCH;
}

donice(p, n)
	register struct proc *p;
	register int n;
{

	if (u.u_uid && u.u_ruid &&
	    u.u_uid != p->p_uid && u.u_ruid != p->p_uid) {
		u.u_error = EPERM;
		return;
	}
	if (n > BSD43_PRIO_MAX)
		n = BSD43_PRIO_MAX;
	if (n < BSD43_PRIO_MIN)
		n = BSD43_PRIO_MIN;
	if (n < (int)(p->p_nice - NZERO) && !suser()) {
		u.u_error = EACCES;
		return;
	}
	p->p_nice = n + NZERO;
	(void) setpri(p);
}

setrlimit()
{
	register struct a {
		u_int	which;
		struct	bsd43_rlimit *lim;
	} *uap = (struct a *)u.u_ap;
	struct bsd43_rlimit alim;
	register struct bsd43_rlimit *alimp;
#ifdef NOTDEF
	extern unsigned maxdmap;
#endif NOTDEF

	if (uap->which >= BSD43_RLIM_NLIMITS) {
		u.u_error = EINVAL;
		return;
	}
	alimp = &u.u_rlimit[uap->which];
	u.u_error = bsd_copyin((caddr_t)uap->lim, (caddr_t)&alim,
		sizeof (struct bsd43_rlimit));
	if (u.u_error)
		return;
	if (alim.rlim_cur > alimp->rlim_max || alim.rlim_max > alimp->rlim_max)
		if (!suser())
			return;
	switch (uap->which) {

	case BSD43_RLIMIT_DATA:
#ifdef NOTDEF
		if (alim.rlim_cur > maxdmap)
			alim.rlim_cur = maxdmap;
		if (alim.rlim_max > maxdmap)
			alim.rlim_max = maxdmap;
#endif NOTDEF
		break;

	case BSD43_RLIMIT_STACK:
#ifdef NOTDEF
		if (alim.rlim_cur > maxdmap)
			alim.rlim_cur = maxdmap;
		if (alim.rlim_max > maxdmap)
			alim.rlim_max = maxdmap;
#endif NOTDEF
		break;

	case BSD43_RLIMIT_FSIZE:
		/*
		 * This is somewhat more complicated for us.  The [ur]limit
		 * can be set here, or using the SYSV ulimit() call.  We
		 * maintain u.u_limit == u.u_rlimit[RLIMIT_FSIZE].rlim_cur
		 * rounded up to the next 512-byte boundary.
		 * We must be very careful around RLIM_INFINITY.
		 * Also see code in os/sys4.c:ulimit().
		 */
		if (alim.rlim_cur <= 0)
			u.u_limit = 0; /* good enough to stop all writes */
		else
			u.u_limit = ((uint)alim.rlim_cur+NBPSCTR-1) >> SCTRSHFT;
		break;
	}
	*alimp = alim;
#ifdef NOTDEF
	if (uap->which == BSD43_RLIMIT_RSS)
		u.u_procp->p_maxrss = alim.rlim_cur/NBPG;
#endif NOTDEF
}

getrlimit()
{
	register struct a {
		u_int	which;
		struct	bsd43_rlimit *rlp;
	} *uap = (struct a *)u.u_ap;

	if (uap->which >= BSD43_RLIM_NLIMITS) {
		u.u_error = EINVAL;
		return;
	}
	u.u_error = bsd_copyout((caddr_t)&u.u_rlimit[uap->which], (caddr_t)uap->rlp,
	    sizeof (struct bsd43_rlimit));
}

getrusage()
{
	register struct a {
		int	who;
		struct	bsd43_rusage *rusage;
	} *uap = (struct a *)u.u_ap;
	register struct bsd43_rusage *rup;

	switch (uap->who) {

	case BSD43_RUSAGE_SELF:
		rup = &u.u_ru;
		break;

	case BSD43_RUSAGE_CHILDREN:
		rup = &u.u_cru;
		break;

	default:
		u.u_error = EINVAL;
		return;
	}
	u.u_error = bsd_copyout((caddr_t)rup, (caddr_t)uap->rusage,
	    sizeof (struct bsd43_rusage));
}

#ifdef RISCOS
/*
 * mips_getrusage() provides the same functionality as getrusage() above but
 * allows the rusage structure to grow so that mips specific stats can be
 * returned in the future.
 */
mips_getrusage(who, rusage, rusage_size)
int	who;
struct	bsd43_rusage *rusage;
int	rusage_size;
{
	register struct bsd43_rusage *rup;
	int error;

	switch (who) {

	case BSD43_RUSAGE_SELF:
		update_rusage();
		rup = &u.u_ru;
		break;

	case BSD43_RUSAGE_CHILDREN:
		rup = &u.u_cru;
		break;

	default:
		return(EINVAL);
	}
	if(rusage_size < 0 || rusage_size > sizeof(struct bsd43_rusage)){
		return(EINVAL);
	}
	error = bsd_copyout((caddr_t)rup, (caddr_t)rusage, rusage_size);
	return(error);
}
#endif RISCOS

ruadd(ru, ru2)
	register struct bsd43_rusage *ru, *ru2;
{
	register long *ip, *ip2;
	register int i;

	timevaladd(&ru->ru_utime, &ru2->ru_utime);
	timevaladd(&ru->ru_stime, &ru2->ru_stime);
	if (ru->ru_maxrss < ru2->ru_maxrss)
		ru->ru_maxrss = ru2->ru_maxrss;
	ip = &ru->bsd43_ru_first; ip2 = &ru2->bsd43_ru_first;
	for (i = &ru->bsd43_ru_last - &ru->bsd43_ru_first; i >= 0; i--)
		*ip++ += *ip2++;
}


setpri(p)
     	struct proc *p;
{
	p->p_pri = calcppri(p);
}


update_rusage()
{
	u.u_ru.ru_inblock = u.u_ior;
	u.u_ru.ru_oublock = u.u_iow;
}
