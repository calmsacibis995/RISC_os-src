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
#ident	"$Header: kern_prot.c,v 1.7.1.4 90/05/10 04:40:17 wje Exp $"

/*
 * Routines from BSD kern_prot.c
 *
 * System calls related to processes and protection
 *
 * Copyright (c) 1982, 1986 Regents of the University of California.
 * All rights reserved.  The Berkeley software License Agreement
 * specifies the terms and conditions for redistribution.
 *
 *	@(#)kern_prot.c	7.1 (Berkeley) 6/5/86
 */

/*
 * :r !includes proc.h user.h errno.h
 */

#include "sys/types.h"
#include "sys/signal.h"		/* needed by user.h */
#include "sys/pcb.h"		/* needed by user.h */
#include "sys/user.h"
#include "sys/immu.h"		/* needed by proc.h */
#include "sys/sbd.h"		/* needed by proc.h */
#include "sys/region.h"		/* needed by proc.h */
#include "sys/proc.h"
#include "sys/errno.h"
#include "sys/debug.h"
#include "sys/numips.h"

extern struct proc *bsd43_pfind();

void crfree();

bsd43_getpgrp()
{
	register struct a {
		int	pid;
	} *uap = (struct a *)u.u_ap;
	register struct proc *p;

#if RISCOS
	if (uap->pid == 0) {
		uap->pid = u.u_procp->p_pid;	/* useless ? */
		p = u.u_procp;			/* don't bother searching */
	}
	else
		p = bsd43_pfind(uap->pid);
#else
	if (uap->pid == 0)
		uap->pid = u.u_procp->p_pid;
	p = bsd43_pfind(uap->pid);
#endif
	if (p == 0) {
		u.u_error = ESRCH;
		return;
	}
	u.u_rval1 = p->p_jcpgrp;
}

getgroups()
{
	register struct	a {
		u_int	gidsetsize;
		int	*gidset;
	} *uap = (struct a *)u.u_ap;
	register gid_t *gp;
	register int *lp;
	int groups[NGROUPS];

	for (gp = &u.u_groups[NGROUPS]; gp > u.u_groups; gp--)
		if (gp[-1] != NOGROUP)
			break;
	if (uap->gidsetsize == 0 && POSIX_SYSCALL) {
		u.u_rval1 = gp - u.u_groups;
		return;
	}
	if (uap->gidsetsize < gp - u.u_groups) {
		u.u_error = EINVAL;
		return;
	}
	uap->gidsetsize = gp - u.u_groups;
	for (lp = groups, gp = u.u_groups; lp < &groups[uap->gidsetsize]; )
		*lp++ = (int)*gp++;
	if (BSD_SYSCALL || SYSV_SYSCALL)
		u.u_error = bsd_copyout((caddr_t)groups, (caddr_t)uap->gidset,
		    uap->gidsetsize * sizeof (groups[0]));
	else if (POSIX_SYSCALL)
		u.u_error = bsd_copyout((caddr_t)u.u_groups, 
	   	    (caddr_t)uap->gidset,
		    uap->gidsetsize * sizeof (u.u_groups[0]));
	if (u.u_error)
		return;
	u.u_rval1 = uap->gidsetsize;
}


bsd43_setpgrp()
{
	register struct proc *p;
	register struct proc *q;
	register struct a {
		int	pid;
		int	pgrp;
	} *uap = (struct a *)u.u_ap;

#if RISCOS
	if (uap->pid == 0) {
		uap->pid = u.u_procp->p_pid;	/* useless ? */
		p = u.u_procp;			/* don't bother searching */
	}
	else
		p = bsd43_pfind(uap->pid);
#else
	if (uap->pid == 0)
		uap->pid = u.u_procp->p_pid;
	p = bsd43_pfind(uap->pid);
#endif
	if (p == 0) {
		u.u_error = ESRCH;
		return;
	}

	if (p->p_uid != u.u_uid && u.u_uid && !inferior(p)) {
	    		/* process group in use by other user or session */
		u.u_error = EPERM;
		return;
	}
	/* If pid does not already belong to a session, make it a 
	 * session leader 
	 */
	if (uap->pgrp != 0 && p->p_pgrp == 0) {
		p->p_pgrp = uap->pgrp;
	}
	p->p_jcpgrp = uap->pgrp;
}

setreuid()
{
	struct a {
		int	ruid;
		int	euid;
	} *uap;
	register uid_t ruid, euid;

	uap = (struct a *)u.u_ap;
	ruid = uap->ruid;
	if (ruid == (uid_t)-1)
		ruid = u.u_ruid;
	euid = uap->euid;
	if (euid == (uid_t)-1)
		euid = u.u_uid;
	if (ruid >= MAXUID ||
	    euid >= MAXUID) {
	  	u.u_error = EINVAL;
		return;
	};
	if (u.u_ruid != ruid && u.u_uid != ruid && !suser())
		return;
	if (u.u_ruid != euid && u.u_uid != euid && !suser())
		return;
	/*
	 * Everything's okay, do it.
	 */
	u.u_cred = crcopy(u.u_cred);
	u.u_procp->p_uid = euid;
	u.u_procp->p_suid = euid;
	u.u_ruid = ruid;
	u.u_uid = euid;
}

setregid()
{
	register struct a {
		int	rgid;
		int	egid;
	} *uap;
	register gid_t rgid, egid;

	uap = (struct a *)u.u_ap;
	rgid = uap->rgid;
	if (rgid == (gid_t)-1)
		rgid = u.u_rgid;
	egid = uap->egid;
	if (egid == (gid_t)-1)
		egid = u.u_gid;
	if (((u_int) rgid) >= MAXGID ||
	    ((u_int) egid) >= MAXGID) {
		u.u_error = EINVAL;
		return;
	};
	if (u.u_rgid != rgid && u.u_gid != rgid && !suser())
		return;
	if (u.u_rgid != egid && u.u_gid != egid && !suser())
		return;
	u.u_cred = crcopy(u.u_cred);
	if (u.u_rgid != rgid) {
		leavegroup(u.u_rgid);
		(void) entergroup(rgid);
		u.u_rgid = rgid;
	}
	u.u_gid = egid;
}

setgroups()
{
	register struct	a {
		u_int	gidsetsize;
		int	*gidset;
	} *uap = (struct a *)u.u_ap;
	register gid_t *gp;
	register int *lp;
	int groups[NGROUPS];
	struct ucred *newcr, *tmpcr;

	if (!suser())
		return;
	if (uap->gidsetsize > sizeof (u.u_groups) / sizeof (u.u_groups[0])) {
		u.u_error = EINVAL;
		return;
	}
	newcr = crdup(u.u_cred);
	/*
	 *	Can't copy gid array directly, we still have to cast
	 *	them to a gid_t !
	 */
	u.u_error = bsd_copyin((caddr_t)uap->gidset, (caddr_t)groups,
	    uap->gidsetsize * sizeof (uap->gidset[0]));
	if (u.u_error) {
		crfree(newcr);
		return;
	}
	for(lp = groups,gp = newcr->cr_groups;
		lp < &groups[uap->gidsetsize];lp++,gp++) 
		*gp = (gid_t) *lp;
	tmpcr = u.u_cred;
	u.u_cred = newcr;
	crfree(tmpcr);
	for (gp = &u.u_groups[uap->gidsetsize]; gp < &u.u_groups[NGROUPS]; gp++)
		*gp = NOGROUP;
}

/*
 * Group utility functions.
 */

/*
 * Delete gid from the group set and compress.
 */
leavegroup(gid)
	gid_t gid;
{
	register gid_t *gp;

	for (gp = u.u_groups; gp < &u.u_groups[NGROUPS]; gp++)
		if (*gp == gid)
			goto found;
	return;
found:
	for (; gp < &u.u_groups[NGROUPS-1]; gp++)
		*gp = *(gp+1);
	*gp = NOGROUP;
}

/*
 * Add gid to the group set.
 */
entergroup(gid)
	gid_t gid;
{
	register gid_t *gp;

	for (gp = u.u_groups; gp < &u.u_groups[NGROUPS]; gp++) {
		if (*gp == gid)
			return (0);
		if (*gp == NOGROUP) {
			*gp = gid;
			return (0);
		}
	}
	return (-1);
}

/*
 * Check if gid is a member of the group set.
 */
groupmember(gid)
	gid_t gid;
{
	register gid_t *gp;

	if (u.u_gid == gid)
		return (1);
	for (gp = u.u_groups; gp < &u.u_groups[NGROUPS] && *gp != NOGROUP; gp++)
		if (*gp == gid)
			return (1);
	return (0);
}

/*
 * Routines to allocate and free credentials structures
 */

int cractive = 0;

struct credlist {
	union {
		struct ucred cru_cred;
		struct credlist *cru_next;
	} cl_U;
#define	cl_cred	cl_U.cru_cred
#define	cl_next	cl_U.cru_next
};

struct credlist *crfreelist = NULL;

/*
 * Allocate a zeroed cred structure and crhold it.
 */
struct ucred *
crget()
{
	register struct ucred *cr;

	if (crfreelist) {
		cr = &crfreelist->cl_cred;
		crfreelist = ((struct credlist *)cr)->cl_next;
	} else {
		cr = (struct ucred *)kmem_alloc((u_int)sizeof(*cr));
	}
	bzero((caddr_t)cr, sizeof(*cr));
	crhold(cr);
	cractive++;
	return(cr);
}

/*
 * Free a cred structure.
 * Throws away space when ref count gets to 0.
 */
void
crfree(cr)
	struct ucred *cr;
{
	int	s = splhigh();

	if (--cr->cr_ref != 0) {
		(void) splx(s);
		return;
	}
	((struct credlist *)cr)->cl_next = crfreelist;
	crfreelist = (struct credlist *)cr;
	cractive--;
	(void) splx(s);
}

/*
 * Copy cred structure to a new one and free the old one.
 */
struct ucred *
crcopy(cr)
	struct ucred *cr;
{
	struct ucred *newcr;

	newcr = crget();
	*newcr = *cr;
	crfree(cr);
	newcr->cr_ref = 1;
	return(newcr);
}

/*
 * Dup cred struct to a new held one.
 */
struct ucred *
crdup(cr)
	struct ucred *cr;
{
	struct ucred *newcr;

	newcr = crget();
	*newcr = *cr;
	newcr->cr_ref = 1;
	return(newcr);
}

