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
#ident	"$Header: quota_syscalls.c,v 1.3.1.2.1.1.1.2 91/01/14 19:12:46 beacker Exp $"
/*
 * Quota system calls.
 */

#include "sys/types.h"
#include "sys/errno.h"
#include "sys/systm.h"
#include "sys/signal.h"		/* needed by user.h */
#include "sys/pcb.h"		/* needed by user.h */
#include "sys/user.h"
#include "sys/vfs.h"
#include "sys/vnode.h"
#include "sys/uio.h"
#if defined(RISCOS)
#include "sys/buf.h"
#endif
#include "sys/fs/ufs_quota.h"
#include "sys/fs/ufs_inode.h"
#include "sys/fs/ufs_mount.h"
#include "sys/fs/ufs_fs.h"

/*
 * Sys call to allow users to find out
 * their current position wrt quota's
 * and to allow super users to alter it.
 */
quotactl(uap)
	register struct a {
		int	cmd;
		caddr_t	fdev;
		int	uid;
		caddr_t	addr;
	} *uap;
{
	struct mount *mp;

	if (uap->uid < 0)
		uap->uid = u.u_ruid;
#if defined(RISCOS) && defined(TAHOE_QUOTA)
	if ((uap->cmd == Q_SYNC && uap->fdev == NULL) ||
	    (uap->cmd == Q_DOWARN &&
	     (uap->fdev == NULL || uap->fdev == (caddr_t)NODEV))) {
#else
	if (uap->cmd == Q_SYNC && uap->fdev == NULL) {
#endif
		mp = NULL;
	} else {
		u.u_error = fdevtomp(uap->fdev, &mp);
		if (u.u_error)
			return;
	}
	switch (uap->cmd) {

	case Q_QUOTAON:
		u.u_error = opendq(mp, uap->addr);
		break;

	case Q_QUOTAOFF:
		u.u_error = closedq(mp);
		break;

	case Q_SETQUOTA:
	case Q_SETQLIM:
		u.u_error = setquota(uap->cmd, uap->uid, mp, uap->addr);
		break;

	case Q_GETQUOTA:
		u.u_error = getquota(uap->uid, mp, uap->addr);
		break;

	case Q_SYNC:
		u.u_error = qsync(mp);
		break;

#if defined(RISCOS) && defined(TAHOE_QUOTA)
	case Q_DOWARN:
		u.u_error = dowarn(uap->uid);
		break;
#endif /* RISCOS */
	default:
		u.u_error = EINVAL;
		break;
	}
}

/* XXX */
oldquota()
{
	printf("oldquota\n");
}

/*
 * Set the quota file up for a particular file system.
 * Called as the result of a setquota system call.
 */
int
opendq(mp, addr)
	register struct mount *mp;
	caddr_t addr;			/* quota file */
{
	struct vnode *vp;
	struct dquot *dqp;
	int error;

	if (!suser())
		return (EPERM);
	error =
	    lookupname(addr, UIO_USERSPACE, FOLLOW_LINK,
		(struct vnode **)0, &vp);
	if (error)
		return (error);
	if (VFSTOM(vp->v_vfsp) != mp || vp->v_type != VREG) {
		VN_RELE(vp);
		return (EACCES);
	}
	if (mp->m_qflags & MQ_ENABLED)
		(void) closedq(mp);
	if (mp->m_qinod != NULL) {	/* open/close in progress */
		VN_RELE(vp);
		return (EBUSY);
	}
	mp->m_qinod = VTOI(vp);
#if defined(RISCOS) && defined(TAHOE_QUOTA)
	/*
	 * The file system max warning limits are in the super user
	 * dquot.  The warnings set the number of warnings given to
	 * a user before, he can not perform any more writing.
	 * If it is zero, a default is used.  (see quota.h).
	 */
	if ((error = getdiskquota(0, mp, 1, &dqp)) == 0) {
		mp->m_bwarn = (dqp->dq_bwarn ? dqp->dq_bwarn : MAX_BQ_WARN);
		mp->m_fwarn = (dqp->dq_fwarn ? dqp->dq_fwarn : MAX_FQ_WARN);
#else /* !RISCOS */
	/*
	 * The file system time limits are in the super user dquot.
	 * The time limits set the relative time the other users
	 * can be over quota for this file system.
	 * If it is zero a default is used (see quota.h).
	 */
	error = getdiskquota(0, mp, 1, &dqp);
	if (error == 0) {
		mp->m_btimelimit =
		    (dqp->dq_btimelimit? dqp->dq_btimelimit: DQ_BTIMELIMIT);
		mp->m_ftimelimit =
		    (dqp->dq_ftimelimit? dqp->dq_ftimelimit: DQ_FTIMELIMIT);
#endif /* RISCOS */
		dqput(dqp);			/* release this dquot */
		mp->m_qflags = MQ_ENABLED;	/* enable quotas */
	} else {
		/*
		 * Some sort of I/O error on the quota file.
		 */
		irele(mp->m_qinod);
		mp->m_qinod = NULL;
	}
	return (error);
}

/*
 * Close off disk quotas for a file system.
 */
int
closedq(mp)
	register struct mount *mp;
{
	register struct dquot *dqp;
	register struct inode *ip;
	register struct inode *qip;
#if defined(RISCOS)
	register int i;
	register struct dquot *dq_next;
#endif

	if (!suser())
		return (EPERM);
	if ((mp->m_qflags & MQ_ENABLED) == 0)
		return (0);
	qip = mp->m_qinod;
	if (qip == NULL)
		panic("closedq");
	mp->m_qflags = 0;	/* disable quotas */
loop:
	/*
	 * Run down the inode table and release all dquots assciated with
	 * inodes on this filesystem.
	 */
	for (ip = inode; ip < inodeNINODE; ip++) {
		dqp = ip->i_dquot;
		if (dqp != NULL && dqp->dq_mp == mp) {
			if (dqp->dq_flags & DQ_LOCKED) {
				dqp->dq_flags |= DQ_WANT;
#if defined(RISCOS)
				/* Incr cnt to make sure it doesn't get freed */
				dqp->dq_cnt++;
				(void) sleep((caddr_t)dqp, PINOD+2);
				dqp->dq_cnt--;
#else
				(void) sleep((caddr_t)dqp, PINOD+2);
#endif /* RISCOS */
				goto loop;
			}
			dqp->dq_flags |= DQ_LOCKED;
			dqput(dqp);
			ip->i_dquot = NULL;
		}
	}
#if defined(RISCOS)
	/*
	 * Although every structure should be on the free list because
	 * only one is used at a time, we'll go down both the hash list
	 * and the freelist just to be sure.  dqinval() removes structures
	 * from both the hash queue and the free queue, so we shouldn't
	 * process structures twice.
	 */
	for (i = 0; i < NDQHASH; i++) {
	  register struct dqhead *dhp = &dqhead[i];

	  for (dqp = dhp->dqh_forw; dqp != (DQptr)dhp; dqp = dq_next) {
		dq_next = dqp->dq_forw;
		if (dqp->dq_mp == mp) {
			if (dqp->dq_flags & DQ_LOCKED) {
				dqp->dq_flags |= DQ_WANT;
				/* Incr cnt to make sure it doesn't get freed */
				dqp->dq_cnt++;
				(void) sleep((caddr_t)dqp, PINOD+2);
				dqp->dq_cnt--;
				goto loop;
			}
			dqp->dq_flags |= DQ_LOCKED;
			if (dqp->dq_flags & DQ_MOD)
				dqupdate(dqp);
			dqinval(dqp);
		}
	  }
	} /* for i */

	for (dqp=dqfreelist.dq_freef; dqp != (DQptr)&dqfreelist; dqp=dq_next) {
		dq_next = dqp->dq_freef;
		if (dqp->dq_mp == mp) {
			if (dqp->dq_flags & DQ_LOCKED) {
				dqp->dq_flags |= DQ_WANT;
				/* Incr cnt to make sure it doesn't get freed */
				dqp->dq_cnt++;
				(void) sleep((caddr_t)dqp, PINOD+2);
				dqp->dq_cnt--;
				goto loop;
			}
			dqp->dq_flags |= DQ_LOCKED;
			if (dqp->dq_flags & DQ_MOD)
				dqupdate(dqp);
			dqinval(dqp);
		}
	}
#else /* !RISCOS */
	/*
	 * Run down the dquot table and clean and invalidate the
	 * dquots for this file system.
	 */
	for (dqp = dquot; dqp < dquotNDQUOT; dqp++) {
		if (dqp->dq_mp == mp) {
			if (dqp->dq_flags & DQ_LOCKED) {
				dqp->dq_flags |= DQ_WANT;
				(void) sleep((caddr_t)dqp, PINOD+2);
				goto loop;
			}
			dqp->dq_flags |= DQ_LOCKED;
			if (dqp->dq_flags & DQ_MOD)
				dqupdate(dqp);
			dqinval(dqp);
		}
	}
#endif /* RISCOS */
	/*
	 * Sync and release the quota file inode.
	 */
	ilock(qip);
	(void) syncip(qip);
	iput(qip);
	mp->m_qinod = NULL;
	return (0);
}

/*
 * Set various feilds of the dqblk according to the command.
 * Q_SETQUOTA - assign an entire dqblk structure.
 * Q_SETQLIM - assign a dqblk structure except for the usage.
 */
int
setquota(cmd, uid, mp, addr)
	int cmd;
	short uid;
	struct mount *mp;
	caddr_t addr;
{
	register struct dquot *dqp;
	struct dquot *xdqp;
	struct dqblk newlim;
	int error;

	if (!suser())
		return (EPERM);
	if ((mp->m_qflags & MQ_ENABLED) == 0)
		return (ESRCH);
	error = copyin(addr, (caddr_t)&newlim, sizeof (struct dqblk));
#ifdef RISCOS
	if (error)
		error = EFAULT;
#endif /* RISCOS */
	if (error)
		return (error);
	error = getdiskquota(uid, mp, 0, &xdqp);
	if (error)
		return (error);
	dqp = xdqp;
	/*
	 * Don't change disk usage on Q_SETQLIM
	 */
	if (cmd == Q_SETQLIM) {
		newlim.dqb_curblocks = dqp->dq_curblocks;
		newlim.dqb_curfiles = dqp->dq_curfiles;
	}
	dqp->dq_dqb = newlim;
	if (uid == 0) {
#if defined(RISCOS) && defined(TAHOE_QUOTA)
		/* Warn limits for the super user set the relative number
		 * of warnings a user can have before he/she isn't allowed
		 * to write anymore.  Do I believe this?  If it is zero,
		 * a default is used (see quota.h).
		 */
		mp->m_bwarn = newlim.dqb_bwarn? newlim.dqb_bwarn : MAX_BQ_WARN;
		mp->m_fwarn = newlim.dqb_fwarn? newlim.dqb_fwarn : MAX_FQ_WARN;
	} else {/*
		 * If the user is now over quota, it doesn't matter because we
		 * are using warning counts.  Next time the user writes and is
		 * over quota, the warning will be issued at that time.
		 */
		dqp->dq_flags &= ~(DQ_BLKS|DQ_FILES);/* warn user */
	}
#else /* !RISCOS */
		/*
		 * Timelimits for the super user set the relative time
		 * the other users can be over quota for this file system.
		 * If it is zero a default is used (see quota.h).
		 */
		mp->m_btimelimit =
		    newlim.dqb_btimelimit? newlim.dqb_btimelimit: DQ_BTIMELIMIT;
		mp->m_ftimelimit =
		    newlim.dqb_ftimelimit? newlim.dqb_ftimelimit: DQ_FTIMELIMIT;
	} else {
		/*
		 * If the user is now over quota, start the timelimit.
		 * The user will not be warned.
		 */
		if (dqp->dq_curblocks >= dqp->dq_bsoftlimit &&
		    dqp->dq_bsoftlimit && dqp->dq_btimelimit == 0)
			dqp->dq_btimelimit = time.tv_sec + mp->m_btimelimit;
		else
			dqp->dq_btimelimit = 0;
		if (dqp->dq_curfiles >= dqp->dq_fsoftlimit &&
		    dqp->dq_fsoftlimit && dqp->dq_ftimelimit == 0)
			dqp->dq_ftimelimit = time.tv_sec + mp->m_ftimelimit;
		else
			dqp->dq_ftimelimit = 0;
		dqp->dq_flags &= ~(DQ_BLKS|DQ_FILES);
	}
#endif /* RISCOS */
	dqp->dq_flags |= DQ_MOD;
	dqupdate(dqp);
	dqput(dqp);
	return (0);
}

/*
 * Q_GETQUOTA - return current values in a dqblk structure.
 */
int
getquota(uid, mp, addr)
	short uid;
	struct mount *mp;
	caddr_t addr;
{
	register struct dquot *dqp;
	struct dquot *xdqp;
	int error;

	if (uid != u.u_ruid && !suser())
		return (EPERM);
	if ((mp->m_qflags & MQ_ENABLED) == 0)
		return (ESRCH);
	error = getdiskquota(uid, mp, 0, &xdqp);
	if (error)
		return (error);
	dqp = xdqp;
	if (dqp->dq_fhardlimit == 0 && dqp->dq_fsoftlimit == 0 &&
	    dqp->dq_bhardlimit == 0 && dqp->dq_bsoftlimit == 0) {
		error = ESRCH;
	} else {
		error =
		    copyout((caddr_t)&dqp->dq_dqb, addr, sizeof (struct dqblk));
#ifdef RISCOS
		if (error)
			error = EFAULT;
#endif /* RISCOS */
	}
	dqput(dqp);
	return (error);
}

/*
 * Q_SYNC - sync quota files to disk.
 */
int
qsync(mp)
	register struct mount *mp;
{
	register struct dquot *dqp;
#if defined(RISCOS)
	register struct dqhead *dhp;
	register int i;
#endif

	if (mp != NULL && (mp->m_qflags & MQ_ENABLED) == 0)
		return (ESRCH);

#if defined(RISCOS)
	/*
	 * The following goes down every entry in the hashed queues.
	 * This should be equivalent to the static case because 
	 * modified/non-flushed structures should always be on the
	 * the hash list.
	 */
	for (i = 0; i < NDQHASH; i++) {
	  dhp = &dqhead[i];
	  for (dqp = dhp->dqh_forw; dqp != (DQptr)dhp; dqp = dqp->dq_forw) {
		if ((dqp->dq_flags & DQ_MOD) &&
		    (mp == NULL || dqp->dq_mp == mp) &&
		    (dqp->dq_mp->m_qflags & MQ_ENABLED) &&
		    (dqp->dq_flags & DQ_LOCKED) == 0) {
			dqp->dq_flags |= DQ_LOCKED;
			dqupdate(dqp);
			DQUNLOCK(dqp);
		}
	  }
	} /* for i */
#else
	for (dqp = dquot; dqp < dquotNDQUOT; dqp++) {
		if ((dqp->dq_flags & DQ_MOD) &&
		    (mp == NULL || dqp->dq_mp == mp) &&
		    (dqp->dq_mp->m_qflags & MQ_ENABLED) &&
		    (dqp->dq_flags & DQ_LOCKED) == 0) {
			dqp->dq_flags |= DQ_LOCKED;
			dqupdate(dqp);
			DQUNLOCK(dqp);
		}
	}
#endif /* RISCOS */
	return (0);
}

#if defined(RISCOS) && defined(TAHOE_QUOTA)
/*
 * Search through the mount entries, looking for mounted file systems with
 * quotas enabled.  Currently, we're going to look at nfs mounted systems
 * also.  However, this can be changed.
 */
int
dowarn(uid)
	short uid;
{
	struct dquot *dqp;
	register struct mount *mp;
	int error;

	if (!suser())
	  	return(u.u_error);
	if (u.u_procp->p_ttyvp == NULL)
		return (0);

	for (mp=mounttab; mp != NULL; mp = mp->m_nxt) {
		if (mp->m_bufp == NULL)
			continue;
		if ((mp->m_qflags & MQ_ENABLED) == 0)
			continue;
		if ((error = getdiskquota(uid, mp, 0, &dqp)) != 0)
			return (error);
		if (dqp) { 	/* should be valid anyways */
			error = qwarn(dqp,
				      mp->m_bufp->b_un.b_fs->fs_fsmnt, mp);
			dqput(dqp);
		}
		if (error) break;
	}
	return (error);
}	/* end of dowarn */

/*
 * If the user is over quota, print a message.
 */
int
qwarn(dqp, mount_point, mp)
	register struct dquot *dqp;
	char *mount_point;
	struct mount *mp;
{
	struct dquot *su_dqp;
	int error = 0;

	if ((error = getdiskquota(0, mp, 1, &su_dqp)) != 0)
		return error;
	dqp->dq_flags |= DQ_MOD;
	if (dqp->dq_fsoftlimit && dqp->dq_curfiles >= dqp->dq_fsoftlimit) {
		if (dqp->dq_fwarn && --dqp->dq_fwarn)
		    uprintf(
	"\007\n\nWarning: too many files on %s, %d warning%s left\n\n\n\007"
			    , mount_point
			    , dqp->dq_fwarn
			    , dqp->dq_fwarn > 1 ? "s" : ""
		    );
		else
		    uprintf(
		"\007\n\nWARNING: too many files on %s, NO MORE!!\n\n\n\007"
			    , mount_point
		    );
	} else {
		if (su_dqp != NULL)
			dqp->dq_fwarn = 
			  (su_dqp->dq_fwarn ? su_dqp->dq_fwarn : MAX_FQ_WARN);
	}

	if (dqp->dq_bsoftlimit && dqp->dq_curblocks >= dqp->dq_bsoftlimit) {
 		if (dqp->dq_bwarn && --dqp->dq_bwarn)
		    uprintf(
      "\007\n\nWarning: too much disc space on %s, %d warning%s left\n\n\n\007"
			    , mount_point
			    , dqp->dq_bwarn
			    , dqp->dq_bwarn > 1 ? "s" : ""
			);
		else
		    uprintf(
	"\007\n\nWARNING: too much disc space on %s, NO MORE!!\n\n\n\007"
			, mount_point
		    );
	} else {
		if (su_dqp != NULL)
			dqp->dq_bwarn =
			  (su_dqp->dq_bwarn ? su_dqp->dq_bwarn : MAX_BQ_WARN);
	}
	dqupdate(dqp);
	dqput(su_dqp);
	return(error);
}	/* end of qwarn */
#endif /* RISCOS */

int
fdevtomp(fdev, mpp)
	char *fdev;
	struct mount **mpp;
{
	struct vnode *vp;
	dev_t dev;
	int error;

	error =
	    lookupname(fdev, UIO_USERSPACE, FOLLOW_LINK,
		(struct vnode **)0, &vp);
	if (error)
		return (error);
	if (vp->v_type != VBLK) {
		VN_RELE(vp);
		return (ENOTBLK);
	}
	dev = vp->v_rdev;
	VN_RELE(vp);
	*mpp = getmp(dev);
	if (*mpp == NULL)
		return (ENODEV);
	else
		return (0);
}
