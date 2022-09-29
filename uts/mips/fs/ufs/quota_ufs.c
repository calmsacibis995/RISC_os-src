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
#ident	"$Header: quota_ufs.c,v 1.2.1.2 90/05/10 05:07:26 wje Exp $"
/*
 * SUN QUOTAS
 *
 * Routines used in checking limits on file system usage.
 */
/* Original includes: param.h systm.h user.h proc.h kernel.h vfs.h vnode.h */
/* buf.h uio.h ../ufs/quota.h ../ufs/inode.h ../ufs/mount.h ../ufs/fs.h */

#include "sys/types.h"
#include "sys/errno.h"
#include "sys/systm.h"
#include "sys/signal.h"		/* needed by user.h */
#include "sys/pcb.h"		/* needed by user.h */
#include "sys/user.h"
#include "sys/sbd.h"
#include "sys/immu.h"
#include "sys/region.h"
#include "sys/proc.h"
#include "sys/buf.h"
#include "sys/vfs.h"
#include "sys/vnode.h"
#include "sys/uio.h"
#include "sys/fs/ufs_quota.h"
#include "sys/fs/ufs_inode.h"
#include "sys/fs/ufs_mount.h"
#include "sys/fs/ufs_fs.h"

/*
 * Find the dquot structure that should
 * be used in checking i/o on inode ip.
 */
struct dquot *
getinoquota(ip)
	register struct inode *ip;
{
	register struct dquot *dqp;
	register struct mount *mp;
	struct dquot *xdqp;

	mp = VFSTOM(ip->i_vnode.v_vfsp);
	/*
	 * Check for quotas enabled.
	 */
	if ((mp->m_qflags & MQ_ENABLED) == 0)
		return (NULL);
	/*
	 * Check for someone doing I/O to quota file.
	 */
	if (ip == mp->m_qinod)
		return (NULL);
	if (getdiskquota(ip->i_uid, mp, 0, &xdqp))
		return (NULL);
	dqp = xdqp;
	if (dqp->dq_fhardlimit == 0 && dqp->dq_fsoftlimit == 0 &&
	    dqp->dq_bhardlimit == 0 && dqp->dq_bsoftlimit == 0) {
		dqput(dqp);
		dqp = NULL;
	} else {
		DQUNLOCK(dqp);
	}
	return (dqp);
}

/*
 * Update disk usage, and take corrective action.
 */
int
chkdq(ip, change, force)
	struct inode *ip;
	long change;
	int force;
{
	register struct dquot *dqp;
	register u_long ncurblocks;
	register struct mount *mp;
	struct dquot *su_dqp;
	int error = 0;

	if (change == 0)
		return (0);
	dqp = ip->i_dquot;
	if (dqp == NULL)
		return (0);
	dqp->dq_flags |= DQ_MOD;
	/*
	 * Could check for dqp->dq_bsoftlimit == 0, but this routine
	 * shouldn't be called if people don't have quotas.
	 */
	if (change < 0) {
		if ((int)dqp->dq_curblocks + change >= 0)
			dqp->dq_curblocks += change;
		else
			dqp->dq_curblocks = 0;	/* (overflow == panic)? */
#if defined(RISCOS) && defined(TAHOE_QUOTA)
		if (dqp->dq_curblocks < dqp->dq_bsoftlimit) {
			if (ip->i_dquot == NULL)
		  		panic("chkdq");
			mp = ip->i_dquot->dq_mp;
			if ((error = getdiskquota(0, mp, 1, &su_dqp)) != 0)
				return(error);
			if (su_dqp != NULL)
				dqp->dq_bwarn =	(su_dqp->dq_bwarn ?
						 su_dqp->dq_bwarn :
						 MAX_BQ_WARN);
			dqput(su_dqp);
		}
#else /* !RISCOS */
		if (dqp->dq_curblocks < dqp->dq_bsoftlimit)
			dqp->dq_btimelimit = 0;
#endif /* RISCOS */
		dqp->dq_flags &= ~DQ_BLKS;
		return (0);
	}

	ncurblocks = dqp->dq_curblocks + change;
	/*
	 * Allocation. Check hard and soft limits.
	 * Skip checks for super user.
	 */
	if (u.u_uid == 0)
		goto out;
#if defined(RISCOS) && defined(TAHOE_QUOTA)
	/*
	 * If user has run out of warnings, disallow space allocation.
	 * This case is not really needed and may be a bit harsh but is
	 * added for Tahoe compatibility.  User's will always be allowed
	 * to login; but once this count goes to 0, no more writes are
	 * allowed.
	 */
	if (!force && dqp->dq_bwarn == 0) {
		if ((dqp->dq_flags & DQ_BLKS) == 0  && ip->i_uid == u.u_ruid) {
			uprintf("\nOVER DISC QUOTA: (%s) NO MORE DISC SPACE\n",
				ip->i_fs->fs_fsmnt);
			dqp->dq_flags |= DQ_BLKS;
		}
		error = EDQUOT;
		goto out;
	}
	if (ncurblocks < dqp->dq_bsoftlimit)	/* optimize: the common case */
		goto out;			/* Maybe should go 1st */
#endif /* RISCOS */
	/*
	 * Dissallow allocation if it would bring the current usage over
	 * the hard limit or if the user is over his soft limit and his time
	 * has run out.
	 */
	if (ncurblocks >= dqp->dq_bhardlimit && dqp->dq_bhardlimit && !force) {
		if ((dqp->dq_flags & DQ_BLKS) == 0 && ip->i_uid == u.u_ruid) {
			uprintf("\nDISK LIMIT REACHED (%s) - WRITE FAILED\n",
			   ip->i_fs->fs_fsmnt);
			dqp->dq_flags |= DQ_BLKS;
		}
		error = EDQUOT;
	}
	if (ncurblocks >= dqp->dq_bsoftlimit && dqp->dq_bsoftlimit) {
#if defined(RISCOS) && defined(TAHOE_QUOTA)
		if (dqp->dq_curblocks < dqp->dq_bsoftlimit) {
			if (ip->i_uid == u.u_ruid)
			       uprintf("\nWARNING: disc quota (%s) exceeded\n",
					ip->i_fs->fs_fsmnt);
			goto out;
		}
#else
		if (dqp->dq_curblocks < dqp->dq_bsoftlimit ||
		    dqp->dq_btimelimit == 0) {
			dqp->dq_btimelimit =
			    time.tv_sec +
			    VFSTOM(ip->i_vnode.v_vfsp)->m_btimelimit;
			if (ip->i_uid == u.u_ruid)
			       uprintf("\nWARNING: disk quota (%s) exceeded\n",
				   ip->i_fs->fs_fsmnt);
		} else if (time.tv_sec > dqp->dq_btimelimit && !force) {
			if ((dqp->dq_flags & DQ_BLKS) == 0 &&
			    ip->i_uid == u.u_ruid) {
				uprintf(
				"\nOVER DISK QUOTA: (%s) NO MORE DISK SPACE\n",
				ip->i_fs->fs_fsmnt);
				dqp->dq_flags |= DQ_BLKS;
			}
			error = EDQUOT;
		}
#endif /* RISCOS */
	}
out:
	if (error == 0)
		dqp->dq_curblocks = ncurblocks;
	return (error);
}

/*
 * Check the inode limit, applying corrective action.
 */
int
chkiq(mp, ip, uid, force)
	struct mount *mp;
	struct inode *ip;
	int uid;
	int force;
{
	register struct dquot *dqp;
	register u_long ncurfiles;
	struct dquot *su_dqp;
	struct dquot *xdqp;
	int error = 0;

	/*
	 * Free.
	 */
	if (ip != NULL) {
		dqp = ip->i_dquot;
		if (dqp == NULL)
			return (0);
		dqp->dq_flags |= DQ_MOD;
		if (dqp->dq_curfiles)
			dqp->dq_curfiles--;
#if defined(RISCOS) && defined(TAHOE_QUOTA)
		if (dqp->dq_curfiles < dqp->dq_fsoftlimit) {
			if ((error = getdiskquota(0, mp, 1, &su_dqp)) != 0)
				return(error);
			if (su_dqp != NULL)
			dqp->dq_fwarn =	(su_dqp->dq_fwarn ?
					 su_dqp->dq_fwarn :
					 MAX_FQ_WARN);
			dqput(su_dqp);
		}
#else
		if (dqp->dq_curfiles < dqp->dq_fsoftlimit)
			dqp->dq_ftimelimit = 0;
#endif /* RISCOS */
		dqp->dq_flags &= ~DQ_FILES;
		return (0);
	}

	/*
	 * Allocation. Get dquot for for uid, fs.
	 * Check for quotas enabled.
	 */
	if ((mp->m_qflags & MQ_ENABLED) == 0)
		return (0);
	if (getdiskquota(uid, mp, 0, &xdqp))
		return (0);
	dqp = xdqp;
	if (dqp->dq_fsoftlimit == 0 && dqp->dq_fhardlimit == 0) {
		dqput(dqp);
		return (0);
	}
	dqp->dq_flags |= DQ_MOD;
	/*
	 * Skip checks for super user.
	 */
	if (u.u_uid == 0)
		goto out;
	ncurfiles = dqp->dq_curfiles + 1;
#if defined(RISCOS) && defined(TAHOE_QUOTA)
	if (!force && dqp->dq_fwarn == 0) {
		if ((dqp->dq_flags & DQ_FILES) == 0 && uid == u.u_ruid) {
			uprintf("\nOVER FILE QUOTA - NO MORE FILES (%s)\n",
				mp->m_bufp->b_un.b_fs->fs_fsmnt);
			dqp->dq_flags |= DQ_FILES;
		}
		error = EDQUOT;
		goto out;
	}
	if (ncurfiles < dqp->dq_fsoftlimit)	/* optimize the common case */
		goto out;			/* Maybe should go 1st */
#endif /* RISCOS */
	/*
	 * Dissallow allocation if it would bring the current usage over
	 * the hard limit or if the user is over his soft limit and his time
	 * has run out.
	 */
	if (ncurfiles >= dqp->dq_fhardlimit && dqp->dq_fhardlimit && !force) {
		if ((dqp->dq_flags & DQ_FILES) == 0 && uid == u.u_ruid) {
			uprintf("\nFILE LIMIT REACHED - CREATE FAILED (%s)\n",
			    mp->m_bufp->b_un.b_fs->fs_fsmnt);
			dqp->dq_flags |= DQ_FILES;
		}
		error = EDQUOT;
	} else if (ncurfiles >= dqp->dq_fsoftlimit && dqp->dq_fsoftlimit) {
#if defined(RISCOS) && defined(TAHOE_QUOTA)
		if (ncurfiles >= dqp->dq_fsoftlimit) {
			if (uid == u.u_ruid)
				uprintf("\nWARNING - too many files (%s)\n",
				    mp->m_bufp->b_un.b_fs->fs_fsmnt);
		}
#else
		if (ncurfiles == dqp->dq_fsoftlimit || dqp->dq_ftimelimit==0) {
			dqp->dq_ftimelimit = time.tv_sec + mp->m_ftimelimit;
			if (uid == u.u_ruid)
				uprintf("\nWARNING - too many files (%s)\n",
				    mp->m_bufp->b_un.b_fs->fs_fsmnt);
		} else if (time.tv_sec > dqp->dq_ftimelimit && !force) {
			if ((dqp->dq_flags&DQ_FILES) == 0 && uid == u.u_ruid) {
				uprintf(
				    "\nOVER FILE QUOTA - NO MORE FILES (%s)\n",
				    mp->m_bufp->b_un.b_fs->fs_fsmnt);
				dqp->dq_flags |= DQ_FILES;
			}
			error = EDQUOT;
		}
#endif /* RISCOS */
	}
out:
	if (error == 0)
		dqp->dq_curfiles++;
	dqput(dqp);
	return (error);
}

/*
 * Release a dquot.
 */
void
dqrele(dqp)
	register struct dquot *dqp;
{

	if (dqp != NULL) {
		DQLOCK(dqp);
		if (dqp->dq_cnt == 1 && dqp->dq_flags & DQ_MOD)
			dqupdate(dqp);
		dqput(dqp);
	}
}
