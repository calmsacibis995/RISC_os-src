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
#ident	"$Header: quota.c,v 1.3.1.2.1.1.1.2 91/01/14 19:27:49 beacker Exp $"
/*
 * Code pertaining to management of the in-core data structures.
 */
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
#include "sys/vfs.h"
#include "sys/vnode.h"
#include "sys/uio.h"
#include "sys/fs/ufs_quota.h"
#include "sys/fs/ufs_inode.h"
#include "sys/fs/ufs_mount.h"
#include "sys/fs/ufs_fs.h"

#if defined(RISCOS)
#define insque(q,p)	_insque((caddr_t)(q), (caddr_t)(p))
#define remque(q)	_remque((caddr_t)(q))

#include "sys/kmem.h"
extern int _riscos_max_dquot_cache;	/* kopt'able */
int num_free_dquot = 0;			/* on dqfreelist */

#else /* !RISCOS: added for porting */
#define NDQUOT 5		/* must be 1 per user per mount point */
struct dquot dquot[NDQUOT], *dquotNDQUOT;

#endif /* RISCOS */

#if !defined(RISCOS)	/* defined in sys/fs/ufs_quota.h now */
/*
 * Dquot cache - hash chain headers.
 */
#define	NDQHASH		64		/* smallish power of two */
#define	DQHASH(uid, mp) \
	(((unsigned)(mp) + (unsigned)(uid)) & (NDQHASH-1))

struct	dqhead	{
	struct	dquot	*dqh_forw;	/* MUST be first */
	struct	dquot	*dqh_back;	/* MUST be second */
};
#endif /* RISCOS */

/*
 * Dquot in core hash chain headers
 */
struct	dqhead	dqhead[NDQHASH];

/*
 * Dquot free list.
 */
struct dquot dqfreelist;

#define dqinsheadfree(DQP) { \
	(DQP)->dq_freef = dqfreelist.dq_freef; \
	(DQP)->dq_freeb = &dqfreelist; \
	dqfreelist.dq_freef->dq_freeb = (DQP); \
	dqfreelist.dq_freef = (DQP); \
}

#define dqinstailfree(DQP) { \
	(DQP)->dq_freeb = dqfreelist.dq_freeb; \
	(DQP)->dq_freef = &dqfreelist; \
	dqfreelist.dq_freeb->dq_freef = (DQP); \
	dqfreelist.dq_freeb = (DQP); \
}

#define dqremfree(DQP) { \
	(DQP)->dq_freeb->dq_freef = (DQP)->dq_freef; \
	(DQP)->dq_freef->dq_freeb = (DQP)->dq_freeb; \
}

#if !defined(RISCOS)		/* defined in sys/fs/ufs_quota.h now */
typedef	struct dquot *DQptr;
#endif

/*
 * Initialize quota caches.
 */
void
qtinit()
{
	register struct dqhead *dhp;
	register struct dquot *dqp;
	register int i;

	/*
	 * Initialize the cache between the in-core structures
	 * and the per-file system quota files on disk.
	 */
	for (dhp = &dqhead[0]; dhp < &dqhead[NDQHASH]; dhp++) {
		dhp->dqh_forw = dhp->dqh_back = (DQptr)dhp;
	}
	dqfreelist.dq_freef = dqfreelist.dq_freeb = (DQptr)&dqfreelist;
#if defined(RISCOS)
	num_free_dquot = 0;	/* start with zero on list */
#else
	dquotNDQUOT = &dquot[NDQUOT];
	for (dqp = dquot; dqp < dquotNDQUOT; dqp++) {
		dqp->dq_forw = dqp->dq_back = dqp;
		dqinsheadfree(dqp);
	}
#endif /* RISCOS */
}

/*
 * Obtain the user's on-disk quota limit for file system specified.
 */
int
getdiskquota(uid, mp, force, dqpp)
	register int uid;
	register struct mount *mp;
	int force;			/* don't do enable checks */
	struct dquot **dqpp;		/* resulting dquot ptr */
{
	register struct dquot *dqp;
	register struct dqhead *dhp;
	register struct inode *qip;
	int error;

	dhp = &dqhead[DQHASH(uid, mp)];
loop:
	/*
	 * Check for quotas enabled.
	 */
	if ((mp->m_qflags & MQ_ENABLED) == 0 && !force)
		return (ESRCH);
	qip = mp->m_qinod;
	if (qip == NULL)
		panic("getdiskquota");
	/*
	 * Check the cache first.  Yes, the count can be greater than
	 * one.  Specifically, when files are added, we don't
	 * decrement the count right away.  Also, one of these structures
	 * is attached to each active inode (in the inode cache).  Therefore,
	 * each file owned by the same user on the same partition will have
	 * the same dqp attached to it.  The cnt field represents the # of
	 * active inodes fitting into this category.
	 */
	for (dqp = dhp->dqh_forw; dqp != (DQptr)dhp; dqp = dqp->dq_forw) {
		if (dqp->dq_uid != uid || dqp->dq_mp != mp)
			continue;
		if (dqp->dq_flags & DQ_LOCKED) {
			dqp->dq_flags |= DQ_WANT;
#ifdef RISCOS
			/* Increment count to make sure it doesn't get freed */
			dqp->dq_cnt++;
			(void) sleep((caddr_t)dqp, PINOD+1);
			dqp->dq_cnt--;
#else
			(void) sleep((caddr_t)dqp, PINOD+1);
#endif /* RISCOS */
			goto loop;
		}
		dqp->dq_flags |= DQ_LOCKED;

		/*
		 * Cache hit with no references.
		 * Take the structure off the free list.
		 */
		if (dqp->dq_cnt == 0) {
			dqremfree(dqp);
#ifdef RISCOS
			num_free_dquot--;
#endif
		}
		dqp->dq_cnt++;
		*dqpp = dqp;
		return (0);
	}
#if defined(RISCOS)
	/*
	 * Not in cache.  Get dquot at head of free list unless we
	 * are under the maximum allowed.  Initialize as in qtinit().
	 * This can probably be optimized, so we don't put it on the
	 * list and then take it off further below.
	 */
	if ((dqp = dqfreelist.dq_freef) == &dqfreelist ||
	    num_free_dquot < _riscos_max_dquot_cache) {
		dqp = (struct dquot *) kmemzalloc(sizeof(struct dquot),
						  M_QUOTA, M_NOWAIT);
		if (dqp == NULL) {
			tablefull("dquot");
			return (EUSERS);
		}
		dqp->dq_forw = dqp->dq_back = dqp;
		dqinsheadfree(dqp);
	} else {
		num_free_dquot--;
	}

	/* If person A gets a disk quota warning and then exits, one of the
	 * flags, DQ_BLKS or DQ_FILES, will be set.  Now if we run a find
	 * over another non-quota'ed partition such that the inode cache
	 * fills up, we will release person A's dqp.  If the dqp cache isn't
	 * filled up, this dqp will be put on the dqfreelist.  Now if we
	 * run through another file system which has quotas turned on, we
	 * eventually use the dqp which didn't zero its dq_flags field.
	 * Therefore, we don't panic when we see a DQ_BLKS or DQ_FILES
	 * flag.  However, we shouldn't see any of the below flags.
	 */
	if (dqp->dq_cnt != 0 || (dqp->dq_flags & (DQ_LOCKED|DQ_WANT|DQ_MOD)))
		panic("getdiskquota: Illegal count or flags");
#else /* !RISCOS */
	if ((dqp = dqfreelist.dq_freef) == &dqfreelist) {
		tablefull("dquot");
		return (EUSERS);
	}
	if (dqp->dq_cnt != 0 || dqp->dq_flags != 0)
		panic("diskquota");
#endif /* RISCOS */
	/*
	 * Take it off the free list, and off the hash chain it was on.
	 * Then put it on the new hash chain.
	 */
	dqremfree(dqp);
	remque(dqp);
	dqp->dq_flags = DQ_LOCKED;
	dqp->dq_cnt = 1;
	dqp->dq_uid = uid;
	dqp->dq_mp = mp;
	insque(dqp, dhp);
	if (dqoff(uid) < qip->i_size) {
		/*
		 * Read quota info off disk.
		 */
		error = rdwri(UIO_READ, qip, (caddr_t)&dqp->dq_dqb,
		    sizeof (struct dqblk), dqoff(uid), UIO_SYSSPACE, (int *)0);
		if (error) {
			/*
			 * I/O error in reading quota file.
			 * Put dquot on a private, unfindable hash list,
			 * put dquot at the head of the free list and
			 * reflect the problem to caller.
			 */
			remque(dqp);
			dqp->dq_cnt = 0;
			dqp->dq_mp = NULL;
#if defined(RISCOS)
			if (++num_free_dquot > _riscos_max_dquot_cache) {
				dqp->dq_forw = dqp->dq_back = NULL;
				kmemfree(dqp, M_QUOTA, M_NOWAIT);
				num_free_dquot--;
			} else {
				dqp->dq_forw = dqp->dq_back = dqp;
				dqinsheadfree(dqp);
				DQUNLOCK(dqp);
			}
#else
			dqp->dq_forw = dqp;
			dqp->dq_back = dqp;
			dqinsheadfree(dqp);
			DQUNLOCK(dqp);
#endif /* RISCOS */
			return (EIO);
		}
	} else {
		bzero((caddr_t)&dqp->dq_dqb, sizeof (struct dqblk));
	}
	*dqpp = dqp;
	return (0);
}

/*
 * Release dquot.
 */
void
dqput(dqp)
	register struct dquot *dqp;
{

	if (dqp->dq_cnt == 0 || (dqp->dq_flags & DQ_LOCKED) == 0)
		panic("dqput");
#if defined(RISCOS)
	if (--dqp->dq_cnt == 0) {
		if (++num_free_dquot > _riscos_max_dquot_cache) {
			remque(dqp);
			dqp->dq_forw = dqp->dq_back = NULL;
			kmemfree(dqp, M_QUOTA, M_NOWAIT);
			num_free_dquot--;
		} else {
			dqinstailfree(dqp);
		}
	}
#else
	if (--dqp->dq_cnt == 0)
		dqinstailfree(dqp);
#endif /* RISCOS */
	DQUNLOCK(dqp);
}	/* end of dqput */

/*
 * Update on disk quota info.
 */
void
dqupdate(dqp)
	register struct dquot *dqp;
{
	register struct inode *qip;

	qip = dqp->dq_mp->m_qinod;
	if (qip == NULL ||
	   (dqp->dq_flags & (DQ_LOCKED|DQ_MOD)) != (DQ_LOCKED|DQ_MOD))
		panic("dqupdate");
	dqp->dq_flags &= ~DQ_MOD;
	(void) rdwri(UIO_WRITE, qip, (caddr_t)&dqp->dq_dqb,
	    sizeof (struct dqblk), dqoff(dqp->dq_uid), UIO_SYSSPACE, (int *)0);
}

/*
 * Invalidate a dquot.
 * Take the dquot off its hash list and put it on a private,
 * unfindable hash list. Also, put it at the head of the free list.
 */
dqinval(dqp)
	register struct dquot *dqp;
{

	if (dqp->dq_cnt || (dqp->dq_flags & (DQ_MOD|DQ_WANT)) )
		panic("dqinval");
	dqp->dq_flags = 0;
	remque(dqp);
	dqremfree(dqp);
	dqp->dq_mp = NULL;
	dqp->dq_forw = dqp;
	dqp->dq_back = dqp;
	dqinsheadfree(dqp);
}
