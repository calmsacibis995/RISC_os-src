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
#ident	"$Header: acct.c,v 1.14.1.4 90/05/10 05:45:56 wje Exp $"

/*	Copyright (c) 1984 AT&T	*/
/*	  All Rights Reserved  	*/

/*	THIS IS UNPUBLISHED PROPRIETARY SOURCE CODE OF AT&T	*/
/*	The copyright notice above does not evidence any   	*/
/*	actual or intended publication of such source code.	*/

#include "sys/types.h"
#include "sys/sysmacros.h"
#include "sys/systm.h"
#include "sys/signal.h"		/* needed by user.h */
#include "sys/pcb.h"		/* needed by user.h */
#include "sys/user.h"
#include "sys/immu.h"		/* needed by proc.h */
#include "sys/sbd.h"		/* needed by proc.h */
#include "sys/region.h"		/* needed by proc.h */
#include "sys/proc.h"
#include "sys/uio.h"
#include "sys/acct.h"
#include "sys/errno.h"
#include "sys/cmn_err.h"
#include "sys/debug.h"
#include "sys/vnode.h"
#include "sys/vfs.h"
#include "bsd43/sys/syslog.h"

struct acct	acctbuf;
struct vnode	*acctp;
struct vnode	*savacctp;
struct ucred	*acctcred;
int		aclock;

/*
 * Perform process accounting functions.
 */

sysacct()
{
	struct vnode *vp;
	register struct a {
		char	*fname;
	} *uap;

	uap = (struct a *)u.u_ap;
	if (aclock || !suser())
		return;

	if (savacctp) {
		acctp = savacctp;
		savacctp = NULL;
	}
	aclock++;
	if (uap->fname == NULL) {
		if (vp = acctp) {
			acctp = NULL;
			VN_RELE(vp);
		}
	} else {
		if (acctp) {
			u.u_error = EBUSY;
			goto out;
		}
		u.u_error = lookupname(uap->fname, UIO_USERSPACE, FOLLOW_LINK,
				       (struct vnode **)0, &vp);
		if (u.u_error)
			goto out;
		if (vp->v_type != VREG) {
			u.u_error = EACCES;
			VN_RELE(vp);
			goto out;
		}
		if (vp->v_vfsp->vfs_flag & VFS_RDONLY) {
			u.u_error = EROFS;
			VN_RELE(vp);
			return;
		}
		if (acctcred)
			crfree(acctcred);
		acctcred = crdup(u.u_cred);
		acctp = vp;
		prele(vp);
	}
out:
	aclock--;
}

/*
 * On exit, write a record on the accounting file.
 */
extern int tick;

int	acctsuspend	= 2;	/* stop accounting when less than */
				/* acctsuspend percent blocks left */
int	acctresume	= 4;	/* resume accounting when more than */
				/* acctresume percent blocks left */

acct(st)
{
	register struct vnode *vp;
	off_t siz;
	struct	timeval t;
	int	i;
	struct ucred *oldcred;
	struct bsd43_statfs sb;

	if (savacctp) {
#ifdef RISCOS
		bzero((caddr_t) &sb, sizeof(sb));	/* for safety */
#endif
		(void)VFS_STATFS(savacctp->v_vfsp, &sb);
		if (sb.f_bavail > (acctresume * sb.f_blocks / 100))
		{
			acctp = savacctp;
			savacctp = NULL;
			log(BSD43_LOG_INFO, "Accounting resumed\n");
		}
	}
	if ((vp=acctp) == NULL)
		return;
	VN_HOLD(vp);
#ifdef RISCOS
	bzero((caddr_t) &sb, sizeof(sb));	/* for safety */
#endif
	(void)VFS_STATFS(acctp->v_vfsp, &sb);
	if (sb.f_bavail <= (acctsuspend * sb.f_blocks / 100))
	{
		savacctp = acctp;
		acctp = NULL;
		log(BSD43_LOG_INFO, "Accounting suspended\n");
		VN_RELE(vp);
		return;
	}
	plock(vp);
	bcopy(u.u_comm, acctbuf.ac_comm, sizeof(acctbuf.ac_comm));
	acctbuf.ac_btime = u.u_start;
	acctbuf.ac_utime = compress2(u.u_ru.ru_utime.tv_sec, u.u_ru.ru_utime.tv_usec);
	acctbuf.ac_stime = compress2(u.u_ru.ru_stime.tv_sec, u.u_ru.ru_stime.tv_usec);
	t = time;
	timevalsub(&t, (struct timeval *) &u.u_bsd43_start);
	acctbuf.ac_etime = compress2(t.tv_sec, t.tv_usec);
	t = *((struct timeval *) &u.u_ru.ru_stime);
	timevaladd(&t, (struct timeval *) &u.u_ru.ru_utime);
	if (i = t.tv_sec * HZ + t.tv_usec / tick)
		acctbuf.ac_mem = (u.u_ru.ru_ixrss+
				  u.u_ru.ru_idrss+
				  u.u_ru.ru_isrss) / i;
	else
		acctbuf.ac_mem = 0;
	acctbuf.ac_io = compress(u.u_ioch);
	acctbuf.ac_rw = compress(u.u_ior+u.u_iow);
	acctbuf.ac_uid = u.u_ruid;
	acctbuf.ac_gid = u.u_rgid;
	acctbuf.ac_tty = (u.u_procp->p_ttyvp != NULL
			  	? u.u_procp->p_ttyvp->v_rdev
				: NODEV);
	acctbuf.ac_stat = st;
	acctbuf.ac_flag = u.u_acflag;
	oldcred = u.u_cred;
	u.u_cred = acctcred;
	u.u_error = vn_rdwr(UIO_WRITE, vp, (caddr_t)&acctbuf, sizeof (acctbuf),
			    0, UIO_SYSSPACE, IO_UNIT|IO_APPEND, (int *)0);
	u.u_cred = oldcred;
	VN_RELE(vp);
	prele(vp);
}


/*
 * Produce a pseudo-floating point representation
 * with 3 bits base-8 exponent, 13 bits fraction.
 */
compress2(t, ut)
	register long t;
	long ut;
{
	t = t * HZ;
	if (ut)
		t += ut / (1000000 / HZ);
	return(compress(t));
}


compress(t)
register time_t t;
{
	register exp = 0, round = 0;

	while (t >= 8192) {
		exp++;
		round = t&04;
		t >>= 3;
	}
	if (round) {
		t++;
		if (t >= 8192) {
			t >>= 3;
			exp++;
		}
	}
	return((exp<<13) + t);
}

