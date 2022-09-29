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
#ident	"$Header: strcall.c,v 1.8.1.2.1.3.1.2 90/10/16 12:17:31 beacker Exp $"

/* Stream system calls
 *
 * $Source: /mnt2/co-rel4.52/root/usr/src/uts/mips/os/RCS/strcall.c,v $
 * $Revision: 1.8.1.2.1.3.1.2 $
 * $Date: 90/10/16 12:17:31 $
 */

/* Original includes: h/param.h h/types.h h/errno.h h/file.h
 * h/inode.h h/proc.h h/user.h streams/stream.h streams/stropts.h
 * streams/poll.h streams/strcomp.h ustrm.h tcp.h
 */
#include "sys/types.h"
#include "sys/signal.h"		/* needed by user.h */
#include "sys/pcb.h"		/* needed by user.h */
#include "sys/user.h"
#include "sys/file.h"

#include "sys/sbd.h"
#include "sys/param.h"
#include "sys/conf.h"
#include "sys/errno.h"
#include "sys/file.h"
#include "sys/vnode.h"
#include "sys/immu.h"
#include "sys/pcb.h"
#include "sys/region.h"
#include "bsd/sys/time.h"
#include "sys/proc.h"

#include "sys/stream.h"
#include "sys/stropts.h"
#include "sys/poll.h"
#include "sys/var.h"
#include "sys/debug.h"

#define NFILE v.v_nofiles
#define UNTIMEOUT_ID untimeout

#if NUSTRM > 0
/*
 * getmsg system call
 */
#ifdef RISCOS
getmsg()
#else
getstrmsg()
#endif
{
	msgio(FREAD);
}

/*
 * putmsg system call
 */
#ifdef RISCOS
putmsg()
#else
putstrmsg()
#endif
{
	msgio(FWRITE);
}

/*
 * common code for recv and send calls:
 * check permissions, copy in args, preliminary setup,
 * and switch to appropriate stream routine
 */
msgio(mode)
register mode;
{
	register struct file *fp;
	register struct vnode *vp;
	register struct a {
		int		fdes;
		struct strbuf	*ctl;
		struct strbuf	*data;
		int		flags;
	} *uap;
	struct strbuf msgctl, msgdata;

	uap = (struct a *)u.u_ap;
	if ((fp = getf(uap->fdes)) == NULL)
		return;
	if (!(fp->f_flag&mode)) {
		u.u_error = EBADF;
		return;
	}
	u.u_segflg = 0;
	u.u_fmode = fp->f_flag;
	vp = (struct vnode *)fp->f_data;
	if ((fp->f_type != DTYPE_VNODE) || (vp->v_type != VCHR)
	    || (vp->v_stream == NULL)) {
		u.u_error = ENOSTR;
		return;
	}
	if (uap->ctl) {
		if (copyin((caddr_t)uap->ctl, (caddr_t)&msgctl,
		    sizeof(struct strbuf))) {
			u.u_error = EFAULT;
			return;
		}
	}
	if (uap->data) {
		if (copyin((caddr_t)uap->data, (caddr_t)&msgdata,
		   sizeof(struct strbuf))) {
			u.u_error = EFAULT;
			return;
		}
	}
	if (mode == FREAD) {
		if (!uap->ctl)
			msgctl.maxlen = -1;
		if (!uap->data)
			msgdata.maxlen = -1;
		u.u_rval1 = strgetmsg(vp, &msgctl, &msgdata, uap->flags);
		if (u.u_error)
			return;
		if ((uap->ctl && copyout((caddr_t)&msgctl, (caddr_t)uap->ctl, sizeof(struct strbuf))) ||
		    (uap->data && copyout((caddr_t)&msgdata, (caddr_t)uap->data, sizeof(struct strbuf)))) {
			u.u_error = EFAULT;
			return;
		}
		return;
	}
	/*
	 * FWRITE case
	 */
	if (!uap->ctl)
		msgctl.len = -1;
	if (!uap->data)
		msgdata.len = -1;
	strputmsg(vp, &msgctl, &msgdata, uap->flags);
}

/*
 * Poll system call
 */

poll()
{
	register struct uap  {
		struct	pollfd *fdp;
		unsigned long nfds;
		long	timo;
	} *uap = (struct uap *)u.u_ap;
	struct pollfd pollfd[NPOLLFILE];
	caddr_t fdp;
	register fdcnt = 0;
	register int i, j, s;
	extern time_t lbolt;
	time_t t;
	int rem;
	struct file *fp;
	int polltime();
	struct strevent *timeproc;
	struct vnode *vp;
	int id, size;
	int mark;

	if (uap->nfds > NFILE) {
		u.u_error = EINVAL;
		return;
	}
	t = lbolt;

	/*
	 * retry scan of fds until an event is found or until the
	 * timeout is reached.
	 */
retry:

	/*
	 * Polling the fds is a relatively long process.  Set up the SPOLL
	 * flag so that we can see if something happened
	 * to an fd after we checked it but before we go to sleep.
	 */
	u.u_procp->p_flag |= SPOLL;

	/*
	 * Check fd's for specified events.
	 * Read in pollfd records in blocks of NPOLLFILE.  Test each fd in the
	 * block and store the result of the test in the event field of the
	 * in-core record.  After a block of fds is finished, write the result
	 * out to the user.  Note that if no event is found, the whole
	 * procedure will be repeated after awakenening from the sleep
	 * (subject to timeout).
	 */

	mark = uap->nfds;
	size = 0;
	fdp = (caddr_t)uap->fdp;
	for (i = 0; i < uap->nfds; i++) {
		j = i % NPOLLFILE;
		/*
		 * If we have looped back around to the base of pollfd,
		 * write out the results of the strpoll calls kept in pollfd
		 * to the user fdp.  Read in the next batch of fds to check.
		 */
		if (!j) {
			if (i > 0) {
				ASSERT(size==NPOLLFILE*sizeof(struct pollfd));
				if (copyout(pollfd, fdp, size)) {
					u.u_procp->p_flag &= ~SPOLL;
					u.u_error = EFAULT;
					return;
				}
				fdp += size;
			}
			size = (min(uap->nfds - i, NPOLLFILE)
				* sizeof(struct pollfd));
			if (copyin(fdp, (caddr_t)pollfd, size)) {
				u.u_procp->p_flag &= ~SPOLL;
				u.u_error = EFAULT;
				return;
			}
		}

		if (pollfd[j].fd < 0)
			pollfd[j].revents = 0;
		else if ( (pollfd[j].fd >= NFILE) ||
			  !(fp = u.u_ofile[pollfd[j].fd]) ||
			   (fp->f_type != DTYPE_VNODE) ||
			  !(vp = (struct vnode *)fp->f_data) ||
			   (vp->v_type != VCHR) ||
			  !(vp->v_stream))
			pollfd[j].revents = POLLNVAL;
		else {
			pollfd[j].revents = strpoll(vp->v_stream,
						     pollfd[j].events, fdcnt);
			if (u.u_error) {
				if (!fdcnt) mark = i;
				goto pollout;
			}
		}
		if (pollfd[j].revents && !fdcnt++) mark = i;
	}

	/*
	 * Poll of fds completed.
	 * Copy out the last batch of events.  If the poll was successful,
	 * return fdcnt to user.
	 */
	u.u_rval1 = fdcnt;
	if (copyout((caddr_t)pollfd, fdp, size)) {
		u.u_procp->p_flag &= ~SPOLL;
		u.u_error = EFAULT;
		return;
	}
	if (fdcnt)
		goto pollout;

	/*
	 * If you get here, the poll of fds was unsuccessful.
	 * First make sure your timeout hasn't been reached.
	 * If not then sleep and wait until some fd becomes
	 * readable, writeable, or gets an exception.
	 */
	rem = ( (uap->timo < 0) ? 1 : (uap->timo - ((lbolt - t)*1000)/HZ) );
	if (rem <= 0)
		goto pollout;

	s = splclock();
	/*
	 * If anything has happened on an fd since it was checked, it will
	 * have turned off SPOLL.  Check this and rescan if so.
	 */
	if (!(u.u_procp->p_flag & SPOLL)) {
		splx(s);
		goto retry;
	}
	u.u_procp->p_flag &= ~SPOLL;

	if (!(timeproc = sealloc(SE_SLEEP))) {
		splx(s);
		u.u_error = EAGAIN;
		goto pollout;
	}
	timeproc->se_procp = u.u_procp;
	if (uap->timo > 0)
		id = timeout(polltime, (caddr_t)timeproc, (rem*HZ+999)/1000);

	/*
	 * The sleep will usually be awakened either by this poll's timeout
	 * (which will have nulled timeproc), or by the strwakepoll function
	 * called from a stream head.
	 */
	if (sleep((caddr_t)&pollwait, (PZERO+1)|PCATCH)) {
		if (uap->timo > 0)
			UNTIMEOUT_ID(id);
		splx(s);
		u.u_error = EINTR;
		sefree(timeproc);
		goto pollout;
	}
	splx(s);
	if (uap->timo > 0)
		UNTIMEOUT_ID(id);

	/*
	 * If timeproc is not NULL, you were probably awakened because a
	 * write queue emptied, a read queue got data, or an exception
	 * condition occurred.  If so go back up and poll fds again.
	 * Otherwise, you've timed out so you will fall thru and return.
	 */
	if (timeproc->se_procp) {
		sefree(timeproc);
		goto retry;
	}
	sefree(timeproc);

pollout:

	/*
	 * Poll general cleanup code. Go back to all of the streams
	 * before the mark and reset the wakeup mechanisms that were
	 * set up during the poll.
	 */
	u.u_procp->p_flag &= ~SPOLL;
	fdp = (caddr_t)uap->fdp;
	for (i = 0; i < mark; i++) {
		j = i % NPOLLFILE;
		/*
		 * Read in next block of pollfds.  If the total number of pollfds
		 * is less than NPOLLFILE, don't bother because the pollfds of
		 * interest are still in the pollfd[] array.
		 */
		if (!j && (uap->nfds > NPOLLFILE)) {
			size = min(uap->nfds - i, NPOLLFILE) * sizeof(struct pollfd);
			if (copyin((caddr_t)uap->fdp, (caddr_t)pollfd, size)) {
				u.u_error = EFAULT;
				return;
			}
			fdp += size;
		}

		/*
		 * should use getf, but getf sets u.u_error
		 */
		if ( (pollfd[j].fd < 0) ||
		     (pollfd[j].fd >= NFILE) ||
		    !(fp = u.u_ofile[pollfd[j].fd]) ||
		     (fp->f_type != DTYPE_VNODE) ||
		    !(vp = (struct vnode *)fp->f_data) ||
		     (vp->v_type != VCHR) ||
		    !(vp->v_stream))
			continue;

		pollreset(vp->v_stream);
	}
}

/*
 * This function is placed in the callout table to time out a process
 * waiting on poll.  If the poll completes, this function is removed
 * from the table.  Its argument is a pointer to a variable which holds
 * the process table pointer for the process to be awakened.  This
 * variable is nulled to indicate that polltime ran.
 */
polltime(timeproc)
struct strevent *timeproc;
{
	register struct proc *p = timeproc->se_procp;

	if (p->p_wchan == (caddr_t)&pollwait) {
		unselect(p);
		timeproc->se_procp = NULL;
	}
}
#endif
