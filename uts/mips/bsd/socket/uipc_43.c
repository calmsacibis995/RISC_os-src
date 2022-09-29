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
#ident	"$Header: uipc_43.c,v 1.22.1.3 90/05/10 04:34:37 wje Exp $"

/* these are miscellaneous functions used by the 4.3bsd uipc files, but
 *	found in other parts of 4.3
 *
 * $Header: uipc_43.c,v 1.22.1.3 90/05/10 04:34:37 wje Exp $
 */

#ifdef SVR3
#include "../tcp-param.h"
#include "sys/sysmacros.h"	/* needed so that we pick up correct major() */
#include "sys/types.h"
#include "sys/systm.h"
#include "sys/signal.h"
#include "sys/sbd.h"
#include "sys/pcb.h"
#include "sys/immu.h"
#include "sys/region.h"
#include "bsd/sys/time.h"
#include "sys/user.h"
#include "sys/proc.h"
#include "sys/file.h"
#include "sys/vnode.h"
#include "sys/uio.h"
#include "sys/socketvar.h"
#include "sys/poll.h"
#include "sys/errno.h"
#include "sys/var.h"
#include "sys/conf.h"
#define UNTIMEOUT untimeout_func
#define splhigh() splhi()
#else
#include "../h/param.h"
#include "../h/systm.h"
#include "../h/user.h"
#include "../h/proc.h"
#include "../h/file.h"
#include "../h/inode.h"
#include "../bsd/time.h"
#include "../h/mbuf.h"
#include "../h/uio.h"
#include "../h/socketvar.h"
#include "../streams/poll.h"
#define UNTIMEOUT untimeout
#define splhigh() splmax()
#endif
#include "../net/soioctl.h"
#include "values.h"

#define selwait pollwait		/* combine streams & select wait */

/* following is because fd_mask is signed but the algorithm wants unsigned */
#define R_ONE(x)	(((unsigned)(x)) >> 1)

extern int selwait;

extern short soc_fstyp;


uiomove(cp, n, rw, uio)
	register caddr_t cp;
	register int n;
	enum uio_rw rw;
	register struct uio *uio;
{
	register struct iovec *iov;
	u_int cnt;
	int error = 0;

	while (n > 0 && uio->uio_resid) {
		iov = uio->uio_iov;
		cnt = iov->iov_len;
		if (cnt == 0) {
			uio->uio_iov++;
			uio->uio_iovcnt--;
			continue;
		}
		if (cnt > n)
			cnt = n;
		if (cnt > (u_int)uio->uio_resid)
			cnt = (u_int)uio->uio_resid;
			
		switch (uio->uio_segflg) {

		case UIO_USERSPACE:
		case UIO_USERISPACE:
			if (rw == UIO_READ)
				error = bsd_copyout(cp, iov->iov_base, cnt);
			else
				error = bsd_copyin(iov->iov_base, cp, cnt);
			if (error)
				return (error);
			break;

		case UIO_SYSSPACE:
			if (rw == UIO_READ)
				bcopy((caddr_t)cp, iov->iov_base, cnt);
			else
				bcopy(iov->iov_base, (caddr_t)cp, cnt);
			break;
		}
		iov->iov_base += cnt;
		iov->iov_len -= cnt;
		uio->uio_resid -= cnt;
		uio->uio_offset += cnt;
		cp += cnt;
		n -= cnt;
	}
	return (error);
}

#ifndef SVR3
struct proc *
pfind(pid)
	int pid;
{
	register struct proc *p;

	/* SLOW... */
	for (p = proc; p < procNPROC; p++)
		if (p->p_pid == pid)
			return (p);
	return ((struct proc *)0);
}
#endif


long	hostid;				/* XXX move this... */

gethostid()
{

	u.u_rval1 = hostid;
}

sethostid()
{
	struct a {
		int	hostid;
	} *uap = (struct a *)u.u_ap;

	if (suser())
		hostid = uap->hostid;
}



int	unselect();
int	nselcoll;

/*
 * Select system call.
 */
select()
{
	register struct uap  {
		int	nd;
		fd_set	*in, *ou, *ex;
		struct	timeval *tv;
	} *uap = (struct uap *)u.u_ap;
	fd_set ibits[3], obits[3];
	struct timeval atv;
	long ticks;
	int s, ncoll, ni;

	bzero((caddr_t)ibits, sizeof(ibits));
	bzero((caddr_t)obits, sizeof(obits));
#ifdef SVR3
	if (uap->nd > v.v_nofiles)
		uap->nd = v.v_nofiles;	/* forgiving, if slightly wrong */
#else
	if (uap->nd > NOFILE)
		uap->nd = NOFILE;	/* forgiving, if slightly wrong */
#endif
	ni = howmany(uap->nd, NFDBITS);

#define	getbits(name, x) \
	if (uap->name) { \
		u.u_error = bsd_copyin((caddr_t)uap->name, (caddr_t)&ibits[x], \
		    (unsigned)(ni * sizeof(fd_mask))); \
		if (u.u_error) \
			goto done; \
	}
	getbits(in, 0);
	getbits(ou, 1);
	getbits(ex, 2);
#undef	getbits

	if (uap->tv) {
		u.u_error = bsd_copyin((caddr_t)uap->tv, (caddr_t)&atv,
			sizeof (atv));
		if (u.u_error)
			goto done;
		if (atv.tv_sec < 0
		    || atv.tv_sec >= MAXLONG/HZ-1
		    || atv.tv_usec < 0
		    || atv.tv_usec >= 1000000) {
			u.u_error = EINVAL;
			goto done;
		}
		ticks = atv.tv_sec*HZ + ((atv.tv_usec*HZ + (HZ/2))/1000000);
		if(ticks == 0 && atv.tv_usec != 0)
			ticks = 1;
		ticks += lbolt;
	} else {
		ticks = MAXLONG;
	}
retry:
	ncoll = nselcoll;
	u.u_procp->p_flag |= SSEL;
	u.u_rval1 = selscan(ibits, obits, uap->nd);
	if (u.u_error || u.u_rval1)
		goto done;
	s = splhigh();
	if (ticks <= lbolt) {		/* quit when users time expires */
		splx(s);
		goto done;
	}
	if ((u.u_procp->p_flag & SSEL) == 0 || nselcoll != ncoll) {
		u.u_procp->p_flag &= ~SSEL;
		splx(s);
		goto retry;
	}
	u.u_procp->p_flag &= ~SSEL;
	if (ticks < MAXLONG)
		timeout(unselect, (caddr_t)u.u_procp, (ticks-lbolt));
	if (sleep((caddr_t)&selwait, (PZERO+1)|PCATCH)) {
		if (ticks < MAXLONG)
			UNTIMEOUT(unselect, (caddr_t)u.u_procp);
		u.u_error = EINTR;
		splx(s);
		goto done;
	}
	if (ticks < MAXLONG)
		UNTIMEOUT(unselect, (caddr_t)u.u_procp);
	splx(s);
	goto retry;
done:
#define	putbits(name, x) \
	if (uap->name) { \
		int error = bsd_copyout((caddr_t)&obits[x], (caddr_t)uap->name, \
		    (unsigned)(ni * sizeof(fd_mask))); \
		if (error) \
			u.u_error = error; \
	}
	if (u.u_error == 0) {
		putbits(in, 0);
		putbits(ou, 1);
		putbits(ex, 2);
#undef putbits
	}
	strunscan(ibits, uap->nd);
}

/* tell the streams to stop worrying about polling
 */
static
strunscan(ibits, nfd)
fd_set *ibits;
int nfd;
{
	register int i, j;
	register fd_mask all;
	register struct file *fp;

	i = 0; j = 0;
	for (;;) {
		all = (ibits[0].fds_bits[i] | ibits[1].fds_bits[i] |
		       ibits[2].fds_bits[i]);
		for (;;) {
			if (j > nfd)
				return;

			if (all & 1) {
				register struct vnode *vp;
				fp = u.u_ofile[j];
				if (NULL != fp
				    && DTYPE_VNODE == fp->f_type
				    && NULL != (vp = (struct vnode *)
						fp->f_data)->v_stream
				    && VCHR == vp->v_type)
					pollreset(vp->v_stream);
			}
			all = R_ONE(all);
			if (!all) {
				j = (++i)*NFDBITS;
				if (j >= nfd)
					return;
				break;
			}
			j++;
		}
	}
}

/* scan the files, looking for one that has had something happen to it
 *	This function can poll sockets or streams.  Unfortunately, they
 *	use different mechanisms and data structures.  One could have changed
 *	either to use the others, but...
 *
 *	Therefore, this function has changed substantially.  It scans the bit
 *	masks in parallel, rather than one after the other.
 */
/* As of RISC/os 5.0, we could use the NFS4.0 code directly here.  Call
 * through the f_ops switch; that takes care of sockets.  For vnodes,
 * regular type things and fifos are handled correctly by vno_select,
 * as is.  Streams can be handled in either of 2 ways:  add a special case
 * to vno_select which tests for streams and does what this routine does,
 * or build all stream devices with d_select field pointing to a routine
 * which does the proper interfacing to strpoll.  The latter method is
 * beautifully clean, but requires major work in streams.
 */
static int
selscan(ibits, obits, nfd)
fd_set *ibits, *obits;
int nfd;
{
	register int i;			/* index among words */
	register int j;			/* index among all bits */
	fd_mask in, ou, ex;
	register fd_mask all, bit;
	register struct vnode *vp;
	register struct file *fp;
	int n;				/* # of FDs found */

	n = 0;				/* start with nothing found */
	i = 0;				/* start scan at the 1st word */
	j = 0;				/* and at 1st bit among all words */
	do {
		in = ibits[0].fds_bits[i];
		ou = ibits[1].fds_bits[i];
		ex = ibits[2].fds_bits[i];
		all = in|ou|ex;		/* get all bits of interest in word */
		bit = 1;		/* start at the 'right' */
		while (0 != all		/* scan word until boring */
		       && j < nfd) {	/* & until last valid bit */
			if (all & 1) {
				fp = u.u_ofile[j];
				if (fp == NULL) {
					u.u_error = EBADF;
					return (-1);
				}
				/* Kinda gross; this cast is not legal until
				 * after we read f_type.  This assignment
				 * should be done in the "else if" test
				 * below, but it is here for readability
				 */
				vp = (struct vnode *)fp->f_data;
				if (fp->f_type == DTYPE_SOCKET) {
					if ((bit & in)
					    && soo_select(fp,FREAD)) {
						obits[0].fds_bits[i] |= bit;
						n++;
					}
					if ((bit & ou)
					    && soo_select(fp,FWRITE)) {
						obits[1].fds_bits[i] |= bit;
						n++;
					}
					if ((bit & ex)
					    && soo_select(fp,0)) {
						obits[2].fds_bits[i] |= bit;
						n++;
					}

				} else if (vp->v_stream	/* safe with dirs */
					   && vp->v_type == VCHR) {
					register int revents;
					register short events = 0;
					if (bit & in)
						events |= POLLIN;
					if (bit & ou)
						events |= POLLOUT;
					if (bit & ex)
						events |= POLLPRI;
					revents = strpoll(vp->v_stream,
							  events, n);
					if (revents & POLLIN) {
						obits[0].fds_bits[i] |= bit;
						n++;
					}
					if (revents & POLLOUT) {
						obits[1].fds_bits[i] |= bit;
						n++;
					}
					if (revents & POLLPRI) {
						obits[1].fds_bits[i] |= bit;
						n++;
					} else if ((bit & ex) &&
					    	   (revents & POLLHUP)) {
						obits[2].fds_bits[i] |= bit;
						n++;
					}
					if (u.u_error) return (-1);

				} else if (vp->v_type == VREG
					   || vp->v_type == VDIR
					   || vp->v_type == VLNK) {
					if (bit & in) {
						obits[0].fds_bits[i] |= bit;
						n++;
					}
					if (bit & ou) {
						obits[1].fds_bits[i] |= bit;
						n++;
					}

				} else if (vp->v_type == VCHR) {
					int (*d_select)();
					dev_t rdev = vp->v_rdev;

					d_select=cdevsw[major(rdev)].d_select; 
					if (!d_select) {
						goto contin;	
					}

					if ((bit & in)
					    && d_select(rdev,FREAD)) {
						obits[0].fds_bits[i] |= bit;
						n++;
					}
					if ((bit & ou)
					    && d_select(rdev,FWRITE)) {
						obits[1].fds_bits[i] |= bit;
						n++;
					}
					if ((bit & ex)
					    && d_select(rdev,0)) {
						obits[2].fds_bits[i] |= bit;
						n++;
					}
				} else if (vp->v_type == VFIFO) {
					if ((bit & in)
					    && fifo_select(vp,FREAD,u.u_cred)) {
						obits[0].fds_bits[i] |= bit;
						n++;
					}
					if ((bit & ou)
					    && fifo_select(vp,FWRITE,u.u_cred)) {
						obits[1].fds_bits[i] |= bit;
						n++;
					}
					if ((bit & ex)
					    && fifo_select(vp,0,u.u_cred)) {
						obits[2].fds_bits[i] |= bit;
						n++;
					}
				} else {
					u.u_error = EOPNOTSUPP;
					return (-1);
				}
			}
		contin:
			all = R_ONE(all);	/* advance to the next bit */
			bit <<= 1;
			j++;
		}
		j = (++i)*NFDBITS;	/* advance to 1st bit of next word */
	} while (j < nfd);		/* quit after last valid bit */
	return (n);
}

selwakeup(p, coll)
	register struct proc *p;
	int coll;
{

	if (coll) {
		nselcoll++;
		wakeup((caddr_t)&selwait);
	}
	if (p) {
		int s = splhigh();
		if (p->p_wchan == (caddr_t)&selwait) {
			if (p->p_stat == SSLEEP)
				setrun(p);
			else
				unsleep(p);
		} else if (p->p_flag & SSEL)
			p->p_flag &= ~SSEL;
		splx(s);
	}
}

int					/* return a good errno */
bsd_copyin(from,to,len)
char *from, *to;
int len;
{
	return (copyin(from,to,len) ? EFAULT : 0);
}

#undef copyout
int					/* return a good errno */
bsd_copyout(from,to,len)
char *from, *to;
int len;
{
	return (copyout(from,to,len) ? EFAULT : 0);
}
