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
#ident	"$Header: spty.c,v 1.33.1.3.1.2.1.4 91/01/28 18:10:53 beacker Exp $"

/* Pseudo-teletype Driver
 *	(Actually two drivers, requiring two entries in 'cdevsw')
 *
 */

#include "sys/sbd.h"
#include "sys/cpu_board.h"
#include "sys/param.h"
#include "sys/cmn_err.h"
#include "sys/sysmacros.h"
#include "sys/types.h"
#include "sys/conf.h"
#include "sys/systm.h"
#include "sys/termio.h"
#include "sys/errno.h"
#include "sys/ioctl.h"
#include "sys/signal.h"
#include "sys/pcb.h"
#include "sys/immu.h"
#include "sys/region.h"
#include "sys/fs/s5dir.h"
#include "bsd/sys/time.h"
#include "sys/user.h"
#include "sys/file.h"
#include "sys/proc.h"
#include "sys/buf.h"
#include "sys/debug.h"
#include "sys/pty_ioctl.h"
#include "sys/stream.h"
#include "sys/stropts.h"
#include "sys/strids.h"
#include "sys/stty_ld.h"
#include "bsd43/sys/ioctl.h"
#include "sys/kmem.h"

/* Map user level memory allocation interface to the one supported
 * by the kernel.
 */

#define free(addr) 		kmemfree(addr, M_MISC, M_WAITOK)
#define malloc(sz)		kmemalloc(sz, M_MISC, M_WAITOK)
#define calloc(nelem,elsz) 	kmemzalloc((nelem)*(elsz), M_MISC, M_WAITOK)

extern mblk_t *str_allocb();

extern struct stty_ld def_stty_ld;


/* controller stream stuff
 */
static struct module_info ptcm_info = {
	STRID_PTC,			/* module ID */
	"PTC",				/* module name */
	0,				/* minimum packet size */
	INFPSZ,				/* infinite maximum packet size */
	256,				/* hi-water mark */
	0,				/* lo-water mark */
};

static int ptc_open();
static ptc_rsrv(), ptc_close();
static struct qinit ptc_rinit = {
	NULL, ptc_rsrv, ptc_open, ptc_close, NULL, &ptcm_info, NULL
};

static ptc_wput(), ptc_wsrv();
static struct qinit ptc_winit = {
	ptc_wput, ptc_wsrv, NULL, NULL, NULL, &ptcm_info, NULL
};

struct streamtab ptcinfo = {&ptc_rinit, &ptc_winit, NULL, NULL};


/* slave stream stuff
 */
static struct module_info ptsm_info = {
	STRID_PTS,			/* module ID */
	"PTS",				/* module name */
	0,				/* minimum packet size */
	INFPSZ,				/* infinite maximum packet size */
	256,				/* hi-water mark */
	0,				/* lo-water mark */
};

static int pts_open();
static pts_rsrv(), pts_close();
static struct qinit pts_rinit = {
	NULL, pts_rsrv, pts_open, pts_close, NULL, &ptsm_info, NULL
};

int pts_wput();
static pts_wsrv();
static struct qinit pts_winit = {
	pts_wput, pts_wsrv, NULL, NULL, NULL, &ptsm_info, NULL
};

struct streamtab ptsinfo = {&pts_rinit, &pts_winit, NULL, NULL};

#define SWINSIZE(bp)	((struct winsize *)(bp)->b_cont->b_rptr)



/*
 * We allocate one of these structures for every open ptc/tty pair
 */
struct pty {
	queue_t	*pts_rq, *pts_wq;	/* slave queues			*/
	queue_t	*ptc_rq, *ptc_wq;	/* controller queues		*/

	long	pt_ic;			/* handle for graphics queue	*/

	struct termio pt_termio;
#define pt_line pt_termio.c_line
#define pt_iflag pt_termio.c_iflag
#define pt_cflag pt_termio.c_cflag
#define pt_litc pt_termio.c_cc[V_LNEXT]
#define pt_stopc pt_termio.c_cc[V_STOP]
#define pt_startc pt_termio.c_cc[V_START]

	u_char	pt_ucntl;		/* user control code		*/

	u_char	pt_pkt;			/* pending 'packet' bits	*/

	u_short	pts_state, ptc_state;	/* current state		*/
	struct	winsize	pt_winsize;	/* size of window		*/
	u_short	pt_index;		/* port number			*/
	u_short	pt_pgrp;		/* process group		*/
};

/* 'invalid' value for pt_ic */
#define NULLICNO (-1)

extern int maxpty;
extern int ptc_nummajor;
extern int ptc_majors[];
extern int pts_nummajor;
extern int pts_majors[];
int	max_majors;
static struct pty **pty = NULL;

/* force cflag to this */
#define PT_CFLAG (CS8|CREAD|CLOCAL)

/* slave state bits */
#define PT_UNBORN	0x01		/* never opened */
#define	PT_DEAD		0x02		/* have lived and died already	*/
#define PTS_TXSTOP	0x04		/* output stopped by received XOFF */
#define PTS_LIT		0x08		/* have seen literal character	*/

/* controller state bits */
#define PTC_QUEUED	0x04		/* 'queued' device		*/
#define PTC_RD_ARMED	0x08		/* 'read-ok' armed		*/
#define PTC_PKT		0x10		/* 'packet mode' on		*/
#define PTC_PKT_SET	0x20		/* 'packet mode' set in head	*/
#define PTC_UCNTL	0x100		/* user control mode		*/
#define PTC_REMOTE	0x200		/* remote control tty mode	*/
#define PTC_CLONE_OPEN	0x400		/* opened via clone device	*/

/* general state bits */
#define PT_SIGIO	0x40		/* FIOASYNC enabled		*/
#define PT_XCLUDE	0x80		/* exclusive open 		*/

#define MAX_PKT_DATA	256		/* maximum data in 'packet mode' */



/* initialize this module 
 */
ptcinit()
{
	max_majors = min(ptc_nummajor,pts_nummajor);
	if (maxpty > (max_majors * 256))
		maxpty = (max_majors * 256);
	max_majors = ((maxpty + 255) / 256);
}


/* allocate pty structure for the given dev, if its not already allocated
 */
static struct pty *			/* 0=failed */
pt_alloc(d)
register short d;
{
	register struct pty *pt;

	if (pty == NULL) {
		pty = (struct pty **) calloc(maxpty,sizeof(struct pty *));
		if (pty == NULL)
			return 0;
	};
	for (;;) {
		pt = pty[d];
		if (!pt) {
			pty[d] = (struct pty *)1;
			pt = (struct pty *)calloc(1, sizeof(struct pty));
			if (!pt) {
				pty[d] = 0;
				wakeup((caddr_t)&pty[d]);
				return 0;
			}
			pt->pt_ic = NULLICNO;
			pt->pt_termio = def_stty_ld.st_termio;
			pt->pt_litc = CLNEXT;
			pt->pt_stopc = CSTOP;
			pt->pt_startc = CSTART;
			pt->pt_cflag |= PT_CFLAG|B19200;
			pt->pt_index = d;
			pt->ptc_state = PT_UNBORN;
			pt->pts_state = PT_UNBORN;
			pty[d] = pt;

		} else if (pt == (struct pty *)1) {
			(void)sleep((caddr_t)&pty[d], PCATCH|STIPRI);
			continue;
		}

		return pt;
	}
}



/* do 'XON' */
static
pt_resume(pt)
register struct pty *pt;
{
	pt->pts_state &= ~PTS_TXSTOP;
	ptc_queue(pt, QPTY_START);
	qenable(pt->pts_wq);
}


/* set 'tty' parameters
 */
static
pt_tcset(pt,bp)
register struct pty *pt;
register mblk_t *bp;
{
	register struct iocblk *iocp;
	register struct termio *tp;
	register ushort iflag;
	register ushort old_iflag;

	iocp = (struct iocblk*)bp->b_rptr;
	tp = STERMIO(bp);

	iflag = (((tp->c_iflag & IXON) &&
	  			! (tp->c_cc[V_START] == CDEL &&
				   tp->c_cc[V_STOP] == CDEL))
					? 1 : 0);
	old_iflag = (((pt->pt_iflag & IXON) &&
				! (pt->pt_termio.c_cc[V_START] == CDEL &&
				   pt->pt_termio.c_cc[V_STOP] == CDEL))
					? 1 : 0);
	if (pt->ptc_state & PTC_PKT) {	/* do packet stuff */
		if (iflag ^ old_iflag) {
			register int s = splstr();
			pt->pt_pkt &= ~(TIOCPKT_DOSTOP|TIOCPKT_NOSTOP);
			if (iflag) {
				pt->pt_pkt |= TIOCPKT_DOSTOP;
			} else {
				pt->pt_pkt |= TIOCPKT_NOSTOP;
			}
			splx(s);
		}
	}

	pt->pt_termio = *tp;

	if (PTS_TXSTOP & pt->pts_state
	    && ! iflag)
		pt_resume(pt);

	iocp->ioc_count = 0;
	bp->b_datap->db_type = M_IOCACK;
}



/* slave side open
 */
static int
pts_open(rq, dev, flag, sflag)
register queue_t *rq;			/* our new read queue */
dev_t dev;
int flag;
int sflag;
{
	register struct pty *pt;
	register queue_t *wq = WR(rq);
	register short int d;
	struct proc *pp;

	if (sflag) {			/* do not do slave 'clone' open */
		u.u_error = ENODEV;
		return OPENFAIL;
	} else {
		int	i;
		int	mdev;

		mdev = emajor(dev);
		for (i = 0; ; i++) {
			if (pts_majors[i] == mdev)
				break;
			if (i >= max_majors) {
				u.u_error = ENODEV;
				return OPENFAIL;
			};
		};
		d = makedev(i,minor(dev));
	}
	if (d >= maxpty) {
		u.u_error = ENODEV;
		return OPENFAIL;
	}
	if (!(pt = pt_alloc(d))) {
		u.u_error = ENOMEM;
		return OPENFAIL;
	}

	if (pt->pts_state & PT_XCLUDE && u.u_uid != 0) {
		u.u_error = EBUSY;
		return(OPENFAIL);
	};

	if (!pt->pts_rq) {		/* connect new device to stream */
		if ((pt->pts_state & PT_DEAD)
		    || (pt->ptc_state & PT_DEAD)) {
			u.u_error = EIO;
			return OPENFAIL;	/* ignore zombies */
		}

		if (!strdrv_push(rq,"stty_ld",dev))
			return OPENFAIL;

		rq->q_ptr = (caddr_t)pt;
		wq->q_ptr = (caddr_t)pt;
		pt->pts_rq = rq;
		pt->pts_wq = wq;
		pt->pts_state &= ~PT_UNBORN;


		if (pt->ptc_rq) {
			putctl (pt->ptc_rq->q_next, M_CLRERROR);
		}

		while (!pt->ptc_rq	/* wait for controller if asked */
		       && !(flag & FNDELAY)) {
			if (sleep((caddr_t)pt, STIPRI|PCATCH)) {
				u.u_error = EINTR;
				pt->pts_rq = 0;
				pt->pts_wq = 0;
				if (pt->ptc_state & (PT_UNBORN|PT_DEAD)) {
					free((char*)pt);
					pty[d] = 0;
				} else {	/* send HUP to controller */
					pt->pts_state |= PT_DEAD;
					(void)putctl(pt->ptc_rq->q_next,
						      M_HANGUP);
				}
				return OPENFAIL;
			}
		}
	} else {
		ASSERT((struct pty*)pt->pts_rq->q_ptr == pt);
		ASSERT((struct pty*)pt->pts_wq->q_ptr == pt);
	}


	return d;
}



/* close down a pty, for the slave
 *	- close this end down and adjust state so that controller
 *	  will find out on next operation
 */
static
pts_close(rq)
queue_t *rq;
{
	register struct pty *pt = (struct pty*)rq->q_ptr;
	register u_short d;

	/*
	 * There's only one leitimate reason that pt is null
	 * at this point.  If the graphics board is not present and
	 * someone opens minor number 0 the uart driver will be attached
	 * to the stream before the pseudo device.  The pseudo will not
	 * complete the open, yet when the process closes the stream
	 * this routine and the uart close routine will be called.
	 */
	if (pt == (struct pty *)0) {
	    if (IS_I2000)
	      return;
	    else
	      cmn_err (CE_PANIC, "pts_close: invalid queue pointer");
	}      

	d = pt->pt_index;
	ASSERT(d < maxpty && pty != NULL && pty[d] == pt);

	str_unbcall(rq);		/* stop waiting for buffers */

	/*
	 * If the queue is closed make sure that the kernel knows
	 * that it should output directly to the frame buffer
	 * if this pseudo terminal is the console.
	 */
	graphics_remove_queue(d);


	if (pt->ptc_state & (PT_UNBORN|PT_DEAD)) {
		free((char*)pt);
		pty[d] = 0;

	} else if (! (pt->ptc_state & PTC_CLONE_OPEN)) {
					/* allow slave close and reopen */
					/* if not a clone open (BSD style */
					/* of access)			*/
		pt->pts_state |= PT_UNBORN;
		pt->pts_rq = 0;
		pt->pts_wq = 0;
		(void)putctl1(pt->ptc_rq->q_next, M_ERROR, EIO);
	} else {			/* let controlling tty know */
		pt->pts_state |= PT_DEAD;
		pt->pts_rq = 0;
		pt->pts_wq = 0;
		(void)putctl(pt->ptc_rq->q_next, M_HANGUP);
		flushq(pt->ptc_wq, FLUSHALL);
	}
}



/* slave output 'put' function
 */
pts_wput(wq, bp)
queue_t *wq;
register mblk_t *bp;
{
	register struct pty *pt = (struct pty*)wq->q_ptr;
	register struct iocblk *iocp;

	ASSERT(pt->pt_index < maxpty && pty != NULL && pty[pt->pt_index] == pt);

	switch (bp->b_datap->db_type) {

	case M_FLUSH:
		if (pt->ptc_state & PTC_PKT) {	/* handle packet mode */
			if (*bp->b_rptr & FLUSHW)
				pt->pt_pkt |= TIOCPKT_FLUSHWRITE;
			if (*bp->b_rptr & FLUSHR)
				pt->pt_pkt |= TIOCPKT_FLUSHREAD;
		}
		if (*bp->b_rptr & FLUSHW)
			pt_resume(pt);
		sdrv_flush(wq,bp);
		break;

	case M_DATA:
		putq(wq, bp);
		break;

	case M_DELAY:			/* ignore timing requests */
		freemsg(bp);
		break;

	case M_IOCTL:
		iocp = (struct iocblk*)bp->b_rptr;
		switch (iocp->ioc_cmd) {
		case TCXONC:
			if (! (iocp->ioc_count == sizeof(int))) {
				bp->b_datap->db_type = M_IOCNAK;
				qreply(wq,bp);
				break;
			};
			switch (*(int*)(bp->b_cont->b_rptr)) {
			case 0:		/* stop output */
				if (! (pt->pts_state & PTS_TXSTOP)) {
					pt->pts_state |= PTS_TXSTOP;
					ptc_queue(pt, QPTY_STOP);
				};
				break;
			case 1:		/* resume output */
				if (pt->pts_state & PTS_TXSTOP) 
					pt_resume(pt);
				break;
			case 2:		/* suspend input */
			case 3:		/* resume input  */
				putq(wq, bp);
				bp = NULL;
				break;
			default:
				iocp->ioc_error = EINVAL;
				break;
			}
			if (bp != NULL) {
				bp->b_datap->db_type = M_IOCACK;
				iocp->ioc_count = 0;
				qreply(wq, bp);
			}
			break;

		case TCSETA:
			if (! (iocp->ioc_count == sizeof(struct termio))) {
				bp->b_datap->db_type = M_IOCNAK;
				qreply(wq,bp);
				break;
			};
			pt_tcset(pt,bp);
			qreply(wq,bp);
			break;

		case TCSETAW:
		case TCSETAF:
			if (! (iocp->ioc_count == sizeof(struct termio))) {
				bp->b_datap->db_type = M_IOCNAK;
				qreply(wq,bp);
				break;
			};
			putq(wq, bp);
			break;

		case TCGETA:
			tcgeta(wq,bp, &pt->pt_termio);
			break;

		case TCSBRK:
			putq(wq, bp);
			break;

		case PTCSETCON:
			if ( !suser() ) {
				iocp->ioc_error = EPERM;;
			} else {
				iocp->ioc_error =
					graphics_set_queue(pt->pt_index,wq);
			}
			bp->b_datap->db_type = M_IOCACK;
			iocp->ioc_count = 0;
			qreply(wq,bp);
			break;

		case PTCCLRCON:
			if ( !suser() ) {
				iocp->ioc_error = EPERM;;
			} else {
				iocp->ioc_error = 
					graphics_remove_queue(pt->pt_index);
			}
			bp->b_datap->db_type = M_IOCACK;
			iocp->ioc_count = 0;
			qreply(wq,bp);
			break;

		case FIONREAD:
			fion(RD(wq),bp,0);
			qreply(wq,bp);
			break;

		case TCBLKMD:
			pt->pt_iflag |= IBLKMD;
			iocp->ioc_count = 0;
			bp->b_datap->db_type = M_IOCACK;
			qreply(wq,bp);
			break;

	     /* case BSD43_TIOCSTI: */
		case TIOCSTI:
			if (! (iocp->ioc_count == sizeof(char))) {
				bp->b_datap->db_type = M_IOCNAK;
				qreply(wq,bp);
				break;
			};
			putq(wq,bp);
			break;

		case BSD43_TIOCOUTQ:
#define argi  (*(int*)(bp->b_cont->b_rptr))
			if (! (iocp->ioc_count == sizeof(int))) {
				bp->b_datap->db_type = M_IOCNAK;
				qreply(wq,bp);
				break;
			};
			argi += qmsgdsize(wq);

			bp->b_datap->db_type = M_IOCACK;
			qreply(wq,bp);
			break;
#undef argi

		case TIOCGWINSZ:		/* get window size */
			if (! (iocp->ioc_count == sizeof(struct winsize))) {
				bp->b_datap->db_type = M_IOCNAK;
				qreply(wq,bp);
				break;
			};
			*SWINSIZE(bp) = pt->pt_winsize;
			bp->b_datap->db_type = M_IOCACK;
			qreply(wq,bp);
			break;

		case TIOCSWINSZ:		/* set window size */
			if (! (iocp->ioc_count == sizeof(struct winsize))) {
				bp->b_datap->db_type = M_IOCNAK;
				qreply(wq,bp);
				break;
			};
			pt->pt_winsize = *SWINSIZE(bp);
			bp->b_datap->db_type = M_IOCACK;
			iocp->ioc_count = 0;
			qreply(wq,bp);
			break;

#ifdef TIOC_GISTTY
		case TIOC_GISTTY:
#define argi  (*(int*)(bp->b_cont->b_rptr))
			if (! (iocp->ioc_count == sizeof(int))) {
				bp->b_datap->db_type = M_IOCNAK;
				qreply(wq,bp);
				break;
			};
			argi = 1;
			bp->b_datap->db_type = M_IOCACK;
			qreply(wq,bp);
			break;
#undef argi
#endif TIOC_GISTTY

		case TIOCGPGRP:
#define argi  (*(int*)(bp->b_cont->b_rptr))
			if (! (iocp->ioc_count == sizeof(int))) {
				bp->b_datap->db_type = M_IOCNAK;
				qreply(wq,bp);
				break;
			};
			argi = pt->pt_pgrp;
			bp->b_datap->db_type = M_IOCACK;
			qreply(wq,bp);
			break;
#undef argi

		case TIOCSPGRP:
#define argi  (*(int*)(bp->b_cont->b_rptr))
			if (! (iocp->ioc_count == sizeof(int))) {
				bp->b_datap->db_type = M_IOCNAK;
				qreply(wq,bp);
				break;
			};
			pt->pt_pgrp = argi;
			bp->b_datap->db_type = M_IOCACK;
			/* This is a bit of a kludge.  To tell the other
			 * stream head of the new group, we send an ACK up
			 * the other stream. */
			if (NULL != pt->ptc_rq) {
				register mblk_t *obp = dupmsg(bp);
				if (NULL != obp)
					putnext(pt->ptc_rq,obp);
				else {
					bp->b_datap->db_type = M_IOCTL;
					putq(wq, bp);
					break;
				};
			}
			iocp->ioc_count = 0;
			qreply(wq,bp);
			break;
#undef argi

	     /* case BSD43_TIOCEXCL: */
		case TIOCEXCL:
			pt->pts_state |= PT_XCLUDE;
			bp->b_datap->db_type = M_IOCACK;
			iocp->ioc_count = 0;
			qreply(wq,bp);
			break;
			
	     /* case BSD43_TIOCNXCL: */
		case TIOCNXCL:
			pt->pts_state &= ~PT_XCLUDE;
			bp->b_datap->db_type = M_IOCACK;
			iocp->ioc_count = 0;
			qreply(wq,bp);
			break;
			
		case BSD43_TIOCSTOP:
			if (! (pt->pts_state & PTS_TXSTOP)) {
				pt->pts_state |= PTS_TXSTOP;
				ptc_queue(pt, QPTY_STOP);
			};
			bp->b_datap->db_type = M_IOCACK;
			iocp->ioc_count = 0;
			qreply(wq,bp);
			break;

		case BSD43_TIOCSTART:
			if (pt->pts_state & PTS_TXSTOP)
				pt_resume(pt);
			bp->b_datap->db_type = M_IOCACK;
			iocp->ioc_count = 0;
			qreply(wq,bp);
			break;

		case BSD43_TIOCMODG:
#define argi  (*(int*)(bp->b_cont->b_rptr))
			if (! (iocp->ioc_count == sizeof(int))) {
				bp->b_datap->db_type = M_IOCNAK;
				qreply(wq,bp);
				break;
			};
			
			argi = (BSD43_TIOCM_LE | 
				BSD43_TIOCM_DTR |
				BSD43_TIOCM_RTS |
				BSD43_TIOCM_CD |
				BSD43_TIOCM_CTS |
				BSD43_TIOCM_DSR);
			bp->b_datap->db_type = M_IOCACK;
			qreply(wq,bp);
			break;
#undef argi			

		case BSD43_TIOCMODS:
			if (! (iocp->ioc_count == sizeof(int))) {
				bp->b_datap->db_type = M_IOCNAK;
				qreply(wq,bp);
				break;
			};
		case BSD43_TIOCSBRK:
		case BSD43_TIOCCBRK:
		case BSD43_TIOCSDTR:
		case BSD43_TIOCCDTR:
			putq(wq, bp);
			break;

		case FIOASYNC:
#define argi  (*(int*)(bp->b_cont->b_rptr))
			if (argi)
				pt->pts_state |= PT_SIGIO;
			else
				pt->pts_state &= ~PT_SIGIO;
			bp->b_datap->db_type = M_IOCACK;
			qreply(wq,bp);
			break;
#undef argi

	        case BSD43_TIOCMBIC:
		case BSD43_TIOCMBIS:
		case BSD43_TIOCMGET:
		case BSD43_TIOCMSET:
		default:
			if (pt->ptc_state & PTC_UCNTL &&
			    (iocp->ioc_cmd & ~0xFF) == BSD43_UIOCCMD(0)) {
				pt->pt_ucntl = iocp->ioc_cmd & 0xFF;
				bp->b_datap->db_type = M_IOCACK;
				iocp->ioc_count = 0;
				qreply(wq,bp);
				break;
			};
			
			iocp->ioc_error = ENOTTY;
			iocp->ioc_count = 0;
			bp->b_datap->db_type = M_IOCNAK;
			qreply(wq,bp);
			break;
		}
		break;


	default:
		sdrv_error(wq,bp);
	}
}



/* send characters to the controller, from the slave
 *	We come here only if the controller got behind, or if we encountered
 *	an IOCTL that needed to be done in sequence.
 */
static
pts_wsrv(wq)
register queue_t *wq;
{
	register struct pty *pt = (struct pty*)wq->q_ptr;
	register mblk_t *bp;
	register queue_t *crq = pt->ptc_rq;
	u_char did = 0;
	int i;

	ASSERT(pt->pt_index < maxpty && pty != NULL && pty[pt->pt_index] == pt);

	if (!crq)			/* forget it if no controller */
		return;

	if (pt->ptc_state & PTC_PKT_SET) {
		register struct stroptions *sop;
		bp = str_allocb(sizeof(struct stroptions), wq, BPRI_LO);
		if (!bp)
			return;
		bp->b_datap->db_type = M_SETOPTS;
		sop = (struct stroptions*)bp->b_rptr;
		bp->b_wptr += sizeof(struct stroptions);
		sop->so_flags = SO_READOPT;
		sop->so_readopt = ((pt->ptc_state & PTC_PKT) ? RMSGD : RNORM);
		putnext(crq, bp);
		pt->ptc_state &= ~PTC_PKT_SET;
	}

	for (;;) {
		if (TIOCPKT_DATA != pt->pt_pkt) {
			bp = str_allocb(1,wq,BPRI_LO);
			if (!bp)
				break;
			*bp->b_wptr++ = pt->pt_pkt;
			putnext(crq, bp);
			pt->pt_pkt = TIOCPKT_DATA;
		}
		if (pt->ptc_state & PTC_UCNTL &&
		    pt->pt_ucntl != 0) {
			bp = str_allocb(1,wq,BPRI_LO);
			if (!bp)
				break;
			*bp->b_wptr++ = pt->pt_ucntl;
			putnext(crq, bp);
			pt->pt_pkt = 0;
		};

		if (!(bp = getq(wq)))	/* quit after last message */
			break;

		switch (bp->b_datap->db_type) {
		case M_IOCTL:
			{
				register struct iocblk *iocp;
				register mblk_t *hbp;
				iocp = (struct iocblk*)bp->b_rptr;

				switch (iocp->ioc_cmd) {
				 case TCXONC:
                                        i = *(int*)(bp->b_cont->b_rptr);
                                        switch (i) {
                                        case 2:         /* suspend input */
                                        case 3:         /* resume input  */
                                                if ((pt->pts_state & PTS_TXSTOP)
                                                    || !canput(crq->q_next)) {
							putbq(wq, bp);
                                                        goto for_exit;
                                                }
						hbp = str_allocb(1,wq,BPRI_LO);
                                                if (!hbp) {
                                                        putbq(wq, bp);
                                                        goto for_exit;
                                                }
						if (i == 2)
                                                        *hbp->b_wptr++ =
                                                                pt->pt_stopc;
                                                else
                                                        *hbp->b_wptr++ =
								pt->pt_startc;
						putnext(crq, hbp);
                                                did = 1;
                                                break;
					default:
                                                iocp->ioc_error = EINVAL;
                                                break;
                                        }
					bp->b_datap->db_type = M_IOCACK;
                                        iocp->ioc_count = 0;
                                        break;
				case TCSETAW:
					pt_tcset(pt,bp);
					break;
				case TCSETAF:
					(void)putctl1(RD(wq)->q_next,
						      M_FLUSH,FLUSHR);
					if (pt->ptc_state & PTC_PKT)
						pt->pt_pkt|=TIOCPKT_FLUSHREAD;
					pt_tcset(pt,bp);
					break;
				case TCSBRK:	/* let output empty */
					bp->b_datap->db_type = M_IOCACK;
					iocp->ioc_count = 0;
					break;

#ifdef TIOC_GISTTY
				case TIOC_GISTTY:
#define argi  (*(int*)(bp->b_cont->b_rptr))
					if (! (iocp->ioc_count == sizeof(int))) {
						bp->b_datap->db_type = M_IOCNAK;
						break;
					};
					argi = 1;
					bp->b_datap->db_type = M_IOCACK;
					break;
#undef argi
#endif TIOC_GISTTY

				case TIOCGPGRP:
#define argi  (*(int*)(bp->b_cont->b_rptr))
					if (! (iocp->ioc_count == sizeof(int))) {
						bp->b_datap->db_type = M_IOCNAK;
						break;
					};
					argi = pt->pt_pgrp;
					bp->b_datap->db_type = M_IOCACK;
					break;
#undef argi

				case TIOCSPGRP:
#define argi  (*(int*)(bp->b_cont->b_rptr))
					if (! (iocp->ioc_count == sizeof(int))) {
						bp->b_datap->db_type = M_IOCNAK;
						break;
					};
					pt->pt_pgrp = argi;
					bp->b_datap->db_type = M_IOCACK;
					/* This is a bit of a kludge.  To tell the other
					 * stream head of the new group, we send an ACK up
					 * the other stream. */
					if (NULL != pt->ptc_rq) {
						register mblk_t *obp = dupmsg(bp);
						if (NULL != obp)
							putnext(pt->ptc_rq,obp);
						else {
							(void) bufcall(4,BPRI_HI,qenable,wq);
							bp->b_datap->db_type = M_IOCTL;
							putbq(wq, bp);
							goto for_exit;
						};
					}
					iocp->ioc_count = 0;
					break;
#undef argi

			     /* case BSD43_TIOCSTI: */
				case TIOCSTI:
					{
						register mblk_t *hbp;
	
						if (!canput(RD(wq)->q_next)) {
							putbq(wq, bp);
							goto for_exit;
						}

						hbp = str_allocb(1,wq,BPRI_LO);
						if (!hbp) {
							putbq(wq, bp);
							goto for_exit;
						}
						*hbp->b_wptr++ = 
						    (*(char *)(bp->b_cont->b_rptr));
						putq(RD(wq),hbp);
						iocp->ioc_count = 0;
						bp->b_datap->db_type = M_IOCACK;
					};
					break;
				
				case BSD43_TIOCMODS:
				case BSD43_TIOCSBRK:
				case BSD43_TIOCCBRK:
				case BSD43_TIOCSDTR:
				case BSD43_TIOCCDTR:
					bp->b_datap->db_type = M_IOCACK;
					iocp->ioc_count = 0;
					break;

				default:
					bp->b_datap->db_type = M_IOCNAK;
					break;
				}
				qreply(wq,bp);
			}
			break;


		case M_DATA:
			if ((pt->pts_state & PTS_TXSTOP)
			    || !canput(crq->q_next)) {
				putbq(wq, bp);
				goto for_exit;
			}

			if (pt->ptc_state & PTC_PKT) {	/* need header in */
				register mblk_t *hbp;	/* packet mode */
				hbp = str_allocb(1,wq,BPRI_LO);
				if (!hbp) {
					putbq(wq, bp);
					goto for_exit;
				}
				*hbp->b_wptr++ = TIOCPKT_DATA;
				hbp->b_cont = bp;
				/* The following code limits the size of
				 * packets being sent through the system.
				 * We first check to see if we have a multi-
				 * buffer packet.  If so, we'll get rid of
				 * trailing packets as a first step.
				 * If the remaining buffer is still too big,
				 * then we split up that buffer, keep
				 * MAX_PKT_DATA bytes, and put the rest back
				 * on the front of the queue.
				 */
				if (bp->b_cont	/* limit msg size */
				    && msgdsize(hbp) > MAX_PKT_DATA) {
					putbq(wq, bp->b_cont);
					bp->b_cont = 0;
				}
				if (msgdsize(hbp) > MAX_PKT_DATA) {
					mblk_t *nbp = dupb(bp);
					if (nbp == NULL) {
						hbp->b_cont = 0;
						freemsg(hbp);
						putbq(wq, bp);
						goto for_exit;
					}
					nbp->b_rptr += (MAX_PKT_DATA-1);
					bp->b_wptr = nbp->b_rptr;
					putbq(wq, nbp);
				}
				bp = hbp;
			}
			if (pt->ptc_state & PTC_UCNTL) { /* need header in */
				register mblk_t *hbp;	/* ucntl mode */
				hbp = str_allocb(1,wq,BPRI_LO);
				if (!hbp) {
					putbq(wq, bp);
					goto for_exit;
				}
				*hbp->b_wptr++ = 0;
				hbp->b_cont = bp;
				if (bp->b_cont	/* limit msg size */
				    && msgdsize(hbp) > MAX_PKT_DATA) {
					putbq(wq, bp->b_cont);
					bp->b_cont = 0;
				}
				if (msgdsize(hbp) > MAX_PKT_DATA) {
					mblk_t *nbp = dupb(bp);
					if (nbp == NULL) {
						hbp->b_cont = 0;
						freemsg(hbp);
						putbq(wq, bp);
						goto for_exit;
					}
					nbp->b_rptr += (MAX_PKT_DATA-1);
					bp->b_wptr = nbp->b_rptr;
					putbq(wq, nbp);
				}
				bp = hbp;
			}
			putnext(crq, bp);	/* send data up */
			did = 1;
			break;

		default:		/* the put fnc checked all msgs */
			cmn_err(CE_PANIC, "spty: bad db_type");
		}
	}
for_exit:;

	if (did
	    && (pt->ptc_state & PTC_RD_ARMED)) {
		ptc_queue(pt, QPTY_CANREAD);
		pt->ptc_state &= ~PTC_RD_ARMED;
	}
}



/* slave read service function
 *	Take data from the controller, and send it up to the slave.
 *	We have to scan the data, looking for XON and XOFF.  Worse, we must
 *	discard XON and XOFF when we find them.
 */
static
pts_rsrv(rq)
queue_t *rq;
{
	struct pty *pt = (struct pty*)rq->q_ptr;
	register u_char *cp, *lim;
	register mblk_t *obp;
	register mblk_t *ibp;

	ASSERT(pt->pt_index < maxpty && pty != NULL && pty[pt->pt_index] == pt);

	ibp = NULL;
	for (;;) {
		if (!ibp) {
			if (!canput(rq->q_next))
				break;	/* quit if slave constipated */
			if (!(ibp = getq(rq))) {	/* quit if no more, */
				register queue_t *cwq;	/* after awakening */
				cwq = pt->ptc_wq;	/* controller */
				if (0 != cwq
				    && 0 != cwq->q_first)
					qenable(cwq);
				break;
			}
		}

		ASSERT(M_DATA == ibp->b_datap->db_type);

		if (!(obp = dupb(ibp))) {	/* quit if no msg blocks */
			putbq(rq, ibp);
			break;
		}

		cp = obp->b_rptr;
		lim = obp->b_wptr;
		while (cp < lim) {
			/* start or stop output (if permitted) when we get
			 * XOFF or XON */
			if (pt->pt_iflag & IXON) {
				register u_char cs = *cp;
				if (pt->pt_iflag & ISTRIP)
					cs &= 0x7f;

				if ((PTS_TXSTOP & pt->pts_state)
				    && (cs == pt->pt_startc
					|| ((IXANY & pt->pt_iflag)
					    && (cs != pt->pt_stopc
						|| pt->pt_line == LDISC0)))) {
					pt_resume(pt);
					if (cs == pt->pt_startc) {
						ibp->b_rptr++;
						break;
					}
				} else if (PTS_LIT & pt->pts_state) {
					pt->pts_state &= ~PTS_LIT;
				} else if (cs == pt->pt_stopc) {
					pt->pts_state |= PTS_TXSTOP;
					ptc_queue(pt, QPTY_STOP);
					ibp->b_rptr++;
					break;
				} else if (cs == pt->pt_startc) {
					ibp->b_rptr++;
					break;	/* ignore extra control-Qs */
				} else if (cs == pt->pt_litc
					   && LDISC0 != pt->pt_line) {
					pt->pts_state |= PTS_LIT;
				}
			}

			/* check for ISTRIP */
			if (pt->pt_iflag & ISTRIP)
				*cp &= 0x7f;

			cp++;
		}

		ibp->b_rptr += (cp - obp->b_rptr);
		if (ibp->b_rptr >= ibp->b_wptr) {
			register mblk_t *nbp;
			nbp = rmvb(ibp,ibp);
			freemsg(ibp);
			ibp = nbp;
		}

		obp->b_wptr = cp;
		if (cp > obp->b_rptr) {	/* send the data up stream */
			putnext(rq, obp);
		} else {
			freemsg(obp);
		}
	}
}



/* put something in the controllers graphics queue
 */
ptc_queue(pt, t)
register struct pty *pt;
int t;
{
#ifndef	KOPT_NOGL
	if (pt->pt_ic != NULLICNO
	    && (pt->ptc_state & PTC_QUEUED)) {
		/*
		 * Put something in the queue.  Give the process
		 * the pty index so that if its managing more
		 * than one pty, it can figure out which one
		 * the event is for.
		 */
		gl_anyqenter(pt->pt_ic, t, pt->pt_index);
	}
#endif
}



/* controller open
 */
/*ARGSUSED*/
static int
ptc_open(rq, dev, flag, sflag)
register queue_t *rq;
dev_t dev;
int flag;
int sflag;
{
	register struct pty *pt;
	register queue_t *wq = WR(rq);
	register short int d;

	if (sflag) {			/* do 'clone' open */
	  	if (emajor(dev) == ptc_majors[0]) {
use_first_256:
			for (d = 0;
			     d < 256 && d < maxpty &&
				pty != NULL && NULL != pty[d];
			     d++)
				continue;
			if (d >= 256)
				d = maxpty;
		} else {
			for (d = 256;
			     d < maxpty &&
				pty != NULL && NULL != pty[d];
			     d++)
				continue;
			if (d >= maxpty) 
				goto use_first_256;
		};
	} else {
		int	i;
		int	mdev;

		mdev = emajor(dev);
		for (i = 0; ; i++) {
			if (ptc_majors[i] == mdev)
				break;
			if (i >= max_majors) {
				u.u_error = ENODEV;
				return OPENFAIL;
			};
		};
		d = makedev(i,minor(dev));
	}
	if (d >= maxpty) {
		u.u_error = ENODEV;
		return OPENFAIL;
	}
	if (!(pt = pt_alloc(d))) {
		u.u_error = ENOMEM;
		return OPENFAIL;
	}

	if (pt->ptc_state & PT_XCLUDE && u.u_uid != 0) {
		u.u_error = EBUSY;
		return(OPENFAIL);
	};

	if (!(pt->ptc_state & PT_UNBORN)	/* fail if already open */
	    || (pt->pts_state & PT_DEAD)) {	/* ignore zombies */
		u.u_error = EIO;
		wakeup((caddr_t)pt);	/* awaken slave */
		return OPENFAIL;
	}

	rq->q_ptr = (caddr_t)pt;	/* connect new device to stream */
	wq->q_ptr = (caddr_t)pt;
	pt->ptc_rq = rq;
	pt->ptc_wq = wq;
	pt->ptc_state &= ~PT_UNBORN;
	if (sflag)
		pt->ptc_state |= PTC_CLONE_OPEN;

	wakeup((caddr_t)pt);		/* awaken slave */

	return(makedev((ptc_majors[emajor(d)] - emajor(dev)),minor(d)));
}



/* close down a pty, for the controller
 *	- close this end down and adjust state so that slave
 *	  will find out on next operation
 */
static
ptc_close(rq)
queue_t *rq;
{
	register struct pty *pt = (struct pty*)rq->q_ptr;
	register u_short d = pt->pt_index;

	ASSERT(d < maxpty && pty != NULL && pty[d] == pt);

	str_unbcall(rq);		/* stop waiting for buffers */

	if (pt->pts_state & (PT_UNBORN|PT_DEAD)) {
		free((char*)pt);
		pty[d] = 0;

	} else {			/* let slave know */
		pt->ptc_state |= PT_DEAD;
		pt->ptc_state &= ~PTC_QUEUED;
		pt->pt_ic = NULLICNO;
		pt->ptc_rq = 0;
		pt->ptc_wq = 0;
		(void)putctl(pt->pts_rq->q_next, M_HANGUP);
	}
}



/* controller output 'put' function
 *	queue data for the slave to 'input'
 */
static
ptc_wput(wq, bp)
queue_t *wq;				/* out write queue */
register mblk_t *bp;
{
	register struct pty *pt;
	register struct iocblk *iocp;
	register struct proc *procp;
	register queue_t *srq;		/* slave read queue */

	pt = (struct pty*)wq->q_ptr;
	srq = pt->pts_rq;

	ASSERT(pt->pt_index < maxpty && pty != NULL && pty[pt->pt_index] == pt);

	switch (bp->b_datap->db_type) {

	case M_FLUSH:
		sdrv_flush(wq,bp);
		if (srq)
			qenable(srq);
		break;

	case M_DATA:			/* send data to slave */
		if (!srq)
			freemsg(bp);
		else {
			if (!wq->q_first && canput(srq))
				putq(srq, bp);
			else {		/* if slave is behind, */
				noenable(wq);	/* queue it here to */
				putq(wq, bp);	/* throttle controller head */
			}
		}
		break;

	case M_DELAY:			/* ignore timing requests */
		freemsg(bp);
		break;

	case M_IOCTL:
		iocp = (struct iocblk*)bp->b_rptr;
		switch (iocp->ioc_cmd) {
#ifndef	KOPT_NOGL
		case PTIOC_QUEUE:	/* tag device as queued */
			if (! (iocp->ioc_count == sizeof(procp))) {
				bp->b_datap->db_type = M_IOCNAK;
				qreply(wq,bp);
				break;
			};

			procp = *(struct proc**)(bp->b_cont->b_rptr);
			pt->pt_ic = gl_procptoicno(procp);
			if (pt->pt_ic != NULLICNO)
				pt->ptc_state |= (PTC_QUEUED|PTC_RD_ARMED);
			else
				iocp->ioc_error = EINVAL;
			bp->b_datap->db_type = M_IOCACK;
			iocp->ioc_count = 0;
			qreply(wq,bp);
			break;
#endif

		case TCXONC:
			if (! (iocp->ioc_count == sizeof(int))) {
				bp->b_datap->db_type = M_IOCNAK;
				qreply(wq,bp);
				break;
			};
			switch (*(int*)(bp->b_cont->b_rptr)) {
			case 0:		/* stop output */
				if (! (pt->pts_state & PTS_TXSTOP)) {
					pt->pts_state |= PTS_TXSTOP;
					ptc_queue(pt, QPTY_STOP);
				};
				break;
			case 1:		/* resume output */
				if (pt->pts_state & PTS_TXSTOP) 
					pt_resume(pt);
				break;
			case 2:		/* suspend input */
			case 3:		/* resume input  */
				break;
			default:
				iocp->ioc_error = EINVAL;
				break;
			}
			bp->b_datap->db_type = M_IOCACK;
			iocp->ioc_count = 0;
			qreply(wq, bp);
			break;

		case TIOCGWINSZ:		/* get window size */
			if (! (iocp->ioc_count == sizeof(struct winsize))) {
				bp->b_datap->db_type = M_IOCNAK;
				qreply(wq,bp);
				break;
			};
			*SWINSIZE(bp) = pt->pt_winsize;
			bp->b_datap->db_type = M_IOCACK;
			qreply(wq,bp);
			break;

		case TIOCSWINSZ:		/* set window size */
			if (! (iocp->ioc_count == sizeof(struct winsize))) {
				bp->b_datap->db_type = M_IOCNAK;
				qreply(wq,bp);
				break;
			};
			pt->pt_winsize = *SWINSIZE(bp);
			bp->b_datap->db_type = M_IOCACK;
			iocp->ioc_count = 0;
			qreply(wq,bp);
			break;

		case TCSETAF:
			flushq(wq,FLUSHDATA);	/* flush and fall thru */
		case TCSETA:
		case TCSETAW:
			if (! (iocp->ioc_count == sizeof(struct termio))) {
				bp->b_datap->db_type = M_IOCNAK;
				qreply(wq,bp);
				break;
			};
			pt_tcset(pt,bp);
			/* This is a bit of a kludge.  To tell the line
			 * discipline code of the new mode, we send an ACK up
			 * the other stream. */
			if (NULL != srq) {
				register mblk_t *obp = dupmsg(bp);
				if (NULL != obp)
					putnext(srq,obp);
			}
			qreply(wq,bp);
			break;

		case TCGETA:
			tcgeta(wq,bp, &pt->pt_termio);
			break;

#ifdef TIOC_GISTTY
		case TIOC_GISTTY:
#define argi  (*(int*)(bp->b_cont->b_rptr))
			if (! (iocp->ioc_count == sizeof(int))) {
				bp->b_datap->db_type = M_IOCNAK;
				qreply(wq,bp);
				break;
			};
			argi = 0;
			bp->b_datap->db_type = M_IOCACK;
			qreply(wq,bp);
			break;
#undef argi
#endif TIOC_GISTTY

		case TIOCGPGRP:
#define argi  (*(int*)(bp->b_cont->b_rptr))
			if (! (iocp->ioc_count == sizeof(int))) {
				bp->b_datap->db_type = M_IOCNAK;
				qreply(wq,bp);
				break;
			};
			argi = pt->pt_pgrp;
			bp->b_datap->db_type = M_IOCACK;
			qreply(wq,bp);
			break;
#undef argi

		case TIOCSPGRP:
#define argi  (*(int*)(bp->b_cont->b_rptr))
			if (! (iocp->ioc_count == sizeof(int))) {
				bp->b_datap->db_type = M_IOCNAK;
				qreply(wq,bp);
				break;
			};
			pt->pt_pgrp = argi;
			bp->b_datap->db_type = M_IOCACK;
			/* This is a bit of a kludge.  To tell the other
			 * stream head of the new group, we send an ACK up
			 * the other stream. */
			if (NULL != srq) {
				register mblk_t *obp = dupmsg(bp);
				if (NULL != obp)
					putnext(srq,obp);
				else {
					bp->b_datap->db_type = M_IOCTL;
					putq(wq, bp);
					break;
				};
			}
			iocp->ioc_count = 0;
			qreply(wq,bp);
			break;
#undef argi

		case TCSBRK:		/* similute break to slave */
			if (0 == *(int*)bp->b_cont->b_rptr
			    && BRKINT == (pt->pt_iflag & (IGNBRK|BRKINT))
			    && srq != NULL) {
				flushq(srq, FLUSHDATA);
				(void)putctl1(srq->q_next, M_FLUSH, FLUSHRW);
				(void)putctl1(srq->q_next, M_PCSIG, SIGINT);
			}
			bp->b_datap->db_type = M_IOCACK;
			iocp->ioc_count = 0;
			qreply(wq,bp);
			break;

		case BSD43_TIOCCBRK:
			if (BRKINT == (pt->pt_iflag & (IGNBRK|BRKINT))
			    && srq != NULL) {
				flushq(srq, FLUSHDATA);
				(void)putctl1(srq->q_next, M_FLUSH, FLUSHRW);
				(void)putctl1(srq->q_next, M_PCSIG, SIGINT);
			}
		case BSD43_TIOCSBRK:
			bp->b_datap->db_type = M_IOCACK;
			iocp->ioc_count = 0;
			qreply(wq,bp);
			break;

		case TIOCPKT:
			if (*(int*)bp->b_cont->b_rptr) {
				if (pt->ptc_state & PTC_UCNTL) {
					bp->b_datap->db_type = M_IOCNAK;
					qreply(wq,bp);
					break;
				};
				pt->ptc_state |= PTC_PKT;
				pt->pt_pkt = TIOCPKT_DATA;
			} else {
				pt->ptc_state &= ~PTC_PKT;
			}
			pt->ptc_state |= PTC_PKT_SET;
			if (srq)
				qenable(srq);
			bp->b_datap->db_type = M_IOCACK;
			iocp->ioc_count = 0;
			qreply(wq,bp);
			break;

		case FIONREAD:
			fion(pt->pts_rq,bp,
				(((pt->ptc_state & PTC_PKT) && 
					pt->pt_pkt != TIOCPKT_DATA) ? 1 : 0) +
				(((pt->ptc_state & PTC_UCNTL) &&
					pt->pt_ucntl != 0) ? 1 : 0));
			qreply(wq,bp);
			break;

		case TCBLKMD:
			pt->pt_iflag |= IBLKMD;
			iocp->ioc_count = 0;
			bp->b_datap->db_type = M_IOCACK;
			qreply(wq,bp);
			break;

		case BSD43_TIOCOUTQ:
#define argi  (*(int*)(bp->b_cont->b_rptr))
			if (! (iocp->ioc_count == sizeof(int))) {
				bp->b_datap->db_type = M_IOCNAK;
				qreply(wq,bp);
				break;
			};
			argi += qmsgdsize(pt->pts_wq);

			bp->b_datap->db_type = M_IOCACK;
			qreply(wq,bp);
			break;
#undef argi

	     /* case BSD43_TIOCEXCL: */
		case TIOCEXCL:
			pt->ptc_state |= PT_XCLUDE;
			bp->b_datap->db_type = M_IOCACK;
			iocp->ioc_count = 0;
			qreply(wq,bp);
			break;
			
	     /* case BSD43_TIOCNXCL: */
		case TIOCNXCL:
			pt->ptc_state &= ~PT_XCLUDE;
			bp->b_datap->db_type = M_IOCACK;
			iocp->ioc_count = 0;
			qreply(wq,bp);
			break;
			
		case FIOASYNC:
#define argi  (*(int*)(bp->b_cont->b_rptr))
			pt->ptc_state |= PT_SIGIO;
			bp->b_datap->db_type = M_IOCACK;
			qreply(wq,bp);
			break;
#undef argi

	     /* case BSD43_TIOCUCNTL: */
		case TIOCUCNTL:
#define argi  (*(int*)(bp->b_cont->b_rptr))
			if (argi) {
				if (pt->ptc_state & PTC_PKT) {
					bp->b_datap->db_type = M_IOCNAK;
					qreply(wq,bp);
					break;
				};
				pt->ptc_state |= PTC_UCNTL;
			} else {
				pt->ptc_state &= ~PTC_UCNTL;
			};
			bp->b_datap->db_type = M_IOCACK;
			iocp->ioc_count = 0;
			qreply(wq,bp);
			break;
#undef argi

	     /* case BSD43_TIOCREMOTE: */
		case TIOCREMOTE:
#define argi  (*(int*)(bp->b_cont->b_rptr))
			if (argi) {
				pt->ptc_state |= PTC_REMOTE;
			} else {
				pt->ptc_state &= ~PTC_REMOTE;
			};
			/* XXX: flush read and write queues */
			bp->b_datap->db_type = M_IOCACK;
			iocp->ioc_count = 0;
			qreply(wq,bp);
			break;
#undef argi

		case BSD43_TIOCSTOP:
			if (! (pt->pts_state & PTS_TXSTOP)) {
				pt->pts_state |= PTS_TXSTOP;
				ptc_queue(pt, QPTY_STOP);
			};
			bp->b_datap->db_type = M_IOCACK;
			iocp->ioc_count = 0;
			qreply(wq,bp);
			break;

		case BSD43_TIOCSTART:
			if (pt->pts_state & PTS_TXSTOP)
				pt_resume(pt);
			bp->b_datap->db_type = M_IOCACK;
			iocp->ioc_count = 0;
			qreply(wq,bp);
			break;

		case BSD43_TIOCMODG:
		case BSD43_TIOCMODS:
			if (! (iocp->ioc_count == sizeof(int))) {
				bp->b_datap->db_type = M_IOCNAK;
				qreply(wq,bp);
				break;
			};
		case BSD43_TIOCSDTR:
		case BSD43_TIOCCDTR:
	        case BSD43_TIOCMBIC:
		case BSD43_TIOCMBIS:
		case BSD43_TIOCMGET:
		case BSD43_TIOCMSET:
		default:
			if (pt->ptc_state & PTC_UCNTL &&
			    (iocp->ioc_cmd & ~0xFF) == BSD43_UIOCCMD(0)) {
				pt->pt_ucntl = iocp->ioc_cmd & 0xFF;
				bp->b_datap->db_type = M_IOCACK;
				iocp->ioc_count = 0;
				qreply(wq,bp);
				break;
			};
			
			iocp->ioc_error = ENOTTY;
			bp->b_datap->db_type = M_IOCNAK;
			qreply(wq,bp);
			break;
		}
		break;


	default:
		sdrv_error(wq,bp);
	}
}



/* service controller output queue
 *	The controller enqueues its output directly up toward the slave stream
 *	head.  Therefore, this function is awakened only when the controller
 *	used canput() and was turned down, and things have now drained.
 */
static
ptc_wsrv(rq)
register queue_t *rq;			/* our read queue */
{
	register mblk_t *bp;
	register struct pty *pt = (struct pty*)rq->q_ptr;
	register queue_t *wq, *srq;

	ASSERT(pt->pt_index < maxpty && pty != NULL && pty[pt->pt_index] == pt);

	wq = pt->ptc_wq;
	srq = pt->pts_rq;

	if (!srq)
		flushq(wq, FLUSHALL);

	while (canput(srq)
	       && 0 != (bp = getq(wq)))
		if (bp->b_datap->db_type = M_IOCTL) {
			register struct iocblk *iocp;
			iocp = (struct iocblk*)bp->b_rptr;
			switch (iocp->ioc_cmd) {
			case TIOCSPGRP:
#define argi  (*(int*)(bp->b_cont->b_rptr))
				if (! (iocp->ioc_count == sizeof(int))) {
					bp->b_datap->db_type = M_IOCNAK;
					break;
				}
				pt->pt_pgrp = argi;
				bp->b_datap->db_type = M_IOCACK;
				/* This is a bit of a kludge.  To tell the other
				 * stream head of the new group, we send an ACK up
				 * the other stream. */
				if (NULL != srq) {
					register mblk_t *obp = dupmsg(bp);
					if (NULL != obp)
						putnext(srq,obp);
					else {
						(void) bufcall(4,BPRI_HI,qenable,wq);
						bp->b_datap->db_type = M_IOCTL;
						putbq(wq, bp);
						return;
					};
				};
				iocp->ioc_count = 0;
				break;
#undef argi

			default:
				bp->b_datap->db_type = M_IOCNAK;
				break;
			};
			qreply(wq,bp);
		} else
			putq(srq, bp);
}



/* service controller input queue
 *	The slave enqueues its output directly on the controller stream head.
 *	Therefore, this function is awakened only when the slave used canput()
 *	on the contoller head and was turned down.  The controller stream
 *	has now drained and has 'back-enabled' the controller queue, awakening
 *	this function.
 */
static
ptc_rsrv(rq)
register queue_t *rq;			/* controller read queue */
{
	register struct pty *pt = (struct pty*)rq->q_ptr;
	register queue_t *swq;

	ASSERT(pt->pt_index < maxpty && pty != NULL && pty[pt->pt_index] == pt);

	swq = pt->pts_wq;
	if (swq)			/* just activate slave output */
		qenable(swq);
}
