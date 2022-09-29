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
#ident	"$Header: ss.c,v 1.23.1.13.1.2.1.3 90/11/12 17:51:07 beacker Exp $"

/*
 *	ss - Serial Stream -- Stream support for serial (tty) drivers
 */


#include "sys/sbd.h"
#include "sys/param.h"
#include "sys/types.h"
#include "sys/sysmacros.h"
#include "sys/systm.h"
#include "sys/signal.h"
#include "sys/pcb.h"
#include "sys/immu.h"
#include "sys/region.h"
#include "bsd/sys/time.h"
#include "sys/user.h"
#include "sys/errno.h"
#include "sys/termio.h"
#include "sys/file.h"
#include "sys/stream.h"
#include "sys/stropts.h"
#include "sys/strids.h"
#include "sys/stty_ld.h"
#include "sys/sysinfo.h"
#include "sys/debug.h"
#include "sys/cmn_err.h"
#include "sys/buf.h"
#include "sys/ss.h"
#include "bsd43/sys/ioctl.h"
#include "sys/proc.h"

extern struct stty_ld def_stty_ld;
extern int _posix_vdisable;

#define	TTY_NODELAY

#ifndef MIN
#define MIN(a, b)	((a < b) ? a : b);
#endif

/*
 * This allows for selected print statements to be turned on and off
 * while the kernel is running.
 */

/* ss debug points */
#define SSDBG_PT1	0x0001		/* interrupt status */
#define SSDBG_PT2	0x0002		/* transmitt flow */
#define SSDBG_PT3	0x0004		/* reciever flow */
#define SSDBG_PT4	0x0008		/* ioctl flow */
#define SSDBG_PT5	0x0010		/* DCD interrupts */
#define SSDBG_PT6	0x0020		/* LP interrupts */
int ss_print;

mblk_t	*ss_getbp();

/* ss streams management definitions */
#define MIN_RMSG_LEN 16			/* minimum buffer size */
#define MAX_RMSG_LEN 2048		/* largest msg allowed */
#define XOFF_RMSG_LEN 256		/* send XOFF here */
#define MAX_RBUF_LEN 1024

#define MAX_RSRV_CNT 3			/* continue input timer this long */
#define RSRV_DURATION (HZ/30)		/* send input this often */

#define SWINSIZE(bp)    ((struct winsize *)(bp)->b_cont->b_rptr)
#define CONTROL_CHAR(c,cc)      ((c == cc) && \
				 ((_posix_vdisable != 0) || (c != CDEL)))


/* XXX */
int ss_rsrvexit;

#ifdef SSDEBUG_ENABLE
#	define SSDEBUG(x)	x
#else
#	define SSDEBUG(x)
#endif 

int ss_bticks[16]={   0, HZ/5, HZ/7, HZ/11, HZ/13, HZ/15, HZ/20, HZ/30,
			 HZ/60, HZ/120, HZ/180, HZ/240, HZ/480, HZ/960,
			 HZ/1920, HZ/3840 };

/* The null proc */
ss_null() { return(0); }

extern int _riscos_ttys_default_clocal;	/* defines which ports are local */

ss_open (ssl, rq, dev, flag, sflag, cflag)
	register struct ss_line		*ssl;	/* line of interest */
	queue_t	*rq;
	dev_t	dev;
	int	flag;
	int	sflag;
	register ushort	cflag;
{
	register struct ss_struct	*ssb;	
	register int ctlr, ss, line, s, unit;
	queue_t	*wq = WR(rq);
	struct proc *pp;


	if (!ssl->ss_lenable) {
		u.u_error = ENXIO;	/* No such device or address */
		return (OPENFAIL);
	}
	ssb = ssl->pss;

	s = spltty();
	while (ssl->ss_state & (SS_OPENING | SS_CLOSING)) {	
	  	ssl->ss_state |= SS_WOPEN;
		if (sleep((caddr_t)ssl, STOPRI | PCATCH)) {
			u.u_error = EINTR;
			splx(s);
			return(OPENFAIL);
		};
	};

	if (!(ssl->ss_state & SS_ISOPEN)) {

		ssl->ss_state &= ~(SS_TXSTOP|SS_LIT|SS_BLOCK|
				SS_TX_TXON|SS_TX_TXOFF|SS_FLOW);
		ssl->ss_state |= SS_OPENING;

		cflag &= ~CNEW_RTSCTS;
		/* 
		   _riscos_ttys_default_clocal defines whether or not the lower
		   128 ports will be modem ports or direct ports.  In the default
		   case the upper 128 ports are modems.
		*/


		if ((_riscos_ttys_default_clocal == 0) /* enabled */
			? SSMODEM(dev)
			: ! SSMODEM(dev)) {		/* Modem port */
			cflag &= ~CLOCAL;
			cflag |= CNEW_RTSCTS;
			ssl->ss_state |= SS_FLOW;
		} else {				/* Non modem port */
			cflag |= CLOCAL;
		};



		ssl->ss_litc = CLNEXT;
		ssl->ss_stopc = CSTOP;
		ssl->ss_startc = CSTART;
		if (ss_cont_i (ssl, cflag, &def_stty_ld.st_termio)) {  /*set cflags */
			ss_zap(ssl, 1, 0);		
			if (ssl->ss_state & SS_WOPEN) 
				wakeup((caddr_t) ssl);
			ASSERT((ssl->ss_state & SS_ISOPEN) == 0);
			ssl->ss_state &= ~(SS_WOPEN|SS_OPENING);
			splx (s);
			return OPENFAIL;
		}


		/*
		 * Wait for carrier if no dcd once actived and
		 * not local (using modem) and if we can delay.
		 */

		/* Activate device */
		ssl->ss_modem_state = (BSD43_TIOCM_LE | BSD43_TIOCM_DTR| BSD43_TIOCM_RTS);
		if (ss_act(ssl)) {		/* ss_act returns true is carrier is up */
			ssl->ss_modem_state |= BSD43_TIOCM_CD;
		} else {
			ssl->ss_modem_state &= ~BSD43_TIOCM_CD;
		};

		rq->q_ptr = (caddr_t)ssl;
		wq->q_ptr = (caddr_t)ssl;
		ssl->ss_wq = wq;
		ssl->ss_rq = rq;
		ssl->ss_state |= SS_ISOPEN;
		ssl->ss_cflag |= CREAD;

		if (!strdrv_push (rq, "stty_ld", dev)) {
			ss_zap(ssl, 1, 0);	
			rq->q_ptr = NULL;
			wq->q_ptr = NULL;
			ssl->ss_rq = NULL;
			ssl->ss_wq = NULL;
			if (ssl->ss_state & SS_WOPEN)
				wakeup((caddr_t) ssl);
			ssl->ss_state &= ~(SS_ISOPEN|SS_WOPEN|SS_OPENWAIT|SS_OPENING);
			splx (s);
			return OPENFAIL;
		}

		if (!(ssl->ss_cflag & CLOCAL) && !(flag & FNDELAY)) {
			if (! (ssl->ss_modem_state & BSD43_TIOCM_CD) &&
			    (ssl->ss_state & SS_OPENWAIT) == 0 ) {
				if (! ss_set_str_control(ssl->ss_rq,
					SO_OPENWAIT,1)) {
					ss_zap(ssl, 1, 0);	
					rq->q_ptr = NULL;
					wq->q_ptr = NULL;
					ssl->ss_rq = NULL;
					ssl->ss_wq = NULL;
					if (ssl->ss_state & SS_WOPEN)
						wakeup((caddr_t) ssl);
					ssl->ss_state &= ~(SS_ISOPEN|SS_WOPEN|SS_OPENING);
					u.u_error = ENOSR;
					splx (s);
					return OPENFAIL;
				}
				ssl->ss_state |= SS_OPENWAIT;
			}
		};

		if (ssl->ss_state & SS_WOPEN) {
			wakeup((caddr_t) ssl);
			ssl->ss_state &= ~SS_WOPEN;
		};

		ssl->ss_state &= ~SS_OPENING;
	} else if (ssl->ss_rq == rq) {
		ASSERT(ssl->ss_wq == wq);
		ASSERT(ssl->ss_rq->q_ptr == (caddr_t)ssl);
		ASSERT(ssl->ss_wq->q_ptr == (caddr_t)ssl);

		if (ssl->ss_state & SS_XCLUDE && u.u_uid != 0) {
			u.u_error = EBUSY;
			return(OPENFAIL);
		};

		if (!(ssl->ss_cflag & CLOCAL) && !(flag & FNDELAY)) {
			if (! (ssl->ss_modem_state & BSD43_TIOCM_CD) &&
			    (ssl->ss_state & SS_OPENWAIT) == 0 ) {
				if (! ss_set_str_control(ssl->ss_rq,
					SO_OPENWAIT,1)) {
					u.u_error = ENOSR;
					splx (s);
					return OPENFAIL;
				}
				ssl->ss_state |= SS_OPENWAIT;
			}
		};
	} else {
		u.u_error = EBUSY;
		splx (s);
		return OPENFAIL;
	}

	/* init debug and err counts */
	ssl->ss_ldebug   = 0;
	ssl->ss_overflow = 0;

	splx (s);
	return minor(dev);
}

/*
	We have previously recieved something that told us
	to stop transmitting (ie XOFF, loss of CTS, or loss of DCD).
	We have also been wanting to close for the last 15 seconds
	but cannot because we cannot flush the buffers because
	of the above protocol.  So flush the characters anyway
	and let the close finish.
*/

ss_abort(ssl)
register struct ss_line	*ssl; 
{
	if (ssl->ss_state & (SS_WCLOSE|SS_TXSTOP) == SS_WCLOSE|SS_TXSTOP){
		ssl->ss_state &= ~SS_TXSTOP;
		ssl->ss_tid = 0;

		ss_flushw (ssl, SS_FLUSHCANSLEEP);
		wakeup((caddr_t) ssl);
	}
}
		
ss_close (rq, flag)
	queue_t	*rq;
	int	flag;
{
	register struct ss_line	*ssl = (struct ss_line *)rq->q_ptr;
	register int unit, line, state, s;

	if (!ssl)
		return;

	ASSERT(ssl->ss_rq == rq);

	if (! (flag & FNDELAY)) {
		s = spltty();

		while ((ssl->ss_wbp != NULL) ||
			(ssl->ss_state & (SS_BREAK|SS_BREAK_QUIET))){
			ssl->ss_state |= SS_WCLOSE;
			if (ssl->ss_state & SS_TXSTOP)
			    ssl->ss_tid = timeout(ss_abort, (caddr_t)ssl, HZ * 15);
			if (sleep((caddr_t)ssl, STOPRI | PCATCH)) {
				ssl->ss_state &= ~SS_WCLOSE;
				break;
			}
		};
		if ((ssl->ss_tid) && !(ssl->ss_state & SS_TXSTOP))
			untimeout (ssl->ss_tid);
		splx(s);

		/* get driver to drain its own buffers */
		(*ssl->pss->ss_devdep->ssdd_driver_control)
			(ssl,SS_DC_DRAINOUTPUT,0); /* may sleep */
	};

	s = spltty();

	ASSERT((ssl->ss_state & SS_OPENING) == 0);
	ss_flushr (ssl);
	ss_flushw (ssl, SS_FLUSHCANSLEEP);
	ss_zap(ssl,(ssl->ss_cflag & (HUPCL | CLOCAL)) == HUPCL, 1);	

	ssl->ss_state &= ~(SS_ISOPEN|SS_XCLUDE|SS_OPENWAIT);
	ssl->ss_allocb_fail = ssl->ss_overflow = ssl->ss_framerr =
		ssl->ss_stopcnt = ssl->ss_startcnt = 0;

	ssl->ss_rq = NULL;
	ssl->ss_wq = NULL;

	if (ssl->ss_state & SS_OPENING)		
		wakeup((caddr_t) ssl);		
	
	splx(s);
}

/*
 * finish a delay
 */
static
ss_delay (ssl)
register struct ss_line *ssl;
{
	register int s;

	s = spltty();
	if ((ssl->ss_state & (SS_BREAK|SS_BREAK_QUIET)) != SS_BREAK) {
		ssl->ss_state &= ~(SS_TIMEOUT|SS_BREAK|SS_BREAK_QUIET);
		ss_start(ssl);		/* resume output */

	} else if (ssl->ss_state & SS_BREAK) {
				/* unless need to quiet break */
		(*ssl->pss->ss_devdep->ssdd_modem_control)
			(ssl,SS_MC_STOPBREAK,0);
		ssl->ss_state |= SS_BREAK_QUIET;
		ssl->ss_tid = timeout(ss_delay, (caddr_t)ssl, HZ/20);
	} 
	if (ssl->ss_state & SS_OPENWAIT) {
		if (! ssl->ss_rq ||
		    (ssl->ss_modem_state & BSD43_TIOCM_CD &&
		     ss_set_str_control(ssl->ss_rq,SO_OPENWAIT,0)))
			ssl->ss_state &= ~SS_OPENWAIT;
		else if (! (ssl->ss_state & SS_TIMEOUT)) {
			ssl->ss_state |= SS_TIMEOUT;
			ssl->ss_tid = timeout(ss_delay, (caddr_t)ssl, HZ/4);
		};
	}
	else if (ssl->ss_state & SS_CLOSING) {	
		ssl->ss_state &= ~SS_TIMEOUT;
		wakeup((caddr_t)ssl);
	}				
	splx(s);
}

#ifdef
/*
 * heartbeat to send input up stream
 *	This is used to reduced the large cost of trying to send each,
 *	individual input byte upstream by itself.
 */
static
ss_rsrv_timer (ssl)
register struct ss_line *ssl;
{
	register int s;

	s = spltty();
	if (ssl->ss_rsrv_cnt > 0) {
		ssl->ss_rsrv_cnt--;
		(void)timeout(ss_rsrv_timer, (caddr_t)ssl, RSRV_DURATION);
	}
	splx(s);

	if ((ssl->ss_state & SS_ISOPEN)	/* quit if we are dead */
	    && 0 != ssl->ss_rq) {
		enableok(ssl->ss_rq); /* allow further "putq"s to enable q */
		qenable (ssl->ss_rq); /* schedule the current data         */
	}
}
#endif

/*
 * send a bunch of 1 or more characters up the stream
 *	This should be invoked only because a message could not be sent
 *	upwards by the interrupt. 
 */
ss_rsrv (rq)
register queue_t *rq;			/* our read queue */
{
	register mblk_t *bp;
	register struct ss_line *ssl = (struct ss_line *)rq->q_ptr;
	register int s;

	ASSERT(ssl->ss_rq == rq);
	ASSERT(ssl->ss_state & SS_ISOPEN);

	if (ssl->ss_state & SS_CLOSING)		
		return;				

	s = spltty();

	if (!canput(rq->q_next)) {	
		/* We will be rescheduled by STREAM (we hope) */
		ssl->ss_ldebug |= SS_DBG_RSRVSLEEP;   /* mark asleep */
		splx(s);		
		return;
	}
	ssl->ss_ldebug &= ~SS_DBG_RSRVSLEEP; /* awoken by STREAMS scheduling */
	ssl->ss_ldebug &= ~SS_DBG_QENBSLEEP; /* Service has run */


	if (0 != (bp = ssl->ss_rbp)) {
		/* add the current input message to the "active message" */
		register int sz;
		sz = (bp->b_wptr - bp->b_rptr);
		if (sz > 0
		    && (!ssl->ss_rsrv_cnt || !ssl->ss_rmsg)) {
			str_conmsg(&ssl->ss_rmsg, &ssl->ss_rmsge, bp);
			ssl->ss_rmsg_len += sz;
			ssl->ss_rbp = 0;
		}
	}

	if (0 != (bp = ssl->ss_rmsg)) {
		/* we have an active message */
		ssl->ss_rmsg = 0;
		ssl->ss_rbsize = ssl->ss_rmsg_len;
		ssl->ss_rmsg_len = 0;
		splx(s);		/* without too much blocking, */
		putnext(rq, bp);	/* send the message */
		(void)spltty();
	}

	if (!ssl->ss_rmsg) {
		if (ssl->ss_cflag & CNEW_RTSCTS)
			ss_set_modem(ssl, BSD43_TIOCM_RTS, 0);

		if (ssl->ss_cflag & CNEW_MDMBUF)
			ss_set_modem(ssl, BSD43_TIOCM_DTR, 0);

		if (ssl->ss_state & SS_BLOCK) 	
			ssl->ss_state |= SS_TX_TXON;

		ss_resume_output(ssl);
	}

	if (!ssl->ss_rbp) {
		mblk_t *ss_getbp();
		(void)ss_getbp(ssl, BPRI_LO);
	}

	splx(s);
}

/*
 * slow and hopefully infrequently used function to put characters
 *	somewhere where they will go up stream.
 */
ss_slowr (ssl, c)
register struct ss_line *ssl;
u_char c;				/* send this byte */
{
	register mblk_t *bp;

	if (ssl->ss_iflag & IBLKMD)	/* this kludge apes the old */
		return;			/* block mode hack */

	if (!(bp = ssl->ss_rbp)		/* get buffer if have none */
	    && !(bp = ss_getbp (ssl,BPRI_HI))) {
		ssl->ss_allocb_fail++;;
		return;
	}
	*bp->b_wptr = c;
	if (++bp->b_wptr >= bp->b_datap->db_lim) {
		(void)ss_getbp (ssl,BPRI_LO);	/* send buffer when full */
	}
}

/*
 * process carrier-on interrupt
 */
ss_con (ssl)
register struct ss_line *ssl;
{
	if (ss_print & SSDBG_PT5) 
		cmn_err(CE_CONT,"DCD on interrupt\n");
	ssl->ss_modem_state |= BSD43_TIOCM_CD;


	if (ssl->ss_state & SS_WOPEN) {
		wakeup((caddr_t)ssl);	/* awaken open() requests */
		ssl->ss_state &= ~SS_WOPEN;
	}

	if (ssl->ss_state & SS_OPENWAIT) {
		if (! ssl->ss_rq ||
		    ss_set_str_control(ssl->ss_rq,SO_OPENWAIT,0))
			ssl->ss_state &= ~SS_OPENWAIT;
		else if (! (ssl->ss_state & SS_TIMEOUT)) {
			ssl->ss_state |= SS_TIMEOUT;
			ssl->ss_tid = timeout(ss_delay, (caddr_t)ssl, HZ/4);
		};
	};

	if ((ssl->ss_cflag & CLOCAL) && (ssl->ss_cflag & CNEW_MDMBUF)){
		ssl->ss_state &= ~SS_TXSTOP;
		ss_start(ssl);
	}
}



/*
 * process carrier-off interrupt
 */
ss_coff (ssl)
register struct ss_line *ssl;
{
	if (ss_print & SSDBG_PT5)
		cmn_err(CE_CONT,"DCD off interrupt\n");
	ssl->ss_modem_state &= ~BSD43_TIOCM_CD;

	if (!(ssl->ss_cflag & CLOCAL)) {/* worry about it only for a modem */
		if (ssl->ss_state & SS_CLOSING) {		
			if (ssl->ss_state & SS_TIMEOUT)
				untimeout (ssl->ss_tid);
			ssl->ss_state &= ~SS_TIMEOUT;
			wakeup((caddr_t) ssl);
		}
		else {
			ss_zap(ssl, 0 ,0);
			if (ssl->ss_state & SS_ISOPEN) {
				flushq(ssl->ss_wq, FLUSHDATA);
				(void)putctl1(ssl->ss_rq->q_next, M_FLUSH, FLUSHW);
				(void)putctl(ssl->ss_rq->q_next, M_HANGUP);
			}
		}					
	} else {
		if ((ssl->ss_cflag & CLOCAL)&&(ssl->ss_cflag & CNEW_MDMBUF) )
			ssl->ss_state |= SS_TXSTOP;
	}
		
}

/*
 * Do start/stop processing 
 * This routine is called from the recieve data interrupt.
 * Returns:
 *   0 => start or stop character. Ignore
 *   1 => valid input character
 */
ss_startstop (ssl, c)
	register struct ss_line *ssl;
	char c;		
{
	
	/*
	 * Start or stop output (if permitted)
	 */
	if (ssl->ss_iflag & IXON) {
		register uchar cs = c;
		
		if (ssl->ss_iflag & ISTRIP)
			cs &= 0x7f;

		if ((ssl->ss_state & SS_TXSTOP)
                && (CONTROL_CHAR(ssl->ss_startc, cs)
                   || ((ssl->ss_iflag & IXANY)
			&& (!CONTROL_CHAR(cs, ssl->ss_stopc)
                                  || ssl->ss_ttyline == LDISC0)))) {

				if (ss_print & SSDBG_PT3) {
					cmn_err(CE_CONT,"ss_rint: startc\n");
				}
				ssl->ss_state &= ~SS_TXSTOP;

				/* Call ss to start the line */
				ss_resume_output (ssl);
				return(0);

		} else if (ssl->ss_state & SS_LIT) {
			if (ss_print & SSDBG_PT3) {
				cmn_err(CE_CONT,"ss_rint: literal char");
			}
			ssl->ss_state &= ~SS_LIT;
		} else if (CONTROL_CHAR(cs, ssl->ss_stopc)) {
			if (ss_print & SSDBG_PT3) {
				cmn_err(CE_CONT,"ss_rint: stopc\n");
			}
			ssl->ss_state |= SS_TXSTOP;
			return(0);
		} else if (CONTROL_CHAR(cs, ssl->ss_startc)) {
			if (ss_print & SSDBG_PT3) {
				cmn_err(CE_CONT,"ss_rint: ignored startc\n");
			}
			return(0);	/* ignore extra ^Q's */
		} else if (cs == ssl->ss_litc
			&& ssl->ss_ttyline == LDISC0) {
				if (ss_print & SSDBG_PT3) {
					cmn_err(CE_CONT,"ss_rint: literal next\n");
				}
				ssl->ss_state |= SS_LIT;
		}
	}
	return(1);
}

/*
 * put character on input queue.
 * This routine is called from the recieve data interrupt.
 */
ss_inc (ssl, c)
	register struct ss_line *ssl;
	char c;		
{
	register mblk_t	*bp;
	register int newbuf=0;

	sysinfo.rawch++;

	if (ssl->ss_iflag & ISTRIP)
		c &= 0x7f;
	else 
		c &= 0xff;
				
	if (!(bp = ssl->ss_rbp)) {
		if (!(bp = ss_getbp (ssl, BPRI_HI))) {
			if (ss_print & SSDBG_PT3) {
				cmn_err(CE_CONT,"ss_inc: ss_getbp failed\n");
			}
			ssl->ss_allocb_fail++;
			return;
		}
		newbuf++;
	}

	*bp->b_wptr = c;
	if (++bp->b_wptr >= bp->b_datap->db_lim) {
		(void) ss_getbp (ssl, BPRI_LO);
	}

	if (ss_print & SSDBG_PT3) {
		cmn_err(CE_CONT,"ss_inc: c = 0x%x\n", c);
	}


	/* Queue flow contol logic from putq */
	if ((ssl->ss_state & SS_ISOPEN)  && ssl->ss_rq && 
		 canenable(ssl->ss_rq) && (ssl->ss_rq->q_flag & QWANTR)) {
		qenable (ssl->ss_rq);
	} else {
		/* ss_inc did not qenable */
		/* cleared in ss_rsrv */
		ssl->ss_ldebug |= SS_DBG_QENBSLEEP;
	}
}

/* output starts here */


ss_wput (wq, bp)
	queue_t	*wq;
	register mblk_t	*bp;
{
	register struct ss_line *ssl = (struct ss_line *)wq->q_ptr;
	register struct iocblk *ioss;
	register int s;
	register char flag;

	if (!ssl) {
		sdrv_error(wq,bp);	/* quit now if not open */
		return;
	}

	s = spltty();

	ASSERT(ssl->ss_wq == wq);
	ASSERT(ssl->ss_state & SS_ISOPEN);

	switch (bp->b_datap->db_type) {

	case M_FLUSH:
		flag = *bp->b_rptr;
		if (flag & FLUSHW) {
			if (ss_print & SSDBG_PT4) {
				cmn_err(CE_CONT,"ss_wput: m_flushw\n");
			}
			ss_flushw(ssl, SS_FLUSHNOSLEEP);
			ssl->ss_state &= ~SS_TXSTOP;
			sdrv_flush(wq,bp);
			bp = NULL;
			ss_resume_output (ssl);	/* restart output */
		}
		if (flag & FLUSHR) {
			if (ss_print & SSDBG_PT4) {
				cmn_err(CE_CONT,"ss_wput: m_flushr\n");
			}
			ss_flushr(ssl);
		}
		if (bp != NULL)
			sdrv_flush (wq,bp);   

		break;

	case M_DATA:
	case M_DELAY:
		if (ss_print & (SSDBG_PT2|SSDBG_PT4)) {
			cmn_err(CE_CONT,"ss_wput: m_data|m_delay\n");
		}
		ss_save(ssl, wq, bp);
		break;

	case M_IOCTL:
		ioss = (struct iocblk*)bp->b_rptr;
		if (ss_print & SSDBG_PT4) {
			cmn_err(CE_CONT,"ss_wput: m_ioctl switch %d ", ioss->ioc_cmd);
		}
		switch (ioss->ioc_cmd) {
		case TCXONC:  	
			if (! (ioss->ioc_count == sizeof(int))) {
				bp->b_datap->db_type = M_IOCNAK;
				qreply(wq,bp);
				break;
			};
			if (ss_print & SSDBG_PT4) {
				cmn_err(CE_CONT,"tcxonc switch = %d", 
					*(int*)(bp->b_cont->b_rptr));
			}
			switch (*(int*)(bp->b_cont->b_rptr)) {
			case 0:		/* stop output */
				if (! (ssl->ss_state & SS_TXSTOP)) {
					ssl->ss_state |= SS_TXSTOP;
					ss_suspend_output(ssl);
				};
				break;
			case 1:		/* resume output */
				if (ssl->ss_state & SS_TXSTOP) {
					ssl->ss_state &= ~SS_TXSTOP;
					ss_resume_output(ssl);
				};
				break;
			case 2:         /* stop input */
                                ssl->ss_state |= SS_TX_TXOFF;
                                break;
			case 3:         /* allow input to resume */
                                ssl->ss_state |= SS_TX_TXON;
                                break;
			case 4:
				ss_set_modem(ssl,0,BSD43_TIOCM_RTS);
				break;
			case 5:
				ss_set_modem(ssl,BSD43_TIOCM_RTS,0);
				break;
			default:
				ioss->ioc_error = EINVAL;
				break;
			}
			bp->b_datap->db_type = M_IOCACK;
			ioss->ioc_count = 0;
			qreply(wq, bp);
			break;

		case TCSETA:
			if (! (ioss->ioc_count == sizeof(struct termio))) {
				bp->b_datap->db_type = M_IOCNAK;
				qreply(wq,bp);
				break;
			};
			if (! ss_tcset (ssl, bp))
				bp->b_datap->db_type = M_IOCNAK;
			qreply(wq,bp);
			break;

		case TCSETAW:
		case TCSETAF:
			if (! (ioss->ioc_count == sizeof(struct termio))) {
				bp->b_datap->db_type = M_IOCNAK;
				qreply(wq,bp);
				break;
			};
			ss_save (ssl, wq, bp);
			break;

		case TCGETA:
			tcgeta(wq, bp, &ssl->ss_termio);
			break;

		case TCSBRK:
			ss_save(ssl, wq, bp);
			break;

		case FIONREAD:
			fion(RD(wq),bp, (msgdsize(ssl->ss_rmsg)
					 + msgdsize(ssl->ss_rbp)));
			qreply(wq,bp);
			break;

		case TCBLKMD:
			ssl->ss_iflag |= IBLKMD;
			ioss->ioc_count = 0;
			bp->b_datap->db_type = M_IOCACK;
			qreply(wq,bp);
			break;

	     /* case BSD43_TIOCSTI: */
		case TIOCSTI:
			if (! (ioss->ioc_count == sizeof(char))) {
				bp->b_datap->db_type = M_IOCNAK;
				qreply(wq,bp);
				break;
			};
			ss_slowr(ssl,(*(char *)(bp->b_cont->b_rptr)));
			ioss->ioc_count = 0;
			bp->b_datap->db_type = M_IOCACK;
			qreply(wq, bp);
			break;

		case BSD43_TIOCOUTQ:
#define argi  (*(int*)(bp->b_cont->b_rptr))
			if (! (ioss->ioc_count == sizeof(int))) {
				bp->b_datap->db_type = M_IOCNAK;
				qreply(wq,bp);
				break;
			};
			argi += (qmsgdsize(wq) + 
			  	    (*ssl->pss->ss_devdep->ssdd_driver_control)
					(ssl,SS_DC_TIOCOUTQ,0));

			bp->b_datap->db_type = M_IOCACK;
			qreply(wq,bp);
			break;
#undef argi

	     /* case BSD43_TIOCGWINSZ: */
		case TIOCGWINSZ:		/* get window size */
			if (! (ioss->ioc_count == sizeof(struct winsize))) {
				bp->b_datap->db_type = M_IOCNAK;
				qreply(wq,bp);
				break;
			};
			*SWINSIZE(bp) = ssl->ss_winsize;
			bp->b_datap->db_type = M_IOCACK;
			qreply(wq,bp);
			break;

	     /* case BSD43_TIOCSWINSZ: */
		case TIOCSWINSZ:		/* set window size */
			if (! (ioss->ioc_count == sizeof(struct winsize))) {
				bp->b_datap->db_type = M_IOCNAK;
				qreply(wq,bp);
				break;
			};
			ssl->ss_winsize = *SWINSIZE(bp);
			(*ssl->pss->ss_devdep->ssdd_driver_control)
				(ssl,SS_DC_TIOCSWINSZ,SWINSIZE(bp));
			bp->b_datap->db_type = M_IOCACK;
			ioss->ioc_count = 0;
			qreply(wq,bp);
			break;

	     /* case BSD43_TIOCEXCL: */
		case TIOCEXCL:
			ssl->ss_state |= SS_XCLUDE;
			bp->b_datap->db_type = M_IOCACK;
			ioss->ioc_count = 0;
			qreply(wq,bp);
			break;
			
	     /* case BSD43_TIOCNXCL: */
		case TIOCNXCL:
			ssl->ss_state &= ~SS_XCLUDE;
			bp->b_datap->db_type = M_IOCACK;
			ioss->ioc_count = 0;
			qreply(wq,bp);
			break;
			
		case BSD43_TIOCSTOP:
			if (! (ssl->ss_state & SS_TXSTOP)) {
			  	ssl->ss_state |= SS_TXSTOP;
				ss_suspend_output(ssl);
			};
			bp->b_datap->db_type = M_IOCACK;
			ioss->ioc_count = 0;
			qreply(wq,bp);
			break;

		case BSD43_TIOCSTART:
			if (ssl->ss_state & SS_TXSTOP) {
				ssl->ss_state &= ~SS_TXSTOP;
				ss_resume_output(ssl);
			};
			bp->b_datap->db_type = M_IOCACK;
			ioss->ioc_count = 0;
			qreply(wq,bp);
			break;

	        case BSD43_TIOCMBIC:
		case BSD43_TIOCMBIS:
		case BSD43_TIOCMODG:
		case BSD43_TIOCMODS:
		case BSD43_TIOCMSET:
		case BSD43_TIOCMGET:
			if (! (ioss->ioc_count == sizeof(int))) {
				bp->b_datap->db_type = M_IOCNAK;
				qreply(wq,bp);
				break;
			};
		case BSD43_TIOCCBRK:
			ss_brk_off(ssl);   /* start turning off break now */
					   /* fall through and enqueue this  */
		case BSD43_TIOCSBRK:
		case BSD43_TIOCSDTR:
		case BSD43_TIOCCDTR:
			ss_save(ssl, wq, bp);
			break;

		case FIOASYNC:
#define argi  (*(int*)(bp->b_cont->b_rptr))
			if (argi)
				ssl->ss_state |= SS_SIGIO;
			else
				ssl->ss_state &= ~SS_SIGIO;
#undef argi
			bp->b_datap->db_type = M_IOCACK;
			ioss->ioc_count = 0;
			qreply(wq,bp);
			break;

		default:
			if ( !(*ssl->pss->ss_devdep->ssdd_driver_control)
					(ssl,SS_DC_M_IOCTL,bp) ) {
				ioss->ioc_error = ENOTTY;
				ioss->ioc_count = 0;
				bp->b_datap->db_type = M_IOCNAK;
			}
			qreply(wq,bp);
			break;
		}
		if (ss_print & SSDBG_PT4) {
			cmn_err(CE_CONT,"\n");
		}
		
		break;


	default:
		sdrv_error(wq,bp);
	}

	splx(s);
}
	

/* Simply checks too see if we aren't already sending */

ss_start (ssl)
    struct ss_line	*ssl;
{
    if (!(ssl->ss_state & (SS_TIMEOUT|SS_BREAK|SS_BREAK_QUIET|SS_BUSY))) {
	ss_tx (ssl);
    }
}


/* Main transmit routine. Service Procedure*/
ss_tx (ssl)
    register struct ss_line	*ssl;
{
    register mblk_t	*wbp, *tbp;
    register char	*p;		/* pointer to info */
    register int	nc;		/* number of char to xfer */
    u_char	c;
		
    while (1) {
	if ((ssl->ss_cflag & CNEW_RTSCTS) && (!(ssl->ss_modem_state & BSD43_TIOCM_CTS))){
	    ss_stop (ssl);
	    return;
	}					

	if ((ssl->ss_cflag & CLOCAL)&&(ssl->ss_cflag & CNEW_MDMBUF) 
	   && !(ssl->ss_modem_state & BSD43_TIOCM_CD)){
	    ss_stop (ssl);
	    return;
	}
	if ((ssl->ss_state & SS_ISOPEN) == 0) {
	    ss_stop (ssl);
	    return;
	}

	/* Someone has previoisly decided we need to send on XON
	   so do so  */
	if (ssl->ss_state & SS_TX_TXON) {
	    ++ssl->ss_startcnt;
	    p = &(ssl->ss_startc);
	    nc = 1;
	    ssl->ss_state &= ~(SS_TX_TXON|SS_TX_TXOFF|SS_BLOCK);
	    ssl->ss_state |= SS_XBUSY;
	}
	else if (ssl->ss_state & SS_TX_TXOFF) {
	/* Someone has previoisly decided we need to send on XOFF, so do so  */
	    ++ssl->ss_stopcnt;
	    p = &(ssl->ss_stopc);
	    nc = 1;
	    ssl->ss_state &= ~SS_TX_TXOFF;
	    ssl->ss_state |= (SS_BLOCK | SS_XBUSY);
	}
	else if (ssl->ss_state & (SS_TXSTOP|SS_TIMEOUT)) {
	    /* We have received an XOFF or are in the middle of delaying*/
	    ss_stop (ssl);
	    return;
	}
	else {	/* Standard case */

	/* We will get the characters to write from the write queue */

	/* Make sure we have a write message in the queue. 
	If we don't, call getq to get us the next one.  
	If there are no more, tell hardware to stop.
	If someone is waiting for these chars to go
	out so they can close the line, then clear 
	the bit and wake'em up */

	    if (!(wbp = ssl->ss_wbp)) {
		wbp = getq (ssl->ss_wq);
		if (!wbp) {
		    ss_stop (ssl);
		    if (ssl->ss_state & SS_WCLOSE) {
			ssl->ss_state &= ~SS_WCLOSE;
			wakeup((caddr_t) ssl);
		    };
		    return;
		}

		switch (wbp->b_datap->db_type) {
		case M_DATA:
		    break;

		case M_DELAY:
		/* Call some magical routine and free up the buffer */
		    if (ss_print & SSDBG_PT4) {
			    cmn_err(CE_CONT,"ss_tx: m_delay\n");
		    }
		    ssl->ss_state |= SS_TIMEOUT;
		    ssl->ss_tid = timeout (ss_delay, (caddr_t)ssl,
						*(int *)wbp->b_rptr);
		    freemsg (wbp);
		    continue;

		case M_IOCTL:
		    if (ss_print & SSDBG_PT4) {
			cmn_err(CE_CONT,"ss_tx: m_ioctl\n");
		    }
		    ss_i_ioctl (ssl, wbp);
		    continue;

		default:
		    cmn_err (CE_PANIC, "bad isi_mux msg");
		    break;
		}
	    }

	    if (wbp->b_rptr >= wbp->b_wptr) {
		ASSERT(wbp->b_datap->db_type == M_DATA);
		ssl->ss_wbp = rmvb (wbp, wbp);
		freeb (wbp);
		continue;
	    }
	    ssl->ss_wbp = wbp;
	    p = wbp->b_rptr;
	    nc = wbp->b_wptr - wbp->b_rptr;
	}

	if (nc)  {
	    nc = MIN(ssl->pss->ss_devdep->ssdd_maxoutc, nc);
	    (*ssl->pss->ss_devdep->ssdd_outc)(ssl, p, nc);
	    break;
	}
    }
}

/*
 * interrupt-process an IOCTL
 *	This function processes those IOCTLs that must be done by the output
 *	interrupt.
 */
static
ss_i_ioctl (ssl,bp)
register struct ss_line *ssl;
register mblk_t *bp;
{
	register struct iocblk *ioss;

	ioss = (struct iocblk*)bp->b_rptr;

	switch (ioss->ioc_cmd) {
	case TCSBRK:
		if (*(int*)bp->b_cont->b_rptr == 0) {
			ssl->ss_state |= (SS_TIMEOUT|SS_BREAK);
			(*ssl->pss->ss_devdep->ssdd_modem_control)
				(ssl,SS_MC_STARTBREAK,0);
			ssl->ss_tid = timeout(ss_delay, (caddr_t)ssl, HZ/4);
		}

		ioss->ioc_count = 0;
		bp->b_datap->db_type = M_IOCACK;
		break;

	case TCXONC:
		switch (*(int*)(bp->b_cont->b_rptr)) {
		case 0:			/* stop output */
			ssl->ss_state |= SS_TXSTOP;
			ss_suspend_output(ssl);
			break;
		case 1:			/* resume output */
			ssl->ss_state &= ~SS_TXSTOP;
			ss_resume_output(ssl);
			break;
		case 2:         /* stop input */
			ssl->ss_state |= SS_TX_TXOFF;
			break;
		case 3:         /* allow input to resume */
			ssl->ss_state |= SS_TX_TXON;
			break;
		case 4:
			ss_set_modem(ssl,0,BSD43_TIOCM_RTS);
			break;
		case 5:
			ss_set_modem(ssl,BSD43_TIOCM_RTS,0);
			break;
		default:
			ioss->ioc_error = EINVAL;
			break;
		}
		bp->b_datap->db_type = M_IOCACK;
		ioss->ioc_count = 0;
		break;

	case TCSETAF:
		if (ss_tcset(ssl,bp))
			(void)putctl1(ssl->ss_rq->q_next, M_FLUSH, FLUSHR);
		break;

	case TCSETA:
	case TCSETAW:
		(void)ss_tcset(ssl,bp);
		break;

	case BSD43_TIOCMGET:			/* MODEM GET */
	case BSD43_TIOCMODG:
#define argi	(*(int*)bp->b_cont->b_rptr)
		argi = ssl->ss_modem_state;
		bp->b_datap->db_type = M_IOCACK;
		break;

	case BSD43_TIOCMSET:			/* MODEM SET */
	case BSD43_TIOCMODS:
		ss_set_modem(ssl,argi,~argi);
		
		ioss->ioc_count = 0;
		bp->b_datap->db_type = M_IOCACK;
		break;		

	case BSD43_TIOCMBIC:			/* MODEM BITS CLEAR */
		ss_set_modem(ssl,0,argi);
		ioss->ioc_count = 0;
		bp->b_datap->db_type = M_IOCACK;
		break;		

	case BSD43_TIOCMBIS:			/* MODEM BITS SET */
		ss_set_modem(ssl,argi,0);
#undef argi
		ioss->ioc_count = 0;
		bp->b_datap->db_type = M_IOCACK;
		break;		

	case BSD43_TIOCSBRK:			/* SET BREAK */
		if (ssl->ss_state & SS_TIMEOUT) {
			untimeout (ssl->ss_tid);
			ssl->ss_state &= ~SS_TIMEOUT;
		};
		if ((ssl->ss_state & (SS_BREAK | SS_BREAK_QUIET)) !=
				SS_BREAK) {
			ssl->ss_state |= SS_BREAK;
			ssl->ss_state &= ~SS_BREAK_QUIET;
			(*ssl->pss->ss_devdep->ssdd_modem_control)
				(ssl,SS_MC_STARTBREAK,0);
		};
		bp->b_datap->db_type = M_IOCACK;
		break;

	case BSD43_TIOCCBRK:
		bp->b_datap->db_type = M_IOCACK;
		break;

	case BSD43_TIOCSDTR:
		ss_set_modem(ssl, BSD43_TIOCM_DTR,0);
		bp->b_datap->db_type = M_IOCACK;
		break;

	case BSD43_TIOCCDTR:
		ss_set_modem(ssl, 0, BSD43_TIOCM_DTR);
		bp->b_datap->db_type = M_IOCACK;
		break;

	default:
		ioss->ioc_error = EINVAL;
		ioss->ioc_count = 0;
		bp->b_datap->db_type = M_IOCNAK;
		break;		
	}

	putnext (ssl->ss_rq, bp);
}


/*
 * Set parameters from open or stty into the SS hardware registers.
 */
ss_cont (ssl, cflag, tp)
	register struct ss_line *ssl;
	int cflag;
	struct termio *tp;
{
	register int s;
	register int result;

	s = spltty();
	result = ss_cont_i(ssl,cflag,tp);
	splx(s);
	return(result);
}


ss_cont_i (ssl, cflag, tp)
	register struct ss_line *ssl;
	int cflag;
	struct termio *tp;
{
	register uint diff;
	int	status;

	/*
	 * Block interrupts so parameters will be set before line interrupts.
	 */
	diff = cflag ^ ssl->ss_cflag;
	if (diff & (CBAUD|CSIZE|CSTOPB|PARENB|PARODD)) {
		if ((cflag & CBAUD) == 0) 
			ss_zap(ssl, 1, 0);
		else {
			status = (*ssl->pss->ss_devdep->ssdd_setline) (ssl,
					 cflag, tp);
			if (status)
				return(status);
			if (! (ssl->ss_rmsg_len >= XOFF_RMSG_LEN || !ssl->ss_rbp)) {

				if (cflag & CNEW_RTSCTS)
					ss_set_modem(ssl, BSD43_TIOCM_RTS, 0);

				if (cflag & CNEW_MDMBUF) 
					ss_set_modem(ssl, BSD43_TIOCM_DTR, 0);
			};
		};
	}
  	(*ssl->pss->ss_devdep->ssdd_driver_control)
					(ssl,SS_DC_FLOWCNTL,tp);


	if (tp) {
		ssl->ss_termio = *tp;
	}
	ssl->ss_cflag = cflag;

	return(0);
}


/*
 * get a new buffer
 *	Interrupts ought to be off here.
 * Called from:	
 *    ss_inc - which is called from the driver intr proc to add chars
 */
mblk_t *				/* return NULL or the new buffer */
ss_getbp (ssl, pri)
register struct ss_line *ssl;
uint pri;				/* BPRI_HI=try hard to get buffer */
{
	register int size;
	register mblk_t *bp;
	register mblk_t *rbp;

	rbp = ssl->ss_rbp;
	if (ssl->ss_rmsg_len >= MAX_RMSG_LEN	/* if overflowing */
	    || (0 != rbp		/* or current buffer empty */
		&& rbp->b_rptr >= rbp->b_wptr)) {
		bp = 0;
	} else {
		size = ssl->ss_rbsize;
		if (size > MAX_RBUF_LEN)	/* larger buffer */
			size = MAX_RBUF_LEN;	/* as we get behind */
		for (;;) {
			if (size < MIN_RMSG_LEN)
				size = MIN_RMSG_LEN;

			bp = allocb(size, pri);
			if (0 != bp)
				break;

			if (BPRI_HI == pri
			    && size > MIN_RMSG_LEN) {
				size >>= 2;
				continue;
			}
			break;
		}
	}

	if (0 == rbp) {			/* if we have an old buffer */
		ssl->ss_rbp = bp;
	} else if (0 != bp		/* & a new buffer */
		   || (rbp->b_wptr	/* or old buffer is full */
		       >= rbp->b_datap->db_lim)) {
		str_conmsg(&ssl->ss_rmsg, &ssl->ss_rmsge, rbp);
		ssl->ss_rmsg_len += (rbp->b_wptr - rbp->b_rptr);
		ssl->ss_rbp = bp;
	}

	if (ssl->ss_rmsg_len >= XOFF_RMSG_LEN || !ssl->ss_rbp) {

		if (ssl->ss_cflag & CNEW_RTSCTS)
			ss_set_modem(ssl, 0, BSD43_TIOCM_RTS);

		if (ssl->ss_cflag & CNEW_MDMBUF) 
			ss_set_modem(ssl, 0, BSD43_TIOCM_DTR);

		/* If XOFF handshaking is on & we are ! blocking => send XOFF */
		if ((ssl->ss_iflag & IXOFF)	
		    && !(ssl->ss_state & SS_BLOCK)) {
			ssl->ss_state |= SS_TX_TXOFF;
			ssl->ss_state &= ~SS_TX_TXON;
			ss_start(ssl);
		}
	}
	return bp;
}

/*
 * set parameters
 */
static int				/* 0=bad IOCTL */
ss_tcset (ssl, bp)
register struct ss_line *ssl;
register mblk_t *bp;
{
	register struct iocblk *ioss;
	register struct termio *tp;
	register uint cflag;
	register int baud;

	ioss = (struct iocblk*)bp->b_rptr;
	tp = STERMIO(bp);

	cflag = tp->c_cflag;

	baud = (cflag & CBAUD);

	if (ss_cont(ssl, cflag, tp)) {
		ioss->ioc_count = 0;
		ioss->ioc_error = EINVAL;
		bp->b_datap->db_type = M_IOCNAK;
		return(0);
	};
	tp->c_cflag = ssl->ss_cflag;	/* tell line discipline the results */

	ioss->ioc_count = 0;
	bp->b_datap->db_type = M_IOCACK;
	return 1;
}

/*
 * flush input
 *	interrupts must be safe here
 */
static
ss_flushr (ssl)
register struct ss_line *ssl;
{
	freemsg(ssl->ss_rmsg);
	ssl->ss_rmsg = NULL;
	ssl->ss_rmsg_len = 0;
	freemsg(ssl->ss_rbp);
	ssl->ss_rbp = NULL;

	qenable(ssl->ss_rq);		/* turn input back on */
}

/*
 * flush output
 *	Interrupts must have been made safe here.
 */
static
ss_flushw (ssl, cansleep)
register struct ss_line *ssl;
register int		cansleep;
{
	if ((ssl->ss_state & SS_BREAK) == SS_BREAK) {
		ss_brk_off(ssl);
	}
	if ((ssl->ss_state & SS_TIMEOUT) == SS_TIMEOUT) {
		untimeout (ssl->ss_tid);	/* forget stray timeout */
		ssl->ss_state &= ~SS_TIMEOUT;
		if (ssl->ss_state & SS_BREAK_QUIET){
			ssl->ss_state &=~ (SS_BREAK_QUIET|SS_BREAK);
		}
	}
	(*ssl->pss->ss_devdep->ssdd_driver_control)
		(ssl,SS_DC_SDRVFLUSH,cansleep);
	freemsg(ssl->ss_wbp);
	ssl->ss_wbp = NULL;
}

/*
 * save a message on our write queue,
 *	and start the output interrupt, if necessary
 *
 *	We must be safe from interrupts here.
 */
static
ss_save (ssl, wq, bp)
register struct ss_line *ssl;
queue_t *wq;
mblk_t *bp;
{
	putq(wq,bp);			/* save the message */

	/*
	 * ss_start will do the necessary checking to see if calling ss_tx
	 * is really necessary.
	 */
	ss_start(ssl);
}

/* Routine to stop output from leaving */
/* At this time (9/15/89) only sduart and
   uart drivers implement this */
ss_stop (ssl)
	struct ss_line *ssl;
{
	return((*ssl->pss->ss_devdep->ssdd_driver_control)
			(ssl,SS_DC_STOP,NULL));
}


/* Not currently implemented by any driver.
   I'm not sure what the differencr would be from
   ss_stop()
*/
ss_suspend_output (ssl)
	struct ss_line *ssl;
{
	(*ssl->pss->ss_devdep->ssdd_driver_control)
			(ssl,SS_DC_SUSPEND_OUTPUT,NULL);
}


/* Not currently implemented by any driver.
   Not sure what it should do
*/
ss_resume_output (ssl)
	struct ss_line *ssl;
{
	(*ssl->pss->ss_devdep->ssdd_driver_control)
			(ssl,SS_DC_RESUME_OUTPUT,NULL);
	ss_start(ssl);
}


ss_cts_off (ssl)
register struct ss_line *ssl;
{
	if (ss_print & SSDBG_PT5) 
		cmn_err(CE_CONT,"CTS off interrupt\n");
	ssl->ss_modem_state &= ~BSD43_TIOCM_CTS;
	if (ssl->ss_cflag & CNEW_RTSCTS) 
		ssl->ss_state |= SS_TXSTOP;
}

ss_cts_on (ssl)
register struct ss_line *ssl;
{
	if (ss_print & SSDBG_PT5) 
		cmn_err(CE_CONT,"CTS on interrupt\n");
	ssl->ss_modem_state |= BSD43_TIOCM_CTS;
	if (ssl->ss_cflag & CNEW_RTSCTS) 
		ssl->ss_state &= ~SS_TXSTOP;
	ss_start(ssl);
}


ss_brk_off(ssl)
	struct	ss_line *ssl;
{
	if ((ssl->ss_state & (SS_BREAK | SS_BREAK_QUIET)) == SS_BREAK) {
		(*ssl->pss->ss_devdep->ssdd_modem_control)
			(ssl,SS_MC_STOPBREAK,0);
		if (ssl->ss_state & SS_TIMEOUT) {
			untimeout (ssl->ss_tid);
			ssl->ss_state &= ~SS_TIMEOUT;
		};
		ssl->ss_state |= (SS_BREAK_QUIET | SS_TIMEOUT);
		ssl->ss_tid = timeout(ss_delay, (caddr_t)ssl, HZ/20);
	}
}

ss_zap(ssl, hangup_modem, wait_drop_dcd)
	struct ss_line *ssl;
     	int	hangup_modem; 	/* 1 if should hang up */
	int 	wait_drop_dcd;	/* 1 if should wait for carrier to drop */
{
	if (ssl->pss->ss_devdep->ssdd_zap != NULL) {
		return((*ssl->pss->ss_devdep->ssdd_zap) (ssl));
	};

	ss_brk_off(ssl);
	if (hangup_modem) {
		ss_set_modem(ssl, 0, BSD43_TIOCM_DTR);


		/* wait for modem to drop line */
		if (wait_drop_dcd) {
			ssl->ss_state |= (SS_CLOSING | SS_TIMEOUT);
			ssl->ss_tid = timeout(ss_delay, (caddr_t)ssl, HZ/ 5);
			while (ssl->ss_state & SS_TIMEOUT) {
				if (sleep((caddr_t)ssl, STOPRI | PCATCH)){
					untimeout(ssl->ss_tid);
					break;
				}
			}
			ssl->ss_state &= ~(SS_CLOSING | SS_TIMEOUT);
		}
	}

	return((*ssl->pss->ss_devdep->ssdd_driver_control)(ssl,SS_DC_DISABLELINE,0));
	
}

/* Activate this line */
ss_act(ssl)
	struct	ss_line *ssl;
{
	int dcd;
	if (ssl->pss->ss_devdep->ssdd_act != NULL) {
		return((*ssl->pss->ss_devdep->ssdd_act) (ssl));
	}

	dcd = (*ssl->pss->ss_devdep->ssdd_driver_control)(ssl,SS_DC_ENABLELINE,0);

	return (dcd);
}

ss_set_modem(ssl, on_bits, off_bits)
struct	ss_line *ssl;
unsigned long on_bits, off_bits;
{
	register int s;

	s = spltty();

	on_bits &= ~ssl->ss_modem_state;
	off_bits &=  ssl->ss_modem_state;

	if (on_bits & BSD43_TIOCM_DTR){
		if (ss_print & SSDBG_PT5) 
			cmn_err(CE_CONT,"Turning DTR on\n");
		(*ssl->pss->ss_devdep->ssdd_modem_control) 
			(ssl,SS_MC_ENABLEDTR,0);
		ssl->ss_modem_state |= BSD43_TIOCM_DTR;
	}
	
	if (on_bits & BSD43_TIOCM_RTS){
		if (ss_print & SSDBG_PT5) 
			cmn_err(CE_CONT,"Turning RTS on\n");
		(*ssl->pss->ss_devdep->ssdd_modem_control)
			(ssl,SS_MC_ENABLEFLOW,0);
		ssl->ss_modem_state |= BSD43_TIOCM_RTS;
	}
	if (off_bits & BSD43_TIOCM_DTR){
		if (ss_print & SSDBG_PT5) 
			cmn_err(CE_CONT,"Turning DTR off\n");
		(*ssl->pss->ss_devdep->ssdd_modem_control)
			(ssl,SS_MC_DISABLEDTR,0);
		ssl->ss_modem_state &= ~BSD43_TIOCM_DTR;
	}
	if (off_bits & BSD43_TIOCM_RTS){
		if (ss_print & SSDBG_PT5) 
			cmn_err(CE_CONT,"Turning RTS off\n");
		(*ssl->pss->ss_devdep->ssdd_modem_control)
			(ssl,SS_MC_DISABLEFLOW,0);
		ssl->ss_modem_state &= ~BSD43_TIOCM_RTS;
	}
	splx(s);
}

ss_wait_for_not_busy(ssl)
	struct	ss_line *ssl;
{
	int	s;

	s = spltty();
	while (ssl->ss_state & SS_BUSY) {
		ssl->ss_state |= SS_WCLOSE;
		if (sleep((caddr_t)ssl, STOPRI | PCATCH)) {
			ssl->ss_state &= ~SS_WCLOSE;
			break;
		}
	};
	splx(s);
}
/* Cause a signal */
ss_mk_sig(ssl,sig)
struct ss_line *ssl;
int sig;
{
	 /* flush read queue, write queue will be flushed by M_FLUSH later */

        ss_flushr(ssl);
        flushq(ssl->ss_rq, FLUSHDATA);
	(void)putctl1(ssl->ss_rq->q_next, M_FLUSH,FLUSHRW);
	(void)putctl1(ssl->ss_rq->q_next, M_PCSIG, sig);
	return(0);
}

extern mblk_t *str_allocb();

/* send option bit up the stream */
ss_set_str_control(rq,opt,is_set)
	register queue_t *rq;
	int	opt;
	int	is_set;
{
	register mblk_t *bp;
	register struct stroptions *sop;

	if (! is_set)
		opt |= SO_NOT;

	bp = str_allocb(sizeof(struct stroptions),
				rq,BPRI_LO);
	if (!bp)
		return(0);

	bp->b_datap->db_type = M_SETOPTS;
	sop = (struct stroptions*)bp->b_rptr;
	bp->b_wptr += sizeof(struct stroptions);
	sop->so_flags = opt;

	putnext(rq, bp);
	return(1);
}

ctl_t_msg()
{

	cmn_err(CE_CONT,"*** WARNING: One more ctl-T will crash ");
	cmn_err(CE_CONT, "kernel dumping core!! *** \n");
}
