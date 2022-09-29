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
#ident	"$Header: slip.c,v 1.3.1.2 90/05/10 05:30:30 wje Exp $"
/*
 * Serial Line IP streams module 
 *
 * Derived from work done by Rayan Sachariassen and Doug Kingston.
 *
 * Rayan Zachariassen <rayan@ai.toronto.edu>
 * 880929
 * Doug Kingston <dpk@morgan.com>
 * 881007
 *
 */


#include "sys/types.h"
#include "sys/param.h"
#include "sys/stream.h"
#include "sys/stropts.h"
#include "sys/log.h"
#include "bsd43/sys/syslog.h"
#include "sys/termio.h"
#include "sys/sbd.h"

#include "sys/signal.h"
#include "sys/pcb.h"
#include "sys/user.h"
#include "sys/systm.h"
#include "bsd/sys/mbuf.h"
#include "bsd/sys/socket.h"
#include "sys/errno.h"
#include "sys/ioctl.h"
#include "sys/file.h"

#include "bsd/sys/uio.h"
#include "bsd/net/if.h"
#include "bsd/net/netisr.h"
#include "bsd/net/route.h"
#include "bsd/net/soioctl.h"

#include "bsd/netinet/in.h"
#include "bsd/netinet/in_systm.h"
#include "bsd/netinet/in_var.h"
#include "bsd/netinet/ip.h"
#include "sys/kmem.h"
#include "sys/slip.h"

#define	SLIP_DEBUG	0		/* set it to 1 for debugging messages */

#ifdef	SLIP_FASTECHO
#include "bsd/netinet/tcp.h"

#ifndef	IPPORT_TELNET
#define	IPPORT_TELNET	23
#endif

#ifndef	IPPORT_LOGINSERVER
#define	IPPORT_LOGINSERVER	513
#endif

/* compile time: calculate offsets to the interesting numbers in the packets */
#define	P_PROTO	(int)&(((struct ip *)0)->ip_p)
#define	P_SPORT	(sizeof (struct ip) + (int)&(((struct tcphdr *)0)->th_sport))
#define	P_DPORT	(sizeof (struct ip) + (int)&(((struct tcphdr *)0)->th_dport))

/* define which TCP ports should be favoured */
#define	HIPRIOTCPPORT(p)  ((p) == IPPORT_TELNET || (p) == IPPORT_LOGINSERVER)
#endif	/* SLIP_FASTECHO */


#define SLIP_BUFSIZ	(SLIPMTU + sizeof(struct ifnet *))

#ifndef	MIN
#define	MIN(a, b)	(((a) < (b)) ? (a) : (b))
#endif

/*
 * Stream module related declarations
 *
 * Note: the STREAM module name and the SLIPIFNAME are independent! 
 *
 */

static struct module_info minfo = {
	0x6963, "slip", 0, INFPSZ, 16384, 4096
};

static int slip_open(), slip_rput(), slip_wput(), slip_wsrv(), slip_close();

static struct qinit rinit = {
	slip_rput, NULL, slip_open, slip_close, NULL, &minfo, NULL
};

static struct qinit winit = {
	slip_wput, slip_wsrv, NULL, NULL, NULL, &minfo, NULL
};

struct streamtab slipinfo = { &rinit, &winit, NULL, NULL };


static struct mbuf *slip_btom();



/*
 * Configurable parameters (declared in master.d file)
 */
 
extern int	nslip;			/* configured number of slip units */
extern struct slipdata slippery[];


/*
 * If you need to pre-attach all the slip interfaces (perhaps because your
 * routed is surprised by new interfaces appearing out of the blue), then
 * append "init slip_attach" to the "pseudo-device slip" line in your config
 * file.  For example:
 *
 *	pseudo-device slip5 init slip_attach
 *
 * The slip_attach routine will then be called from the autoconf code
 * for each unit specified.
 */

int
slip_attach(unit)
	int unit;
{
	register struct ifnet *ifp = &slippery[unit].ifnet;
	extern int slip_ioctl(), slip_output();

	ifp->if_name = SLIPIFNAME;
	ifp->if_mtu = SLIPMTU;
	ifp->if_flags = IFF_POINTOPOINT;
	ifp->if_unit = unit;
	ifp->if_ioctl = slip_ioctl;
	ifp->if_output = slip_output;
	/* if_snd.ifq_maxlen is set elsewhere if not set here */
	/* ifp->if_snd.ifq_maxlen = IFQ_MAXLEN; */
	if_attach(ifp);
	return 0;
}

static int
slip_open(q, dev, flag, sflag)
	queue_t	*q;
	dev_t	dev;
	int	flag, sflag;
{
	register int unit;
	register struct slipdata *sdp;
	struct mbuf *p;
	int s;

	if (!suser()) {
		u.u_error = EPERM;
		return OPENFAIL;
	}

	if ((sdp = (struct slipdata *)q->q_ptr) == NULL) {
	    s = splstr();
	    for (unit = 0; unit < nslip; ++unit) {
		sdp = &slippery[unit];

		if (sdp->buf)	/* in use */
			continue;

		if (sdp->ifnet.if_mtu == 0)
			(void) slip_attach(unit);
#ifdef	SLIP_CLUSTERS
		/* assert MCLBYTES > SLIP_BUFSIZ */
		MCLALLOC(p, 1);
		if ((sdp->buf = (u_char *)p) == NULL) {
			(void) splx(s);
			log(BSD43_LOG_ERR, "slip%d: cannot allocate cluster\n", unit);
			return OPENFAIL;
		}
#else	/* !SLIP_CLUSTERS */
		sdp->buf = (u_char *)kmem_alloc(SLIP_BUFSIZ);
		/* kmem_alloc never fails */
#endif	/* SLIP_CLUSTERS */
		sdp->dp = sdp->buf + sizeof (struct ifnet *);
		sdp->inlen = 0;
		sdp->q = WR(q);
#if mips
		sdp->sawhangup = 0;
#endif
		sdp->sawescape = sdp->overrun = 0;
		sdp->userflags = 0;
#ifdef	SLIP_FASTECHO
		sdp->lastpriopkt = NULL;
#endif	/* SLIP_FASTECHO */
#ifdef	SLIP_STATS
		sdp->stats.sl_ibytes = sdp->stats.sl_obytes = 0;
		sdp->stats.sl_ipackets = sdp->stats.sl_opackets = 0;
		sdp->stats.sl_oerrors = sdp->stats.sl_ierrors = 0;
#endif	/* SLIP_STATS */
		WR(q)->q_ptr = q->q_ptr = (caddr_t)sdp;
		(void) splx(s);
#if SLIP_DEBUG
		log(BSD43_LOG_INFO, "slip%d: coming up\n", sdp->ifnet.if_unit);
#endif /* SLIP_DEBUG */
		return unit;
	    }
	    (void) splx(s);
	    log(BSD43_LOG_ERR, "slip: can't allocate interface (nslip is %d)\n", nslip);
	    return OPENFAIL;
	}
	return (sdp-&slippery[0]);
}

static int
slip_close(q, flag)
	queue_t	*q;
	int	flag;
{
	int	s;
	struct slipdata *sdp;

	s = splimp();	/* paranoid */
	sdp = (struct slipdata *)q->q_ptr;
	if (sdp) {
#ifdef	SLIP_RTMAINT
		extern struct mbuf *rthost[], *rtnet[];
		extern void slip_ifrtfree();

		/* remove all routes referring to this interface */
		slip_ifrtfree(&sdp->ifnet, rthost);
		slip_ifrtfree(&sdp->ifnet, rtnet);
#endif	/* SLIP_RTMAINT */
#if SLIP_DEBUG
		log(BSD43_LOG_INFO, "slip%d: going down\n", sdp->ifnet.if_unit);
#endif /* SLIP_DEBUG */
		/* mark the interface down and free resources */
		if_down(&sdp->ifnet);
		/* I really wanted to do an if_detach() here... */
#if mips
		if_detach(&sdp->ifnet);
		sdp->ifnet.if_mtu = 0;
		sdp->sawhangup = 0;
#endif
		if (sdp->buf)
#ifdef	SLIP_CLUSTERS
			MCLFREE((struct mbuf *)sdp->buf);
#else	/* !SLIP_CLUSTERS */
			kmem_free(sdp->buf, SLIP_BUFSIZ);
#endif	/* SLIP_CLUSTERS */
		sdp->buf = 0;
		sdp->q = 0;
	}
	(void) splx(s);
}

#ifdef	SLIP_RTMAINT
/*
 * Go through the entire route table, removing all routes referring to
 * the specified interface.  This is called when we mark the interface
 * down in the slip_close() routine, in order to ensure that all routes
 * get deleted *before* the interface definition is changed.  The reason
 * to do it here and not in a routing process at user level, is that in
 * some circumstances (subnet interfaces) it is not possible to remove
 * a route if its interface is gone.  Obviously doing it here is not
 * clean, but it seems the better of evils.
 */

static void
slip_ifrtfree(ifp, table)
	struct ifnet *ifp;
	struct mbuf *table[];
{
	register struct mbuf *m, **mprev;
	register struct rtentry *rt;
	register int i;
	struct sockaddr_in *sin;
	int s;
	extern int rttrash;
#if later
	extern char *inet_ntoa();
#endif /* later */

	s = splimp();
	for (i = 0; i < RTHASHSIZ; ++i) {	/* ouch! at splimp() too! */
		for (mprev = &table[i]; m = *mprev; mprev = &m->m_next) {
			rt = mtod(m, struct rtentry *);
			if (RTHASHMOD(rt->rt_hash) != i)
				continue;
			if (rt->rt_ifp != ifp || !(rt->rt_flags & RTF_UP))
				continue;
			/* delete the route */
			sin = (struct sockaddr_in *)&rt->rt_dst;
#if later
			log(BSD43_LOG_INFO, "slip%d: deleting route to %s\n",
				      ifp->if_unit, inet_ntoa(sin->sin_addr));
#endif /* later */
			*mprev = m->m_next;
			if (rt->rt_refcnt > 0) {
				rt->rt_flags &= ~RTF_UP;
				rttrash++;
				m->m_next = 0;
			} else
				(void) m_free(m);
		}
	}
	(void) splx(s);
}
#endif	/* SLIP_RTMAINT */

/*
 * The data flows in this scheme are as follows:
 *
 *		User-Level slip.c
 *			|
 *			|
 *		   stream head
 *		   ^	     :
 *                 :	     :
 *		   : 	     v
 * ipintrq <-- slip_rput slip_wput
 *		   ^	     : 
 *		   |         :
 *		   |	      /--- slip_wsrv <--- slip_output <--- ip_output
 *		   |	     v
 *		port stream driver
 *
 * Note: slip_rput and slip_wput only pass non-M_DATA packets on.
 *	 There should be a way for the user-level code to request a copy of
 *	 all incoming and outgoing ip packets.
 *
 */

/*
 * It is not valid to write any data to us, but there might be ioctl's.
 * In any case, all user-initiated messages must be dealt with here.
 */

static int
slip_wput(q, mp)
	queue_t	*q;	/* pointer to read/write q */
	mblk_t	*mp;	/* pointer to message being passed */
{
	struct slipdata *sdp;
	struct iocblk *iocp;
	int size;

	switch (mp->b_datap->db_type) {
	case M_DATA:	/* invalid message type, junk it */
#ifdef	SLIP_DEBUG
		log(BSD43_LOG_DEBUG, "slip%d: dropping data from user\n",
			((struct slipdata *)q->q_ptr)->ifnet.if_unit);
#endif
		freemsg(mp);
		return;
	case M_IOCTL:
		/*
		 * The user-level daemon does an I_STR/SLIOGUNIT ioctl
		 * to retrieve the unit number allocated to the connection.
		 * It then uses this and SLIPIFNAME to affect interface params.
		 * Since this isn't a clone driver, stat() doesn't help.
		 */
		iocp = (struct iocblk *)mp->b_rptr; /* assert word-aligned */
		sdp = (struct slipdata *)q->q_ptr;
		switch (iocp->ioc_cmd) {
		case SLIOGUNIT:
		case SLIOGSTATS:
			if (iocp->ioc_cmd == SLIOGUNIT)
				size = sizeof (int);
			else if (iocp->ioc_cmd == SLIOGSTATS)
				size = sizeof (struct slipstat);
			iocp->ioc_rval = 0;
			if ((mp->b_cont = allocb(size, BPRI_MED)) == NULL) {
				mp->b_datap->db_type = M_IOCNAK;
				iocp->ioc_error = ENOSR;
			} else {
				u_char *cp = mp->b_cont->b_wptr;

				mp->b_datap->db_type = M_IOCACK;
				if (iocp->ioc_cmd == SLIOGUNIT)
					*((int *)cp) = sdp->ifnet.if_unit;
				else /* if (iocp->ioc_cmd == SLIOGSTATS) */
					*((struct slipstat *)cp) = sdp->stats;
				mp->b_cont->b_wptr += size;
				iocp->ioc_count = size;
				iocp->ioc_error = 0;
			}
			break;
		case SLIOSFLAGS:
			sdp->userflags = *(int *)mp->b_cont->b_rptr;
			mp->b_datap->db_type = M_IOCACK;
			iocp->ioc_rval = iocp->ioc_count = iocp->ioc_error = 0;
			break;
		default:
			putnext(q, mp);
			return;
		}
		qreply(q, mp);
		return;
	case M_FLUSH:
		if (*mp->b_rptr & FLUSHW)
			flushq(q, FLUSHDATA);
		break;
	default:	/* who knows what else is coming down here... */
		log(BSD43_LOG_DEBUG, "slip%d: wput: unknown message type (%d)\n",
			sdp->ifnet.if_unit, mp->b_datap->db_type);
	}
	putnext(q, mp);
}

/*
 * Take a message from the network that it wants to route via us,
 * and schedule it to be sent out the tty port below us.
 */

static int
slip_output(ifp, m0, dst)
	struct ifnet *ifp;
	struct mbuf *m0;
	struct sockaddr *dst;
{
	register u_char *cp;
	register int c, n;
	register mblk_t *mp;
	register int	len;
	register struct mbuf *m;
	register struct slipdata *sdp;
	int hiprio, s;

	sdp = &slippery[ifp->if_unit];
#if mips
	if (sdp->sawhangup)
#else
	if (sdp->q == 0)
#endif
		return ENETDOWN;

	ifp->if_opackets++;
#ifdef	SLIP_STATS
	sdp->stats.sl_opackets++;
#endif	/* SLIP_STATS */

	switch (dst->sa_family) {
#ifdef INET
	case AF_INET:
		/* We want to queue the data in m0 onto the write q */
		/* Looking at the data twice is silly, but at this speed... */

#ifdef	SLIP_FASTECHO
		/*
		 * This is an attempt at giving interactive traffic
		 * higher priority than bulk data transfers.
		 */
		/* assert P_DPORT > P_SPORT > P_PROTO */
		hiprio = m0->m_len > P_DPORT
			&& *(mtod(m0, u_char *) + P_PROTO) == IPPROTO_TCP;
		if (hiprio) {
			/* the IP code puts the IP/TCP headers into one mbuf */
			cp = mtod(m0, u_char *) + P_SPORT;
			c = *(u_short *)cp;
			hiprio = HIPRIOTCPPORT(c);
			if (!hiprio) {
				cp += P_DPORT - P_SPORT;
				c = *(u_short *)cp;
				hiprio = HIPRIOTCPPORT(c);
			}
		}
#endif	/* SLIP_FASTECHO */
		/* First count how many bytes we need in total */
		len = 2;	/* start and end END */
		for (m = m0; m != 0; m = m->m_next) {
			cp = mtod(m, u_char *);
			n = m->m_len;
			len += n;
#ifdef	SLIP_STATS
			sdp->stats.sl_obytes += n;
#endif	/* SLIP_STATS */
			while (--n >= 0)
				if ((c = *cp++) == END || c == ESC)
					++len;
		}
		/* now allocate a message block with len bytes in it */
		if ((mp = allocb(len, BPRI_MED)) == NULL) {
			ifp->if_oerrors++;
#ifdef	SLIP_STATS
			sdp->stats.sl_oerrors++;
#endif	/* SLIP_STATS */
			log(BSD43_LOG_INFO, "slip%d: slip_output can't allocb %d\n",
			      ifp->if_unit, len);
			m_freem(m0);
			return ENOSR;
		}
		/* and copy the SLIP'ed data bytes into it */
		*mp->b_wptr++ = END;
		for (m = m0; m != 0; m = m->m_next) {
			cp = mtod(m, u_char *);
			for (n = m->m_len; --n >= 0; ) {
				c = *cp++;
#ifdef	SLIP_UCACHE
				if (!(c & END))
					*mp->b_wptr++ = c;
				else
#endif	/* SLIP_UCACHE */
				if (c == END) {
					*mp->b_wptr++ = ESC;
					*mp->b_wptr++ = ESC_END;
				} else if (c == ESC) {
					*mp->b_wptr++ = ESC;
					*mp->b_wptr++ = ESC_ESC;
				} else
					*mp->b_wptr++ = c;
			}
		}
		*mp->b_wptr++ = END;
		/* then queue it for later transmission through the port */
		s = splstr();
		if (sdp->q) {	/* inside splstr() in case sdp->q = 0 */
#ifdef	SLIP_FASTECHO
			if (hiprio) {	/* macchiavelli mode */
				register mblk_t	*tp = sdp->q->q_next->q_first;
				/*
				 * We want this packet to come before other
				 * IP packets but after other TELNET/LOGINSERVER
				 * packets, ignoring real priority messages.
				 */
				for (; tp != NULL; tp = tp->b_next)
					if (tp == sdp->lastpriopkt)
						break;
				if (tp == NULL)
					tp = sdp->q->q_next->q_first;
				if (tp == NULL)
					/* since insq doesn't schedule the q! */
					putnext(sdp->q, mp);
				else
					insq(sdp->q->q_next, tp, mp);
				sdp->lastpriopkt = mp;
			} else
#endif	/* SLIP_FASTECHO */
				putq(sdp->q, mp);
		} else
			freemsg(mp);
		(void) splx(s);
		m_freem(m0);
		break;
#endif  /* INET */
	default:
		log(BSD43_LOG_INFO, "slip%d: slip_output can't handle af%d\n",
			      ifp->if_unit, dst->sa_family);
		m_freem(m0);
		return EAFNOSUPPORT;
	}
	return 0;
}

/*
 * Shuffle messages from the service queue down to the port.
 */

static int
slip_wsrv(q)
	register queue_t *q;
{
	register mblk_t	*mp;

	while ((mp = getq(q)) != NULL) {
		if (!canput(q->q_next)) {
			putbq(q, mp);
			return;
		}
		putnext(q, mp);
	}
}

/* slip_rput and slip_btom were ``inspired'' by code in the old slip driver */

/*
 * Buffer up the data from messages sent by the port driver in a buffer
 * that we will send to the network code to figure out.
 */

static int
slip_rput(q, mp)
	queue_t	*q;	/* pointer to read/write q */
	mblk_t	*mp;	/* pointer to message being passed */
{
	register u_char ch;
	register u_char *rp;
	register struct slipdata *sdp = (struct slipdata *)q->q_ptr;
	struct mbuf *m;
	mblk_t	*bp;
	int	s;

	if (sdp == NULL || sdp->buf == NULL) {
		log(BSD43_LOG_DEBUG, "slip: rput: sdp%s NULL\n", sdp ? "->buf" : "");
		freemsg(mp);
		return;
	}

	switch (mp->b_datap->db_type) {
	case M_DATA:
		break;
	case M_FLUSH:
		if (*mp->b_rptr & FLUSHR)
			flushq(q, FLUSHDATA);
		putnext(q, mp);
		return;
#ifdef	M_UNHANGUP
	case M_UNHANGUP:
		/* M_UNHANGUP is sent on disconnects AND reconnects... */
		log(BSD43_LOG_DEBUG, "slip%d: UNHANGUP\n", sdp->ifnet.if_unit);
		sdp->q = WR(q);	/* sdp->q may not be 0 before this */
		putnext(q, mp);
		return;
#endif	/* M_UNHANGUP */
	case M_HANGUP:
		/*
		 * The stream head will send a SIGHUP to the process group
		 * (if any) with this controlling TTY.  In the meantime,
		 * set things up so slip_output() returns ENETDOWN.
		 */
#if SLIP_DEBUG
		log(BSD43_LOG_DEBUG, "slip%d: HANGUP\n", sdp->ifnet.if_unit);
#endif /* SLIP_DEBUG */
		s = splstr();
#if mips
		sdp->sawhangup = 1;
#else
		sdp->q = 0;	/* beware the putnext() in slip_output() */
#endif
		(void) splx(s);
		putnext(q, mp);
		return;
	case M_BREAK:
		putnext(q, mp);
		return;
#if mips
	case M_IOCACK:
	case M_IOCNAK:
		putnext(q, mp);
		return;
#endif
	default:
		log(BSD43_LOG_DEBUG, "slip%d: rput: message type %d\n",
			sdp->ifnet.if_unit, mp->b_datap->db_type);
		putnext(q, mp);
		return;
	}

	for (bp = mp; bp != 0; bp = bp->b_cont) {
		rp = bp->b_rptr;
		while (rp < bp->b_wptr) {
			ch = *rp++;
			if (sdp->sawescape) {
				sdp->sawescape = 0;
				if (ch == ESC_END)
					ch = END;
				else if (ch == ESC_ESC)
					ch = ESC;
				else
					sdp->ifnet.if_ierrors++;
				/* accept the character */
			} else if (ch == END) {
				if (sdp->overrun) {
					sdp->overrun = 0;
					log(BSD43_LOG_ERR,
					    "slip%d: inlen(%d) > SLIPMTU(%d)\n",
					    sdp->ifnet.if_unit, sdp->inlen,
					    SLIPMTU);
					sdp->inlen = 0;
					continue;
				}
				if (sdp->inlen == 0)	/* ignore */
					continue;
				m = slip_btom(sdp);
				sdp->dp = sdp->buf + sizeof(struct ifnet *);
#ifdef	SLIP_STATS
				sdp->stats.sl_ibytes += sdp->inlen;
#endif	/* SLIP_STATS */
				sdp->inlen = 0;
				if (m == NULL) {
					sdp->ifnet.if_ierrors++;
#ifdef	SLIP_STATS
					sdp->stats.sl_ierrors++;
#endif	/* SLIP_STATS */
					continue;
				}
				sdp->ifnet.if_ipackets++;
#ifdef	SLIP_STATS
				sdp->stats.sl_ipackets++;
#endif	/* SLIP_STATS */
				s = splimp();
				if (IF_QFULL(&ipintrq)) {
					IF_DROP(&ipintrq);
					sdp->ifnet.if_ierrors++;
					m_freem(m);
					(void) splx(s);
					log(BSD43_LOG_DEBUG,
					    "slip%d: rput: ipintrq full\n",
					    sdp->ifnet.if_unit);
				} else {
					IF_ENQUEUE(&ipintrq, m);
					schednetisr(NETISR_IP);
					(void) splx(s);
				}
				continue;
			} else if (ch == ESC) {
				sdp->sawescape = 1;
				continue;
			}
			if (++sdp->inlen > SLIPMTU) {
				if (sdp->overrun)
					continue;
				sdp->overrun = 1;
				sdp->ifnet.if_ierrors++;
#ifdef	SLIP_STATS
				sdp->stats.sl_ierrors++;
#endif	/* SLIP_STATS */
				sdp->dp = sdp->buf + sizeof(struct ifnet *);
				continue;
			}
			*sdp->dp++ = ch;
		}
		bp->b_rptr = rp;
	}
	freemsg(mp);
}

/*
 * Copy data buffer to mbuf chain; add ifnet pointer ifp.
 */

static
struct mbuf *
slip_btom(sdp)
	struct slipdata *sdp;
{
	register int	len;
	register struct mbuf *m, **mp;
	struct mbuf *top = NULL;
	struct ifnet *ifp;
	caddr_t cp;
	unsigned count;

	cp = (caddr_t)(sdp->buf + sizeof(struct ifnet *));
	mp = &top;
	len = sdp->inlen;
	ifp = &sdp->ifnet;
	while (len > 0) {
		MGET(m, M_DONTWAIT, MT_DATA);
		if ((*mp = m) == NULL) {
			if (top)
				m_freem(top);
			return NULL;
		}
		if (ifp)
			m->m_off += sizeof(ifp);
#ifdef	SLIP_CLUSTERS
		/*
		 * If we have at least MCLBYTES/2 bytes, allocate a new page.
		 * Swap the current buffer page with the new one.  We depend
		 * on having a space left at the beginning of the buffer for
		 * the interface pointer.
		 */
		if (len >= MCLBYTES/2) {
			MCLGET(m);
			if (m->m_len == MCLBYTES) {
				cp = mtod(m, char *);

				/* magic, give old cluster to new mbuf */
				m->m_off = (int)sdp->buf - (int)m;

				/* magic, give new cluster to sdp */
				sdp->buf = (u_char *)cp;

				if (ifp) {
					m->m_off += sizeof(ifp);
					count = MIN(len,
					    MCLBYTES - sizeof(struct ifnet *));
				} else
					count = MIN(len, MCLBYTES);
				goto nocopy;
			}
		}
#endif	/* SLIP_CLUSTERS */
		if (ifp)
			count = MIN(len, MLEN - sizeof(ifp));
		else
			count = MIN(len, MLEN);
		bcopy(cp, mtod(m, caddr_t), count);
nocopy:
		m->m_len = count;
		if (ifp) {
			m->m_off -= sizeof(ifp);
			m->m_len += sizeof(ifp);
			*mtod(m, struct ifnet **) = ifp;
			ifp = NULL;
		}
		cp += count;
		len -= count;
		mp = &m->m_next;
	}
	return top;
}

/*
 * Process an ioctl request.
 */

static int
slip_ioctl(ifp, cmd, data)
	register struct ifnet *ifp;
	int cmd;
	caddr_t data;
{
	register struct ifaddr *ifa = (struct ifaddr *)data;
	int s, error = 0;

	if (ifa == NULL)		/* etherfind can cause this */
		return EFAULT;
	s = splimp();
	switch (cmd) {
	case SIOCSIFADDR:
		switch (ifa->ifa_addr.sa_family) {
#ifdef	AF_INET
		case AF_INET:
			ifp->if_flags |= IFF_UP;
			break;
#endif	/* AF_INET */
		default:
			error = EAFNOSUPPORT;
			break;
		}
		break;
	case SIOCSIFDSTADDR:
		switch (ifa->ifa_addr.sa_family) {
#ifdef	AF_INET
		case AF_INET:
			break;
#endif	/* AF_INET */
		default:
			error = EAFNOSUPPORT;
			break;
		}
		break;
	default:
		error = EINVAL;
	}
	(void) splx(s);
	return error;
}
