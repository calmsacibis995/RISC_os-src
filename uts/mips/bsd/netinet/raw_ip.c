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
#ident	"$Header: raw_ip.c,v 1.4.4.4 90/05/28 17:58:59 wje Exp $"

/*
 * Copyright (c) 1982, 1986 Regents of the University of California.
 * All rights reserved.  The Berkeley software License Agreement
 * specifies the terms and conditions for redistribution.
 *
 *	@(#)raw_ip.c	7.1 (Berkeley) 6/5/86
 */

#ifdef mips
#include "../tcp-param.h"
#include "sys/param.h"
#include "sys/mbuf.h"
#include "sys/socket.h"
#include "sys/protosw.h"
#include "sys/socketvar.h"
#include "sys/errno.h"
#else
#include "../h/param.h"
#include "../h/mbuf.h"
#include "../h/socket.h"
#include "../h/protosw.h"
#include "../h/socketvar.h"
#include "../h/errno.h"
#endif

#include "../net/if.h"
#include "../net/route.h"
#include "../net/raw_cb.h"

#include "in.h"
#include "in_systm.h"
#include "ip.h"
#include "ip_var.h"

/*
 * Raw interface to IP protocol.
 */

struct	sockaddr_in ripdst = { AF_INET };
struct	sockaddr_in ripsrc = { AF_INET };
struct	sockproto ripproto = { PF_INET };
/*
 * Setup generic address and protocol structures
 * for raw_input routine, then pass them along with
 * mbuf chain.
 */
rip_input(m)
	struct mbuf *m;
{
	register struct ip *ip = mtod(m, struct ip *);

	ripproto.sp_protocol = ip->ip_p;
	ripdst.sin_addr = ip->ip_dst;
	ripsrc.sin_addr = ip->ip_src;
	raw_input(m, &ripproto, (struct sockaddr *)&ripsrc,
	  (struct sockaddr *)&ripdst);
}
/*
 * Generate IP header and pass packet to ip_output.
 * Tack on options user may have setup with control call.
 *
 * Note: This procedure was taken from a post Tahoe release, to include
 * the critical hook required to support the 'traceroute' program.
 */
rip_output(m, so)
	register struct mbuf *m;
	struct socket *so;
{
	register struct ip *ip;
	int error;
	struct rawcb *rp = sotorawcb(so);
	struct sockaddr_in *sin;
	short proto = rp->rcb_proto.sp_protocol;

	/*
	 * if the protocol is IPPROTO_RAW, the user handed us a 
	 * complete IP packet.  Otherwise, allocate an mbuf for a
	 * header and fill it in as needed.
	 */
	if (proto != IPPROTO_RAW) {
		/*
		 * Calculate data length and get an mbuf
		 * for IP header.
		 */
		int len = 0;
		struct mbuf *m0;

		for (m0 = m; m; m = m->m_next)
			len += m->m_len;

		m = m_get(M_DONTWAIT, MT_HEADER);
		if (m == 0) {
			m = m0;
			error = ENOBUFS;
			goto bad;
		}
		m->m_off = MMAXOFF - sizeof(struct ip);
		m->m_len = sizeof(struct ip);
		m->m_next = m0;

		ip = mtod(m, struct ip *);
		ip->ip_tos = 0;
		ip->ip_off = 0;
		ip->ip_p = proto;
		ip->ip_len = sizeof(struct ip) + len;
		ip->ip_ttl = MAXTTL;
	} else
		ip = mtod(m, struct ip *);

	if (rp->rcb_flags & RAW_LADDR) {
		sin = (struct sockaddr_in *)&rp->rcb_laddr;
		if (sin->sin_family != AF_INET) {
			error = EAFNOSUPPORT;
			goto bad;
		}
		ip->ip_src.s_addr = sin->sin_addr.s_addr;
	} else
		ip->ip_src.s_addr = 0;

	ip->ip_dst = ((struct sockaddr_in *)&rp->rcb_faddr)->sin_addr;

	return (ip_output(m, rp->rcb_options, &rp->rcb_route, 
	   (so->so_options & SO_DONTROUTE) | IP_ALLOWBROADCAST));
bad:
	m_freem(m);
	return (error);
}
/*
 * Raw IP socket option processing.
 */
rip_ctloutput(op, so, level, optname, m)
	int op;
	struct socket *so;
	int level, optname;
	struct mbuf **m;
{
	int error = 0;
	register struct rawcb *rp = sotorawcb(so);

	if (level != IPPROTO_IP)
		error = EINVAL;
	else switch (op) {

	case PRCO_SETOPT:
		switch (optname) {
		case IP_OPTIONS:
			return (ip_pcbopts(&rp->rcb_options, *m));

		default:
			error = EINVAL;
			break;
		}
		break;

	case PRCO_GETOPT:
		switch (optname) {
		case IP_OPTIONS:
			*m = m_get(M_WAIT, MT_SOOPTS);
			if (rp->rcb_options) {
				(*m)->m_off = rp->rcb_options->m_off;
				(*m)->m_len = rp->rcb_options->m_len;
				bcopy(mtod(rp->rcb_options, caddr_t),
				    mtod(*m, caddr_t), (unsigned)(*m)->m_len);
			} else
				(*m)->m_len = 0;
			break;
		default:
			error = EINVAL;
			break;
		}
		break;
	}
	if (op == PRCO_SETOPT)
		(void)m_free(*m);
	return (error);
}
