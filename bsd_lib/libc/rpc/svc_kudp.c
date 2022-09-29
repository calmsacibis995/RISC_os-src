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
#ident	"$Header: svc_kudp.c,v 1.2.1.4 90/05/10 20:13:15 wje Exp $"
/*
 * @(#)svc_kudp.c 1.2  88/04/28 4.0NFSSRC Copyr 1988 Sun Micro
 * @(#)svc_kudp.c 1.28 88/02/08 SMI
 *
 * svc_kudp.c,
 * Server side for UDP/IP based RPC in the kernel.
 *
 * Original kernel includes:
 * param.h systm.h ../rpc/types.h ../netinet/in.h
 * ../rpc/xdr.h ../rpc/auth.h ../rpc/clnt.h ../rpc/rpc_msg.h ../rpc/svc.h
 * socket.h socketvar.h mbuf.h
 */

#include "sys/types.h"
#include "sys/param.h"
#include "sys/systm.h"
#include "sys/debug.h"

#include "../rpc/types.h"
#include "bsd/netinet/in.h"
#include "../rpc/xdr.h"
#include "../rpc/auth.h"
#include "../rpc/clnt.h"
#include "../rpc/rpc_msg.h"
#include "../rpc/svc.h"
#include "sys/socket.h"
#include "sys/socketvar.h"
#include "sys/mbuf.h"

#define rpc_buffer(xprt) ((xprt)->xp_p1)

/*
 * Routines exported through ops vector.
 */
bool_t		svckudp_recv();
bool_t		svckudp_send();
enum xprt_stat	svckudp_stat();
bool_t		svckudp_getargs();
bool_t		svckudp_freeargs();
void		svckudp_destroy();

/*
 * Server transport operations vector.
 */
struct xp_ops svckudp_op = {
	svckudp_recv,		/* Get requests */
	svckudp_stat,		/* Return status */
	svckudp_getargs,	/* Deserialize arguments */
	svckudp_send,		/* Send reply */
	svckudp_freeargs,	/* Free argument data space */
	svckudp_destroy		/* Destroy transport handle */
};


struct mbuf	*ku_recvfrom();
void		xdrmbuf_init();

/*
 * Transport private data.
 * Kept in xprt->xp_p2.
 */
struct udp_data {
#ifdef RISCOS
	u_short	ud_flags;			/* flag bits, see below */
	signed short ud_count;			/* buffer reference count */
#else
	int	ud_flags;			/* flag bits, see below */
#endif
	u_long 	ud_xid;				/* id */
	struct	mbuf *ud_inmbuf;		/* input mbuf chain */
	XDR	ud_xdrin;			/* input xdr stream */
	XDR	ud_xdrout;			/* output xdr stream */
	char	ud_verfbody[MAX_AUTH_BYTES];	/* verifier */
};

/*
 * Flags
 */
#define	UD_BUSY		0x001		/* buffer is busy */
#define	UD_WANTED	0x002		/* buffer wanted */

/*
 * Server statistics
 */
struct {
	int	rscalls;
	int	rsbadcalls;
	int	rsnullrecv;
	int	rsbadlen;
	int	rsxdrcall;
} rsstat;

/*
 * Create a transport record.
 * The transport record, output buffer, and private data structure
 * are allocated.  The output buffer is serialized into using xdrmem.
 * There is one transport record per user process which implements a
 * set of services.
 */
SVCXPRT *
svckudp_create(sock, port)
	struct socket	*sock;
	u_short		 port;
{
	register SVCXPRT	 *xprt;
	register struct udp_data *ud;

#ifdef RPCDEBUG
	rpc_debug(4, "svckudp_create so = %x, port = %d\n", sock, port);
#endif
	xprt = (SVCXPRT *)kmem_alloc((u_int)sizeof(SVCXPRT));
	rpc_buffer(xprt) = (caddr_t)kmem_alloc((u_int)UDPMSGSIZE);
	ud = (struct udp_data *)kmem_alloc((u_int)sizeof(struct udp_data));
	bzero((caddr_t)ud, sizeof(*ud));
	xprt->xp_addrlen = 0;
	xprt->xp_p2 = (caddr_t)ud;
	xprt->xp_verf.oa_base = ud->ud_verfbody;
	xprt->xp_ops = &svckudp_op;
	xprt->xp_port = port;
	xprt->xp_sock = sock;
	xprt_register(xprt);
	return (xprt);
}
 
/*
 * Destroy a transport record.
 * Frees the space allocated for a transport record.
 */
void
svckudp_destroy(xprt)
	register SVCXPRT   *xprt;
{
	register struct udp_data *ud = (struct udp_data *)xprt->xp_p2;

#ifdef RPCDEBUG
	rpc_debug(4, "usr_destroy %x\n", xprt);
#endif
	if (ud->ud_inmbuf) {
		m_freem(ud->ud_inmbuf);
	}
	kmem_free((caddr_t)ud, (u_int)sizeof(struct udp_data));
	kmem_free((caddr_t)rpc_buffer(xprt), (u_int)UDPMSGSIZE);
	kmem_free((caddr_t)xprt, (u_int)sizeof(SVCXPRT));
}

/*
 * Receive rpc requests.
 * Pulls a request in off the socket, checks if the packet is intact,
 * and deserializes the call packet.
 */
bool_t
svckudp_recv(xprt, msg)
	register SVCXPRT	 *xprt;
	struct rpc_msg		 *msg;
{
	register struct udp_data *ud = (struct udp_data *)xprt->xp_p2;
	register XDR	 *xdrs = &(ud->ud_xdrin);
	register struct mbuf	 *m;
	int			  s;

#ifdef RPCDEBUG
	rpc_debug(4, "svckudp_recv %x\n", xprt);
#endif
	rsstat.rscalls++;
	s = splnet();
	m = ku_recvfrom(xprt->xp_sock, &(xprt->xp_raddr));
	(void) splx(s);
	if (m == NULL) {
		rsstat.rsnullrecv++;
		return (FALSE);
	}

	if (m->m_len < 4*sizeof(u_long)) {
		rsstat.rsbadlen++;
		goto bad;
	}
	xdrmbuf_init(&ud->ud_xdrin, m, XDR_DECODE);
	if (! xdr_callmsg(xdrs, msg)) {
		rsstat.rsxdrcall++;
		goto bad;
	}
	ud->ud_xid = msg->rm_xid;
	ud->ud_inmbuf = m;
#ifdef RPCDEBUG
	rpc_debug(5, "svckudp_recv done\n");
#endif
	return (TRUE);

bad:
	m_freem(m);
	ud->ud_inmbuf = NULL;
	rsstat.rsbadcalls++;
	return (FALSE);
}

#ifdef RISCOS
static
buffree(ud)
	register struct udp_data *ud;
{
	ASSERT(ud->ud_count > 0);
	if (--ud->ud_count <= 0 && ud->ud_flags & UD_WANTED) {
		ud->ud_count = 0;
		ud->ud_flags &= ~UD_WANTED;
		wakeup((caddr_t)ud);
	}
}
static
bufdup(ud)
	register struct udp_data *ud;
{
	ASSERT(ud->ud_count > 0);
	ud->ud_count++;
}
#else
static
buffree(ud)
	register struct udp_data *ud;
{
	ud->ud_flags &= ~UD_BUSY;
	if (ud->ud_flags & UD_WANTED) {
		ud->ud_flags &= ~UD_WANTED;
		wakeup((caddr_t)ud);
	}
}
#endif

/*
 * Send rpc reply.
 * Serialize the reply packet into the output buffer then
 * call ku_sendto to make an mbuf out of it and send it.
 */
bool_t
/* ARGSUSED */
svckudp_send(xprt, msg)
	register SVCXPRT *xprt; 
	struct rpc_msg *msg; 
{
	register struct udp_data *ud = (struct udp_data *)xprt->xp_p2;
	register XDR *xdrs = &(ud->ud_xdrout);
	register int slen;
	register int stat = FALSE;
	int s;
	struct mbuf *m, *mclgetx();

#ifdef RPCDEBUG
	rpc_debug(4, "svckudp_send %x\n", xprt);
#endif
	s = splimp();
#ifdef RISCOS
	while (ud->ud_count > 0) {
#else
	while (ud->ud_flags & UD_BUSY) {
#endif
		ud->ud_flags |= UD_WANTED;
		(void) sleep((caddr_t)ud, PZERO-2);
	}
#ifdef RISCOS
	ASSERT(ud->ud_count == 0);
	ud->ud_count = 1;
#else
	ud->ud_flags |= UD_BUSY;
#endif
	(void) splx(s);
#ifdef RISCOS
	m = mclgetx(buffree, (int)ud, bufdup, (int)ud, rpc_buffer(xprt),
		    UDPMSGSIZE, M_WAIT);
#else
	m = mclgetx(buffree, (int)ud, rpc_buffer(xprt), UDPMSGSIZE, M_WAIT);
#endif
	if (m == NULL) {
		buffree(ud);
		return (stat);
	}

	xdrmbuf_init(&ud->ud_xdrout, m, XDR_ENCODE);
	msg->rm_xid = ud->ud_xid;
	if (xdr_replymsg(xdrs, msg)) {
		slen = (int)XDR_GETPOS(xdrs);
		if (m->m_next == 0) {		/* XXX */
			m->m_len = slen;
		}
		if (!ku_sendto_mbuf(xprt->xp_sock, m, &xprt->xp_raddr))
			stat = TRUE;
	} else {
		printf("svckudp_send: xdr_replymsg failed\n");
		m_freem(m);
	}
	/*
	 * This is completely disgusting.  If public is set it is
	 * a pointer to a structure whose first field is the address
	 * of the function to free that structure and any related
	 * stuff.  (see rrokfree in nfs_xdr.c).
	 */
	if (xdrs->x_public) {
		(**((int (**)())xdrs->x_public))(xdrs->x_public);
	}
#ifdef RPCDEBUG
	rpc_debug(5, "svckudp_send done\n");
#endif
	return (stat);
}

/*
 * Return transport status.
 */
/*ARGSUSED*/
enum xprt_stat
svckudp_stat(xprt)
	SVCXPRT *xprt;
{

	return (XPRT_IDLE); 
}

/*
 * Deserialize arguments.
 */
bool_t
svckudp_getargs(xprt, xdr_args, args_ptr)
	SVCXPRT	*xprt;
	xdrproc_t	 xdr_args;
	caddr_t		 args_ptr;
{

	return ((*xdr_args)(&(((struct udp_data *)(xprt->xp_p2))->ud_xdrin), args_ptr));
}

bool_t
svckudp_freeargs(xprt, xdr_args, args_ptr)
	SVCXPRT	*xprt;
	xdrproc_t	 xdr_args;
	caddr_t		 args_ptr;
{
	register XDR *xdrs =
	    &(((struct udp_data *)(xprt->xp_p2))->ud_xdrin);
	register struct udp_data *ud = (struct udp_data *)xprt->xp_p2;

	if (ud->ud_inmbuf) {
		m_freem(ud->ud_inmbuf);
	}
	ud->ud_inmbuf = (struct mbuf *)0;
	if (args_ptr) {
		xdrs->x_op = XDR_FREE;
		return ((*xdr_args)(xdrs, args_ptr));
	} else {
		return (TRUE);
	}
}

/*
 * the dup cacheing routines below provide a cache of non-failure
 * transaction id's.  rpc service routines can use this to detect
 * retransmissions and re-send a non-failure response.
 */

struct dupreq {
	u_long		dr_xid;
	struct sockaddr_in dr_addr;
	u_long		dr_proc;
	u_long		dr_vers;
	u_long		dr_prog;
#ifdef RISCOS
	int		dr_busy;	/* request has been dispatched */
	int		dr_mark;	/* DUP_BUSY, DUP_DONE, DUP_FAIL */
	struct timeval	dr_time;	/* time associated with req */
#endif /* RISCOS */
	struct dupreq	*dr_next;
	struct dupreq	*dr_chain;
};

#ifndef RISCOS
/*
 * MAXDUPREQS is the number of cached items.  It should be adjusted
 * to the service load so that there is likely to be a response entry
 * when the first retransmission comes in.
 */

#define	MAXDUPREQS	400
#endif /* RISCOS */

#define	DUPREQSZ	(sizeof(struct dupreq) - 2*sizeof(caddr_t))
#define	DRHASHSZ	32
#define	XIDHASH(xid)	((xid) & (DRHASHSZ-1))
#define	DRHASH(dr)	XIDHASH((dr)->dr_xid)
#define	REQTOXID(req)	((struct udp_data *)((req)->rq_xprt->xp_p2))->ud_xid

int	ndupreqs;
#ifdef RISCOS
int	busyreqs;
int	secondreqs;
int	dupreqs_done;
int	dupreqs_busy;
int	dupreqs_fail;
int	dupcache_max;
#else
int	dupreqs;
#endif /* RISCOS */
int	dupchecks;
struct dupreq *drhashtbl[DRHASHSZ];

#ifdef RISCOS
/* routine to compare dup cache entries */
#define NOTDUP(dr, xid, req) (dr->dr_xid != xid || \
			    dr->dr_prog != req->rq_prog || \
			    dr->dr_vers != req->rq_vers || \
			    dr->dr_proc != req->rq_proc || \
			    bcmp((caddr_t)&dr->dr_addr, \
			     (caddr_t)&req->rq_xprt->xp_raddr, \
			     sizeof(dr->dr_addr)) != 0)

#endif /* RISCOS */

/*
 * drmru points to the head of a circular linked list in lru order.
 * drmru->dr_next == drlru
 */
struct dupreq *drmru;

#ifdef RISCOS
svckudp_dupsave(req, transtime, transmark)
	register struct svc_req *req;
	struct timeval transtime;
	int	transmark;
#else
svckudp_dupsave(req)
	register struct svc_req *req;
#endif /* RISCOS */
{
	register struct dupreq *dr;
#ifdef RISCOS
	u_long xid;
	int n = 0;
	extern long nfs_maxdupreqs;

	xid = REQTOXID(req);
	dr = drhashtbl[XIDHASH(xid)]; 

	while (dr != NULL) { 
		++n;
		if (NOTDUP(dr, xid, req)) {
			dr = dr->dr_chain;
			continue;
		}
		break;
	}

	if (dr == NULL) {
		if (ndupreqs < nfs_maxdupreqs) {
			dr = (struct dupreq *)kmem_alloc(sizeof(*dr));
#else

	if (ndupreqs < MAXDUPREQS) {
		dr = (struct dupreq *)kmem_alloc(sizeof(*dr));
#endif /* RISCOS */
			if (drmru) {
				dr->dr_next = drmru->dr_next;
				drmru->dr_next = dr;
			} else {
				dr->dr_next = dr;
			}
			ndupreqs++;
		} else {
			dr = drmru->dr_next;
			unhash(dr);
		}
		drmru = dr;

		dr->dr_xid = REQTOXID(req);
		dr->dr_prog = req->rq_prog;
		dr->dr_vers = req->rq_vers;
		dr->dr_proc = req->rq_proc;
		dr->dr_addr = req->rq_xprt->xp_raddr;
		dr->dr_chain = drhashtbl[DRHASH(dr)];
		drhashtbl[DRHASH(dr)] = dr;
#ifdef RISCOS
	}

	dr->dr_time = transtime;
	dr->dr_mark = transmark;
	if (transmark == DUP_BUSY)
		dr->dr_busy = 1;
#endif /* RISCOS */
}

#ifdef RISCOS
svckudp_dup(req, ptime, pmark)
	register struct svc_req *req;
	struct timeval *ptime;
	int *pmark;
#else
svckudp_dup(req)
	register struct svc_req *req;
#endif /* RISCOS */
{
	register struct dupreq *dr;
	u_long xid;
	 
	dupchecks++;
	xid = REQTOXID(req);
	dr = drhashtbl[XIDHASH(xid)]; 
	while (dr != NULL) { 
#ifdef RISCOS
		if (NOTDUP(dr, xid, req)) {
			dr = dr->dr_chain;
			continue;
		}
		if (!dr->dr_busy)
			printf("svckudp_dup: not busy\n");
		if (dr->dr_mark == DUP_DONE) {
			dupreqs_done++;
			*pmark = dr->dr_mark;
			*ptime = dr->dr_time;
			return (1);
		}
		if (dr->dr_mark == DUP_BUSY) 
			dupreqs_busy++;
		else
			dupreqs_fail++;

		return (0);
#else
		if (dr->dr_xid != xid ||
		    dr->dr_prog != req->rq_prog ||
		    dr->dr_vers != req->rq_vers ||
		    dr->dr_proc != req->rq_proc ||
		    bcmp((caddr_t)&dr->dr_addr,
		     (caddr_t)&req->rq_xprt->xp_raddr,
		     sizeof(dr->dr_addr)) != 0) {
			dr = dr->dr_chain;
			continue;
		} else {
			dupreqs++;
			return (1);
		}
#endif /* RISCOS */
	}
	return (0);
}

#ifdef RISCOS
int avoid_seconds = 6;

svckudp_dupbusy(req)
	register struct svc_req *req;
{
	register struct dupreq *dr;
	u_long xid;
	 
	xid = REQTOXID(req);

	dr = drhashtbl[XIDHASH(xid)]; 
	while (dr != NULL) { 
		if (NOTDUP(dr, xid, req)) {
			dr = dr->dr_chain;
			continue;
		}
		if (dr->dr_busy) {
			busyreqs++;
			return (1);
		}
		if (avoid_seconds && dr->dr_mark == DUP_DONE &&
			time.tv_sec - dr->dr_time.tv_sec < avoid_seconds) {
			secondreqs++;
			return (1);
		}
		dr->dr_busy = 1;
		return (0);
	}

	svckudp_dupsave(req, time, DUP_BUSY);
	return (0);

}

svckudp_dupdone(req)
	register struct svc_req *req;
{
	register struct dupreq *dr;
	u_long xid;
	 
	xid = REQTOXID(req);

	dr = drhashtbl[XIDHASH(xid)]; 
	while (dr != NULL) { 
		if (NOTDUP(dr, xid, req)) {
			dr = dr->dr_chain;
			continue;
 		}
		dr->dr_busy = 0;
		return (0);
	}

	return (0);

}
#endif /* RISCOS */

static
unhash(dr)
	struct dupreq *dr;
{
	struct dupreq *drt;
	struct dupreq *drtprev = NULL;
	 
	drt = drhashtbl[DRHASH(dr)]; 
	while (drt != NULL) { 
		if (drt == dr) { 
			if (drtprev == NULL) {
				drhashtbl[DRHASH(dr)] = drt->dr_chain;
			} else {
				drtprev->dr_chain = drt->dr_chain;
			}
			return; 
		}	
		drtprev = drt;
		drt = drt->dr_chain;
	}	
}
