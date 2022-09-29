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
#ident	"$Header: clnt_kudp.c,v 1.2.1.6 90/06/06 17:28:21 wje Exp $"
/*
 * @(#)clnt_kudp.c 1.6 88/07/27 4.0NFSSRC SMI
 *
 * clnt_kudp.c
 * Implements a kernel UPD/IP based, client side RPC.
 *
 * Original includes: param.h systm.h user.h kernel.h proc.h socket.h
 * socketvar.h mbuf.h ../net/if.h ../net/route.h ../netinet/in.h
 * ../netinet/in_pcb.h ../rpc/types.h ../rpc/xdr.h ../rpc/auth.h
 * ../rpc/clnt.h ../rpc/rpc_msg.h
 */

#include "sys/types.h"
#include "sys/signal.h"		/* needed by user.h */
#include "sys/pcb.h"		/* needed by user.h */
#include "sys/user.h"

#include "sys/immu.h"		/* needed by proc.h */
#include "sys/sbd.h"		/* needed by proc.h */
#include "sys/region.h"		/* needed by proc.h */
#include "sys/proc.h"

#include "sys/errno.h"
#include "sys/param.h"
#include "sys/systm.h"
#include "sys/utsname.h"

#include "sys/socket.h"
#include "sys/socketvar.h"
#include "sys/mbuf.h"
#include "sys/debug.h"

#include "bsd/net/if.h"
#include "bsd/net/route.h"
#include "bsd/netinet/in.h"
#include "bsd/netinet/in_pcb.h"

#include "../rpc/types.h"
#include "../rpc/xdr.h"
#include "../rpc/auth.h"
#include "../rpc/clnt.h"
#include "../rpc/rpc_msg.h"


struct mbuf	*ku_recvfrom();
int		ckuwakeup();

enum clnt_stat	clntkudp_callit();
void		clntkudp_abort();
void		clntkudp_error();
bool_t		clntkudp_freeres();
bool_t		clntkudp_control();
void		clntkudp_destroy();

void		xdrmbuf_init();

/*
 * Operations vector for UDP/IP based RPC
 */
/* static */ struct clnt_ops udp_ops = {
	clntkudp_callit,	/* do rpc call */
	clntkudp_abort,		/* abort call */
	clntkudp_error,		/* return error status */
	clntkudp_freeres,	/* free results */
	clntkudp_destroy,	/* destroy rpc handle */
	clntkudp_control	/* the ioctl() of rpc */
};

/*
 * Private data per rpc handle.  This structure is allocated by
 * clntkudp_create, and freed by cku_destroy.
 */
struct cku_private {
#ifdef RISCOS
	u_short			 cku_flags;	/* see below */
	signed short		 cku_count;	/* count of clustered mbufs */
	                                        /* holding outbuf references */
#else
	u_int			 cku_flags;	/* see below */
#endif
	CLIENT			 cku_client;	/* client handle */
	int			 cku_retrys;	/* request retrys */
	struct socket		*cku_sock;	/* open udp socket */
	struct sockaddr_in	 cku_addr;	/* remote address */
	struct rpc_err		 cku_err;	/* error status */
	XDR			 cku_outxdr;	/* xdr routine for output */
	XDR			 cku_inxdr;	/* xdr routine for input */
	u_int			 cku_outpos;	/* position of in output mbuf */
	char			*cku_outbuf;	/* output buffer */
	char			*cku_inbuf;	/* input buffer */
	struct mbuf		*cku_inmbuf;	/* input mbuf */
	struct ucred		*cku_cred;	/* credentials */
};

struct {
	int	rccalls;
	int	rcbadcalls;
	int	rcretrans;
	int	rcbadxids;
	int	rctimeouts;
	int	rcwaits;
	int	rcnewcreds;
	int	rcbadverfs;
} rcstat;


#define	ptoh(p)		(&((p)->cku_client))
#define	htop(h)		((struct cku_private *)((h)->cl_private))

/* cku_flags */
#define	CKU_TIMEDOUT	0x001
#define	CKU_BUSY	0x002
#define	CKU_WANTED	0x004

#define	CKU_BUFBUSY	0x008
#define	CKU_BUFWANTED	0x010
#define CKU_ONCE_ONLY	0x020
#ifdef RISCOS
#define CKU_INTERRUPTABLE 0x080
#endif

/* Times to retry */
#define	RECVTRIES	2
#define	SNDTRIES	4

int	clntkudpxid;		/* transaction id used by all clients */

clntkudp_once(cl, flag)
	CLIENT  *cl;
{
	struct cku_private *p = (struct cku_private *)cl->cl_private;

	if (flag != 0) {
		p->cku_flags |= CKU_ONCE_ONLY;
	} else {
		p->cku_flags &= ~CKU_ONCE_ONLY;
	}
}

#ifdef RISCOS
int
clntkudp_interruptable(cl, setflag)
        CLIENT  *cl;
        bool_t  setflag;
{
        struct cku_private *p = (struct cku_private *)cl->cl_private;

        if (setflag) {
	  p->cku_flags |= CKU_INTERRUPTABLE;
	}
	else {
	  p->cku_flags &= ~CKU_INTERRUPTABLE;
	}
}
#endif

static
noop()
{
}

static
buffree(p)
	struct cku_private *p;
{
#ifdef RISCOS
	ASSERT(p->cku_count > 0);
	if (--p->cku_count <= 0 && (p->cku_flags & CKU_BUFWANTED)) {
	        p->cku_count = 0;
#else
	p->cku_flags &= ~CKU_BUFBUSY;
	if (p->cku_flags & CKU_BUFWANTED) {
#endif
		p->cku_flags &= ~CKU_BUFWANTED;
		wakeup((caddr_t)&p->cku_outbuf);
	}
}

#ifdef RISCOS
static
bufdup(p)
	struct cku_private *p;
{
#ifdef RISCOS
	ASSERT(p->cku_count > 0);
	p->cku_count++;
#else
	p->cku_flags |= CKU_BUFBUSY;
#endif
}
#endif

/*
 * Create an rpc handle for a udp rpc connection.
 * Allocates space for the handle structure and the private data, and
 * opens a socket.  Note sockets and handles are one to one.
 */
CLIENT *
clntkudp_create(addr, pgm, vers, retrys, cred)
	struct sockaddr_in *addr;
	u_long pgm;
	u_long vers;
	int retrys;
	struct ucred *cred;
{
	register CLIENT *h;
	register struct cku_private *p;
	int error = 0;
	struct rpc_msg call_msg;
	struct mbuf *m, *mclgetx();
	extern int nfs_portmon;

#ifdef RPCDEBUG
	rpc_debug(4, "clntkudp_create(%X, %d, %d, %d\n",
	    addr->sin_addr.s_addr, pgm, vers, retrys);
#endif
	p = (struct cku_private *)kmem_zalloc(sizeof *p);
	h = ptoh(p);

	if (!clntkudpxid) {
		clntkudpxid = time.tv_usec;
	}

	/* handle */
	h->cl_ops = &udp_ops;
	h->cl_private = (caddr_t) p;
	h->cl_auth = authkern_create();

	/* call message, just used to pre-serialize below */
	call_msg.rm_xid = 0;
	call_msg.rm_direction = CALL;
	call_msg.rm_call.cb_rpcvers = RPC_MSG_VERSION;
	call_msg.rm_call.cb_prog = pgm;
	call_msg.rm_call.cb_vers = vers;

	/* private */
	clntkudp_init(h, addr, retrys, cred);
	p->cku_outbuf = (char *)kmem_alloc((u_int)UDPMSGSIZE);
#ifdef RISCOS
	m = mclgetx(noop, 0, noop, 0, p->cku_outbuf, UDPMSGSIZE, M_WAIT);
#else
	m = mclgetx(noop, 0, p->cku_outbuf, UDPMSGSIZE, M_WAIT);
#endif
	if (m == NULL)
		goto bad;
	xdrmbuf_init(&p->cku_outxdr, m, XDR_ENCODE);

	/* pre-serialize call message header */
	if (! xdr_callhdr(&(p->cku_outxdr), &call_msg)) {
		printf("clntkudp_create - Fatal header serialization error.");
		(void) m_freem(m);
		goto bad;
	}
	p->cku_outpos = XDR_GETPOS(&(p->cku_outxdr));
	(void) m_free(m);

	/* open udp socket */
	error = socreate(AF_INET, &p->cku_sock, SOCK_DGRAM, IPPROTO_UDP);
	if (error) {
		printf("clntkudp_create: socket creation problem, %d", error);
		goto bad;
	}
	if (error = bindresvport(p->cku_sock)) {
		printf("clntkudp_create: socket bind problem, %d", error);
		goto bad;
	}
	return (h);

bad:
	kmem_free((caddr_t)p->cku_outbuf, (u_int)UDPMSGSIZE);
	kmem_free((caddr_t)p, (u_int)sizeof (struct cku_private));
#ifdef RPCDEBUG
	rpc_debug(4, "create failed\n");
#endif
	return ((CLIENT *)NULL);
}

clntkudp_init(h, addr, retrys, cred)
	CLIENT *h;
	struct sockaddr_in *addr;
	int retrys;
	struct ucred *cred;
{
	struct cku_private *p = htop(h);

	p->cku_retrys = retrys;
	p->cku_addr = *addr;
	p->cku_cred = cred;
#ifdef RISCOS
	p->cku_flags &= (CKU_BUFWANTED);
#else
	p->cku_flags &= (CKU_BUFBUSY | CKU_BUFWANTED);
#endif
}

/*
 * Time out back off function. tim is in hz
 */
#define MAXTIMO	(60 * HZ)
#define backoff(tim)	((((tim) << 1) > MAXTIMO) ? MAXTIMO : ((tim) << 1))

/*
 * Call remote procedure.
 * Most of the work of rpc is done here.  We serialize what is left
 * of the header (some was pre-serialized in the handle), serialize
 * the arguments, and send it off.  We wait for a reply or a time out.
 * Timeout causes an immediate return, other packet problems may cause
 * a retry on the receive.  When a good packet is received we deserialize
 * it, and check verification.  A bad reply code will cause one retry
 * with full (longhand) credentials.
 */
enum clnt_stat
clntkudp_callit_addr(h, procnum, xdr_args, argsp, xdr_results, resultsp, wait,
		sin)
	register CLIENT	*h;
	u_long		procnum;
	xdrproc_t	xdr_args;
	caddr_t		argsp;
	xdrproc_t	xdr_results;
	caddr_t		resultsp;
	struct timeval	wait;
	struct sockaddr_in *sin;
{
	register struct cku_private *p = htop(h);
	register XDR		   *xdrs;
	struct sockaddr_in         from;
	register struct socket	   *so = p->cku_sock;
	int			   rtries;
	int			   stries = p->cku_retrys;
	int			   s;
	struct ucred		   *tmpcred;
	struct mbuf		   *m;
	struct rpc_msg		   reply_msg;
	int timohz;
	u_long xid;
	u_int rempos = 0;
	int refreshes = 2;	/* number of times to refresh credential */
#ifdef RISCOS
	int interrupted;        /* return from sleep() */
	int smask;              /* saved signal mask */
	struct proc *pp = u.u_procp;
#endif

#ifdef RPCDEBUG
	rpc_debug(4, "cku_callit\n");
#endif
	rcstat.rccalls++;

	while (p->cku_flags & CKU_BUSY) {
		rcstat.rcwaits++;
		p->cku_flags |= CKU_WANTED;
		(void) sleep((caddr_t)h, PZERO-2);
	}
	p->cku_flags |= CKU_BUSY;

	if ((p->cku_flags & CKU_ONCE_ONLY) != 0)
		stries = 1;

	/*
	 * Set credentials into the u structure
	 */
	tmpcred = u.u_cred;
	u.u_cred = p->cku_cred;

	xid = clntkudpxid++;

	/*
	 * This is dumb but easy: keep the time out in units of hz
	 * so it is easy to call timeout and modify the value.
	 */
	timohz = wait.tv_sec * HZ + (wait.tv_usec * HZ) / 1000000;

call_again:

	/*
	 * Wait til buffer gets freed then make a type 2 mbuf point at it
	 * The buffree routine clears CKU_BUFBUSY and does a wakeup when
	 * the mbuf gets freed.
	 */
	s = splimp();
#ifdef RISCOS
	while (p->cku_count > 0) {
#else
	while (p->cku_flags & CKU_BUFBUSY) {
#endif
		p->cku_flags |= CKU_BUFWANTED;
		/*
		 * This is a kludge to avoid deadlock in the case of a
		 * loop-back call.  The client can block waiting for
		 * the server to free the mbuf while the server is blocked
		 * waiting for the client to free the reply mbuf.  Avoid this
		 * by flushing the input queue every once in a while while
		 * we are waiting.
		 */
		timeout(wakeup, (caddr_t)&p->cku_outbuf, HZ);
		(void) sleep((caddr_t)&p->cku_outbuf, PZERO-3);
		sbflush(&so->so_rcv);
	}
#ifdef RISCOS
	ASSERT(p->cku_count == 0);
	p->cku_count = 1;
#else
	p->cku_flags |= CKU_BUFBUSY;
#endif
	(void) splx(s);

#ifdef RISCOS
	m = mclgetx(buffree, (int)p, bufdup, (int)p, p->cku_outbuf,
		    UDPMSGSIZE, M_WAIT);
#else
	m = mclgetx(buffree, (int)p, p->cku_outbuf, UDPMSGSIZE, M_WAIT);
#endif
	if (m == NULL) {
		p->cku_err.re_status = RPC_SYSTEMERROR;
		p->cku_err.re_errno = ENOBUFS;
		buffree(p);
		goto done;
	}

	xdrs = &p->cku_outxdr;
	/*
	 * The transaction id is the first thing in the
	 * preserialized output buffer.
	 */
	(*(u_long *)(p->cku_outbuf)) = xid;

	xdrmbuf_init(xdrs, m, XDR_ENCODE);

	if (rempos != 0) {
		XDR_SETPOS(xdrs, rempos);
	} else {
		/*
		 * Serialize dynamic stuff into the output buffer.
		 */
		XDR_SETPOS(xdrs, p->cku_outpos);
		if ((! XDR_PUTLONG(xdrs, (long *)&procnum)) ||
		    (! AUTH_MARSHALL(h->cl_auth, xdrs)) ||
		    (! (*xdr_args)(xdrs, argsp))) {
			p->cku_err.re_status = RPC_CANTENCODEARGS;
			p->cku_err.re_errno = EIO;
			(void) m_freem(m);
			goto done;
		}
		rempos = XDR_GETPOS(xdrs);
	}
	m->m_len = rempos;

	if ((p->cku_err.re_errno = ku_sendto_mbuf(so, m, &p->cku_addr)) != 0) {
		p->cku_err.re_status = RPC_CANTSEND;
		p->cku_err.re_errno = EIO;
		goto done;
	}

	for (rtries = RECVTRIES; rtries; rtries--) {
		s = splnet();
		while (so->so_rcv.sb_cc == 0) {
			/*
			 * Set timeout then wait for input, timeout
			 * or interrupt.
			 */
#ifdef RPCDEBUG
			rpc_debug(3, "callit: waiting %d\n", timohz);
#endif
			timeout(ckuwakeup, (caddr_t)p, timohz);
			so->so_rcv.sb_flags |= SB_WAIT;
#ifdef RISCOS
                        /*
                         * Juggling the sigmask every time is overkill in
			 * addition to the fact that not everyone who calls
			 * us handles the RPC_INTR return value well. Hence
			 * we optimize the case by having the calling client
			 * explicit set the interruptable flag in the client
			 * handle before calling us when the mounted filesystem
			 * is hard mounted and has the intr option set.
                         */
                        if (p->cku_flags & CKU_INTERRUPTABLE) {
				/* Optimized Sun bug 1012533 fix */
				smask = pp->p_hold;
				pp->p_hold |= ~(sigmask(SIGHUP) |
						   sigmask(SIGINT) |
						   sigmask(SIGQUIT) |
						   sigmask(SIGTERM));

				interrupted = sleep((caddr_t)&so->so_rcv.sb_cc,
						    (PZERO+1 | PCATCH));
				pp->p_hold = smask;
			}
			else {
			  interrupted = 0;
			  (void) sleep((caddr_t)&so->so_rcv.sb_cc, PRIBIO);
			}
			untimeout_func(ckuwakeup, (caddr_t)p);
#else
			/* Original reference port code */
			(void) sleep((caddr_t)&so->so_rcv.sb_cc, PRIBIO);
			untimeout(ckuwakeup, (caddr_t)p);
#endif

#ifdef RISCOS
			if (interrupted) {
				(void) splx(s);
				p->cku_err.re_status = RPC_INTR;
				p->cku_err.re_errno = EINTR;
				goto done;
			}
#endif
			if (p->cku_flags & CKU_TIMEDOUT) {
				p->cku_flags &= ~CKU_TIMEDOUT;
				(void) splx(s);
				p->cku_err.re_status = RPC_TIMEDOUT;
				p->cku_err.re_errno = ETIMEDOUT;
				rcstat.rctimeouts++;
				goto done;
			}
		} /* end of while */

		if (so->so_error) {
			so->so_error = 0;
			(void) splx(s);
			continue;
		}

		p->cku_inmbuf = ku_recvfrom(so, &from);
		if (sin) {
			*sin = from;
		}

		(void) splx(s);

		if (p->cku_inmbuf == NULL) {
			continue;
		}

		p->cku_inbuf = mtod(p->cku_inmbuf, char *);
		if (p->cku_inmbuf->m_len < sizeof (u_long)) {
			m_freem(p->cku_inmbuf);
			continue;
		}
		/*
		 * If reply transaction id matches id sent
		 * we have a good packet.
		 */
		if (*((u_long *)(p->cku_inbuf))
		     != *((u_long *)(p->cku_outbuf))) {
			rcstat.rcbadxids++;
			m_freem(p->cku_inmbuf);
			continue;
		}
		/*
		 * Flush the rest of the stuff on the input queue
		 * for the socket.
		 */
		s = splnet();
		sbflush(&so->so_rcv);
		(void) splx(s);
		break;
	}

	if (rtries == 0) {
		p->cku_err.re_status = RPC_CANTRECV;
		p->cku_err.re_errno = EIO;
		goto done;
	}

	/*
	 * Process reply
	 */
	xdrs = &(p->cku_inxdr);
	xdrmbuf_init(xdrs, p->cku_inmbuf, XDR_DECODE);

	reply_msg.acpted_rply.ar_verf = _null_auth;
	reply_msg.acpted_rply.ar_results.where = resultsp;
	reply_msg.acpted_rply.ar_results.proc = xdr_results;

	/*
	 * Decode and validate the response.
	 */
	if (xdr_replymsg(xdrs, &reply_msg)) {
		_seterr_reply(&reply_msg, &(p->cku_err));

		if (p->cku_err.re_status == RPC_SUCCESS) {
			/*
			 * Reply is good, check auth.
			 */
			if (! AUTH_VALIDATE(h->cl_auth,
			    &reply_msg.acpted_rply.ar_verf)) {
				p->cku_err.re_status = RPC_AUTHERROR;
				p->cku_err.re_why = AUTH_INVALIDRESP;
				rcstat.rcbadverfs++;
			}
			if (reply_msg.acpted_rply.ar_verf.oa_base !=
			    NULL) {
				/* free auth handle */
				xdrs->x_op = XDR_FREE;
				(void) xdr_opaque_auth(xdrs,
				    &(reply_msg.acpted_rply.ar_verf));
			}
		} else {
		  	p->cku_err.re_errno = EIO;
			/*
			 * Maybe our credential needs refreshed
			 */
			if (refreshes > 0 && AUTH_REFRESH(h->cl_auth)) {
				refreshes--;
				rcstat.rcnewcreds++;
				rempos = 0;
			}
		}
	} else {
		p->cku_err.re_status = RPC_CANTDECODERES;
		p->cku_err.re_errno = EIO;
	}

	m_freem(p->cku_inmbuf);

#ifdef RPCDEBUG
	rpc_debug(4, "cku_callit done\n");
#endif

/*
 * done stop and cleanup
 */
done:
	if ((p->cku_err.re_status != RPC_SUCCESS) &&
#ifdef RISCOS
	    (p->cku_err.re_status != RPC_INTR) &&
#endif
	    (p->cku_err.re_status != RPC_CANTENCODEARGS) &&
	    (--stries > 0)) {
		rcstat.rcretrans++;
		timohz = backoff(timohz);
		if (p->cku_err.re_status == RPC_SYSTEMERROR ||
		    p->cku_err.re_status == RPC_CANTSEND) {
			/*
			 * Errors due to lack of resources, wait a bit
			 * and try again.
			 */
			(void) sleep((caddr_t)&lbolt, PZERO-4);
		}
		goto call_again;
	}
	u.u_cred = tmpcred;
	/*
	 * Insure that buffer is not busy prior to releasing client handle.
	 */
	s = splimp();
#ifdef RISCOS
	while (p->cku_count > 0) {
#else
	while (p->cku_flags & CKU_BUFBUSY) {
#endif
		p->cku_flags |= CKU_BUFWANTED;
		timeout(wakeup, (caddr_t)&p->cku_outbuf, HZ);
		(void) sleep((caddr_t)&p->cku_outbuf, PZERO-3);
		sbflush(&so->so_rcv);
	}
	(void) splx(s);

	p->cku_flags &= ~CKU_BUSY;
	if (p->cku_flags & CKU_WANTED) {
		p->cku_flags &= ~CKU_WANTED;
		wakeup((caddr_t)h);
	}
	if (p->cku_err.re_status != RPC_SUCCESS) {
		rcstat.rcbadcalls++;
	}
	return (p->cku_err.re_status);
}

enum clnt_stat
clntkudp_callit(h, procnum, xdr_args, argsp, xdr_results, resultsp, wait)
	register CLIENT *h;
	u_long		procnum;
	xdrproc_t	xdr_args;
	caddr_t		argsp;
	xdrproc_t	xdr_results;
	caddr_t		resultsp;
	struct timeval	wait;
{
	register struct cku_private *p = htop(h);

	return (clntkudp_callit_addr(h, procnum, xdr_args, argsp, xdr_results,
		resultsp, wait, (struct sockaddr_in *)0));
}

/*
 * Wake up client waiting for a reply.
 */
ckuwakeup(p)
	register struct cku_private *p;
{
	register struct socket	*so = p->cku_sock;

#ifdef RPCDEBUG
	rpc_debug(4, "cku_timeout\n");
#endif
	p->cku_flags |= CKU_TIMEDOUT;
	sbwakeup(&so->so_rcv);
}

/*
 * Return error info on this handle.
 */
void
clntkudp_error(h, err)
	CLIENT *h;
	struct rpc_err *err;
{
	register struct cku_private *p = htop(h);

	*err = p->cku_err;
}

/* static */ bool_t
clntkudp_freeres(cl, xdr_res, res_ptr)
	CLIENT *cl;
	xdrproc_t xdr_res;
	caddr_t res_ptr;
{
	register struct cku_private *p = (struct cku_private *)cl->cl_private;
	register XDR *xdrs = &(p->cku_outxdr);

	xdrs->x_op = XDR_FREE;
	return ((*xdr_res)(xdrs, res_ptr));
}

void
clntkudp_abort()
{
}

bool_t
clntkudp_control()
{
	return (FALSE);
}


/*
 * Destroy rpc handle.
 * Frees the space used for output buffer, private data, and handle
 * structure, and closes the socket for this handle.
 */
void
clntkudp_destroy(h)
	CLIENT *h;
{
	register struct cku_private *p = htop(h);

#ifdef RPCDEBUG
	rpc_debug(4, "cku_destroy %x\n", h);
#endif
	(void) soclose(p->cku_sock);
	kmem_free((caddr_t)p->cku_outbuf, (u_int)UDPMSGSIZE);
	kmem_free((caddr_t)p, sizeof (*p));
}

/*
 * try to bind to a reserved port
 */
bindresvport(so)
	struct socket *so;
{
	struct sockaddr_in *sin;
	struct mbuf *m;
	u_short i;
	int error;
	struct ucred *tmpcred;
	struct ucred *savecred;

#	define MAX_PRIV	(IPPORT_RESERVED-1)
#	define MIN_PRIV	(IPPORT_RESERVED/2)

	m = m_get(M_WAIT, MT_SONAME);
	if (m == NULL) {
		printf("bindresvport: couldn't alloc mbuf");
		return (ENOBUFS);
	}

	sin = mtod(m, struct sockaddr_in *);
	sin->sin_family = AF_INET;
	sin->sin_addr.s_addr = INADDR_ANY;
	m->m_len = sizeof (struct sockaddr_in);

	/*
	 * Only root can bind to a privileged port number, so
	 * temporarily change the uid to 0 to do the bind.
	 */
	tmpcred = crdup(u.u_cred);
	savecred = u.u_cred;
	u.u_cred = tmpcred;
	u.u_uid = 0;
	error = EADDRINUSE;
	for (i = MAX_PRIV; error == EADDRINUSE && i >= MIN_PRIV; i--) {
		sin->sin_port = htons(i);
		error = sobind(so, m);
	}
	(void) m_freem(m);
	u.u_cred = savecred;
	crfree(tmpcred);
	return (error);
}
