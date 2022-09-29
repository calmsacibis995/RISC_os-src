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
#ident	"$Header: clnt_raw.c,v 1.4.1.2 90/05/07 20:57:31 wje Exp $"
/*
 * @(#)clnt_raw.c 1.4 88/07/27 4.0NFSSRC Copyr 1988 Sun Micro
 * @(#)clnt_raw.c 1.23 88/02/08 SMI
 *
 * clnt_raw.c
 *
 * Memory based rpc for simple testing and timing.
 * Interface to create an rpc client and server in the same process.
 * This lets us similate rpc and get round trip overhead, without
 * any interference from the kernal.
 */

#include <rpc/rpc.h>

#ifdef SYSTYPE_BSD43
#include <syslog.h>
#endif
#ifdef SYSTYPE_SYSV
#include <bsd/syslog.h>
#endif

#define MCALL_MSG_SIZE 24

/*
 * This is the "network" we will be moving stuff over.
 */
static struct clntraw_private {
	CLIENT	client_object;
	XDR	xdr_stream;
	char	_raw_buf[UDPMSGSIZE];
	char	mashl_callmsg[MCALL_MSG_SIZE];
	u_int	mcnt;
} *clntraw_private;

static enum clnt_stat	clntraw_call();
static void		clntraw_abort();
static void		clntraw_geterr();
static bool_t		clntraw_freeres();
static bool_t		clntraw_control();
static void		clntraw_destroy();

static struct clnt_ops client_ops = {
	clntraw_call,
	clntraw_abort,
	clntraw_geterr,
	clntraw_freeres,
	clntraw_destroy,
	clntraw_control
};

void	svc_getreq();

/*
 * Create a client handle for memory based rpc.
 */
CLIENT *
clntraw_create(prog, vers)
	u_long prog;
	u_long vers;
{
	register struct clntraw_private *clp = clntraw_private;
	struct rpc_msg call_msg;
	XDR *xdrs;
	CLIENT	*client;

	if (clp == 0) {
		clp = (struct clntraw_private *)calloc(1, sizeof (*clp));
		if (clp == 0)
			return (0);
		clntraw_private = clp;
	}
	/*
	 * pre-serialize the staic part of the call msg and stash it away
	 */
#ifdef RISCOS
	client = &clp->client_object;
	xdrs = &clp->xdr_stream;
#endif
	call_msg.rm_direction = CALL;
	call_msg.rm_call.cb_rpcvers = RPC_MSG_VERSION;
	call_msg.rm_call.cb_prog = prog;
	call_msg.rm_call.cb_vers = vers;
	xdrmem_create(xdrs, clp->mashl_callmsg, MCALL_MSG_SIZE, XDR_ENCODE); 
	if (! xdr_callhdr(xdrs, &call_msg)) {
#ifdef RISCOS
		syslog(LOG_ERR,"clnt_raw.c - Fatal header serialization error.");
#else
		perror("clnt_raw.c - Fatal header serialization error.");
#endif
	}
	clp->mcnt = XDR_GETPOS(xdrs);
	XDR_DESTROY(xdrs);

	/*
	 * Set xdrmem for client/server shared buffer
	 */
	xdrmem_create(xdrs, clp->_raw_buf, UDPMSGSIZE, XDR_FREE);

	/*
	 * create client handle
	 */
	client->cl_ops = &client_ops;
	client->cl_auth = authnone_create();
	return (client);
}

static enum clnt_stat 
clntraw_call(h, proc, xargs, argsp, xresults, resultsp, timeout)
	CLIENT *h;
	u_long proc;
	xdrproc_t xargs;
	caddr_t argsp;
	xdrproc_t xresults;
	caddr_t resultsp;
	struct timeval timeout;
{
	register struct clntraw_private *clp = clntraw_private;
	register XDR *xdrs = &clp->xdr_stream;
	struct rpc_msg msg;
	enum clnt_stat status;
	struct rpc_err error;

	if (clp == 0)
		return (RPC_FAILED);
call_again:
	/*
	 * send request
	 */
	xdrs->x_op = XDR_ENCODE;
	XDR_SETPOS(xdrs, 0);
	((struct rpc_msg *)clp->mashl_callmsg)->rm_xid ++ ;
	if ((! XDR_PUTBYTES(xdrs, clp->mashl_callmsg, clp->mcnt)) ||
	    (! XDR_PUTLONG(xdrs, (long *)&proc)) ||
	    (! AUTH_MARSHALL(h->cl_auth, xdrs)) ||
	    (! (*xargs)(xdrs, argsp))) {
		return (RPC_CANTENCODEARGS);
	}
	(void)XDR_GETPOS(xdrs);  /* called just to cause overhead */

	/*
	 * We have to call server input routine here because this is
	 * all going on in one process. Yuk.
	 */
	svc_getreq(1);

	/*
	 * get results
	 */
	xdrs->x_op = XDR_DECODE;
	XDR_SETPOS(xdrs, 0);
	msg.acpted_rply.ar_verf = _null_auth;
	msg.acpted_rply.ar_results.where = resultsp;
	msg.acpted_rply.ar_results.proc = xresults;
	if (! xdr_replymsg(xdrs, &msg))
		return (RPC_CANTDECODERES);
	_seterr_reply(&msg, &error);
	status = error.re_status;

	if (status == RPC_SUCCESS) {
		if (! AUTH_VALIDATE(h->cl_auth, &msg.acpted_rply.ar_verf)) {
			status = RPC_AUTHERROR;
		}
	}  /* end successful completion */
	else {
		if (AUTH_REFRESH(h->cl_auth))
			goto call_again;
	}  /* end of unsuccessful completion */

	if (status == RPC_SUCCESS) {
		if (! AUTH_VALIDATE(h->cl_auth, &msg.acpted_rply.ar_verf)) {
			status = RPC_AUTHERROR;
		}
		if (msg.acpted_rply.ar_verf.oa_base != NULL) {
			xdrs->x_op = XDR_FREE;
			(void)xdr_opaque_auth(xdrs, &(msg.acpted_rply.ar_verf));
		}
	}

	return (status);
}

static void
clntraw_geterr()
{
}


static bool_t
clntraw_freeres(cl, xdr_res, res_ptr)
	CLIENT *cl;
	xdrproc_t xdr_res;
	caddr_t res_ptr;
{
	register struct clntraw_private *clp = clntraw_private;
	register XDR *xdrs = &clp->xdr_stream;

	if (clp == 0)
		return ((bool_t)RPC_FAILED);
	xdrs->x_op = XDR_FREE;
	return ((*xdr_res)(xdrs, res_ptr));
}

static void
clntraw_abort()
{
}

static bool_t
clntraw_control()
{
	return (FALSE);
}

static void
clntraw_destroy()
{
}
