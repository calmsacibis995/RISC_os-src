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
#ident	"$Header: pmap_rmt.c,v 1.3.1.3 90/05/07 21:00:16 wje Exp $"
/*
 * @(#)pmap_rmt.c 1.1 86/09/24 Copyr 1984 Sun Micro
 *
 * pmap_rmt.c
 * Client interface to pmap rpc service.
 * remote call and broadcast service
 */

#ifdef KERNEL
#include "sys/types.h"
#include "sys/errno.h"
#include "sys/signal.h"		/* needed by user.h */
#include "sys/pcb.h"		/* needed by user.h */
#include "sys/user.h"

#include "bsd/sys/time.h"
#include "sys/socket.h"
#include "bsd/net/if.h"
#include "bsd/net/soioctl.h"
#include "bsd/arpa/inet.h"

#ifdef notdef
#include "bsd43/sys/msgbuf.h"
#include "bsd43/sys/syslog.h"
#endif

#include "../rpc/rpc.h"
#include "../rpc/pmap_prot.h"
#include "../rpc/pmap_clnt.h"
#define rpc_perror(x) log(BSD43_LOG_ERR,"%s\n",x)

#else
#include <rpc/rpc.h>
#include <rpc/pmap_prot.h>
#include <rpc/pmap_clnt.h>
#include <stdio.h>
#include <errno.h>
#ifdef SYSTYPE_BSD43
#include <sys/socket.h>
#include <sys/time.h>
#include <net/if.h>
#include <sys/ioctl.h>
#include <arpa/inet.h>
#include <sys/syslog.h>
#endif
#ifdef SYSTYPE_SYSV
#include <bsd/sys/socket.h>
#include <bsd/sys/time.h>
#include <bsd/net/if.h>
#include <bsd/sys/ioctl.h>
#include <bsd/arpa/inet.h>
#include <bsd/syslog.h>
#endif
#endif /* KERNEL */
#define MAX_BROADCAST_SIZE 1400

extern int errno;
static struct timeval timeout = { 3, 0 };

/*
 * Structures and XDR routines for parameters to and replys from
 * the pmapper remote-call-service.
 */

struct rmtcallargs {
	u_long prog, vers, proc, arglen;
	caddr_t args_ptr;
	xdrproc_t xdr_args;
};

bool_t xdr_rmtcall_args();

struct rmtcallres {
	u_long *port_ptr;
	u_long resultslen;
	caddr_t results_ptr;
	xdrproc_t xdr_results;
};

#ifdef KERNEL
extern CLIENT *clntkudp_create(/* addr, pgm, vers, retries, cred */);
#define retries 4
#endif

bool_t xdr_rmtcallres();

/*
 * pmapper remote-call-service interface.
 * This routine is used to call the pmapper remote call service
 * which will look up a service program in the port maps, and then
 * remotely call that routine with the given parameters.  This allows
 * programs to do a lookup and call in one step.
*/
#ifdef KERNEL
static enum clnt_stat
#else
enum clnt_stat
#endif
pmap_rmtcall(addr, prog, vers, proc, xdrargs, argsp, xdrres, resp, tout, port_ptr)
	struct sockaddr_in *addr;
	u_long prog, vers, proc;
	xdrproc_t xdrargs, xdrres;
	caddr_t argsp, resp;
	struct timeval tout;
	u_long *port_ptr;
{
#ifndef KERNEL
	int socket = -1;
#endif
	register CLIENT *client;
	struct rmtcallargs a;
	struct rmtcallres r;
	enum clnt_stat stat;

	addr->sin_port = htons(PMAPPORT);

#ifdef KERNEL
	client = clntkudp_create(addr, PMAPPROG, PMAPVERS, retries, u.u_cred);
#else
	client = clntudp_create(addr, PMAPPROG, PMAPVERS, timeout, &socket);
#endif
	if (client != (CLIENT *)NULL) {
		a.prog = prog;
		a.vers = vers;
		a.proc = proc;
		a.args_ptr = argsp;
		a.xdr_args = xdrargs;
		r.port_ptr = port_ptr;
		r.results_ptr = resp;
		r.xdr_results = xdrres;
		stat = CLNT_CALL(client, PMAPPROC_CALLIT, xdr_rmtcall_args, &a,
		    xdr_rmtcallres, &r, tout);
		CLNT_DESTROY(client);
	} else {
		stat = RPC_FAILED;
	}
#ifndef KERNEL
	(void)close(socket);
#endif
	addr->sin_port = 0;
	return (stat);
}

/*
 * XDR remote call arguments
 * written for XDR_ENCODE direction only
 */
bool_t
xdr_rmtcall_args(xdrs, cap)
	register XDR *xdrs;
	register struct rmtcallargs *cap;
{
	u_int lenposition, argposition, position;

	if (xdr_u_long(xdrs, &(cap->prog)) &&
	    xdr_u_long(xdrs, &(cap->vers)) &&
	    xdr_u_long(xdrs, &(cap->proc))) {
		lenposition = XDR_GETPOS(xdrs);
		if (! xdr_u_long(xdrs, &(cap->arglen)))
		    return (FALSE);
		argposition = XDR_GETPOS(xdrs);
		if (! (*(cap->xdr_args))(xdrs, cap->args_ptr))
		    return (FALSE);
		position = XDR_GETPOS(xdrs);
		cap->arglen = (u_long)position - (u_long)argposition;
		XDR_SETPOS(xdrs, lenposition);
		if (! xdr_u_long(xdrs, &(cap->arglen)))
		    return (FALSE);
		XDR_SETPOS(xdrs, position);
		return (TRUE);
	}
	return (FALSE);
}

/*
 * XDR remote call results
 * written for XDR_DECODE direction only
 */
bool_t
xdr_rmtcallres(xdrs, crp)
	register XDR *xdrs;
	register struct rmtcallres *crp;
{

	if (xdr_reference(xdrs, &crp->port_ptr, sizeof (u_long), xdr_u_long) &&
		xdr_u_long(xdrs, &crp->resultslen))
		return ((*(crp->xdr_results))(xdrs, crp->results_ptr));
	return (FALSE);
}

#ifndef KERNEL

/*
 * The following is kludged-up support for simple rpc broadcasts.
 * Someday a large, complicated system will replace these trivial 
 * routines which only support udp/ip .
 */
static int
getbroadcastnets(addrs, sock, buf)
	struct in_addr *addrs;
	int sock;  /* any valid socket will do */
	char *buf;  /* why allocxate more when we can use existing... */
{
	struct ifconf ifc;
        struct ifreq ifreq, *ifr;
	struct sockaddr_in *sin;
        int n, i;

	ifc.ifc_len = MAX_BROADCAST_SIZE;
        ifc.ifc_buf = buf;
        if (ioctl(sock, SIOCGIFCONF, (char *)&ifc) < 0) {
                rpc_perror("broadcast: ioctl (get interface configuration)");
                return (0);
        }
        ifr = ifc.ifc_req;
        for (i = 0, n = ifc.ifc_len/sizeof (struct ifreq); n > 0; n--, ifr++) {
                ifreq = *ifr;
                if (ioctl(sock, SIOCGIFFLAGS, (char *)&ifreq) < 0) {
                        rpc_perror("broadcast: ioctl (get interface flags)");
                        continue;
                }
                if ((ifreq.ifr_flags & (IFF_BROADCAST|IFF_UP)) != 
		    (IFF_BROADCAST|IFF_UP))
			continue;
		if (ioctl(sock, SIOCGIFBRDADDR, (char *)&ifreq) < 0) {
			rpc_perror("broadcast: ioctl (SIOCGIFBRDADDR)");
			continue;
		}
		if (ifr->ifr_addr.sa_family == AF_INET) {
			sin = (struct sockaddr_in *)&ifreq.ifr_dstaddr;
			addrs[i++] = sin->sin_addr;
                }
        }
	return (i);
}

typedef bool_t (*resultproc_t)();

enum clnt_stat 
clnt_broadcast(prog, vers, proc, xargs, argsp, xresults, resultsp, eachresult)
	u_long		prog;		/* program number */
	u_long		vers;		/* version number */
	u_long		proc;		/* procedure number */
	xdrproc_t	xargs;		/* xdr routine for args */
	caddr_t		argsp;		/* pointer to args */
	xdrproc_t	xresults;	/* xdr routine for results */
	caddr_t		resultsp;	/* pointer to results */
	resultproc_t	eachresult;	/* call with each result obtained */
{
	enum clnt_stat stat;
	AUTH *unix_auth = authunix_create_default();
	XDR xdr_stream;
	register XDR *xdrs = &xdr_stream;
	int outlen, inlen, fromlen, readfds, nets;
	register int sock, mask, i;
	bool_t done = FALSE;
	register u_long xid;
	u_long port;
	struct in_addr addrs[20];
	struct sockaddr_in baddr, raddr; /* broadcast and response addresses */
	struct rmtcallargs a;
	struct rmtcallres r;
	struct rpc_msg msg;
	struct timeval t; 
	char outbuf[MAX_BROADCAST_SIZE], inbuf[MAX_BROADCAST_SIZE];

	/*
	 * check the unix_auth structure
	 */
	if ( unix_auth == NULL ) {
		rpc_perror("Cannot create unix_auth structure");
		stat = RPC_FAILED;
		goto done_broad;
	}
	/*
	 * initialization: create a socket, a broadcast address, and
	 * preserialize the arguments into a send buffer.
	 */
	if ((sock = socket(AF_INET, SOCK_DGRAM, IPPROTO_UDP)) < 0) {
		rpc_perror("Cannot create socket for broadcast rpc");
		stat = RPC_CANTSEND;
		goto done_broad;
	}
	mask = (1 << sock);
	nets = getbroadcastnets(addrs, sock, inbuf);
	bzero(&baddr, sizeof (baddr));
	baddr.sin_family = AF_INET;
	baddr.sin_port = htons(PMAPPORT);
	baddr.sin_addr.s_addr = htonl(INADDR_ANY);
	(void)gettimeofday(&t, (struct timezone *)0);
	msg.rm_xid = xid = getpid() ^ t.tv_sec ^ t.tv_usec;
	t.tv_usec = 0;
	msg.rm_direction = CALL;
	msg.rm_call.cb_rpcvers = RPC_MSG_VERSION;
	msg.rm_call.cb_prog = PMAPPROG;
	msg.rm_call.cb_vers = PMAPVERS;
	msg.rm_call.cb_proc = PMAPPROC_CALLIT;
	msg.rm_call.cb_cred = unix_auth->ah_cred;
	msg.rm_call.cb_verf = unix_auth->ah_verf;
	a.prog = prog;
	a.vers = vers;
	a.proc = proc;
	a.xdr_args = xargs;
	a.args_ptr = argsp;
	r.port_ptr = &port;
	r.xdr_results = xresults;
	r.results_ptr = resultsp;
	xdrmem_create(xdrs, outbuf, MAX_BROADCAST_SIZE, XDR_ENCODE);
	if ((! xdr_callmsg(xdrs, &msg)) || (! xdr_rmtcall_args(xdrs, &a))) {
		stat = RPC_CANTENCODEARGS;
		goto done_broad;
	}
	outlen = (int)xdr_getpos(xdrs);
	xdr_destroy(xdrs);
	/*
	 * Basic loop: broadcast a packet and wait a while for response(s).
	 * The response timeout grows larger per iteration.
	 */
#ifdef RISCOS
	/* change the retransmission rate to exponential backoff */
	for (t.tv_sec = 1; t.tv_sec <= 16; t.tv_sec = t.tv_sec << 1) {
#else
	for (t.tv_sec = 4; t.tv_sec <= 14; t.tv_sec += 2) {
#endif
		for (i = 0; i < nets; i++) {
			char buf[10];
			buf[0] = 1;
			if (setsockopt(sock, SOL_SOCKET, SO_BROADCAST, buf, 10)
			    < 0) {
				rpc_perror("Cannot set broadcast permission");
				stat = RPC_CANTSEND;
				goto done_broad;
			}
			baddr.sin_addr = addrs[i];
			if (sendto(sock, outbuf, outlen, 0,
				(struct sockaddr *)&baddr,
				sizeof (struct sockaddr)) != outlen) {
				rpc_perror("Cannot send broadcast packet");
				stat = RPC_CANTSEND;
				goto done_broad;
			}
		}
	recv_again:
		msg.acpted_rply.ar_verf = _null_auth;
		msg.acpted_rply.ar_results.where = (caddr_t)&r;
                msg.acpted_rply.ar_results.proc = xdr_rmtcallres;
		readfds = mask;
		switch (select(32, &readfds, (int *)NULL, (int *)NULL, &t)) {

		case 0:  /* timed out */
			stat = RPC_TIMEDOUT;
			continue;

		case -1:  /* some kind of error */
			if (errno == EINTR)
				goto recv_again;
			rpc_perror("Broadcast select problem");
			stat = RPC_CANTRECV;
			goto done_broad;

		}  /* end of select results switch */
		if ((readfds & mask) == 0)
			goto recv_again;
	try_again:
		fromlen = sizeof(struct sockaddr);
		inlen = recvfrom(sock, inbuf, MAX_BROADCAST_SIZE, 0,
			(struct sockaddr *)&raddr, &fromlen);
		if (inlen < 0) {
			if (errno == EINTR)
				goto try_again;
			rpc_perror("Cannot receive reply to broadcast");
			stat = RPC_CANTRECV;
			goto done_broad;
		}
		if (inlen < sizeof(u_long))
			goto recv_again;
		/*
		 * see if reply transaction id matches sent id.
		 * If so, decode the results.
		 */
		xdrmem_create(xdrs, inbuf, inlen, XDR_DECODE);
		if (xdr_replymsg(xdrs, &msg)) {
			if ((msg.rm_xid == xid) &&
				(msg.rm_reply.rp_stat == MSG_ACCEPTED) &&
				(msg.acpted_rply.ar_stat == SUCCESS)) {
				raddr.sin_port = htons((u_short)port);
				done = (*eachresult)(resultsp, &raddr);
			}
			/* otherwise, we just ignore the errors ... */
		} else {
#ifdef notdef
			/* some kind of deserialization problem ... */
			if (msg.rm_xid == xid) {
			    rpc_perror("Broadcast deserialization problem");
			}
			/* otherwise, just random garbage */
#endif
		}
		xdrs->x_op = XDR_FREE;
		msg.acpted_rply.ar_results.proc = xdr_void;
		(void)xdr_replymsg(xdrs, &msg);
		(void)(*xresults)(xdrs, resultsp);
		xdr_destroy(xdrs);
		if (done) {
			stat = RPC_SUCCESS;
			goto done_broad;
		} else {
			goto recv_again;
		}
	}
done_broad:
	(void)close(sock);
	if (unix_auth != NULL) {
	  AUTH_DESTROY(unix_auth);
	}
	return (stat);
}
#endif /* !KERNEL */
