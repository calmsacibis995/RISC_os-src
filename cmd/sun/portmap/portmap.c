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
#ident	"$Header: portmap.c,v 2.4.2.5 90/05/09 19:15:21 wje Exp $"

#define sgi	1

/*
 * Copyright (c) 1984 by Sun Microsystems, Inc.
 */

/*
 * portmap.c, Implements the program,version to port number mapping for
 * rpc.
 */

/*
 * Sun RPC is a product of Sun Microsystems, Inc. and is provided for
 * unrestricted use provided that this legend is included on all tape
 * media and as a part of the software program in whole or part.  Users
 * may copy or modify Sun RPC without charge, but are not authorized
 * to license or distribute it to anyone else except as part of a product or
 * program developed by the user.
 * 
 * SUN RPC IS PROVIDED AS IS WITH NO WARRANTIES OF ANY KIND INCLUDING THE
 * WARRANTIES OF DESIGN, MERCHANTIBILITY AND FITNESS FOR A PARTICULAR
 * PURPOSE, OR ARISING FROM A COURSE OF DEALING, USAGE OR TRADE PRACTICE.
 * 
 * Sun RPC is provided with no support and without any obligation on the
 * part of Sun Microsystems, Inc. to assist in its use, correction,
 * modification or enhancement.
 * 
 * SUN MICROSYSTEMS, INC. SHALL HAVE NO LIABILITY WITH RESPECT TO THE
 * INFRINGEMENT OF COPYRIGHTS, TRADE SECRETS OR ANY PATENTS BY SUN RPC
 * OR ANY PART THEREOF.
 * 
 * In no event will Sun Microsystems, Inc. be liable for any lost revenue
 * or profits or other special, indirect and consequential damages, even if
 * Sun has been advised of the possibility of such damages.
 * 
 * Sun Microsystems, Inc.
 * 2550 Garcia Avenue
 * Mountain View, California  94043
 */

#include <rpc/rpc.h>
#include <rpc/pmap_prot.h>
#include <stdio.h>
#include <bsd/netdb.h>
#include <bsd/sys/socket.h>
#include <bsd/sys/time.h>
#include <bsd/sys/ioctl.h>
#ifdef RISCOS
#include <bsd43/syslog.h>
#endif

#ifdef sgi
#include <signal.h>
#endif

char *malloc();
int reg_service();
static int debugging = 0;

main()
{
	SVCXPRT *xprt;
	int sock, pid, t;
	struct sockaddr_in addr;
	int len = sizeof(struct sockaddr_in);

#ifdef RISCOS
	openlog("portmap",BSD43_LOG_PID,BSD43_LOG_DAEMON);
#endif
#ifdef sgi
	(void) signal(SIGHUP, SIG_IGN);
#endif

#ifndef DEBUG
	pid = fork();
	if (pid < 0) {
#ifdef RISCOS
		syslog(BSD43_LOG_ERR,"fork - %m\n");
#else
		perror("portmap: fork");
#endif
		exit(1);
	}
	if (pid != 0)
		exit(0);
	for (t = 0; t < 20; t++)
		close(t);
 	open("/", 0);
 	dup2(0, 1);
 	dup2(0, 2);
 	t = open("/dev/tty", 2);
 	if (t >= 0) {
 		ioctl(t, TIOCNOTTY, (char *)0);
 		close(t);
 	}
#endif
	if ((sock = socket(AF_INET, SOCK_DGRAM, IPPROTO_UDP)) < 0) {
#ifdef RISCOS
		syslog(BSD43_LOG_ERR,"cannot create socket - %m\n");
#else
		perror("portmap cannot create socket");
#endif
		exit(1);
	}

#ifdef sgi
	addr.sin_addr.s_addr = 0;
#else
	addr.sin_addr.S_un.S_addr = 0;
#endif
	addr.sin_family = AF_INET;
	addr.sin_port = htons(PMAPPORT);
	if (bind(sock, (struct sockaddr *)&addr, len) != 0) {
#ifdef RISCOS
		syslog(BSD43_LOG_ERR,"cannot bind - %m\n");
#else
		perror("portmap cannot bind");
#endif
		exit(1);
	}

	if ((xprt = svcudp_create(sock)) == (SVCXPRT *)NULL) {
#ifdef RISCOS
		syslog(BSD43_LOG_ERR, "couldn't do udp_create\n");
#else
		fprintf(stderr, "couldn't do udp_create\n");
#endif
		exit(1);
	}

	if ((sock = socket(AF_INET, SOCK_STREAM, IPPROTO_TCP)) < 0) {
#ifdef RISCOS
		syslog(BSD43_LOG_ERR,"cannot create socket - %m\n");
#else
		perror("portmap cannot create socket");
#endif
		exit(1);
	}
	if (bind(sock, (struct sockaddr *)&addr, len) != 0) {
#ifdef RISCOS
		syslog(BSD43_LOG_ERR,"cannot bind - %m\n");
#else
		perror("portmap cannot bind");
#endif
		exit(1);
	}
	if ((xprt = svctcp_create(sock, RPCSMALLMSGSIZE, RPCSMALLMSGSIZE))
	    == (SVCXPRT *)NULL) {
#ifdef RISCOS
		syslog(BSD43_LOG_ERR, "couldn't do tcp_create\n");
#else
		fprintf(stderr, "couldn't do tcp_create\n");
#endif
		exit(1);
	}

        (void)svc_register(xprt, PMAPPROG, PMAPVERS, reg_service, FALSE);
	svc_run();
#ifdef RISCOS
	syslog(BSD43_LOG_ERR, "run_svc returned unexpectedly\n");
#else
	fprintf(stderr, "run_svc returned unexpectedly\n");
#endif
	abort();
}

struct pmaplist *pmaplist;

static struct pmaplist *
find_service(prog, vers, prot)
	u_long prog;
	u_long vers;
{
	register struct pmaplist *hit = NULL;
	register struct pmaplist *pml;

	for (pml = pmaplist; pml != NULL; pml = pml->pml_next) {
		if ((pml->pml_map.pm_prog != prog) ||
			(pml->pml_map.pm_prot != prot))
			continue;
		hit = pml;
		if (pml->pml_map.pm_vers == vers)
		    break;
	}
	return (hit);
}

/* 
 * 1 OK, 0 not
 */
reg_service(rqstp, xprt)
	struct svc_req *rqstp;
	SVCXPRT *xprt;
{
	struct pmap reg;
	struct pmaplist *pml, *prevpml, *fnd;
	int ans, port;
	caddr_t t;
	
#ifdef DEBUG
#ifdef RISCOS
	syslog(BSD43_LOG_ERR, "server: about to switch on %d\n", rqstp->rq_proc);
#else
	fprintf(stderr, "server: about to switch on %d\n", rqstp->rq_proc);
#endif
#endif
	switch (rqstp->rq_proc) {

	case PMAPPROC_NULL:
		/*
		 * Null proc call
		 */
		if ((!svc_sendreply(xprt, xdr_void, NULL)) && debugging) {
			abort();
		}
		break;

	case PMAPPROC_SET:
		/*
		 * Set a program,version to port mapping
		 */
		if (!svc_getargs(xprt, xdr_pmap, &reg))
			svcerr_decode(xprt);
		else {
			/*
			 * check to see if already used
			 * find_service returns a hit even if
			 * the versions don't match, so check for it
			 */
			fnd = find_service(reg.pm_prog, reg.pm_vers, reg.pm_prot);
			if (fnd && fnd->pml_map.pm_vers == reg.pm_vers) {
				if (fnd->pml_map.pm_port == reg.pm_port) {
					ans = 1;
					goto done;
				}
				else {
					ans = 0;
					goto done;
				}
			} else {
				/* 
				 * add to END of list
				 */
				pml = (struct pmaplist *)
				    malloc((u_int)sizeof(struct pmaplist));
				pml->pml_map = reg;
				pml->pml_next = 0;
				if (pmaplist == 0) {
					pmaplist = pml;
				} else {
					for (fnd= pmaplist; fnd->pml_next != 0;
					    fnd = fnd->pml_next);
					fnd->pml_next = pml;
				}
				ans = 1;
			}
		done:
			if ((!svc_sendreply(xprt, xdr_long, (caddr_t)&ans)) &&
			    debugging) {
#ifdef RISCOS
				syslog(BSD43_LOG_ERR, "svc_sendreply\n");
#else
				fprintf(stderr, "svc_sendreply\n");
#endif
				abort();
			}
		}
		break;

	case PMAPPROC_UNSET:
		/*
		 * Remove a program,version to port mapping.
		 */
		if (!svc_getargs(xprt, xdr_pmap, &reg))
			svcerr_decode(xprt);
		else {
			ans = 0;
			for (prevpml = NULL, pml = pmaplist; pml != NULL; ) {
				if ((pml->pml_map.pm_prog != reg.pm_prog) ||
					(pml->pml_map.pm_vers != reg.pm_vers)) {
					/* both pml & prevpml move forwards */
					prevpml = pml;
					pml = pml->pml_next;
					continue;
				}
				/* found it; pml moves forward, prevpml stays */
				ans = 1;
				t = (caddr_t)pml;
				pml = pml->pml_next;
				if (prevpml == NULL)
					pmaplist = pml;
				else
					prevpml->pml_next = pml;
				free(t);
			}
			if ((!svc_sendreply(xprt, xdr_long, (caddr_t)&ans)) &&
			    debugging) {
#ifdef RISCOS
				syslog(BSD43_LOG_ERR, "svc_sendreply\n");
#else
				fprintf(stderr, "svc_sendreply\n");
#endif
				abort();
			}
		}
		break;

	case PMAPPROC_GETPORT:
		/*
		 * Lookup the mapping for a program,version and return its port
		 */
		if (!svc_getargs(xprt, xdr_pmap, &reg))
			svcerr_decode(xprt);
		else {
			fnd = find_service(reg.pm_prog, reg.pm_vers, reg.pm_prot);
			if (fnd)
				port = fnd->pml_map.pm_port;
			else
				port = 0;
			if ((!svc_sendreply(xprt, xdr_long, (caddr_t)&port)) &&
			    debugging) {
#ifdef RISCOS
				syslog(BSD43_LOG_ERR, "svc_sendreply\n");
#else
				fprintf(stderr, "svc_sendreply\n");
#endif
				abort();
			}
		}
		break;

	case PMAPPROC_DUMP:
		/*
		 * Return the current set of mapped program,version
		 */
		if (!svc_getargs(xprt, xdr_void, NULL))
			svcerr_decode(xprt);
		else {
			if ((!svc_sendreply(xprt, xdr_pmaplist,
			    (caddr_t)&pmaplist)) && debugging) {
#ifdef RISCOS
				syslog(BSD43_LOG_ERR, "svc_sendreply\n");
#else
				fprintf(stderr, "svc_sendreply\n");
#endif
				abort();
			}
		}
		break;

	case PMAPPROC_CALLIT:
		/*
		 * Calls a procedure on the local machine.  If the requested
		 * procedure is not registered this procedure does not return
		 * error information!!
		 * This procedure is only supported on rpc/udp and calls via 
		 * rpc/udp.  It passes null authentication parameters.
		 */
		callit(rqstp, xprt);
		break;

	default:
		svcerr_noproc(xprt);
		break;
	}
}


/*
 * Stuff for the rmtcall service
 */
#define ARGSIZE 32000		/* was 9000 */

typedef struct encap_parms {
	u_long arglen;
	char *args;
};

static bool_t
xdr_encap_parms(xdrs, epp)
	XDR *xdrs;
	struct encap_parms *epp;
{

	return (xdr_bytes(xdrs, &(epp->args), &(epp->arglen), ARGSIZE));
}

typedef struct rmtcallargs {
	u_long	rmt_prog;
	u_long	rmt_vers;
	u_long	rmt_port;
	u_long	rmt_proc;
	struct encap_parms rmt_args;
};

static bool_t
xdr_rmtcall_args(xdrs, cap)
	register XDR *xdrs;
	register struct rmtcallargs *cap;
{

	/* does not get a port number */
	if (xdr_u_long(xdrs, &(cap->rmt_prog)) &&
	    xdr_u_long(xdrs, &(cap->rmt_vers)) &&
	    xdr_u_long(xdrs, &(cap->rmt_proc))) {
		return (xdr_encap_parms(xdrs, &(cap->rmt_args)));
	}
	return (FALSE);
}

static bool_t
xdr_rmtcall_result(xdrs, cap)
	register XDR *xdrs;
	register struct rmtcallargs *cap;
{
	if (xdr_u_long(xdrs, &(cap->rmt_port)))
		return (xdr_encap_parms(xdrs, &(cap->rmt_args)));
	return (FALSE);
}

/*
 * only worries about the struct encap_parms part of struct rmtcallargs.
 * The arglen must already be set!!
 */
static bool_t
xdr_opaque_parms(xdrs, cap)
	XDR *xdrs;
	struct rmtcallargs *cap;
{

	return (xdr_opaque(xdrs, cap->rmt_args.args, cap->rmt_args.arglen));
}

/*
 * This routine finds and sets the length of incoming opaque paraters
 * and then calls xdr_opaque_parms.
 */
static bool_t
xdr_len_opaque_parms(xdrs, cap)
	register XDR *xdrs;
	struct rmtcallargs *cap;
{
	register u_int beginpos, lowpos, highpos, currpos, pos;

	beginpos = lowpos = pos = xdr_getpos(xdrs);
	highpos = lowpos + ARGSIZE;
	while ((int)(highpos - lowpos) >= 0) {
		currpos = (lowpos + highpos) / 2;
		if (xdr_setpos(xdrs, currpos)) {
			pos = currpos;
			lowpos = currpos + 1;
		} else {
			highpos = currpos - 1;
		}
	}
	xdr_setpos(xdrs, beginpos);
	cap->rmt_args.arglen = pos - beginpos;
	return (xdr_opaque_parms(xdrs, cap));
}

/*
 * Call a remote procedure service
 * This procedure is very quiet when things go wrong.
 * The proc is written to support broadcast rpc.  In the broadcast case,
 * a machine should shut-up instead of complain, less the requestor be
 * overrun with complaints at the expense of not hearing a valid reply ...
 */
static
callit(rqstp, xprt)
	struct svc_req *rqstp;
	SVCXPRT *xprt;
{
	char buf[ARGSIZE];
	struct rmtcallargs a;
	struct pmaplist *pml;
	u_short port;
	struct sockaddr_in me;
	int socket = -1;
	CLIENT *client;
	struct authunix_parms *au = (struct authunix_parms *)rqstp->rq_clntcred;
	struct timeval timeout;

	timeout.tv_sec = 5;
	timeout.tv_usec = 0;
	a.rmt_args.args = buf;
	if (!svc_getargs(xprt, xdr_rmtcall_args, &a))
	    return;
	if ((pml = find_service(a.rmt_prog, a.rmt_vers, IPPROTO_UDP)) == NULL)
	    return;
	port = pml->pml_map.pm_port;
	get_myaddress(&me);
	me.sin_port = htons(port);
	client = clntudp_create(&me, a.rmt_prog, a.rmt_vers, timeout, &socket);
	if (client != (CLIENT *)NULL) {
		if (rqstp->rq_cred.oa_flavor == AUTH_UNIX) {
			client->cl_auth = authunix_create(au->aup_machname,
			   au->aup_uid, au->aup_gid, au->aup_len, au->aup_gids);
		}
		a.rmt_port = (u_long)port;
		if (clnt_call(client, a.rmt_proc, xdr_opaque_parms, &a,
		    xdr_len_opaque_parms, &a, timeout) == RPC_SUCCESS) {
			svc_sendreply(xprt, xdr_rmtcall_result, &a);
		}
		AUTH_DESTROY(client->cl_auth);
		clnt_destroy(client);
	}
	(void)close(socket);
}
