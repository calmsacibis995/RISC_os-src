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
#ident	"$Header: svc_simple.c,v 1.3.1.2 90/05/07 21:02:32 wje Exp $"
/*
 * @(#)svc_simple.c 1.1 86/09/24 Copyr 1984 Sun Micro
 * 
 * svc_simple.c
 * Simplified front end to rpc.
 */

#include <stdio.h>
#include <rpc/rpc.h>
#ifdef SYSTYPE_BSD43
#include <sys/socket.h>
#include <sys/time.h>
#include <netdb.h>
#include <syslog.h>
#endif
#ifdef SYSTYPE_SYSV
#include <bsd/sys/socket.h>
#include <bsd/sys/time.h>
#include <bsd/netdb.h>
#include <bsd/syslog.h>
#endif

static struct proglst {
	char *(*p_progname)();
	int  p_prognum;
	int  p_procnum;
	xdrproc_t p_inproc, p_outproc;
	struct proglst *p_nxt;
} *proglst;
int universal();
static SVCXPRT *transp;
static madetransp;
struct proglst *pl;

registerrpc(prognum, versnum, procnum, progname, inproc, outproc)
	char *(*progname)();
	xdrproc_t inproc, outproc;
{
	
	if (procnum == NULLPROC) {
		rpc_perror1("can't reassign procedure number %d", NULLPROC);
		return (-1);
	}
	if (!madetransp) {
		madetransp = 1;
		transp = svcudp_create(RPC_ANYSOCK);
		if (transp == NULL) {
			rpc_perror("couldn't create an rpc server");
			return (-1);
		}
	}
	pmap_unset(prognum, versnum);
	if (!svc_register(transp, prognum, versnum, universal, IPPROTO_UDP)) {
		rpc_perror2("couldn't register prog %d vers %d",
			    prognum, versnum);
		return (-1);
	}
	pl = (struct proglst *)malloc(sizeof(struct proglst));
	if (pl == NULL) {
		rpc_perror("registerrpc: out of memory\n");
		return (-1);
	}
	pl->p_progname = progname;
	pl->p_prognum = prognum;
	pl->p_procnum = procnum;
	pl->p_inproc = inproc;
	pl->p_outproc = outproc;
	pl->p_nxt = proglst;
	proglst = pl;
	return (0);
}

static
universal(rqstp, transp)
	struct svc_req *rqstp;
	SVCXPRT *transp;
{
	int prog, proc;
	char *outdata;
	char xdrbuf[UDPMSGSIZE];
	struct proglst *pl;

	/* 
	 * enforce "procnum 0 is echo" convention
	 */
	if (rqstp->rq_proc == NULLPROC) {
		if (svc_sendreply(transp, xdr_void, (char *)NULL) == FALSE) {
			exit(1);
		}
		return;
	}
	prog = rqstp->rq_prog;
	proc = rqstp->rq_proc;
	for (pl = proglst; pl != NULL; pl = pl->p_nxt)
	{
		if (pl->p_prognum == prog && pl->p_procnum == proc) {
			/* decode arguments into a CLEAN buffer */
			bzero(xdrbuf, sizeof(xdrbuf)); /* required ! */
			if (!svc_getargs(transp, pl->p_inproc, xdrbuf)) {
				svcerr_decode(transp);
				return;
			}
			outdata = (*(pl->p_progname))(xdrbuf);
			if (outdata == NULL && pl->p_outproc != xdr_void)
				/* there was an error */
				return;
			if (!svc_sendreply(transp, pl->p_outproc, outdata)) {
				rpc_perror1("trouble replying to prog %d",
					    pl->p_prognum);
#ifdef RISCOS
                        /* svc_sendreply can fail merely because a route has
                           temporarily disappered, and is not a reason to kill
                           off the entire server.
                         */
#else
				exit(1);
#endif
			}
			/* free the decoded arguments */
			(void)svc_freeargs(transp, pl->p_inproc, xdrbuf);
			return;
		}
	}
	rpc_perror1("never registered prog %d", prog);
	exit(1);
}

