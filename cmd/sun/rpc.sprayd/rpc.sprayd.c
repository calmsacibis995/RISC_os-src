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
#ident	"$Header: rpc.sprayd.c,v 1.4.2.4 90/05/09 19:22:50 wje Exp $"

/*
 * Copyright (c) 1985 by Sun Microsystems, Inc.
 */

#include <stdio.h>
#include <signal.h>
#include <rpc/rpc.h>
#include <bsd/sys/time.h>
#include <rpcsvc/spray.h>
#ifdef RISCOS
#include <bsd43/syslog.h>
#endif

#define CLOSEDOWN 120		/* how many secs to wait before exiting */

int dirty;
unsigned cnt;
struct timeval tv;
struct spraycumul cumul;
int spray(), closedown();

main(argc, argv)
	char **argv;
{
	SVCXPRT *transp;
	int len = sizeof(struct sockaddr_in);
	struct sockaddr_in addr;
	
#ifdef RISCOS
	openlog("rpc.sprayd", BSD43_LOG_PID, BSD43_LOG_DAEMON);
#endif
	if (getsockname(0, &addr, &len) != 0) {
#ifdef RISCOS
		syslog(BSD43_LOG_ERR,"getsockname - m%\n");
#else
		perror("rstat: getsockname");
#endif
		exit(1);
	}
	transp = svcudp_create(0);
	if (transp == NULL) {
#ifdef RISCOS
		syslog(BSD43_LOG_ERR, "%s: couldn't create RPC server\n", argv[0]);
#else
		fprintf(stderr, "%s: couldn't create RPC server\n", argv[0]);
#endif
		exit(1);
	}
	if (!svc_register(transp, SPRAYPROG, SPRAYVERS, spray, 0)) {
#ifdef RISCOS
		syslog(BSD43_LOG_ERR,"%s: couldn't register SPRAYPROG\n", argv[0]);
#else
		fprintf(stderr, "%s: couldn't register SPRAYPROG\n", argv[0]);
#endif
		exit(1);
	}
	signal(SIGALRM, closedown);
	alarm(CLOSEDOWN);
	svc_run();
#ifdef RISCOS
	syslog(BSD43_LOG_ERR,"%s shouldn't reach this point\n", argv[0]);
#else
	fprintf(stderr, "%s shouldn't reach this point\n", argv[0]);
#endif
	exit(1);
}

spray(rqstp, transp)
	struct svc_req *rqstp;
	SVCXPRT *transp;
{
	dirty = 1;
	switch (rqstp->rq_proc) {
		case NULLPROC:
			if (!svc_sendreply(transp, xdr_void, 0)) {
#ifdef RISCOS
				syslog(BSD43_LOG_ERR,"couldn't reply to RPC call\n");
#else
				fprintf(stderr,"couldn't reply to RPC call\n");
#endif
				exit(1);
			}
			return;
		case SPRAYPROC_SPRAY:
			cumul.counter++;
			break;
		case SPRAYPROC_GET:
			gettimeofday(&cumul.clock, 0);
			if (cumul.clock.tv_usec < tv.tv_usec) {
				cumul.clock.tv_usec += 1000000;
				cumul.clock.tv_sec -= 1;
			}
			cumul.clock.tv_sec -= tv.tv_sec;
			cumul.clock.tv_usec -= tv.tv_usec;
			if (!svc_sendreply(transp, xdr_spraycumul, &cumul)) {
#ifdef RISCOS
				syslog(BSD43_LOG_ERR,"couldn't reply to RPC call\n");
#else
				fprintf(stderr,"couldn't reply to RPC call\n");
#endif
				exit(1);
			}
			return;
		case SPRAYPROC_CLEAR:
			cumul.counter = 0;
			gettimeofday(&tv, 0);
			if (!svc_sendreply(transp, xdr_void, 0)) {
#ifdef RISCOS
				syslog(BSD43_LOG_ERR,"couldn't reply to RPC call\n");
#else
				fprintf(stderr,"couldn't reply to RPC call\n");
#endif
				exit(1);
			}
			return;
		default:
			svcerr_noproc(transp);
			return;
	}
}

closedown()
{
	if (dirty) {
		dirty = 0;
	}
	else {
		exit(0);
	}
	alarm(CLOSEDOWN);
}
