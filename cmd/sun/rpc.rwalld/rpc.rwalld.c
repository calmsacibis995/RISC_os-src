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
#ident	"$Header: rpc.rwalld.c,v 1.5.2.4 90/05/09 19:22:36 wje Exp $"

#define sgi	1

/*
 * Copyright (c) 1984 by Sun Microsystems, Inc.
 */

#include <rpcsvc/rwall.h>
#include <rpc/rpc.h>
#include <stdio.h>
#include <bsd/netdb.h>
#include <bsd/sys/socket.h>
#ifdef RISCOS
#include <bsd43/syslog.h>
#endif

int splat();

main()
{
	register SVCXPRT *transp;
	struct sockaddr_in addr;
	int len = sizeof(struct sockaddr_in);
	
#ifdef RISCOS
	openlog("rpc.rwalld", BSD43_LOG_PID, BSD43_LOG_DAEMON);
#endif
	if (getsockname(0, &addr, &len) != 0) {
#ifdef RISCOS
		syslog(BSD43_LOG_ERR,"getsockname - %m\n");
#else
		perror("rstat: getsockname");
#endif
		exit(1);
	}
	if ((transp = svcudp_create(0)) == NULL) {
#ifdef RISCOS
		syslog(BSD43_LOG_ERR,"svc_rpc_udp_create: error\n");
#else
		fprintf(stderr, "svc_rpc_udp_create: error\n");
#endif
		exit(1);
	}
	if (!svc_register(transp, WALLPROG, WALLVERS, splat, 0)) {
#ifdef RISCOS
		syslog(BSD43_LOG_ERR,"svc_rpc_register: error\n");
#else
		fprintf(stderr, "svc_rpc_register: error\n");
#endif
		exit(1);
	}
	svc_run();
#ifdef RISCOS
	syslog(BSD43_LOG_ERR,"Error: svc_run shouldn't have returned\n");
#else
	fprintf(stderr, "Error: svc_run shouldn't have returned\n");
#endif
}

splat(rqstp, transp)
	register struct svc_req *rqstp;
	register SVCXPRT *transp;
{
	FILE *fp, *popen();
	char *msg;
        struct sockaddr_in addr;	
	struct hostent *hp;
	char buf[256];

	switch (rqstp->rq_proc) {
		case 0:
			if (svc_sendreply(transp, xdr_void, 0)  == FALSE) {
#ifdef RISCOS
				syslog(BSD43_LOG_ERR,"err - %m\n");
#else
				fprintf(stderr, "err: rwalld");
#endif
				exit(1);
			    }
			exit(0);
		case WALLPROC_WALL:
			if (!svc_getargs(transp, xdr_wrapstring, &msg)) {
			    	svcerr_decode(transp);
				exit(1);
			}
			if (svc_sendreply(transp, xdr_void, 0)  == FALSE) {
#ifdef RISCOS
                                syslog(BSD43_LOG_ERR,"err - %m\n");
#else
     	                   	fprintf(stderr, "err: rwalld");
#endif
				exit(1);
			}
			if (fork() == 0) {/* fork off child to do it */
#ifdef sgi
				fp = popen("/etc/wall", "w");
#else
				fp = popen("/bin/wall", "w");
#endif
				fprintf(fp, "%s", msg);
				pclose(fp);
				exit(0);
			}
			exit(0);
		default: 
			svcerr_noproc(transp);
			exit(0);
	}
}
