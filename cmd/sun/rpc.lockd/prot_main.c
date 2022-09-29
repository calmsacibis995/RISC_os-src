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
 * |         950 DeGuigne Drive                                |
 * |         Sunnyvale, CA 94086                               |
 * |-----------------------------------------------------------|
 */
#ident	"$Header: prot_main.c,v 1.3.1.2.1.3.1.2 90/11/17 11:53:58 beacker Exp $"
/* @(#)nfs.cmds:nfs/lockd/prot_main.c	1.3 */

#include <sys/types.h>
#include <stdio.h>
#include <netinet/in.h>
#include "prot_time.h"
#include "prot_lock.h"
#include "priv_prot.h"

int debug;
XDR x;
extern int report_sharing_conflicts;
extern int klm, nlm, HASH_SIZE;
extern FILE *fp;

extern void xtimer();
extern void priv_prog(), klm_prog(), nlm_prog();

main(argc, argv)
	int argc;
	char ** argv;
{
#ifdef RISCOS
	SVCXPRT *Transp;
#endif
	int c, t, ppid;
	FILE *fopen();
	extern char *optarg;

	LM_GRACE = LM_GRACE_DEFAULT;
	LM_TIMEOUT = LM_TIMEOUT_DEFAULT;
	HASH_SIZE = 29;
	report_sharing_conflicts = 0;

	while ((c = getopt(argc, argv, "s:t:d:g:h:")) != EOF)
		switch (c) {
		case 's':
			report_sharing_conflicts++;
			break;
		case 't':
			(void) sscanf(optarg, "%d", &LM_TIMEOUT);
			break;
		case 'd':
			(void) sscanf(optarg, "%d", &debug);
			break;
		case 'g':
			(void) sscanf(optarg, "%d", &t);
			LM_GRACE = 1 + t/LM_TIMEOUT;
			break;
		case 'h':
			(void) sscanf(optarg, "%d", &HASH_SIZE);
			break;
		default:
			fprintf(stderr, "rpc.lockd -t[timeout] -g[grace_period] -d[debug]\n");
			return (0);
		}
	if (debug)
		printf("lm_timeout = %d secs, grace_period = %d secs, hashsize = %d\n",
		 LM_TIMEOUT,  LM_GRACE, HASH_SIZE);

	if (!debug) {
		ppid = fork();
		if (ppid == -1) {
			(void) fprintf(stderr, "rpc.lockd: fork failure\n");
			(void) fflush(stderr);
			abort();
		}
		if (ppid != 0) {
			exit(0);
		}
		for (t = 0; t< 20; t++) {
			(void) close(t);
		}

#ifndef RISCOS
		(void) open("/dev/console", 2);
		(void) open("/dev/console", 2);
		(void) open("/dev/console", 2);
#endif
		(void) setpgrp(0, 0);
	}
	else {
		setlinebuf(stderr);
		setlinebuf(stdout);
	}

	(void) signal(SIGALRM, xtimer);

#ifdef RISCOS
        /* NLM declaration */
        pmap_unset(NLM_PROG, NLM_VERS);

        Transp = svctcp_create(RPC_ANYSOCK, 0, 0);
        if (Transp == NULL) {
                fprintf(stderr,"cannot create tcp service.\n");
                exit(1);
	      }
        if (!svc_register(Transp, NLM_PROG, NLM_VERS, nlm_prog, IPPROTO_TCP)) {
           fprintf(stderr,"unable to register (NLM_PROG, NLM_VERS, tcp).\n");
                exit(1);
	      }

        Transp = svcudp_bufcreate(RPC_ANYSOCK, 1000, 1000);
        if (Transp == NULL) {
                fprintf(stderr,"cannot create udp service.\n");
                exit(1);
	      }
        if (!svc_register(Transp, NLM_PROG, NLM_VERS, nlm_prog, IPPROTO_UDP)) {
            fprintf(stderr,"unable to register (NLM_PROG, NLM_VERS, udp).\n");
                exit(1);
	      }
        if (!svcudp_enablecache(Transp, 15)) {
                fprintf(stderr,"svcudp_enablecache failed\n");
                exit(1);
	      }

        /* NLM V3 declaration */
        pmap_unset(NLM_PROG, NLM_VERSX);

        Transp = svctcp_create(RPC_ANYSOCK, 0, 0);
        if (Transp == NULL) {
                fprintf(stderr,"cannot create tcp service.\n");
                exit(1);
	      }
        if (!svc_register(Transp,NLM_PROG,NLM_VERSX,nlm_prog,IPPROTO_TCP)) {
           fprintf(stderr,"unable to register (NLM_PROG, NLM_VERSX, tcp).\n");
                exit(1);
	      }

        Transp = svcudp_bufcreate(RPC_ANYSOCK, 1000, 1000);
        if (Transp == NULL) {
                fprintf(stderr,"cannot create udp service.\n");
                exit(1);
	      }
        if (!svc_register(Transp, NLM_PROG, NLM_VERSX, nlm_prog,IPPROTO_UDP)) {
           fprintf(stderr,"unable to register (NLM_PROG, NLM_VERSX, udp).\n");
                exit(1);
	      }
        if (!svcudp_enablecache(Transp, 15)) {
                fprintf(stderr,"svcudp_enablecache failed\n");
                exit(1);
	      }
        /* KLM declaration */
        pmap_unset(KLM_PROG, KLM_VERS);

        Transp = svcudp_bufcreate(RPC_ANYSOCK, 1000, 1000);
        if (Transp == NULL) {
                fprintf(stderr,"cannot create udp service.\n");
                exit(1);
	      }
        if (!svc_register(Transp, KLM_PROG, KLM_VERS, klm_prog, IPPROTO_UDP)) {
            fprintf(stderr,"unable to register (KLM_PROG, KLM_VERS, udp).\n");
                exit(1);
	      }
        if (!svcudp_enablecache(Transp, 15)) {
                fprintf(stderr,"svcudp_enablecache failed\n");
                exit(1);
	      }

        Transp = svctcp_create(RPC_ANYSOCK, 0, 0);
        if (Transp == NULL) {
                fprintf(stderr,"cannot create tcp service.\n");
                exit(1);
	      }
        if (!svc_register(Transp, KLM_PROG, KLM_VERS, klm_prog, IPPROTO_TCP)) {
            fprintf(stderr,"unable to register (KLM_PROG, KLM_VERS, tcp).\n");
                exit(1);
	      }

        /* PRIV declaration */
        pmap_unset(PRIV_PROG, PRIV_VERS);

        Transp = svctcp_create(RPC_ANYSOCK, 0, 0);
        if (Transp == NULL) {
                fprintf(stderr,"cannot create tcp service.\n");
                exit(1);
	      }
        if (!svc_register(Transp,PRIV_PROG,PRIV_VERS,priv_prog,IPPROTO_TCP)) {
          fprintf(stderr,"unable to register (PRIV_PROG, PRIV_VERS, tcp).\n");
                exit(1);
	}
#else
        /* NLM declaration */
        if (!svc_create(nlm_prog, NLM_PROG, NLM_VERS, "netpath")) {
                perror("svc_create");
                fprintf(stderr, "unable to create (NLM_PROG, NLM_VERS) for netp\
ath.");
                exit(1);
	      }

        /* KLM declaration */
        if (!svc_create_local_service(klm_prog, KLM_PROG, KLM_VERS, "netpath",
                "lockd")) {
                perror("svc_create");
                fprintf(stderr, "unable to create (KLM_PROG, KLM_VERS) for netp\
ath.");
                exit(1);
	      }

        /* PRIV declaration */
        if (!svc_create(priv_prog, PRIV_PROG, PRIV_VERS, "netpath")) {
                perror("svc_create");
                fprintf(stderr, "unable to create (PRIV_PROG, PRIV_VERS) for ne\
tpath.");
                exit(1);
	      }
#endif /* RISCOS */

	init();
	init_nlm_share();

	if (debug == 3) {
		printf("lockd create logfile\n");
		klm = KLM_PROG;
		nlm = NLM_PROG;
		if ((fp = fopen("logfile", "w+")) == NULL) {
			perror("logfile fopen:");
			exit(1);
		}
		xdrstdio_create(&x, fp, XDR_ENCODE);
	}
	(void) alarm(LM_TIMEOUT);
	svc_run();
	fprintf(stderr, "svc_run returned\n");
	exit(1);
	/* NOTREACHED */
}
