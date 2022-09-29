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
#ident	"$Header: nfsd.c,v 1.6.2.4 90/05/09 19:14:12 wje Exp $"

#define sgi	1

/*
 * Copyright (c) 1985 Sun Microsystems, Inc.
 */

/* NFS server */

#include <bsd/sys/param.h>
#include <rpc/rpc.h>
#include <bsd/sys/socket.h>
#include <sys/errno.h>
#include <bsd/sys/time.h>
#include <bsd/sys/ioctl.h>
#include <bsd/sys/file.h>
#if defined mips
# include <sys/fs/nfs.h>
#else
# include <nfs/nfs.h>
#endif
#include <stdio.h>
#include <signal.h>
#ifdef RISCOS
#include <bsd43/syslog.h>
#endif


catch()
{
}

main(argc, argv)
char	*argv[];
{
	register int sock;
	struct sockaddr_in addr;
	int len = sizeof(struct sockaddr_in);
	char *dir = "/";
	int nservers = 1;
	int pid, t;

#ifdef RISCOS
	openlog("nfsd", BSD43_LOG_PID, BSD43_LOG_DAEMON);
#endif
	if (argc > 2) {
		fprintf(stderr, "usage: %s [servers]\n", argv[0]);
		exit(1);
	}
	if (argc == 2) {
		nservers = atoi(argv[1]);
	}

	/*
	 * Set current and root dir to server root
	 */
	if (chroot(dir) < 0) {
#ifdef RISCOS
		syslog(BSD43_LOG_ERR,"chroot error %m\n");
#else
		perror(dir);
#endif
		exit(1);
	}
	if (chdir(dir) < 0) {
#ifdef RISCOS
		syslog(BSD43_LOG_ERR,"chdir error %m\n");
#else
		perror(dir);
#endif
		exit(1);
	}

#ifndef DEBUG
	/*
	 * Background 
	 */
        pid = fork();
	if (pid < 0) {
#ifdef RISCOS
		syslog(BSD43_LOG_ERR,"fork: %m\n");
#else
		perror("nfsd: fork");
#endif
		exit(1);
	}
	if (pid != 0)
		exit(0);

	{ int s;
	for (s = 0; s < 10; s++)
		(void) close(s);
	}
	(void) open("/", O_RDONLY);
	(void) dup2(0, 1);
	(void) dup2(0, 2);
#endif
	{ int tt = open("/dev/tty", O_RDWR);
	  if (tt > 0) {
		ioctl(tt, TIOCNOTTY, 0);
		close(tt);
	  }
	}

#ifdef sgi
	addr.sin_addr.s_addr = 0;
#else
	addr.sin_addr.S_un.S_addr = 0;
#endif
	addr.sin_family = AF_INET;
	addr.sin_port = htons(NFS_PORT);
	if ( ((sock = socket(AF_INET, SOCK_DGRAM, IPPROTO_UDP)) < 0)
	    || (bind(sock, &addr, len) != 0)
	    || (getsockname(sock, &addr, &len) != 0) ) {
		(void)close(sock);
#ifdef RISCOS
		syslog(BSD43_LOG_ERR, "%s: server already active\n", argv[0]);
#else
		fprintf(stderr, "%s: server already active", argv[0]);
#endif
		exit(1);
	}
	/* register with the portmapper */
	pmap_unset(NFS_PROGRAM, NFS_VERSION);
	pmap_set(NFS_PROGRAM, NFS_VERSION, IPPROTO_UDP, NFS_PORT);
	while (--nservers) {
		if (!fork()) {
			break;
		}
	}
	signal(SIGTERM, catch);
	nfssvc(sock);
}
