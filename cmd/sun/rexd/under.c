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
#ident	"$Header: under.c,v 1.1.1.5 90/05/09 19:15:58 wje Exp $"
#ifndef lint
static char sccsid[] = 	"@(#)under.c	1.2 88/08/15 4.0NFSSRC Copyr 1988 Sun Micro";
#endif

/*
 *  under.c - program to execute a command under a given directory
 *
 * Copyright (c) 1985 Sun Microsystems, Inc.
 */

#include <stdio.h>
#include <errno.h>
#include <signal.h>
#include <sys/time.h>
#include <rpc/rpc.h>
#define bsd43_bool_t bool_t
#include <nfs/nfs.h>
#include <rpcsvc/mount.h>
#ifdef RISCOS
#include <sys/syslog.h>
#endif

main(argc, argv)
	int argc;
	char **argv;
{
	static char usage[] = "Usage: under dir command...\n";
	char *dir, *p;
	char *index(), *mktemp();
	char hostname[255];
	char *tmpdir, *subdir, *parsefs();
	char dirbuf[1024];
	char error[1024];
	int status;
	int len;

	if (argc < 3) {
		fprintf(stderr, usage);
		exit(1);
	}
#ifdef RISCOS
	openlog("under", LOG_PID, LOG_DAEMON);
#endif
	gethostname(hostname, 255);
	strcat(hostname, ":/");
	len = strlen(hostname);
	dir = argv[1];
	if (strlen(dir) > len && strncmp(dir, hostname, len) == 0)
		dir = index(dir, ':') + 1;
	else if (p = index(dir, ':')) {
		if (p[1] != '/') {
#ifdef RISCOS
			syslog(LOG_ERR, "%s invalid name\n", dir);
#else
			fprintf(stderr, "under: %s invalid name\n", dir);
#endif
			exit(1);
		}
		tmpdir = mktemp("/tmp/underXXXXXX");
		if (mkdir(tmpdir, 0777)) {
			perror(tmpdir);
			exit(1);
		}
		subdir = parsefs(dir,error);
		if (subdir == NULL) {
			exit(1);
		}
		if (mount_nfs(dir, tmpdir, error)) {
			exit(1);
		}
		strcpy(dirbuf, tmpdir);
		strcat(dirbuf, "/");
		strcat(dirbuf, subdir);
		status = runcmd(dirbuf, argv[2], &argv[2]);
		if (umount_nfs(dir, tmpdir))
#ifdef RISCOS
			syslog(LOG_ERR, "couldn't umount %s\n", dir);
#else
			fprintf(stderr, "under: couldn't umount %s\n", dir);
#endif
		rmdir(tmpdir);
		exit(status);
	}
		
	setgid(getgid());
	setuid(getuid());
	if (chdir(dir)) {
		perror(dir);
		exit(1);
	}
	execvp(argv[2], &argv[2]);
	perror(argv[2]);
	exit(1);
}

typedef int (*sig_t)();

runcmd(dir, cmd, args)
	char *cmd;
	char **args;
{
	int pid, child, status;
	sig_t sigint, sigquit;

	sigint = signal(SIGINT, SIG_IGN);
	sigquit = signal(SIGQUIT, SIG_IGN);
	pid = fork();
	if (pid == -1)
		return (0177);
	if (pid == 0) {
		setgid(getgid());
		setuid(getuid());
		if (chdir(dir)) {
			perror(dir);
			exit(1);
		}
		(void) signal(SIGINT, sigint);
		(void) signal(SIGQUIT, sigquit);
		execvp(cmd, args);
		perror(cmd);
		exit(1);
	}
	while ((child = wait(&status)) != pid && child != -1)
		;
	(void) signal(SIGINT, sigint);
	(void) signal(SIGQUIT, sigquit);
	if (child == -1)
		return (0177);
	if (status & 0377)
		return (status & 0377);
	return ((status >> 8) & 0377);
}
