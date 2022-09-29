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
#ident	"$Header: popen.c,v 1.2.1.2 90/05/07 20:39:52 wje Exp $"

/*
 * Copyright (c) 1980 Regents of the University of California.
 * All rights reserved.  The Berkeley software License Agreement
 * specifies the terms and conditions for redistribution.
 */

#if defined(LIBC_SCCS) && !defined(lint)
static char sccsid[] = "@(#)popen.c	5.5 (Berkeley) 9/30/87";
#endif LIBC_SCCS and not lint

#include <stdio.h>
#include <signal.h>

#define	tst(a,b)	(*mode == 'r'? (b) : (a))
#define	RDR	0
#define	WTR	1

extern	char *malloc();

static	int *popen_pid;
static	int nfiles;

FILE *
popen(cmd,mode)
	char *cmd;
	char *mode;
{
	int p[2];
	int myside, hisside, pid;
	int fd;

	if (nfiles <= 0)
		nfiles = getdtablesize();
	if (popen_pid == NULL) {
		popen_pid = (int *)malloc(nfiles * sizeof *popen_pid);
		if (popen_pid == NULL)
			return (NULL);
		for (pid = 0; pid < nfiles; pid++)
			popen_pid[pid] = -1;
	}
	if (pipe(p) < 0)
		return (NULL);
	myside = tst(p[WTR], p[RDR]);
	hisside = tst(p[RDR], p[WTR]);
	if ((pid = vfork()) == 0) {
		/* myside and hisside reverse roles in child */
		close(myside);
		if (hisside != tst(0, 1)) {
			dup2(hisside, tst(0, 1));
			close(hisside);
		}
		/* close all file descriptors except 0-2 */
		fd = getdtablesize();
		while (fd > 2) {
			(void) close(fd);
			fd--;
		}
		execl("/bin/sh", "sh", "-c", cmd, (char *)NULL);
		_exit(127);
	}
	if (pid == -1) {
		close(myside);
		close(hisside);
		return (NULL);
	}
	popen_pid[myside] = pid;
	close(hisside);
	return (fdopen(myside, mode));
}

pclose(ptr)
	FILE *ptr;
{
	int child, pid, status, omask;

	child = popen_pid[fileno(ptr)];
	popen_pid[fileno(ptr)] = -1;
	fclose(ptr);
	if (child == -1)
		return (-1);
	omask = sigblock(sigmask(SIGINT)|sigmask(SIGQUIT)|sigmask(SIGHUP));
	while ((pid = wait(&status)) != child && pid != -1)
		;
	(void) sigsetmask(omask);
	return (pid == -1 ? -1 : status);
}
