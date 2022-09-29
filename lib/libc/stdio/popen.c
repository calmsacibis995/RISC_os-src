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
#ident	"$Header: popen.c,v 1.7.2.2 90/05/10 01:47:10 wje Exp $"

/*	Copyright (c) 1984 AT&T	*/
/*	  All Rights Reserved  	*/

/*	THIS IS UNPUBLISHED PROPRIETARY SOURCE CODE OF AT&T	*/
/*	The copyright notice above does not evidence any   	*/
/*	actual or intended publication of such source code.	*/

/*LINTLIBRARY*/
#include <stdio.h>
#include <signal.h>
#include <fcntl.h>
#include <errno.h>

#define	tst(a,b) (*mode == 'r'? (b) : (a))
#define	RDR	0
#define	WTR	1

extern FILE *fdopen();
extern int execl(), fork(), pipe(), close(), fcntl();
static int popen_pid[20];

static char bin_shell[] = "/bin/sh" ;
static char shell[] = "sh";
static char shflg[] = "-c";

FILE *
popen(cmd, mode)
char	*cmd, *mode;
{
	int	p[2];
	register int *poptr;
	register int myside, yourside, pid;

	if(pipe(p) < 0)
		return(NULL);
	myside = tst(p[WTR], p[RDR]);
	yourside = tst(p[RDR], p[WTR]);
	if((pid = fork()) == 0) {
		/* myside and yourside reverse roles in child */
		int	stdio;

		/* close all pipes from other popen's */
		for (poptr = popen_pid; poptr < popen_pid+20; poptr++) {
			if(*poptr)
				close(poptr - popen_pid);
		}
		stdio = tst(0, 1);
		(void) close(myside);
		(void) close(stdio);
		(void) fcntl(yourside, F_DUPFD, stdio);
		(void) close(yourside);
		(void) execl(bin_shell, shell, shflg, cmd, (char *)0);
		_exit(1);
	}
	if(pid == -1)
		return(NULL);
	popen_pid[myside] = pid;
	(void) close(yourside);
	return(fdopen(myside, mode));
}

int
pclose(ptr)
FILE	*ptr;
{
	register int f, r;
	int status;
	void (*hstat)(), (*istat)(), (*qstat)();

	f = fileno(ptr);
	(void) fclose(ptr);
	istat = signal(SIGINT, SIG_IGN);
	qstat = signal(SIGQUIT, SIG_IGN);
	hstat = signal(SIGHUP, SIG_IGN);

/*
 * while the child is not done and no error has occured wait in the loop
 */
	while((r = wait(&status)) != popen_pid[f]
	      && (r != -1 || errno == EINTR))
		;
	if(r == -1)
		status = -1;
	(void) signal(SIGINT, istat);
	(void) signal(SIGQUIT, qstat);
	(void) signal(SIGHUP, hstat);
	/* mark this pipe closed */
	popen_pid[f] = 0;
	return(status);
}
