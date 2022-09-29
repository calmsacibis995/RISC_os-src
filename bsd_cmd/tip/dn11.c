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
#ident	"$Header: dn11.c,v 1.1.2.4 90/05/28 17:52:31 wje Exp $"

/*
 * Routines for dialing up on DN-11
 */
#include "tip.h"

int dn_abort(), alarmtr();
static jmp_buf jmpbuf;
static int child = -1, dn;

dn_dialer(num, acu)
	char *num, *acu;
{
	extern errno;
	char *p, *q, phone[40];
	int lt, nw, connected = 1;
	register int timelim;

	if (boolean(value(VERBOSE)))
		printf("\nstarting call...");
	if ((dn = open(acu, 1)) < 0) {
		if (errno == EBUSY)
			printf("line busy...");
		else
			printf("acu open error...");
		return (0);
	}
	if (setjmp(jmpbuf)) {
		kill(child, SIGKILL);
		close(dn);
		return (0);
	}
	signal(SIGALRM, alarmtr);
	timelim = 5 * strlen(num);
	alarm(timelim < 30 ? 30 : timelim);
	if ((child = fork()) == 0) {
		/*
		 * ignore this stuff for aborts
		 */
		signal(SIGALRM, SIG_IGN);
		signal(SIGINT, SIG_IGN);
		signal(SIGQUIT, SIG_IGN);
		sleep(2);
		nw = write(dn, num, lt = strlen(num));
		exit(nw != lt);
	}
	/*
	 * open line - will return on carrier
	 */
	if ((FD = open(DV, 2)) < 0) {
		if (errno == EIO)
			printf("lost carrier...");
		else
			printf("dialup line open failed...");
		alarm(0);
		kill(child, SIGKILL);
		close(dn);
		return (0);
	}
	alarm(0);
	ioctl(dn, TIOCHPCL, 0);
	signal(SIGALRM, SIG_DFL);
	while ((nw = wait(&lt)) != child && nw != -1)
		;
	fflush(stdout);
	close(dn);
	if (lt != 0) {
		close(FD);
		return (0);
	}
	return (1);
}

alarmtr()
{

	alarm(0);
	longjmp(jmpbuf, 1);
}

/*
 * Insurance, for some reason we don't seem to be
 *  hanging up...
 */
dn_disconnect()
{

	sleep(2);
	if (FD > 0) {
		ioctl(FD, TIOCCDTR, 0);
#ifdef RISCOS
		sleep(1);
		ioctl(FD,TIOCSDTR,0);
		restore_termio(FD);
#endif RISCOS
	}
	close(FD);
}

dn_abort()
{

	sleep(2);
	if (child > 0)
		kill(child, SIGKILL);
	if (dn > 0)
		close(dn);
	if (FD > 0) {
		ioctl(FD, TIOCCDTR, 0);
#ifdef RISCOS
		sleep(1);
		ioctl(FD,TIOCSDTR,0);
		restore_termio(FD);
#endif RISCOS
	}
	close(FD);
}
