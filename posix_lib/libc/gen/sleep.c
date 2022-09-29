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
#ident	"$Header: sleep.c,v 1.1.1.3 90/05/10 04:14:13 wje Exp $"

/*	Copyright (c) 1984 AT&T	*/
/*	  All Rights Reserved  	*/

/*	THIS IS UNPUBLISHED PROPRIETARY SOURCE CODE OF AT&T	*/
/*	The copyright notice above does not evidence any   	*/
/*	actual or intended publication of such source code.	*/

/*LINTLIBRARY*/
/*
 * Suspend the process for `sleep_tm' seconds - using alarm/pause
 * system calls.  If caller had an alarm already set to go off `n'
 * seconds from now, then Case 1: (sleep_tm >= n) sleep for n, and
 * cause the callers previously specified alarm interrupt routine
 * to be executed, then return the value (sleep_tm - n) to the caller
 * as the unslept amount of time, Case 2: (sleep_tm < n) sleep for
 * sleep_tm, after which, reset alarm to go off when it would have
 * anyway.  In case process is aroused during sleep by any caught
 * signal, then reset any prior alarm, as above, and return to the
 * caller the (unsigned) quantity of (requested) seconds unslept.
 */
#include <signal.h>

extern int pause();
extern unsigned alarm();

#define	setact(act, a, mask) \
	act.sa_handler = a; act.sa_mask = mask; act.sa_flags = 0

static void
awake() {
	/* do nothing? */
}

unsigned
sleep(sleep_tm)
unsigned sleep_tm;
{
	int  alrm_flg;
	unsigned unslept, alrm_tm, left_ovr;
	sigset_t		mask, omask;
	struct	sigaction	act, oact;

	if(sleep_tm == 0)
		return(0);

	alrm_tm = alarm(0);			/* prev. alarm time */
	(void) sigemptyset(&mask);
	setact(act, awake, mask);
	(void) sigaction(SIGALRM, &act, &oact);

	alrm_flg = 0;
	left_ovr = 0;

	if(alrm_tm != 0) {	/* skip all this if no prev. alarm */
		if(alrm_tm > sleep_tm) {	/* alarm set way-out */
			alrm_tm -= sleep_tm;
			++alrm_flg;
		} else {	/* alarm will shorten sleep time */
			left_ovr = sleep_tm - alrm_tm;
			sleep_tm = alrm_tm;
			alrm_tm = 0;
			--alrm_flg;
			(void) sigaction(SIGALRM, &oact, (struct sigaction *)0);
		}
	}

	sigaddset(&mask, SIGALRM);
	sigprocmask(SIG_BLOCK, &mask, &omask);
	mask = omask;
	sigdelset(&mask, SIGALRM);
	(void) alarm(sleep_tm);
	sigsuspend(&mask);
	(void) sigaction(SIGALRM, &oact, (struct sigaction *)0);
	(void) sigprocmask(SIG_SETMASK, &omask, (sigset_t *)0);
	unslept = alarm(0);
	if(alrm_flg > 0 || (alrm_flg < 0 && unslept != 0))
		(void) alarm(alrm_tm + unslept);
	return(left_ovr + unslept);
}
