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
#ident	"$Header: siglist.c,v 1.3.2.2 90/05/07 20:42:29 wje Exp $"

#include <signal.h>

char	*sys_siglist[NSIG] = {
	"Signal 0",			/* 0 - */
	"Hangup",			/* 1 -  SIGHUP */
	"Interrupt",			/* 2 -  SIGINT */
	"Quit",				/* 3 -  SIGQUIT */
	"Illegal instruction",		/* 4 -  SIGILL */
	"Trace/BPT trap",		/* 5 -  SIGTRAP */
	"Abort",			/* 6 -  SIGABRT */
	"Cputime limit exceeded",	/* 7 -  SIGXCPU */
	"Floating point exception",	/* 8 -  SIGFPE */
	"Killed",			/* 9 -  SIGKILL */
	"Bus error",			/* 10 - SIGBUS */
	"Segmentation fault",		/* 11 - SIGSEGV */
	"Bad system call",		/* 12 - SIGSYS */
	"Broken pipe",			/* 13 - SIGPIPE */
	"Alarm clock",			/* 14 - SIGALRM */
	"Terminated",			/* 15 - SIGTERM */
	"User defined signal 1",	/* 16 - SIGUSR1 */
	"User defined signal 2",	/* 17 - SIGUSR2 */
	"Child exited",			/* 18 - SIGCHLD */
	"Filesize limit exceeded",	/* 19 - SIGXFSZ */
	"Stopped (signal)",		/* 20 - SIGSTOP */
	"Stopped",			/* 21 - SIGTSTP */
	"Pollable event occured",	/* 22 - SIGPOLL (!ATT) */
	"I/O possible",			/* 23 - SIGIO */
	"Urgent I/O condition",		/* 24 - SIGURG */
	"Window size changes",		/* 25 - SIGWINCH */
	"Virtual timer expired",	/* 26 - SIGVTALRM */
	"Profiling timer expired",	/* 27 - SIGPROF */
	"Continued",			/* 28 - SIGCONT */
	"Stopped (tty input)",		/* 29 - SIGTTIN */
	"Stopped (tty output)",		/* 30 - SIGTTOU */
	"Signal 31",			/* 31 - */
};

