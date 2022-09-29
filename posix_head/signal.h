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
/* $Header: signal.h,v 1.3.1.3 90/05/10 04:09:06 wje Exp $ */

#ifndef	_POSIX_SIGNAL_
#define	_POSIX_SIGNAL_	1

#include <sys/signal.h>

#ifdef LANGUAGE_C
extern	void (*signal())();
extern	int kill();
extern	int sigemptyset();
extern	int sigfillset();
extern	int sigaddset();
extern	int sigdelset();
extern	int sigismember();
extern	int sigaction();
extern	int sigprocmask();
extern	int sigpending();
extern	int sigsuspend();
#endif LANGUAGE_C

#endif	_POSIX_SIGNAL_
