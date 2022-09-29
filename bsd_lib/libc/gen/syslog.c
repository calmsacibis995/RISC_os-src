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
#ident	"$Header: syslog.c,v 1.3.1.4 90/05/07 20:43:57 wje Exp $"

/*
 * Copyright (c) 1983, 1988 Regents of the University of California.
 * All rights reserved.
 *
 * Redistribution and use in source and binary forms are permitted
 * provided that the above copyright notice and this paragraph are
 * duplicated in all such forms and that any documentation,
 * advertising materials, and other materials related to such
 * distribution and use acknowledge that the software was developed
 * by the University of California, Berkeley.  The name of the
 * University may not be used to endorse or promote products derived
 * from this software without specific prior written permission.
 * THIS SOFTWARE IS PROVIDED ``AS IS'' AND WITHOUT ANY EXPRESS OR
 * IMPLIED WARRANTIES, INCLUDING, WITHOUT LIMITATION, THE IMPLIED
 * WARRANTIES OF MERCHANTIBILITY AND FITNESS FOR A PARTICULAR PURPOSE.
 */

/*
 * SYSLOG -- print message on log file
 *
 * This routine looks a lot like printf, except that it
 * outputs to the log file instead of the standard output.
 * Also:
 *	adds a timestamp,
 *	prints the module name in front of the message,
 *	has some other formatting types (or will sometime),
 *	adds a newline on the end of the message.
 *
 * The output of this routine is intended to be read by /etc/syslogd.
 *
 * Author: Eric Allman
 * Modified to use UNIX domain IPC by Ralph Campbell
 */

#ifndef	SYSTYPE_SYSV
#include <sys/types.h>
#include <sys/socket.h>
#include <sys/file.h>
#include <sys/signal.h>
#include <sys/syslog.h>
#include <netdb.h>
#include <strings.h>
#else	SYSTYPE_SYSV
#include <bsd/sys/types.h>
#include <bsd/sys/socket.h>
#include <bsd/sys/file.h>
#include <signal.h>
#include <bsd/syslog.h>
#include <bsd/netdb.h>
#include <bsd/strings.h>
#ifdef SYSTYPE_POSIX
extern void (*signal())();
#define sigset signal
#endif SYSTYPE_POSIX
#endif	SYSTYPE_SYSV

#define	MAXLINE	8192			/* max message size */
#define NULL	0			/* manifest */

#define IMPORTANT 	LOG_ERR

#if	RISCOS
static char	logname[] = "/dev/syslog";
#else	!RISCOS
static char	logname[] = "/dev/log";
#endif	!RISCOS
static char	ctty[] = "/dev/console";

static int	LogFile = -1;		/* fd for log */
static int	connected;		/* have done connect */
static int	LogStat	= 0;		/* status bits, set by openlog() */
static char	*LogTag = "syslog";	/* string to tag the entry with */
static int	LogMask = 0xff;		/* mask of priorities to be logged */
static int	LogFacility = LOG_USER;	/* default facility code */

static struct sockaddr SyslogAddr;	/* AF_UNIX address of local logger */

extern	int errno, sys_nerr;
extern	char *sys_errlist[];

syslog(pri, fmt, p0, p1, p2, p3, p4)
	int pri;
	char *fmt;
{
	char buf[MAXLINE + 1], outline[MAXLINE + 1];
	register char *b, *f, *o;
	register int c;
	long now;
	int pid, olderrno = errno;

	/* see if we should just throw out this message */
	if ((unsigned) LOG_FAC(pri) >= LOG_NFACILITIES ||
	    LOG_MASK(LOG_PRI(pri)) == 0 ||
	    (pri &~ (LOG_PRIMASK|LOG_FACMASK)) != 0)
		return;
	if (LogFile < 0 || !connected)
		openlog(LogTag, LogStat | LOG_NDELAY, 0);

	/* set default facility if none specified */
	if ((pri & LOG_FACMASK) == 0)
		pri |= LogFacility;

	/* build the message */
	o = outline;
	(void)sprintf(o, "<%d>", pri);
	o += strlen(o);
	time(&now);
	(void)sprintf(o, "%.15s ", ctime(&now) + 4);
	o += strlen(o);
	if (LogTag) {
		strcpy(o, LogTag);
		o += strlen(o);
	}
	if (LogStat & LOG_PID) {
		(void)sprintf(o, "[%d]", getpid());
		o += strlen(o);
	}
	if (LogTag) {
		strcpy(o, ": ");
		o += 2;
	}

	b = buf;
	f = fmt;
	while ((c = *f++) != '\0' && c != '\n' && b < &buf[MAXLINE-2]) {
		if (c != '%') {
			*b++ = c;
			continue;
		}
		if ((c = *f++) != 'm') {
			*b++ = '%';
			*b++ = c;
			continue;
		}
		if ((unsigned)olderrno > sys_nerr)
			(void)sprintf(b, "error %d", olderrno);
		else
			strcpy(b, sys_errlist[olderrno]);
		b += strlen(b);
	}
	*b++ = '\n';
	*b = '\0';
	(void)sprintf(o, buf, p0, p1, p2, p3, p4);
	c = strlen(outline);
	if (c > MAXLINE)
		c = MAXLINE;

	/* output the message to the local logger */
	if (send(LogFile, outline, c, 0) >= 0)
		return;
	if (!(LogStat & LOG_CONS))
		return;

	/* output the message to the console */
#ifndef	SYSTYPE_SYSV
	pid = vfork();
#else	SYSTYPE_SYSV
	pid = fork();
#endif	SYSTYPE_SYSV
	if (pid == -1)
		return;
	if (pid == 0) {
		int fd;

		signal(SIGALRM, SIG_DFL);
		/* hold all signals except SIGALRM */
#ifndef	SYSTYPE_SYSV
		sigsetmask(sigblock(0L) & ~sigmask(SIGALRM));
#else	SYSTYPE_SYSV
		sigset(SIGHUP, SIG_IGN);   
		sigset(SIGINT, SIG_IGN); 
		sigset(SIGQUIT, SIG_IGN); 
		sigset(SIGILL, SIG_IGN); 
		sigset(SIGTRAP, SIG_IGN); 
		sigset(SIGIOT, SIG_IGN); 
		sigset(SIGEMT, SIG_IGN);
		sigset(SIGFPE, SIG_IGN);
		sigset(SIGBUS, SIG_IGN);
		sigset(SIGSEGV, SIG_IGN);
		sigset(SIGSYS, SIG_IGN);
		sigset(SIGPIPE, SIG_IGN);
		sigset(SIGTERM, SIG_IGN);
		sigset(SIGURG, SIG_IGN);
		sigset(SIGTSTP, SIG_IGN);
		sigset(SIGCONT, SIG_IGN);
		sigset(SIGCHLD, SIG_IGN);
		sigset(SIGTTIN, SIG_IGN);
		sigset(SIGTTOU, SIG_IGN);
#endif	SYSTYPE_SYSV
		alarm(5);
		fd = open(ctty, O_WRONLY);
		alarm(0);
		strcat(o, "\r");
		o = index(outline, '>') + 1;
		write(fd, o, c + 1 - (o - outline));
		close(fd);
		_exit(0);
	}
	if (!(LogStat & LOG_NOWAIT))
		while ((c = wait((int *)0)) > 0 && c != pid)
			;
}

/*
 * OPENLOG -- open system log
 */

openlog(ident, logstat, logfac)
	char *ident;
	int logstat, logfac;
{
	if (ident != NULL)
		LogTag = ident;
	LogStat = logstat;
	if (logfac != 0 && (logfac &~ LOG_FACMASK) == 0)
		LogFacility = logfac;
	if (LogFile == -1) {
		SyslogAddr.sa_family = AF_UNIX;
		strncpy(SyslogAddr.sa_data, logname, sizeof SyslogAddr.sa_data);
		if (LogStat & LOG_NDELAY) {
			LogFile = socket(AF_UNIX, SOCK_DGRAM, 0);
			fcntl(LogFile, F_SETFD, 1);
		}
	}
	if (LogFile != -1 && !connected &&
	    connect(LogFile, &SyslogAddr, sizeof(SyslogAddr)) != -1)
		connected = 1;
}

/*
 * CLOSELOG -- close the system log
 */

closelog()
{

	(void) close(LogFile);
	LogFile = -1;
	connected = 0;
}

/*
 * SETLOGMASK -- set the log mask level
 */
setlogmask(pmask)
	int pmask;
{
	int omask;

	omask = LogMask;
	if (pmask != 0)
		LogMask = pmask;
	return (omask);
}
