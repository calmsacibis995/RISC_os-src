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
#ident	"$Header: telnetd.c,v 1.18.1.5 90/05/09 17:40:55 wje Exp $"

/*
 * Copyright (c) 1983, 1986 Regents of the University of California.
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

#ifndef lint
char copyright[] =
"@(#) Copyright (c) 1983, 1986 Regents of the University of California.\n\
 All rights reserved.\n";
#endif /* not lint */

#ifndef lint
static char sccsid[] = "@(#)telnetd.c	5.29 (Berkeley) 6/18/88";
#endif /* not lint */

/*
 * Telnet server.
 */
#include <sys/param.h>
#include <sys/socket.h>
#include <sys/wait.h>
#include <sys/file.h>
#include <sys/stat.h>
#include <sys/time.h>

#include <netinet/in.h>

#include <arpa/telnet.h>

#include <stdio.h>
#include <signal.h>
#include <errno.h>
#if RISCOS
#include <sysv/sys/termio.h>
/* These next constants are used as args to "mode()".  Just define them as
 * unique integers for our purposes.  They must not conflict with ECHO.
 */
#define RAW 1
#define CRMOD 2
#else
#include <sgtty.h>
#endif
#include <netdb.h>
#include <syslog.h>
#include <ctype.h>

#define	OPT_NO			0		/* won't do this option */
#define	OPT_YES			1		/* will do this option */
#define	OPT_YES_BUT_ALWAYS_LOOK	2
#define	OPT_NO_BUT_ALWAYS_LOOK	3
char	hisopts[256];
char	myopts[256];

char	doopt[] = { IAC, DO, '%', 'c', 0 };
char	dont[] = { IAC, DONT, '%', 'c', 0 };
char	will[] = { IAC, WILL, '%', 'c', 0 };
char	wont[] = { IAC, WONT, '%', 'c', 0 };

/*
 * I/O data buffers, pointers, and counters.
 */
char	ptyibuf[BUFSIZ], *ptyip = ptyibuf;
#ifdef RISCOS
char	bannerbuf[BUFSIZ];	/* buffer to read login banner */
#endif

char	ptyobuf[BUFSIZ], *pfrontp = ptyobuf, *pbackp = ptyobuf;

char	netibuf[BUFSIZ], *netip = netibuf;
#define	NIACCUM(c)	{   *netip++ = c; \
			    ncc++; \
			}

char	netobuf[BUFSIZ], *nfrontp = netobuf, *nbackp = netobuf;
char	*neturg = 0;		/* one past last bye of urgent data */
	/* the remote system seems to NOT be an old 4.2 */
int	not42 = 1;

#if  RISCOS
#define	BANNER	"\r\n\r\nRISC/os (%s)\r\n\r\r\n\r"
#else
#define	BANNER	"\r\n\r\n4.3 BSD UNIX (%s)\r\n\r\r\n\r"
#endif

		/* buffer for sub-options */
char	subbuffer[100], *subpointer= subbuffer, *subend= subbuffer;
#define	SB_CLEAR()	subpointer = subbuffer;
#define	SB_TERM()	{ subend = subpointer; SB_CLEAR(); }
#define	SB_ACCUM(c)	if (subpointer < (subbuffer+sizeof subbuffer)) { \
				*subpointer++ = (c); \
			}
#define	SB_GET()	((*subpointer++)&0xff)
#define	SB_EOF()	(subpointer >= subend)

int	pcc, ncc;

int	pty, net;
int	inter;
extern	char **environ;
extern	int errno;
#if  RISCOS
struct termio cookedstate = {  /* has echo off, for init reasons */
	BRKINT+IGNPAR+ISTRIP+ICRNL+IXON,/* iflags */
	OPOST+ONLCR+TAB3,		/* oflags */
	B38400+CS8+CREAD+CLOCAL,	/* cflags */
	ISIG+ICANON+ECHOK,		/* lflags */
	LDISC0,				/* line discipline */
	{ CINTR,CQUIT,CERASE,CKILL,	/* INTR, QUIT, ERASE, KILL */
	  CEOF,CNUL,CNUL,CNUL,		/* EOF, EOL, EOL2, VSWTCH  */
	  CSTART, CSTOP, CNUL, CNUL, 	/* START, STOP, SUSP, DSUSP */
	  CRPRNT, CFLUSH, CWERASE, CLNEXT, /* RPRNT, FLUSH, WERASE, LNEXT */
	  CDEL, CDEL, CDEL, CDEL,	/* STATUS, SAVED_EOF, SAVED_EOL, ? */
	  CDEL, CDEL, CDEL		/* ?, ?, ? */
	},
	0				/* saved_flags */
};
char	*line = "/dev/ttyq12345";
#else
char	*line;
#endif
int	SYNCHing = 0;		/* we are in TELNET SYNCH mode */
/*
 * The following are some clocks used to decide how to interpret
 * the relationship between various variables.
 */

struct {
    int
	system,			/* what the current time is */
	echotoggle,		/* last time user entered echo character */
	modenegotiated,		/* last time operating mode negotiated */
	didnetreceive,		/* last time we read data from network */
	ttypeopt,		/* ttype will/won't received */
	ttypesubopt,		/* ttype subopt is received */
	getterminal,		/* time started to get terminal information */
	gotDM;			/* when did we last see a data mark */
} clocks;

#define	settimer(x)	(clocks.x = ++clocks.system)
#define	sequenceIs(x,y)	(clocks.x < clocks.y)

main(argc, argv)
	char *argv[];
{
	struct sockaddr_in from;
	int on = 1, fromlen;

#if	defined(DEBUG)
	{
	    int s, ns, foo;
	    struct servent *sp;
	    static struct sockaddr_in sin = { AF_INET };

	    sp = getservbyname("telnet", "tcp");
	    if (sp == 0) {
		    fprintf(stderr, "telnetd: tcp/telnet: unknown service\n");
		    exit(1);
	    }
	    sin.sin_port = sp->s_port;
	    argc--, argv++;
	    if (argc > 0) {
		    sin.sin_port = atoi(*argv);
		    sin.sin_port = htons((u_short)sin.sin_port);
	    }

	    s = socket(AF_INET, SOCK_STREAM, 0);
	    if (s < 0) {
		    perror("telnetd: socket");;
		    exit(1);
	    }
	    if (bind(s, &sin, sizeof sin) < 0) {
		perror("bind");
		exit(1);
	    }
	    if (listen(s, 1) < 0) {
		perror("listen");
		exit(1);
	    }
	    foo = sizeof sin;
	    ns = accept(s, &sin, &foo);
	    if (ns < 0) {
		perror("accept");
		exit(1);
	    }
	    dup2(ns, 0);
	    close(s);
	}
#endif	/* defined(DEBUG) */
	openlog("telnetd", LOG_PID | LOG_ODELAY, LOG_DAEMON);
	fromlen = sizeof (from);
	if (getpeername(0, &from, &fromlen) < 0) {
		fprintf(stderr, "%s: ", argv[0]);
		perror("getpeername");
		_exit(1);
	}
	if (setsockopt(0, SOL_SOCKET, SO_KEEPALIVE, &on, sizeof (on)) < 0) {
		syslog(LOG_WARNING, "setsockopt (SO_KEEPALIVE): %m");
	}
	doit(0, &from);
}

#if RISCOS
/* Give a default value to terminal type in case negotiation fails.  If
 * a TTYPE suboption is received, we ignore this initialization.
 * This is principally for old 4.2 client compatibility.
 */
char	*terminaltype = "TERM=network";
#else
char	*terminaltype = 0;
#endif
char	*envinit[2];
int	cleanup(), sigabort();

/*
 * ttloop
 *
 *	A small subroutine to flush the network output buffer, get some data
 * from the network, and pass it through the telnet state machine.  We
 * also flush the pty input buffer (by dropping its data) if it becomes
 * too full.
 */

void
ttloop()
{
    if (nfrontp-nbackp) {
	netflush();
    }
    ncc = read(net, netibuf, sizeof netibuf);
    if (ncc < 0) {
	syslog(LOG_INFO, "ttloop:  read: %m\n");
	exit(1);
    } else if (ncc == 0) {
	syslog(LOG_INFO, "ttloop:  peer died: %m\n");
	exit(1);
    }
    netip = netibuf;
    telrcv();			/* state machine */
    if (ncc > 0) {
	pfrontp = pbackp = ptyobuf;
	telrcv();
    }
}

/*
 * getterminaltype
 *
 *	Ask the other end to send along its terminal type.
 * Output is the variable terminaltype filled in.
 */

void
getterminaltype()
{
    static char sbuf[] = { IAC, DO, TELOPT_TTYPE };

    settimer(getterminal);
    bcopy(sbuf, nfrontp, sizeof sbuf);
    nfrontp += sizeof sbuf;
    hisopts[TELOPT_TTYPE] = OPT_YES_BUT_ALWAYS_LOOK;
    while (sequenceIs(ttypeopt, getterminal)) {
	ttloop();
    }
    if (hisopts[TELOPT_TTYPE] == OPT_YES) {
	static char sbbuf[] = { IAC, SB, TELOPT_TTYPE, TELQUAL_SEND, IAC, SE };

	bcopy(sbbuf, nfrontp, sizeof sbbuf);
	nfrontp += sizeof sbbuf;
	while (sequenceIs(ttypesubopt, getterminal)) {
	    ttloop();
	}
    }
}

/*
 * Get a pty, scan input lines.
 */
doit(f, who)
	int f;
	struct sockaddr_in *who;
{
	char *host, *inet_ntoa();
	int i, p, t;
#ifndef RISCOS
	struct sgttyb b;
#endif RISCOS
	struct hostent *hp;
	int c;

#if  RISCOS
	{
		struct stat stb;
		int	i;
		int	rdev;

		p = open("/dev/ptcm", O_RDWR | O_NDELAY);
		if (p < 0 || fstat(p, &stb) < 0) {
			if (p >= 0) {
				close(p);
				p = -1;
			};
		} else {
			rdev = stb.st_rdev;
			for (i = 0; i <= 9; i++) {
				sprintf(&line[0],"/dev/ptmc%d",i);
				if (stat(line, &stb) >= 0 &&
				    major(rdev) == major(stb.st_rdev))
					break;
			};
			if (i > 9) {
				close(p);
				p = -1;
			} else {
				sprintf(&line[0],"/dev/ttyq%d",
					(i * 256) + minor(rdev));
			};
		};
		if (p < 0) {
			p = open("/dev/ptc", O_RDWR | O_NDELAY);
			if (p < 0 || fstat(p, &stb) < 0)
				fatal(f, "All network ports in use");
			sprintf(&line[0], "/dev/ttyq%d", minor(stb.st_rdev));
		};
	}
	dup2(f, 0);
#else
	for (c = 'p'; c <= 's'; c++) {
		struct stat stb;

		line = "/dev/ptyXX";
		line[strlen("/dev/pty")] = c;
		line[strlen("/dev/ptyp")] = '0';
		if (stat(line, &stb) < 0)
			break;
		for (i = 0; i < 16; i++) {
			line[sizeof("/dev/ptyp") - 1] = "0123456789abcdef"[i];
			p = open(line, O_RDWR);
			if (p > 0)
				goto gotpty;
		}
	}
	fatal(f, "All network ports in use");
	/*NOTREACHED*/
gotpty:
	dup2(f, 0);
	line[strlen("/dev/")] = 't';
#endif
	t = open("/dev/tty", O_RDWR);
	if (t >= 0) {
		ioctl(t, TIOCNOTTY, 0);
		close(t);
	}
	t = open(line, O_RDWR);
	if (t < 0)
		fatalperror(f, line);
	if (fchmod(t, 0))
		fatalperror(f, line);
	(void)signal(SIGHUP, SIG_IGN);
	vhangup();
	(void)signal(SIGHUP, SIG_DFL);
	i = t;
	t = open(line, O_RDWR);
	if (t < 0)
		fatalperror(f, line);
	(void)close(i); /* vhangup disables, doesn't close */

	/* Call sigabort() to clean up before exiting on signals 
	 * that cause termination.
	 */
	signal(SIGHUP, sigabort);
	signal(SIGINT, sigabort);
	signal(SIGQUIT, sigabort);
	signal(SIGILL, sigabort);
	signal(SIGTRAP, sigabort);
	signal(SIGIOT, sigabort);
	signal(SIGEMT, sigabort);
	signal(SIGFPE, sigabort);
	signal(SIGBUS, sigabort);
	signal(SIGSEGV, sigabort);
	signal(SIGSYS, sigabort);
	signal(SIGPIPE, sigabort);
	signal(SIGALRM, sigabort);
	signal(SIGTERM, sigabort);
	signal(SIGXCPU, sigabort);
	signal(SIGXFSZ, sigabort);
	signal(SIGVTALRM, sigabort);
	signal(SIGPROF, sigabort);
	signal(SIGUSR1, sigabort);
	signal(SIGUSR2, sigabort);

#if RISCOS
	/* This should be an open of a previously unused "tty".
	 * As such, we initialize all of the termio struct to a
	 * known, sane state.  We don't even bother to do a TCGETA,
	 * since the previous values aren't meaningful.
	 */

	ioctl(t, TCSETA, &cookedstate);
#else
	ioctl(t, TIOCGETP, &b);
	b.sg_flags = CRMOD|XTABS|ANYP;
	ioctl(t, TIOCSETP, &b);
	ioctl(p, TIOCGETP, &b);
	b.sg_flags &= ~ECHO;
	ioctl(p, TIOCSETP, &b);
#endif RISCOS

	hp = gethostbyaddr(&who->sin_addr, sizeof (struct in_addr),
		who->sin_family);
	if (hp)
		host = hp->h_name;
	else
		host = inet_ntoa(who->sin_addr);

	net = f;
	pty = p;

	/*
	 * get terminal type.
	 */
	getterminaltype();

#if RISCOS
	{ int on=1;
	ioctl(f, FIONBIO, &on);
	ioctl(p, FIONBIO, &on);
	ioctl(p, TIOCPKT, &on);
	}
#endif
	if ((i = fork()) < 0)
		fatalperror(f, "fork");
	if (i)
		telnet(f, p);
	close(f);
	close(p);
	dup2(t, 0);
	dup2(t, 1);
	dup2(t, 2);
	close(t);
	envinit[0] = terminaltype;
	envinit[1] = 0;
	environ = envinit;
#if  RISCOS
	adut();
#endif
	/*
	 * -h : pass on name of host.
	 *		WARNING:  -h is accepted by login if and only if
	 *			getuid() == 0.
	 * -p : don't clobber the environment (so terminal type stays set).
	 */
#if RISCOS
	execl("/bin/login", "login", "-h", host, 0);
#else
	execl("/bin/login", "login", "-h", host,
					terminaltype ? "-p" : 0, 0);
#endif
	fatalperror(f, "/bin/login");
	/*NOTREACHED*/
}

fatal(f, msg)
	int f;
	char *msg;
{
	char buf[BUFSIZ];

	(void) sprintf(buf, "telnetd: %s.\r\n", msg);
	(void) write(f, buf, strlen(buf));
	exit(1);
}

fatalperror(f, msg)
	int f;
	char *msg;
{
	char buf[BUFSIZ];
	extern char *sys_errlist[];

	(void) sprintf(buf, "%s: %s\r\n", msg, sys_errlist[errno]);
	fatal(f, buf);
}


/*
 * Check a descriptor to see if out of band data exists on it.
 */


stilloob(s)
int	s;		/* socket number */
{
    static struct timeval timeout = { 0 };
    fd_set	excepts;
    int value;

    do {
	FD_ZERO(&excepts);
	FD_SET(s, &excepts);
	value = select(s+1, (fd_set *)0, (fd_set *)0, &excepts, &timeout);
    } while ((value == -1) && (errno == EINTR));

    if (value < 0) {
	fatalperror(pty, "select");
    }
    if (FD_ISSET(s, &excepts)) {
	return 1;
    } else {
	return 0;
    }
}

/*
 * Main loop.  Select from pty and network, and
 * hand data to telnet receiver finite state machine.
 */
telnet(f, p)
{
	int on = 1;
	char hostname[MAXHOSTNAMELEN];
#define	TABBUFSIZ	512
	char	defent[TABBUFSIZ];
	char	defstrs[TABBUFSIZ];
#undef	TABBUFSIZ
	char *HE;
	char *HN;
	char *IM;
#ifdef RISCOS
	int bnfd;	/* banner file fd */
	int nrd;	/* read count from banner file */
#endif

#ifndef RISCOS
	ioctl(f, FIONBIO, &on);
	ioctl(p, FIONBIO, &on);
	ioctl(p, TIOCPKT, &on);
#endif
#if	defined(SO_OOBINLINE)
	setsockopt(net, SOL_SOCKET, SO_OOBINLINE, &on, sizeof on);
#endif	/* defined(SO_OOBINLINE) */
	signal(SIGTSTP, SIG_IGN);
	/*
	 * Ignoring SIGTTOU keeps the kernel from blocking us
	 * in ttioctl() in /sys/tty.c.
	 */
	signal(SIGTTOU, SIG_IGN);
	signal(SIGCHLD, cleanup);
	setpgrp(0, 0);

	/*
	 * Request to do remote echo and to suppress go ahead and
	 * to negotiate about Window Size.
	 */
	if (!myopts[TELOPT_ECHO]) {
	    dooption(TELOPT_ECHO);
	}
	if (!myopts[TELOPT_SGA]) {
	    dooption(TELOPT_SGA);
	}
	if (!myopts[TELOPT_NAWS]) {
	    willoption(TELOPT_NAWS);
	}

	/*
	 * Is the client side a 4.2 (NOT 4.3) system?  We need to know this
	 * because 4.2 clients are unable to deal with TCP urgent data.
	 *
	 * To find out, we send out a "DO ECHO".  If the remote system
	 * answers "WILL ECHO" it is probably a 4.2 client, and we note
	 * that fact ("WILL ECHO" ==> that the client will echo what
	 * WE, the server, sends it; it does NOT mean that the client will
	 * echo the terminal input).
	 */
	(void) sprintf(nfrontp, doopt, TELOPT_ECHO);
	nfrontp += sizeof doopt-2;
	hisopts[TELOPT_ECHO] = OPT_YES_BUT_ALWAYS_LOOK;

	/*
	 * Show banner that getty never gave.
	 *
	 * We put the banner in the pty input buffer.  This way, it
	 * gets carriage return null processing, etc., just like all
	 * other pty --> client data.
	 */

	gethostname(hostname, sizeof (hostname));
#if RISCOS
	if ( (bnfd = open("/etc/banner",O_RDONLY)) < 0 )
	{
	    /* no banner file, use default internal banner */
	    sprintf(ptyibuf+1, BANNER, hostname);
	}
	else
	{
	    /* read the banner file into the pty input buffer */
	    /* the -2 is because the ptyibuf[0] is used internally,
	     * and sprintf will put a null at the end.
	     */
	    nrd = read(bnfd,bannerbuf,sizeof(ptyibuf)-2);  
	    close(bnfd);
	    if (nrd == (sizeof(ptyibuf) - 2))
		syslog(LOG_WARNING,"/etc/banner file may be too large");
	    sprintf(ptyibuf+1,bannerbuf,hostname);
	}
#else
	if (getent(defent, "default") == 1) {
		char *getstr();
		char *p=defstrs;

		HE = getstr("he", &p);
		HN = getstr("hn", &p);
		IM = getstr("im", &p);
		if (HN && *HN)
			strcpy(hostname, HN);
		edithost(HE, hostname);
		if (IM && *IM)
			putf(IM, ptyibuf+1);
	} else {
		sprintf(ptyibuf+1, BANNER, hostname);
	}
#endif

	ptyip = ptyibuf+1;		/* Prime the pump */
	pcc = strlen(ptyip);		/* ditto */

	/* Clear ptybuf[0] - where the packet information is received */
	ptyibuf[0] = 0;

	/*
	 * Call telrcv() once to pick up anything received during
	 * terminal type negotiation.
	 */
	telrcv();

	for (;;) {
		fd_set ibits, obits, xbits;
		register int c;

		if (ncc < 0 && pcc < 0)
			break;

		FD_ZERO(&ibits);
		FD_ZERO(&obits);
		FD_ZERO(&xbits);
		/*
		 * Never look for input if there's still
		 * stuff in the corresponding output buffer
		 */
		if (nfrontp - nbackp || pcc > 0) {
			FD_SET(f, &obits);
			FD_SET(p, &xbits);
		} else {
			FD_SET(p, &ibits);
		}
		if (pfrontp - pbackp || ncc > 0) {
			FD_SET(p, &obits);
		} else {
			FD_SET(f, &ibits);
		}
		if (!SYNCHing) {
			FD_SET(f, &xbits);
		}
		if ((c = select(16, &ibits, &obits, &xbits,
						(struct timeval *)0)) < 1) {
			if (c == -1) {
				if (errno == EINTR) {
					continue;
				}
			}
			sleep(5);
			continue;
		}

		/*
		 * Any urgent data?
		 */
		if (FD_ISSET(net, &xbits)) {
		    SYNCHing = 1;
		}

		/*
		 * Something to read from the network...
		 */
		if (FD_ISSET(net, &ibits)) {
#if	!defined(SO_OOBINLINE)
			/*
			 * In 4.2 (and 4.3 beta) systems, the
			 * OOB indication and data handling in the kernel
			 * is such that if two separate TCP Urgent requests
			 * come in, one byte of TCP data will be overlaid.
			 * This is fatal for Telnet, but we try to live
			 * with it.
			 *
			 * In addition, in 4.2 (and...), a special protocol
			 * is needed to pick up the TCP Urgent data in
			 * the correct sequence.
			 *
			 * What we do is:  if we think we are in urgent
			 * mode, we look to see if we are "at the mark".
			 * If we are, we do an OOB receive.  If we run
			 * this twice, we will do the OOB receive twice,
			 * but the second will fail, since the second
			 * time we were "at the mark", but there wasn't
			 * any data there (the kernel doesn't reset
			 * "at the mark" until we do a normal read).
			 * Once we've read the OOB data, we go ahead
			 * and do normal reads.
			 *
			 * There is also another problem, which is that
			 * since the OOB byte we read doesn't put us
			 * out of OOB state, and since that byte is most
			 * likely the TELNET DM (data mark), we would
			 * stay in the TELNET SYNCH (SYNCHing) state.
			 * So, clocks to the rescue.  If we've "just"
			 * received a DM, then we test for the
			 * presence of OOB data when the receive OOB
			 * fails (and AFTER we did the normal mode read
			 * to clear "at the mark").
			 */
		    if (SYNCHing) {
			int atmark;

			ioctl(net, SIOCATMARK, (char *)&atmark);
			if (atmark) {
			    ncc = recv(net, netibuf, sizeof (netibuf), MSG_OOB);
			    if ((ncc == -1) && (errno == EINVAL)) {
				ncc = read(net, netibuf, sizeof (netibuf));
				if (sequenceIs(didnetreceive, gotDM)) {
				    SYNCHing = stilloob(net);
				}
			    }
			} else {
			    ncc = read(net, netibuf, sizeof (netibuf));
			}
		    } else {
			ncc = read(net, netibuf, sizeof (netibuf));
		    }
		    settimer(didnetreceive);
#else	/* !defined(SO_OOBINLINE)) */
		    ncc = read(net, netibuf, sizeof (netibuf));
#endif	/* !defined(SO_OOBINLINE)) */
		    if (ncc < 0 && errno == EWOULDBLOCK)
			ncc = 0;
		    else {
			if (ncc <= 0) {
			    break;
			}
			netip = netibuf;
		    }
		}

		/*
		 * Something to read from the pty...
		 */
		if (FD_ISSET(p, &xbits)) {
			if (read(p, ptyibuf, 1) != 1) {
				break;
			}
		}
		if (FD_ISSET(p, &ibits)) {
			pcc = read(p, ptyibuf, BUFSIZ);
			if (pcc < 0 && errno == EWOULDBLOCK)
				pcc = 0;
			else {
				if (pcc <= 0)
					break;
				/* Skip past "packet" */
				pcc--;
				ptyip = ptyibuf+1;
			}
		}
		if (ptyibuf[0] & TIOCPKT_FLUSHWRITE) {
			netclear();	/* clear buffer back */
			*nfrontp++ = IAC;
			*nfrontp++ = DM;
			neturg = nfrontp-1;  /* off by one XXX */
			ptyibuf[0] = 0;
		}

		while (pcc > 0) {
			if ((&netobuf[BUFSIZ] - nfrontp) < 2)
				break;
			c = *ptyip++ & 0377, pcc--;
			if (c == IAC)
				*nfrontp++ = c;
			*nfrontp++ = c;
			/* Don't do CR-NUL if we are in binary mode */
			if ((c == '\r') && (myopts[TELOPT_BINARY] == OPT_NO)) {
				if (pcc > 0 && ((*ptyip & 0377) == '\n')) {
					*nfrontp++ = *ptyip++ & 0377;
					pcc--;
				} else
					*nfrontp++ = '\0';
			}
		}
		if (FD_ISSET(f, &obits) && (nfrontp - nbackp) > 0)
			netflush();
		if (ncc > 0)
			telrcv();
		if (FD_ISSET(p, &obits) && (pfrontp - pbackp) > 0)
			ptyflush();
	}
	cleanup();
}
	
/*
 * State for recv fsm
 */
#define	TS_DATA		0	/* base state */
#define	TS_IAC		1	/* look for double IAC's */
#define	TS_CR		2	/* CR-LF ->'s CR */
#define	TS_SB		3	/* throw away begin's... */
#define	TS_SE		4	/* ...end's (suboption negotiation) */
#define	TS_WILL		5	/* will option negotiation */
#define	TS_WONT		6	/* wont " */
#define	TS_DO		7	/* do " */
#define	TS_DONT		8	/* dont " */

telrcv()
{
	register int c;
	static int state = TS_DATA;

	while (ncc > 0) {
		if ((&ptyobuf[BUFSIZ] - pfrontp) < 2)
			return;
		c = *netip++ & 0377, ncc--;
		switch (state) {

		case TS_CR:
			state = TS_DATA;
			/* Strip off \n or \0 after a \r */
			if ((c == 0) || (c == '\n')) {
				break;
			}
			/* FALL THROUGH */

		case TS_DATA:
			if (c == IAC) {
				state = TS_IAC;
				break;
			}
			if (inter > 0)
				break;
			/*
			 * We now map \r\n ==> \r for pragmatic reasons.
			 * Many client implementations send \r\n when
			 * the user hits the CarriageReturn key.
			 *
			 * We USED to map \r\n ==> \n, since \r\n says
			 * that we want to be in column 1 of the next
			 * printable line, and \n is the standard
			 * unix way of saying that (\r is only good
			 * if CRMOD is set, which it normally is).
			 */
			if ((c == '\r') && (hisopts[TELOPT_BINARY] == OPT_NO)) {
				state = TS_CR;
			}
			*pfrontp++ = c;
			break;

		case TS_IAC:
			switch (c) {

			/*
			 * Send the process on the pty side an
			 * interrupt.  Do this with a NULL or
			 * interrupt char; depending on the tty mode.
			 */
			case IP:
				interrupt();
				break;

			case BREAK:
				sendbrk();
				break;

			/*
			 * Are You There?
			 */
			case AYT:
				strcpy(nfrontp, "\r\n[Yes]\r\n");
				nfrontp += 9;
				break;

			/*
			 * Abort Output
			 */
			case AO: {
#if RISCOS
					struct termio b;

					ptyflush();	/* half-hearted */
					ioctl(pty, TCGETA, &b);
					if (b.c_cc[V_FLUSH] != CDEL) {
						*pfrontp++ = b.c_cc[V_FLUSH];
					}
#else
					struct ltchars tmpltc;

					ptyflush();	/* half-hearted */
					ioctl(pty, TIOCGLTC, &tmpltc);
					if (tmpltc.t_flushc != '\377') {
						*pfrontp++ = tmpltc.t_flushc;
					}
#endif RISCOS
					netclear();	/* clear buffer back */
					*nfrontp++ = IAC;
					*nfrontp++ = DM;
					neturg = nfrontp-1; /* off by one XXX */
					break;
				}

			/*
			 * Erase Character and
			 * Erase Line
			 */
			case EC:
			case EL: {
#if RISCOS
					struct termio b;
					char ch;

					ptyflush();	/* half-hearted */
					ioctl(pty, TCGETA, &b);
					ch = (c == EC) ?
						b.c_cc[VERASE] : b.c_cc[VKILL];
					if (ch != CDEL) {
						*pfrontp++ = ch;
					}
#else
					struct sgttyb b;
					char ch;

					ptyflush();	/* half-hearted */
					ioctl(pty, TIOCGETP, &b);
					ch = (c == EC) ?
						b.sg_erase : b.sg_kill;
					if (ch != '\377') {
						*pfrontp++ = ch;
					}
#endif RISCOS
					break;
				}

			/*
			 * Check for urgent data...
			 */
			case DM:
				SYNCHing = stilloob(net);
				settimer(gotDM);
				break;


			/*
			 * Begin option subnegotiation...
			 */
			case SB:
				state = TS_SB;
				continue;

			case WILL:
				state = TS_WILL;
				continue;

			case WONT:
				state = TS_WONT;
				continue;

			case DO:
				state = TS_DO;
				continue;

			case DONT:
				state = TS_DONT;
				continue;

			case IAC:
				*pfrontp++ = c;
				break;
			}
			state = TS_DATA;
			break;

		case TS_SB:
			if (c == IAC) {
				state = TS_SE;
			} else {
				SB_ACCUM(c);
			}
			break;

		case TS_SE:
			if (c != SE) {
				if (c != IAC) {
					SB_ACCUM(IAC);
				}
				SB_ACCUM(c);
				state = TS_SB;
			} else {
				SB_TERM();
				suboption();	/* handle sub-option */
				state = TS_DATA;
			}
			break;

		case TS_WILL:
			if (hisopts[c] != OPT_YES)
				willoption(c);
			state = TS_DATA;
			continue;

		case TS_WONT:
			if (hisopts[c] != OPT_NO)
				wontoption(c);
			state = TS_DATA;
			continue;

		case TS_DO:
			if (myopts[c] != OPT_YES)
				dooption(c);
			state = TS_DATA;
			continue;

		case TS_DONT:
			if (myopts[c] != OPT_NO) {
				dontoption(c);
			}
			state = TS_DATA;
			continue;

		default:
			syslog(LOG_ERR, "telnetd: panic state=%d\n", state);
			printf("telnetd: panic state=%d\n", state);
			exit(1);
		}
	}
}

willoption(option)
	int option;
{
	char *fmt;

	switch (option) {

	case TELOPT_BINARY:
		mode(RAW, 0);
		fmt = doopt;
		break;

	case TELOPT_ECHO:
		not42 = 0;		/* looks like a 4.2 system */
		/*
		 * Now, in a 4.2 system, to break them out of ECHOing
		 * (to the terminal) mode, we need to send a "WILL ECHO".
		 * Kludge upon kludge!
		 */
		if (myopts[TELOPT_ECHO] == OPT_YES) {
		    dooption(TELOPT_ECHO);
		}
		fmt = dont;
		break;

	case TELOPT_TTYPE:
		settimer(ttypeopt);
		if (hisopts[TELOPT_TTYPE] == OPT_YES_BUT_ALWAYS_LOOK) {
		    hisopts[TELOPT_TTYPE] = OPT_YES;
		    return;
		}
		fmt = doopt;
		break;

	case TELOPT_NAWS:
		fmt = doopt;
		break;

	case TELOPT_SGA:
		fmt = doopt;
		break;

	case TELOPT_TM:
		fmt = dont;
		break;

	default:
		fmt = dont;
		break;
	}
	if (fmt == doopt) {
		hisopts[option] = OPT_YES;
	} else {
		hisopts[option] = OPT_NO;
	}
	(void) sprintf(nfrontp, fmt, option);
	nfrontp += sizeof (dont) - 2;
}

wontoption(option)
	int option;
{
	char *fmt;

	switch (option) {
	case TELOPT_ECHO:
		not42 = 1;		/* doesn't seem to be a 4.2 system */
		break;

	case TELOPT_BINARY:
		mode(0, RAW);
		break;

	case TELOPT_TTYPE:
	    settimer(ttypeopt);
	    break;
	}

	fmt = dont;
	hisopts[option] = OPT_NO;
	(void) sprintf(nfrontp, fmt, option);
	nfrontp += sizeof (doopt) - 2;
}

dooption(option)
	int option;
{
	char *fmt;

	switch (option) {

	case TELOPT_TM:
		fmt = wont;
		break;

	case TELOPT_ECHO:
		mode(ECHO|CRMOD, 0);
		fmt = will;
		break;

	case TELOPT_BINARY:
		mode(RAW, 0);
		fmt = will;
		break;

	case TELOPT_SGA:
		fmt = will;
		break;

	default:
		fmt = wont;
		break;
	}
	if (fmt == will) {
	    myopts[option] = OPT_YES;
	} else {
	    myopts[option] = OPT_NO;
	}
	(void) sprintf(nfrontp, fmt, option);
	nfrontp += sizeof (doopt) - 2;
}


dontoption(option)
int option;
{
    char *fmt;

    switch (option) {
    case TELOPT_ECHO:		/* we should stop echoing */
	mode(0, ECHO);
	fmt = wont;
	break;

    default:
	fmt = wont;
	break;
    }

    if (fmt = wont) {
	myopts[option] = OPT_NO;
    } else {
	myopts[option] = OPT_YES;
    }
    (void) sprintf(nfrontp, fmt, option);
    nfrontp += sizeof (wont) - 2;
}

/*
 * suboption()
 *
 *	Look at the sub-option buffer, and try to be helpful to the other
 * side.
 *
 *	Currently we recognize:
 *
 *	Terminal type is
 */

suboption()
{
    switch (SB_GET()) {
    case TELOPT_TTYPE: {		/* Yaaaay! */
	static char terminalname[5+41] = "TERM=";

	settimer(ttypesubopt);

	if (SB_GET() != TELQUAL_IS) {
	    return;		/* ??? XXX but, this is the most robust */
	}

	terminaltype = terminalname+strlen(terminalname);

	while ((terminaltype < (terminalname + sizeof terminalname-1)) &&
								    !SB_EOF()) {
	    register int c;

	    c = SB_GET();
	    if (isupper(c)) {
		c = tolower(c);
	    }
	    *terminaltype++ = c;    /* accumulate name */
	}
	*terminaltype = 0;
	terminaltype = terminalname;
	break;
    } /* end of case TELOPT_TTYPE */

    case TELOPT_NAWS: {
	/*
	 * Change window size as requested by client.
	 */

#ifdef	TIOCSWINSZ
	{
	struct winsize ws;

	if (SB_EOF())
		return;
	ws.ws_col = SB_GET() << 8;
	if (SB_EOF())
		return;
	ws.ws_col |= SB_GET();
	if (SB_EOF())
		return;
	ws.ws_row = SB_GET() << 8;
	if (SB_EOF())
		return;
	ws.ws_row |= SB_GET();

	(void) ioctl(pty, TIOCSWINSZ, (char *)&ws);
	netflush();
	}
#endif	/* TIOCSWINSZ */
	break;

    }  /* end of case TELOPT_NAWS */
    default:
	;
    }
    SB_CLEAR();		/* Reset things for the next one. */
}

mode(on, off)
	int on, off;
{
#if RISCOS
	struct termio b;

	ptyflush();
	ioctl(pty, TCGETA, &b);
	if (on & RAW) {
		/* We were not binary-like, so save these flags */
		if (b.c_iflag || b.c_oflag || (b.c_cflag & PARENB) ||
		    (b.c_lflag & (ISIG|ICANON|XCASE)))
			cookedstate = b;
		b.c_iflag = 0;
		b.c_oflag = 0;
		b.c_cflag &= ~PARENB;
		b.c_lflag &= ~(ISIG|ICANON|XCASE);
	} else if (off & RAW) {
		b = cookedstate;
	}
	if (on & ECHO) {
		b.c_lflag |= ECHO;
	} else if (off & ECHO) {
		b.c_lflag &= ~ECHO;
	}
	if (on & CRMOD) {
		b.c_iflag |= ICRNL;
		b.c_oflag |= ONLCR;
	} else if (off & CRMOD) {
		b.c_iflag &= ~ICRNL;
		b.c_oflag &= ~ONLCR;
	}
	ioctl(pty, TCSETAF, &b);
#else
	struct sgttyb b;

	ptyflush();
	ioctl(pty, TIOCGETP, &b);
	b.sg_flags |= on;
	b.sg_flags &= ~off;
	ioctl(pty, TIOCSETP, &b);
#endif RISCOS
}

/*
 * Send interrupt to process on other side of pty.
 * If it is in raw mode, just write NULL;
 * otherwise, write intr char.
 */
interrupt()
{
#if RISCOS
	struct termio b;

	/* We can do this properly */
	ioctl(pty, TCGETA, &b);
	if (b.c_iflag & BRKINT) {
		ioctl(pty, TCSBRK, 0);
	} else {
		b.c_iflag |= BRKINT;
		ioctl(pty, TCSETA, &b);
		ioctl(pty, TCSBRK, 0);
		b.c_iflag &= ~BRKINT;
		ioctl(pty, TCSETAW, &b);
	}
#else
	struct sgttyb b;
	struct tchars tchars;

	ptyflush();	/* half-hearted */
	ioctl(pty, TIOCGETP, &b);
	if (b.sg_flags & RAW) {
		*pfrontp++ = '\0';
		return;
	}
	*pfrontp++ = ioctl(pty, TIOCGETC, &tchars) < 0 ?
		'\177' : tchars.t_intrc;
#endif RISCOS
}

/*
 * Send quit to process on other side of pty.
 * If it is in raw mode, just write NULL;
 * otherwise, write quit char.
 */
sendbrk()
{
#if RISCOS
	/* Do not be confused by the words BREAK or sendbrk.  This procedure
	 * has nothing to do with ASCII breaks.  This is called if the user
	 * types the QUIT character.  As such, I'm not sure why we don't just
	 * send that character to the application....
	 * We use ICANON to indicate whether we are "raw" or not.  It should
	 * suffice, even though that bit does not encode everything that 
	 * "raw"ness implies.
	 */
	struct termio b;

	ptyflush();	/* half-hearted */
	ioctl(pty, TCGETA, &b);
	if (b.c_lflag & ICANON == 0)
		*pfrontp++ = '\0';
	else
		*pfrontp++ = b.c_cc[VQUIT];
#else
	struct sgttyb b;
	struct tchars tchars;

	ptyflush();	/* half-hearted */
	ioctl(pty, TIOCGETP, &b);
	if (b.sg_flags & RAW) {
		*pfrontp++ = '\0';
		return;
	}
	*pfrontp++ = ioctl(pty, TIOCGETC, &tchars) < 0 ?
		'\034' : tchars.t_quitc;
#endif RISCOS
}

ptyflush()
{
	int n;

	if ((n = pfrontp - pbackp) > 0)
		n = write(pty, pbackp, n);
	if (n < 0)
		return;
	pbackp += n;
	if (pbackp == pfrontp)
		pbackp = pfrontp = ptyobuf;
}

/*
 * nextitem()
 *
 *	Return the address of the next "item" in the TELNET data
 * stream.  This will be the address of the next character if
 * the current address is a user data character, or it will
 * be the address of the character following the TELNET command
 * if the current address is a TELNET IAC ("I Am a Command")
 * character.
 */

char *
nextitem(current)
char	*current;
{
    if ((*current&0xff) != IAC) {
	return current+1;
    }
    switch (*(current+1)&0xff) {
    case DO:
    case DONT:
    case WILL:
    case WONT:
	return current+3;
    case SB:		/* loop forever looking for the SE */
	{
	    register char *look = current+2;

	    for (;;) {
		if ((*look++&0xff) == IAC) {
		    if ((*look++&0xff) == SE) {
			return look;
		    }
		}
	    }
	}
    default:
	return current+2;
    }
}


/*
 * netclear()
 *
 *	We are about to do a TELNET SYNCH operation.  Clear
 * the path to the network.
 *
 *	Things are a bit tricky since we may have sent the first
 * byte or so of a previous TELNET command into the network.
 * So, we have to scan the network buffer from the beginning
 * until we are up to where we want to be.
 *
 *	A side effect of what we do, just to keep things
 * simple, is to clear the urgent data pointer.  The principal
 * caller should be setting the urgent data pointer AFTER calling
 * us in any case.
 */

netclear()
{
    register char *thisitem, *next;
    char *good;
#define	wewant(p)	((nfrontp > p) && ((*p&0xff) == IAC) && \
				((*(p+1)&0xff) != EC) && ((*(p+1)&0xff) != EL))

    thisitem = netobuf;

    while ((next = nextitem(thisitem)) <= nbackp) {
	thisitem = next;
    }

    /* Now, thisitem is first before/at boundary. */

    good = netobuf;	/* where the good bytes go */

    while (nfrontp > thisitem) {
	if (wewant(thisitem)) {
	    int length;

	    next = thisitem;
	    do {
		next = nextitem(next);
	    } while (wewant(next) && (nfrontp > next));
	    length = next-thisitem;
	    bcopy(thisitem, good, length);
	    good += length;
	    thisitem = next;
	} else {
	    thisitem = nextitem(thisitem);
	}
    }

    nbackp = netobuf;
    nfrontp = good;		/* next byte to be sent */
    neturg = 0;
}

/*
 *  netflush
 *		Send as much data as possible to the network,
 *	handling requests for urgent data.
 */


netflush()
{
    int n;

    if ((n = nfrontp - nbackp) > 0) {
	/*
	 * if no urgent data, or if the other side appears to be an
	 * old 4.2 client (and thus unable to survive TCP urgent data),
	 * write the entire buffer in non-OOB mode.
	 */
	if ((neturg == 0) || (not42 == 0)) {
	    n = write(net, nbackp, n);	/* normal write */
	} else {
	    n = neturg - nbackp;
	    /*
	     * In 4.2 (and 4.3) systems, there is some question about
	     * what byte in a sendOOB operation is the "OOB" data.
	     * To make ourselves compatible, we only send ONE byte
	     * out of band, the one WE THINK should be OOB (though
	     * we really have more the TCP philosophy of urgent data
	     * rather than the Unix philosophy of OOB data).
	     */
	    if (n > 1) {
		n = send(net, nbackp, n-1, 0);	/* send URGENT all by itself */
	    } else {
		n = send(net, nbackp, n, MSG_OOB);	/* URGENT data */
	    }
	}
    }
    if (n < 0) {
	if (errno == EWOULDBLOCK)
	    return;
	/* should blow this guy away... */
	return;
    }
    nbackp += n;
    if (nbackp >= neturg) {
	neturg = 0;
    }
    if (nbackp == nfrontp) {
	nbackp = nfrontp = netobuf;
    }
}

sigabort(sig)
int	sig;
{
	/* Cleanup, then resend signal to get default action, 
	 * which may include producing a core dump.
	 */
	rmut();
	shutdown(net, 2);
	signal(sig, SIG_DFL);
	kill(getpid(), sig);
	exit(1);
}

cleanup()
{

	rmut();
	shutdown(net, 2);
	exit(1);
}

#if  RISCOS
#include <utmp.h>

#define SCPYN(a, b)	strncpy(a, b, sizeof(a))
#define SCMPN(a, b)	strncmp(a, b, sizeof(a))

static struct utmp entry;

/* add ourself to utmp
 */
adut()
{
	register FILE *fp;

	SCPYN(entry.ut_user, "telnet");
	SCPYN(entry.ut_id, &line[(sizeof("/dev/tty") - 1)]);
	SCPYN(entry.ut_line, line+sizeof("/dev/")-1);
	entry.ut_pid = getpid();
	entry.ut_type = LOGIN_PROCESS;
	entry.ut_time = time(0);
	setutent();
	(void)pututline(&entry);
	endutent();

/* Now attempt to add to the end of the wtmp file.  Do not create
 * if it does not already exist.  **  Note  ** This is the reason
 * "r+" is used instead of "a+".  "r+" will not create a file, while
 * "a+" will. */
	if ((fp = fopen("/etc/wtmp","r+")) != NULL) {
		fseek(fp,0L,2);
		fwrite((char*)&entry,sizeof(entry),1,fp);
		fclose(fp);
	}
}

rmut()
{
	register FILE *fp;

	SCPYN(entry.ut_user, "telnet");
	SCPYN(entry.ut_id, &line[(sizeof("/dev/tty") - 1)]);
	SCPYN(entry.ut_line, line+sizeof("/dev/")-1);
	entry.ut_pid = getpid();
	entry.ut_type = DEAD_PROCESS;
	entry.ut_time = time(0);
	setutent();
	(void)pututline(&entry);
	endutent();
	if ((fp = fopen("/etc/wtmp","r+")) != NULL) {
		entry.ut_user[0] = '\0';
		fseek(fp,0L,2);
		fwrite((char*)&entry,sizeof(entry),1,fp);
		fclose(fp);
	}

	chmod(line, 0666);
	chown(line, 0, 0);
}
#else
#include <utmp.h>

struct	utmp wtmp;
char	wtmpf[]	= "/usr/adm/wtmp";
char	utmpf[] = "/etc/utmp";
#define SCPYN(a, b)	strncpy(a, b, sizeof(a))
#define SCMPN(a, b)	strncmp(a, b, sizeof(a))

rmut()
{
	register f;
	int found = 0;
	struct utmp *u, *utmp;
	int nutmp;
	struct stat statbf;

	f = open(utmpf, O_RDWR);
	if (f >= 0) {
		fstat(f, &statbf);
		utmp = (struct utmp *)malloc(statbf.st_size);
		if (!utmp)
			syslog(LOG_ERR, "utmp malloc failed");
		if (statbf.st_size && utmp) {
			nutmp = read(f, utmp, statbf.st_size);
			nutmp /= sizeof(struct utmp);

			for (u = utmp ; u < &utmp[nutmp] ; u++) {
				if (SCMPN(u->ut_line, line+5) ||
				    u->ut_name[0]==0)
					continue;
				lseek(f, ((long)u)-((long)utmp), L_SET);
				SCPYN(u->ut_name, "");
				SCPYN(u->ut_host, "");
				time(&u->ut_time);
				write(f, (char *)u, sizeof(wtmp));
				found++;
			}
		}
		close(f);
	}
	if (found) {
		f = open(wtmpf, O_WRONLY|O_APPEND);
		if (f >= 0) {
			SCPYN(wtmp.ut_line, line+5);
			SCPYN(wtmp.ut_name, "");
			SCPYN(wtmp.ut_host, "");
			time(&wtmp.ut_time);
			write(f, (char *)&wtmp, sizeof(wtmp));
			close(f);
		}
	}
	chmod(line, 0666);
	chown(line, 0, 0);
	line[strlen("/dev/")] = 'p';
	chmod(line, 0666);
	chown(line, 0, 0);
}
#endif

char	editedhost[32];

edithost(pat, host)
	register char *pat;
	register char *host;
{
	register char *res = editedhost;

	if (!pat)
		pat = "";
	while (*pat) {
		switch (*pat) {

		case '#':
			if (*host)
				host++;
			break;

		case '@':
			if (*host)
				*res++ = *host++;
			break;

		default:
			*res++ = *pat;
			break;

		}
		if (res == &editedhost[sizeof editedhost - 1]) {
			*res = '\0';
			return;
		}
		pat++;
	}
	if (*host)
		strncpy(res, host, sizeof editedhost - (res - editedhost) - 1);
	else
		*res = '\0';
	editedhost[sizeof editedhost - 1] = '\0';
}

static char *putlocation;

puts(s)
register char *s;
{

	while (*s)
		putchr(*s++);
}

putchr(cc)
{
	*putlocation++ = cc;
}

#ifndef RISCOS
putf(cp, where)
register char *cp;
char *where;
{
	char *slash;
	char datebuffer[60];
	extern char *rindex();

	putlocation = where;

	while (*cp) {
		if (*cp != '%') {
			putchr(*cp++);
			continue;
		}
		switch (*++cp) {

		case 't':
			slash = rindex(line, '/');
			if (slash == (char *) 0)
				puts(line);
			else
				puts(&slash[1]);
			break;

		case 'h':
			puts(editedhost);
			break;

		case 'd':
			get_date(datebuffer);
			puts(datebuffer);
			break;

		case '%':
			putchr('%');
			break;
		}
		cp++;
	}
}
#endif
