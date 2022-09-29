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
#ident	"$Header: rlogind.c,v 1.21.1.3 90/05/09 17:16:57 wje Exp $"

/*
 * Copyright (c) 1983 Regents of the University of California.
 * All rights reserved.  The Berkeley software License Agreement
 * specifies the terms and conditions for redistribution.
 */

#ifndef lint
char copyright[] =
"@(#) Copyright (c) 1983 Regents of the University of California.\n\
 All rights reserved.\n";
#endif not lint

#ifndef lint
static char sccsid[] = "@(#)rlogind.c	5.15 (Berkeley) 5/23/88";
#endif not lint

/*
 * remote login server:
 *	remuser\0
 *	locuser\0
 *	terminal info\0
 *	data
 */

#include <stdio.h>
#include <sys/types.h>
#include <sys/stat.h>
#include <sys/socket.h>
#include <sys/wait.h>
#include <sys/file.h>

#include <netinet/in.h>

#include <errno.h>
#include <pwd.h>
#include <signal.h>
#if RISCOS
#include <sysv/sys/termio.h>
#else
#include <sgtty.h>
#endif
#include <stdio.h>
#include <netdb.h>
#include <syslog.h>
#include <strings.h>

# ifndef TIOCPKT_WINDOW
# define TIOCPKT_WINDOW 0x80
# endif TIOCPKT_WINDOW

extern	int errno;
int	reapchild();
struct	passwd *getpwnam();
char	*malloc();

#ifdef RISCOS
int	simulation_ok;
#endif RISCOS

/*ARGSUSED*/
main(argc, argv)
	int argc;
	char **argv;
{
	int on = 1, fromlen;
	struct sockaddr_in from;
#ifdef RISCOS
	struct	stat	sb;
#endif RISCOS

	openlog("rlogind", LOG_PID | LOG_AUTH, LOG_AUTH);
	fromlen = sizeof (from);
	if (getpeername(0, &from, &fromlen) < 0) {
		fprintf(stderr, "%s: ", argv[0]);
		perror("getpeername");
		_exit(1);
	}
	if (setsockopt(0, SOL_SOCKET, SO_KEEPALIVE, &on, sizeof (on)) < 0) {
		syslog(LOG_WARNING, "setsockopt (SO_KEEPALIVE): %m");
	}
#ifdef RISCOS
	simulation_ok = (stat("/etc/rlogind.simulation.ok", &sb) == 0);
#endif RISCOS
	doit(0, &from);
}

int	child;
int	cleanup(), sigabort();
int	netf;
#if RISCOS
struct termio deftermio = {
	0,				/* iflags */
	0,				/* oflags */
	B38400+CS8+CREAD+CLOCAL,	/* cflags */
	0,				/* lflags */
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
int	cur_stop_mode;
struct	termio	cur_termio;
int	simulate_istrip = 0;
int	simulate_parenb = 0;
int	simulate_parodd = 0;

char	*line = "/dev/ttyq12345";
#else
char	*line;
#endif
extern	char	*inet_ntoa();

struct winsize win = { 0, 0, 0, 0 };


doit(f, fromp)
	int f;
	struct sockaddr_in *fromp;
{
	int i, p, t, pid, on = 1;
	register struct hostent *hp;
	struct hostent hostent;
	char c;

	alarm(60);
	read(f, &c, 1);
	if (c != 0)
		exit(1);
	alarm(0);
	fromp->sin_port = ntohs((u_short)fromp->sin_port);
	hp = gethostbyaddr(&fromp->sin_addr, sizeof (struct in_addr),
		fromp->sin_family);
	if (hp == 0) {
		/*
		 * Only the name is used below.
		 */
		hp = &hostent;
		hp->h_name = inet_ntoa(fromp->sin_addr);
	}
	if (fromp->sin_family != AF_INET ||
	    fromp->sin_port >= IPPORT_RESERVED ||
	    fromp->sin_port < IPPORT_RESERVED/2)
		fatal(f, "Permission denied");
	write(f, "", 1);
#ifdef RISCOS
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
				fatalperror(f, "Out of ptys", errno);
			sprintf(&line[0], "/dev/ttyq%d", minor(stb.st_rdev));
		};
	}
	(void) ioctl(p, TIOCSWINSZ, &win);
	netf = f;
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
	fatal(f, "Out of ptys");
	/*NOTREACHED*/
gotpty:
	(void) ioctl(p, TIOCSWINSZ, &win);
	netf = f;
	line[strlen("/dev/")] = 't';
#endif
	t = open(line, O_RDWR);
	if (t < 0)
		fatalperror(f, line);
	if (fchmod(t, 0))
		fatalperror(f, line);
	(void)signal(SIGHUP, SIG_IGN);
	vhangup();
	(void)signal(SIGHUP, SIG_DFL);
#ifdef RISCOS
	i = t;
#endif RISCOS
	t = open(line, O_RDWR);
	if (t < 0)
		fatalperror(f, line);
#ifdef RISCOS
	(void)close (i); /* vhangup disables, doesn't close */
#endif RISCOS
#if RISCOS
	/* This should be an open of a previously unused "tty".
	 * As such, we initialize all of the termio struct to a
	 * known, sane state.  We don't even bother to do a TCGETA,
	 * since the previous values aren't meaningful.
	 */

	ioctl(t, TCSETA, &deftermio);
#else
	{
		struct sgttyb b;

		(void)ioctl(t, TIOCGETP, &b);
		b.sg_flags = RAW|ANYP;
		(void)ioctl(t, TIOCSETP, &b);
	}
#endif RISCOS
#ifdef DEBUG
	{
		int tt = open("/dev/tty", O_RDWR);
		if (tt > 0) {
			(void)ioctl(tt, TIOCNOTTY, 0);
			(void)close(tt);
		}
	}
#endif
	pid = fork();
	if (pid < 0)
		fatalperror(f, "");
	if (pid == 0) {
		close(f), close(p);
		dup2(t, 0), dup2(t, 1), dup2(t, 2);
		close(t);
#if RISCOS
		adut();
#endif
		execl("/bin/login", "login", "-r", hp->h_name, 0);
		fatalperror(2, "/bin/login");
		/*NOTREACHED*/
	}
	close(t);
	ioctl(f, FIONBIO, &on);
	ioctl(p, FIONBIO, &on);
	ioctl(p, TIOCPKT, &on);

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

	signal(SIGTSTP, SIG_IGN);
	signal(SIGCHLD, cleanup);
	setpgrp(0, 0);
	protocol(f, p);
	signal(SIGCHLD, SIG_IGN);
	cleanup();
}

char	magic[2] = { 0377, 0377 };
char	oobdata[] = {TIOCPKT_WINDOW};

#ifdef RISCOS
/*
 *	Compute whether we should simulate parenb and istrip
 */
check_pkcontrol(f,pty,new_control)
	int	pty;
	int	new_control;
{
	if (! simulation_ok)
		return;
	if (ioctl(pty,TCGETA,&cur_termio) == -1) {
		fatalperror(f, "ioctl");
	};
	simulate_istrip = cur_termio.c_iflag & ISTRIP;
	simulate_parenb = ((new_control & TIOCPKT_NOSTOP) != 0 &&
			   (cur_termio.c_cflag & PARENB) != 0);
	simulate_parodd = (cur_termio.c_cflag & PARODD);
	cur_stop_mode = new_control;
}


/*
 *	Fix up input data if needed
 */

#define FIXUP_INPUT_DATA(buf,len) ((! simulate_istrip) || \
				   fixup_input_data((buf),(len)))

fixup_input_data(buf,len)
	char	*buf;
	int	len;
{
	if (! simulate_istrip ||
	    len <= 0)
		return;
	for (; len > 0 ; len--, buf++) 
		*buf &= 0x7f;
}


/*
 *	Fix up input data if needed
 */
char	even_parity[128] = {
	/* 0x0 */	0x0,
	/* 0x1 */	0x81,
	/* 0x2 */	0x82,
	/* 0x3 */	0x3,
	/* 0x4 */	0x84,
	/* 0x5 */	0x5,
	/* 0x6 */	0x6,
	/* 0x7 */	0x87,
	/* 0x8 */	0x88,
	/* 0x9 */	0x9,
	/* 0xa */	0xa,
	/* 0xb */	0x8b,
	/* 0xc */	0xc,
	/* 0xd */	0x8d,
	/* 0xe */	0x8e,
	/* 0xf */	0xf,
	/* 0x10 */	0x90,
	/* 0x11 */	0x11,
	/* 0x12 */	0x12,
	/* 0x13 */	0x93,
	/* 0x14 */	0x14,
	/* 0x15 */	0x95,
	/* 0x16 */	0x96,
	/* 0x17 */	0x17,
	/* 0x18 */	0x18,
	/* 0x19 */	0x99,
	/* 0x1a */	0x9a,
	/* 0x1b */	0x1b,
	/* 0x1c */	0x9c,
	/* 0x1d */	0x1d,
	/* 0x1e */	0x1e,
	/* 0x1f */	0x9f,
	/* 0x20 */	0xa0,
	/* 0x21 */	0x21,
	/* 0x22 */	0x22,
	/* 0x23 */	0xa3,
	/* 0x24 */	0x24,
	/* 0x25 */	0xa5,
	/* 0x26 */	0xa6,
	/* 0x27 */	0x27,
	/* 0x28 */	0x28,
	/* 0x29 */	0xa9,
	/* 0x2a */	0xaa,
	/* 0x2b */	0x2b,
	/* 0x2c */	0xac,
	/* 0x2d */	0x2d,
	/* 0x2e */	0x2e,
	/* 0x2f */	0xaf,
	/* 0x30 */	0x30,
	/* 0x31 */	0xb1,
	/* 0x32 */	0xb2,
	/* 0x33 */	0x33,
	/* 0x34 */	0xb4,
	/* 0x35 */	0x35,
	/* 0x36 */	0x36,
	/* 0x37 */	0xb7,
	/* 0x38 */	0xb8,
	/* 0x39 */	0x39,
	/* 0x3a */	0x3a,
	/* 0x3b */	0xbb,
	/* 0x3c */	0x3c,
	/* 0x3d */	0xbd,
	/* 0x3e */	0xbe,
	/* 0x3f */	0x3f,
	/* 0x40 */	0xc0,
	/* 0x41 */	0x41,
	/* 0x42 */	0x42,
	/* 0x43 */	0xc3,
	/* 0x44 */	0x44,
	/* 0x45 */	0xc5,
	/* 0x46 */	0xc6,
	/* 0x47 */	0x47,
	/* 0x48 */	0x48,
	/* 0x49 */	0xc9,
	/* 0x4a */	0xca,
	/* 0x4b */	0x4b,
	/* 0x4c */	0xcc,
	/* 0x4d */	0x4d,
	/* 0x4e */	0x4e,
	/* 0x4f */	0xcf,
	/* 0x50 */	0x50,
	/* 0x51 */	0xd1,
	/* 0x52 */	0xd2,
	/* 0x53 */	0x53,
	/* 0x54 */	0xd4,
	/* 0x55 */	0x55,
	/* 0x56 */	0x56,
	/* 0x57 */	0xd7,
	/* 0x58 */	0xd8,
	/* 0x59 */	0x59,
	/* 0x5a */	0x5a,
	/* 0x5b */	0xdb,
	/* 0x5c */	0x5c,
	/* 0x5d */	0xdd,
	/* 0x5e */	0xde,
	/* 0x5f */	0x5f,
	/* 0x60 */	0x60,
	/* 0x61 */	0xe1,
	/* 0x62 */	0xe2,
	/* 0x63 */	0x63,
	/* 0x64 */	0xe4,
	/* 0x65 */	0x65,
	/* 0x66 */	0x66,
	/* 0x67 */	0xe7,
	/* 0x68 */	0xe8,
	/* 0x69 */	0x69,
	/* 0x6a */	0x6a,
	/* 0x6b */	0xeb,
	/* 0x6c */	0x6c,
	/* 0x6d */	0xed,
	/* 0x6e */	0xee,
	/* 0x6f */	0x6f,
	/* 0x70 */	0xf0,
	/* 0x71 */	0x71,
	/* 0x72 */	0x72,
	/* 0x73 */	0xf3,
	/* 0x74 */	0x74,
	/* 0x75 */	0xf5,
	/* 0x76 */	0xf6,
	/* 0x77 */	0x77,
	/* 0x78 */	0x78,
	/* 0x79 */	0xf9,
	/* 0x7a */	0xfa,
	/* 0x7b */	0x7b,
	/* 0x7c */	0xfc,
	/* 0x7d */	0x7d,
	/* 0x7e */	0x7e,
	/* 0x7f */	0xff };

#define FIXUP_OUTPUT_DATA(buf,len) ((! simulate_parenb) || \
				   fixup_output_data((buf),(len)))

fixup_output_data(buf,len)
	char	*buf;
	int	len;
{
	if (! simulate_parenb ||
	    len <= 0)
		return;
	for (; len > 0 ; len--, buf++) {
		*buf = even_parity[(*buf & 0x7f)];
		if (simulate_parodd)
			*buf ^= 0x80;
	}
}
#endif RISCOS


/*
 * Handle a "control" request (signaled by magic being present)
 * in the data stream.  For now, we are only willing to handle
 * window size changes.
 */
control(pty, cp, n)
	int pty;
	char *cp;
	int n;
{
	struct winsize w;

	if (n < 4+sizeof (w) || cp[2] != 's' || cp[3] != 's')
		return (0);
	oobdata[0] &= ~TIOCPKT_WINDOW;	/* we know he heard */
	bcopy(cp+4, (char *)&w, sizeof(w));
	w.ws_row = ntohs(w.ws_row);
	w.ws_col = ntohs(w.ws_col);
	w.ws_xpixel = ntohs(w.ws_xpixel);
	w.ws_ypixel = ntohs(w.ws_ypixel);
	(void)ioctl(pty, TIOCSWINSZ, &w);
	return (4+sizeof (w));
}

/*
 * rlogin "protocol" machine.
 */
protocol(f, p)
	int f, p;
{
	char pibuf[1024], fibuf[1024], *pbp, *fbp;
	register pcc = 0, fcc = 0;
	int cc;
	char cntl;

	/*
	 * Must ignore SIGTTOU, otherwise we'll stop
	 * when we try and set slave pty's window shape
	 * (our controlling tty is the master pty).
	 */
	(void) signal(SIGTTOU, SIG_IGN);

#ifdef RISCOS
	cur_stop_mode = (oobdata[0] ^ (TIOCPKT_NOSTOP | TIOCPKT_DOSTOP));
	check_pkcontrol(f,p,oobdata[0]);
#endif RISCOS

	send(f, oobdata, 1, MSG_OOB);	/* indicate new rlogin */
	for (;;) {
		int ibits, obits, ebits;

		ibits = 0;
		obits = 0;
		if (fcc)
			obits |= (1<<p);
		else
			ibits |= (1<<f);
		if (pcc >= 0)
			if (pcc)
				obits |= (1<<f);
			else
				ibits |= (1<<p);
		ebits = (1<<p);
		if (select(16, &ibits, &obits, &ebits, 0) < 0) {
			if (errno == EINTR)
				continue;
			fatalperror(f, "select");
		}
		if (ibits == 0 && obits == 0 && ebits == 0) {
			/* shouldn't happen... */
			sleep(5);
			continue;
		}
#define	pkcontrol(c)	((c)&(TIOCPKT_FLUSHWRITE|TIOCPKT_NOSTOP|TIOCPKT_DOSTOP))
		if (ebits & (1<<p)) {
			cc = read(p, &cntl, 1);
			if (cc == 1 && pkcontrol(cntl)) {
				cntl |= oobdata[0];
#ifdef RISCOS
				check_pkcontrol(f,p,cntl);
#endif RISCOS
				send(f, &cntl, 1, MSG_OOB);
				if (cntl & TIOCPKT_FLUSHWRITE) {
					pcc = 0;
					ibits &= ~(1<<p);
				}
			}
		}
		if (ibits & (1<<f)) {
			fcc = read(f, fibuf, sizeof (fibuf));
			if (fcc < 0 && errno == EWOULDBLOCK)
				fcc = 0;
			else {
				register char *cp;
				int left, n;

				if (fcc <= 0)
					break;
				fbp = fibuf;

			top:
				for (cp = fibuf; cp < fibuf+fcc-1; cp++)
					if (cp[0] == magic[0] &&
					    cp[1] == magic[1]) {
						left = fcc - (cp-fibuf);
						n = control(p, cp, left);
						if (n) {
							left -= n;
							if (left > 0)
								bcopy(cp+n, cp, left);
							fcc -= n;
							goto top; /* n^2 */
						}
					}
			}
		}

		if ((obits & (1<<p)) && fcc > 0) {
#ifdef RISCOS
			FIXUP_INPUT_DATA(fbp,fcc);
#endif RISCOS
			cc = write(p, fbp, fcc);
			if (cc > 0) {
				fcc -= cc;
				fbp += cc;
			}
		}

		if (ibits & (1<<p)) {
			pcc = read(p, pibuf, sizeof (pibuf));
			pbp = pibuf;
			if (pcc < 0 && errno == EWOULDBLOCK)
				pcc = 0;
			else if (pcc <= 0)
				break;
			else if (pibuf[0] == 0)
				pbp++, pcc--;
			else {
				if (pkcontrol(pibuf[0])) {
					pibuf[0] |= oobdata[0];
#ifdef RISCOS
					check_pkcontrol(f,p,pibuf[0]);
#endif RISCOS
					send(f, &pibuf[0], 1, MSG_OOB);
				}
				pcc = 0;
			}
		}
		if ((obits & (1<<f)) && pcc > 0) {
#ifdef RISCOS
			FIXUP_OUTPUT_DATA(pbp,pcc);
#endif RISCOS
			cc = write(f, pbp, pcc);
			if (cc < 0 && errno == EWOULDBLOCK) {
				/* also shouldn't happen */
				sleep(5);
				continue;
			}
			if (cc > 0) {
				pcc -= cc;
				pbp += cc;
			}
		}
	}
}

sigabort(sig)
int	sig;
{
	/* Cleanup, then resend signal to get default action, 
	 * which may include producing a core dump.
	 */
	rmut();
	shutdown(netf, 2);
	signal(sig, SIG_DFL);
	kill(getpid(), sig);
	exit(1);
}

cleanup()
{
	rmut();
	shutdown(netf, 2);
	exit(1);
}

fatal(f, msg)
	int f;
	char *msg;
{
	char buf[BUFSIZ];

	buf[0] = '\01';		/* error indicator */
	(void) sprintf(buf + 1, "rlogind: %s.\r\n", msg);
	(void) write(f, buf, strlen(buf));
	exit(1);
}

fatalperror(f, msg)
	int f;
	char *msg;
{
	char buf[BUFSIZ];
	extern int sys_nerr;
	extern char *sys_errlist[];

	if ((unsigned)errno < sys_nerr)
		(void) sprintf(buf, "%s: %s", msg, sys_errlist[errno]);
	else
		(void) sprintf(buf, "%s: Error %d", msg, errno);
	fatal(f, buf);
}

#if RISCOS
#include <utmp.h>

#define SCPYN(a, b)	strncpy(a, b, sizeof(a))
#define SCMPN(a, b)	strncmp(a, b, sizeof(a))

static struct utmp entry;

/* add ourself to utmp
 */
adut()
{
	register FILE *fp;

	SCPYN(entry.ut_user, "rlogin");
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

	SCPYN(entry.ut_user, "rlogin");
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
