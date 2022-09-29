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
#ident	"$Header: script.c,v 1.5.2.3 90/05/10 00:30:53 wje Exp $"
/*
 * Copyright (c) 1980 Regents of the University of California.
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
 *   
 *  This program generates three processes, identified in a rather misleading
 *  fashion by the booleans "child" and "subchild":
 *
 *      Parent (child > 0 && subchild == 0)
 *
 *	 In procedure "doinput", reads from stdin and writes to the master
 *	 end of a pseudotty.
 *
 *      Child (child > 0 && subchild > 0)
 *
 *	 In procedure "dooutput", reads from the master end of the pseudotty
 *	 and writes to both stdout and the script file.
 *
 *      Subchild (child == 0 && subchild == 0)
 *
 *	 In procedure "doshell", closes unwanted file descriptors, dups
 *	 stdin, stdout, and stderr onto the slave end of the pseudotty,
 *	 and execs the appropriate shell, which then both reads from and
 *	 writes to the slave end of the pseudotty.
 *
 *   The program employs several pieces of magic which I do not understand.
 *   The parent puts stdin into raw, noecho mode. The subchild puts the slave
 *   end of the pseudotty back into the tty mode which was in effect at the
 *   beginning of the program (presumably icanon or cooked). This has the
 *   happy effect that a ^d on the keyboard does not cause the parent to see
 *   eof, but does cause the subchild to see eof. It also has the happy
 *   effect that keystrokes are echoed on stdout without explicit work on the
 *   part of the program.
 *
 *   When the subchild exits (because it sees eof, or because it sees an
 *   "exit" command, or for any other reason), the script catches the
 *   SIGCHLD signal and thereby the other two processes finish their work and
 *   quit. 
 *
 */

#define SYSV	1
#ifndef lint
char copyright[] =
"@(#) Copyright (c) 1980 Regents of the University of California.\n\
 All rights reserved.\n";
#endif /* not lint */

#ifndef lint
static char sccsid[] = "@(#)script.c	5.6 (Berkeley) 6/29/88";
#endif /* not lint */

/*
 * script
 */
#if	TERMIO
#include <bsd/sys/types.h>
#include <bsd/sys/time.h>
#include <sys/sysmacros.h>
#include <termio.h>
#include <fcntl.h>
#else	!TERMIO
#include <sys/types.h>
#include <sys/time.h>
#endif	!TERMIO
#include <sys/stat.h>
#include <sys/ioctl.h>
#include <sys/file.h>
#include <stdio.h>
#include <signal.h>

char	*shell;
FILE	*fscript;
int	master;
int	slave;
int	child;
int	subchild;
char	*fname;

#if	TERMIO
static	struct	termio savetty;
#else	!TERMIO
struct	sgttyb b;
struct	tchars tc;
struct	ltchars lc;
#endif	!TERMIO
struct	winsize win;
int	lb;
int	l;
#ifndef	TERMIO
char	*line = "/dev/ptyXX";
#endif	!TERMIO
int	aflg;

main(argc, argv)
	int argc;
	char *argv[];
{
	extern char *optarg;
	extern int optind;
	int ch;
	int finish();
	char *getenv();

	while ((ch = getopt(argc, argv, "a")) != EOF)
		switch((char)ch) {
		case 'a':
			aflg++;
			break;
		case '?':
		default:
			fprintf(stderr, "usage: script [-a] [file]\n");
			exit(1);
		}
	argc -= optind;
	argv += optind;

	if (argc > 0)
		fname = argv[0];
	else
		fname = "typescript";
	if ((fscript = fopen(fname, aflg ? "a" : "w")) == NULL) {
		perror(fname);
		fail();
	}

	shell = getenv("SHELL");
	if (shell == NULL)
		shell = "/bin/sh";

	getmaster();
	printf("Script started, file is %s\n", fname);
	fixtty();

	(void) signal(SIGCHLD, finish);
	child = fork();
	if (child < 0) {
		perror("fork");
		fail();
	}
	if (child == 0) {
		subchild = child = fork();
		if (child < 0) {
			perror("fork");
			fail();
		}
		if (child)
			dooutput();
		else
			doshell();
	}
	doinput();
}

doinput()
{
	register int cc;
	char ibuf[BUFSIZ];

	(void) fclose(fscript);
	while ((cc = read(0, ibuf, BUFSIZ)) > 0)
		(void) write(master, ibuf, cc);
	done();
}

#if	TERMIO
#include <bsd/sys/wait.h>
#include <bsd43/sys/syscall.h>
#define wait3(x,y,z) syscall(BSD43_SYS_wait3,(x),(y),(z))
#else	!TERMIO
#include <sys/wait.h>
#endif	!TERMIO

finish()
{
	union wait status;
	register int pid;
	register int die = 0;

	while ((pid = wait3(&status, WNOHANG, 0)) > 0)
		if (pid == child)
			die = 1;

	if (die)
		done();
}

dooutput()
{
	register int cc;
	time_t tvec, time();
	char obuf[BUFSIZ], *ctime();

	(void) close(0);
	tvec = time((time_t *)NULL);
	fprintf(fscript, "Script started on %s", ctime(&tvec));
	for (;;) {
		cc = read(master, obuf, sizeof (obuf));
		if (cc <= 0)
			break;
		(void) write(1, obuf, cc);
		(void) fwrite(obuf, 1, cc, fscript);
	}
	done();
}

doshell()
{
	int t;

	t = open("/dev/tty", O_RDWR);
	if (t >= 0) {
		(void) ioctl(t, TIOCNOTTY, (char *)0);
		(void) close(t);
	}
	getslave();
	(void) close(master);
	(void) fclose(fscript);
	(void) dup2(slave, 0);
	(void) dup2(slave, 1);
	(void) dup2(slave, 2);
	(void) close(slave);
	execl(shell, "sh", "-i", 0);
	perror(shell);
	fail();
}

fixtty()
{
#if	TERMIO
	struct termio	nstty;
	
	/* 
	 * Ignore breaks; allow xon/xoff with any character good for xon;
	 * turn off output processing; disable parity generation/detection;
	 * turn off canonical input processing; and set basic line discipline.
	 */
	nstty = savetty;
	nstty.c_iflag = IGNBRK | IXON | IXANY; 
	nstty.c_oflag = 0;
	nstty.c_cflag &= ~PARENB;
	nstty.c_lflag = 0;
	nstty.c_line = 0;

	nstty.c_cc[VINTR] = '\377';
	nstty.c_cc[VQUIT] = '\377';
	nstty.c_cc[VERASE] = '\377';
	nstty.c_cc[VKILL] = '\377';
	nstty.c_cc[VEOF] = 1;
	nstty.c_cc[VEOL] = 128;
	
	if (ioctl(0, (int)TCSETAW, (char *)&nstty) < 0) {
		perror("ioctl(TCSETAW)");
		done();
	}
#else	!TERMIO
	struct sgttyb sbuf;

	sbuf = b;
	sbuf.sg_flags |= RAW;
	sbuf.sg_flags &= ~ECHO;
	(void) ioctl(0, TIOCSETP, (char *)&sbuf);
#endif	!TERMIO
}

fail()
{
#ifdef TERMIO
#include <bsd43/sys/syscall.h>
#define	kill(x,y) syscall(BSD43_SYS_kill,(x),(y))
#endif TERMIO

	(void) kill(0, SIGTERM);
	done();
}

done()
{
	time_t tvec, time();
	char *ctime();

	if (subchild) {
		tvec = time((time_t *)NULL);
		fprintf(fscript,"\nscript done on %s", ctime(&tvec));
		(void) fclose(fscript);
		(void) close(master);
	} else {
#if	TERMIO
		if (ioctl(0, (int)TCSETAW, (char *)&savetty) < 0) {
			perror("done: ioctl(TCSETAW)");
			exit(1);
		}
#else	!TERMIO
		(void) ioctl(0, TIOCSETP, (char *)&b);
		printf("Script done, file is %s\n", fname);
#endif	!TERMIO
	}
	exit(0);
}

#ifdef  TERMIO
#define MASTER "/dev/ptc"		/* Name of cloning master pty */
#define SLAVE "/dev/ttyq%d"		/* Format of name of slave pty */

getmaster()
{
	int ok;
	if ((master = open(MASTER, O_RDWR)) < 0) {
		perror("master pty");
		fail();
	}
	if (ioctl(0, (int)TCGETA, (char *)&savetty) < 0) {
		perror("getmaster: ioctl(TCGETA)");
		fail();
	}
	(void) ioctl(0, TIOCGWINSZ, (char *)&win);
}

getslave()
{
	char line[(sizeof SLAVE) + 16];
	struct stat stb;

	if (master < 0 || fstat(master, &stb) < 0) {
	  	perror("slave name");
		fail();
		}
	sprintf(line, SLAVE, minor(stb.st_rdev));
	slave = open(line, O_RDWR);
	if (slave < 0) {
		perror(line);
		fail();
	}
	if (ioctl(slave, (int)TCSETAW, (char *)&savetty) < 0) {
		perror("getslave: ioctl(TCSETAW)");
		fail();
	}
	(void) ioctl(slave, TIOCSWINSZ, (char *)&win);
}
#else   !TERMIO 
getmaster()
{
	char *pty, *bank, *cp;
	struct stat stb;

	pty = &line[strlen("/dev/ptyp")];
	for (bank = "pqrs"; *bank; bank++) {
		line[strlen("/dev/pty")] = *bank;
		*pty = '0';
		if (stat(line, &stb) < 0)
			break;
		for (cp = "0123456789abcdef"; *cp; cp++) {
			*pty = *cp;
			master = open(line, O_RDWR);
			if (master >= 0) {
				char *tp = &line[strlen("/dev/")];
				int ok;

				/* verify slave side is usable */
				*tp = 't';
				ok = access(line, R_OK|W_OK) == 0;
				*tp = 'p';
				if (ok) {
				    (void) ioctl(0, TIOCGETP, (char *)&b);
				    (void) ioctl(0, TIOCGETC, (char *)&tc);
				    (void) ioctl(0, TIOCGETD, (char *)&l);
				    (void) ioctl(0, TIOCGLTC, (char *)&lc);
				    (void) ioctl(0, TIOCLGET, (char *)&lb);
				    (void) ioctl(0, TIOCGWINSZ, (char *)&win);
					return;
				}
				(void) close(master);
			}
		}
	}
	fprintf(stderr, "Out of pty's\n");
	fail();
}

getslave()
{

	line[strlen("/dev/")] = 't';
	slave = open(line, O_RDWR);
	if (slave < 0) {
		perror(line);
		fail();
	}
	(void) ioctl(slave, TIOCSETP, (char *)&b);
	(void) ioctl(slave, TIOCSETC, (char *)&tc);
	(void) ioctl(slave, TIOCSLTC, (char *)&lc);
	(void) ioctl(slave, TIOCLSET, (char *)&lb);
	(void) ioctl(slave, TIOCSETD, (char *)&l);
	(void) ioctl(slave, TIOCSWINSZ, (char *)&win);
}
#endif	!TERMIO
