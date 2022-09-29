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
#ident	"$Header: getpass.c,v 1.2.1.3 90/05/07 20:37:35 wje Exp $"

#if defined(LIBC_SCCS) && !defined(lint)
static char sccsid[] = "@(#)getpass.c	5.2 (Berkeley) 3/9/86";
#endif LIBC_SCCS and not lint

#include <stdio.h>
#include <signal.h>
#include <sgtty.h>


char *
getpass(prompt)
char *prompt;
{
	struct sgttyb ttyb;
	int flags;
	register char *p;
	register c;
	FILE *fi;
	static char pbuf[9];
#ifdef SYSTYPE_SYSV
#define signal sigset
	void (*sig)();
#else SYSTYPE_SYSV
	int (*signal())();
	int (*sig)();
#endif SYSTYPE_SYSV

	if ((fi = fdopen(open("/dev/tty", 2), "r")) == NULL)
		fi = stdin;
	else
		setbuf(fi, (char *)NULL);
	sig = signal(SIGINT, SIG_IGN);
	ioctl(fileno(fi), TIOCGETP, &ttyb);
	flags = ttyb.sg_flags;
	ttyb.sg_flags &= ~ECHO;
	ioctl(fileno(fi), TIOCSETP, &ttyb);
	fprintf(stderr, "%s", prompt); fflush(stderr);
	for (p=pbuf; (c = getc(fi))!='\n' && c!=EOF;) {
		if (p < &pbuf[8])
			*p++ = c;
	}
	*p = '\0';
	fprintf(stderr, "\n"); fflush(stderr);
	ttyb.sg_flags = flags;
	ioctl(fileno(fi), TIOCSETP, &ttyb);
	signal(SIGINT, sig);
	if (fi != stdin)
		fclose(fi);
	return(pbuf);
}
