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
#ident	"$Header: tipout.c,v 1.1.2.6 90/05/28 17:53:25 wje Exp $"

#include "tip.h"
/*
 * tip
 *
 * lower fork of tip -- handles passive side
 *  reading from the remote host
 */

static	jmp_buf sigbuf;

/*
 * TIPOUT wait state routine --
 *   sent by TIPIN when it wants to posses the remote host
 */
intIOT()
{

	write(repdes[1],&ccc,1);
	read(fildes[0], &ccc,1);
	longjmp(sigbuf, 1);
}

#ifdef MIPS
static int display = 1;	/* boolean: display when scripting flag */
#endif
/*
 * Scripting command interpreter --
 *  accepts script file name over the pipe and acts accordingly
 */
intEMT()
{
	char c, line[256];
	register char *pline = line;
	char reply;
#ifdef MIPS
	char *mode;
#endif

	read(fildes[0], &c, 1);
	while (c != '\n') {
		*pline++ = c;
		read(fildes[0], &c, 1);
	}
	*pline = '\0';
	if (boolean(value(SCRIPT)) && fscript != NULL)
		fclose(fscript);
	if (pline == line) {
		boolean(value(SCRIPT)) = FALSE;
		reply = 'y';
	} else {
#ifdef MIPS
		mode = (line[0] == 'a') ? "a" : "w";
		display = (line[1] == 'd');
		if ((fscript = fopen(&line[2], mode)) == NULL)
			reply = 'n';
		else {
			reply = 'y';
			boolean(value(SCRIPT)) = TRUE;
		}
#else
		if ((fscript = fopen(line, "a")) == NULL)
			reply = 'n';
		else {
			reply = 'y';
			boolean(value(SCRIPT)) = TRUE;
		}
#endif
	}
	write(repdes[1], &reply, 1);
	longjmp(sigbuf, 1);
}

intTERM()
{

	if (boolean(value(SCRIPT)) && fscript != NULL)
		fclose(fscript);
	exit(0);
}

intSYS()
{

	boolean(value(BEAUTIFY)) = !boolean(value(BEAUTIFY));
	longjmp(sigbuf, 1);
}

/*
 * ****TIPOUT   TIPOUT****
 */
#ifdef RISCOS
extern int parent_pid;
#endif RISCOS

tipout()
{
	char buf[BUFSIZ];
	register char *cp;
	register int cnt;
	int omask;
	extern int errno;

	signal(SIGINT, SIG_IGN);
	signal(SIGQUIT, SIG_IGN);
	signal(SIGEMT, intEMT);		/* attention from TIPIN */
	signal(SIGTERM, intTERM);	/* time to go signal */
	signal(SIGIOT, intIOT);		/* scripting going on signal */
	signal(SIGHUP, intTERM);	/* for dial-ups */
	signal(SIGSYS, intSYS);		/* beautify toggle */
	(void) setjmp(sigbuf);
	for (omask = 0;; sigsetmask(omask)) {
		cnt = read(FD, buf, BUFSIZ);
		if (cnt <= 0) {
			/* lost carrier */
			if (cnt < 0 && ((errno == EIO)  || 
					(errno == ENXIO))) {
				sigblock(sigmask(SIGTERM));
#ifdef RISCOS
				kill(parent_pid,SIGEMT);
#endif RISCOS
				intTERM();
				/*NOTREACHED*/
			}
			continue;
		}
#define	ALLSIGS	sigmask(SIGEMT)|sigmask(SIGTERM)|sigmask(SIGIOT)|sigmask(SIGSYS)
		omask = sigblock(ALLSIGS);
		for (cp = buf; cp < buf + cnt; cp++)
			*cp &= 0177;
#ifdef MIPS
		if (!boolean(value(SCRIPT)) || fscript == NULL || display)
			write(1, buf, cnt);
#else
		write(1, buf, cnt);
#endif
		if (boolean(value(SCRIPT)) && fscript != NULL) {
			if (!boolean(value(BEAUTIFY))) {
				fwrite(buf, 1, cnt, fscript);
				continue;
			}
			for (cp = buf; cp < buf + cnt; cp++)
				if ((*cp >= ' ' && *cp <= '~') ||
				    any(*cp, value(EXCEPTIONS)))
					putc(*cp, fscript);
		}
	}
}
