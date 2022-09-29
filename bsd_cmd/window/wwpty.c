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
#ident	"$Header: wwpty.c,v 1.2.2.2 90/05/07 20:02:29 wje Exp $"

#include "ww.h"


#ifdef RISCOS

#include <sys/file.h>
#include <sys/types.h>
#include <sys/stat.h>

wwgetpty(w)
register struct ww *w;
{

#define MASTER "/dev/ptc"		/* Name of cloning master pty */
#define SLAVE "/dev/ttyq%d"		/* Format of name of slave pty */

	struct stat stb;
	int on = 1;

	if ((w->ww_pty = open(MASTER, O_RDWR)) < 0) {
		perror("master pty");
		w->ww_pty = -1;
		wwerrno = WWE_NOPTY;
		return -1;
	}

	if (fstat(w->ww_pty, &stb) < 0) {
	  	perror("slave name");
		(void) close (w->ww_pty);
		w->ww_pty = -1;
		wwerrno = WWE_NOPTY;
		return -1;
	}

	sprintf(w->ww_ttyname, SLAVE, minor(stb.st_rdev));

	if (ioctl(w->ww_pty, TIOCPKT, (char *)&on) < 0) {
		perror ("TIOCPKT on master");
		(void) close (w->ww_pty);
		w->ww_pty = -1;
		wwerrno = WWE_NOPTY;
		return -1;
	}

	return 0;
}
#else
wwgetpty(w)
register struct ww *w;
{
	register char c, *p;
	int tty;
	int on = 1;
#define PTY "/dev/XtyXX"
#define _PT	5
#define _PQRS	8
#define _0_9	9

	(void) strcpy(w->ww_ttyname, PTY);
	for (c = 'p'; c <= 'u'; c++) {
		w->ww_ttyname[_PT] = 'p';
		w->ww_ttyname[_PQRS] = c;
		w->ww_ttyname[_0_9] = '0';
		if (access(w->ww_ttyname, 0) < 0)
			break;
		for (p = "0123456789abcdef"; *p; p++) {
			w->ww_ttyname[_PT] = 'p';
			w->ww_ttyname[_0_9] = *p;
			if ((w->ww_pty = open(w->ww_ttyname, 2)) < 0)
				continue;
			w->ww_ttyname[_PT] = 't';
			if ((tty = open(w->ww_ttyname, 2)) < 0) {
				(void) close(w->ww_pty);
				continue;
			}
			(void) close(tty);
			if (ioctl(w->ww_pty, TIOCPKT, (char *)&on) < 0) {
				(void) close(w->ww_pty);
				continue;
			}
			return 0;
		}
	}
	w->ww_pty = -1;
	wwerrno = WWE_NOPTY;
	return -1;
}
#endif RISCOS
