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
#ident	"$Header: wwspawn.c,v 1.1.2.2 90/05/07 20:03:55 wje Exp $"

#include "ww.h"
#include <sys/signal.h>

/*
 * There is a dead lock with vfork and closing of pseudo-ports.
 * So we have to be sneaky about error reporting.
 */
wwspawn(wp, file, argv)
register struct ww *wp;
char *file;
char **argv;
{
	int pid;
	int ret;
	char erred = 0;
	int s;

	s = sigblock(sigmask(SIGCHLD));
	switch (pid = vfork()) {
	case -1:
		wwerrno = WWE_SYS;
		ret = -1;
		break;
	case 0:
		if (wwenviron(wp) >= 0) {
			char wbuf[2];
			wbuf[0] = wp->ww_id + '1';
			wbuf[1] = '\0';
			setenv("WINDOW_ID", wbuf, 1);
			execvp(file, argv);
		}
		erred = 1;
		_exit(1);
	default:
		if (erred) {
			wwerrno = WWE_SYS;
			ret = -1;
		} else {
			wp->ww_pid = pid;
			wp->ww_state = WWS_HASPROC;
			ret = pid;
		}
	}
	(void) sigsetmask(s);
	if (wp->ww_socket >= 0) {
		(void) close(wp->ww_socket);
		wp->ww_socket = -1;
	}
	return ret;
}
