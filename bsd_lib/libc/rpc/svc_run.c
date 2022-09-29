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
#ident	"$Header: svc_run.c,v 1.3.1.2 90/05/07 21:02:26 wje Exp $"
/*
 * @(#)svc_run.c 1.2 88/07/27 4.0NFSSRC Copyr 1988 Sun Micro
 *
 * This is the rpc server side idle loop
 * Wait for input, call server program.
 */
#include <rpc/rpc.h>
#include <sys/errno.h>
#ifdef SYSTYPE_BSD43
#include <syslog.h>
#endif
#ifdef SYSTYPE_SYSV
#include <bsd/syslog.h>
#endif

void
svc_run()
{
	fd_set readfds;
	extern int errno;

	for (;;) {
		readfds = svc_fdset;
		switch (select(_rpc_dtablesize(), &readfds, (int *)0, (int *)0,
			       (struct timeval *)0)) {
		case -1:
			if (errno == EINTR) {
				continue;
			}
			syslog(LOG_ERR,"svc_run: - select failed");
			return;
		case 0:
			continue;
		default:
			svc_getreqset(&readfds);
		}
	}
}
