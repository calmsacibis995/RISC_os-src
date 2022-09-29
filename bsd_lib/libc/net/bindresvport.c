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
#ident	"$Header: bindresvport.c,v 1.2.1.2 90/05/07 20:50:53 wje Exp $"

#if !defined(lint) && defined(SCCSIDS)
static  char sccsid[] = "@(#)bindresvport.c	1.1 88/03/22 4.0NFSSRC; from 1.8 88/02/08 SMI"; /* from UCB 4.2 83/06/27 */
#endif

/*
 * Copyright (c) 1987 by Sun Microsystems, Inc.
 */

#include <sys/types.h>
#include <sys/errno.h>
#ifdef SYSTYPE_SYSV
#include <bsd/sys/socket.h>
#include <bsd/netinet/in.h>
#endif
#ifdef SYSTYPE_BSD43
#include <sys/socket.h>
#include <netinet/in.h>
#endif

/*
 * Bind a socket to a privileged IP port
 */
bindresvport(sd, sin)
	int sd;
	struct sockaddr_in *sin;
{
	int res;
	static short port;
	struct sockaddr_in myaddr;
	extern int errno;
	int i;

#define STARTPORT 600
#define ENDPORT (IPPORT_RESERVED - 1)
#define NPORTS	(ENDPORT - STARTPORT + 1)

	if (sin == (struct sockaddr_in *)0) {
		sin = &myaddr;
		bzero(sin, sizeof (*sin));
		sin->sin_family = AF_INET;
	} else if (sin->sin_family != AF_INET) {
		errno = EPFNOSUPPORT;
		return (-1);
	}
	if (port == 0) {
		port = (getpid() % NPORTS) + STARTPORT;
	}
	res = -1;
	errno = EADDRINUSE;
	for (i = 0; i < NPORTS && res < 0 && errno == EADDRINUSE; i++) {
		sin->sin_port = htons(port++);
		if (port > ENDPORT) {
			port = STARTPORT;
		}
		res = bind(sd, sin, sizeof(struct sockaddr_in));
	}
	return (res);
}
