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
#ident	"$Header: rtime.c,v 1.2.1.2 90/05/07 21:01:03 wje Exp $"
#if defined(LIBC_SCCS) && !defined(lint)
static char sccsid[] = 	"@(#)rtime.c	1.2 88/07/27 4.0NFSSRC Copyr 1988 Sun Micro";
#endif

/* 
 * Copyright (c) 1988 by Sun Microsystems, Inc.
 * 1.8 88/02/08 
 */

/*
 * rtime - get time from remote machine
 *
 * gets time, obtaining value from host
 * on the udp/time socket.  Since timeserver returns
 * with time of day in seconds since Jan 1, 1900,  must
 * subtract seconds before Jan 1, 1970 to get
 * what unix uses.
 */
#ifdef SYSTYPE_BSD43
#include <sys/types.h>
#include <sys/socket.h>
#include <sys/time.h>
#include <sys/errno.h>
#include <netinet/in.h>
#endif
#ifdef SYSTYPE_SYSV
#include <bsd/sys/types.h>
#include <bsd/sys/socket.h>
#include <bsd/sys/time.h>
#include <errno.h>
#include <bsd/netinet/in.h>
#endif

#ifndef NULL
#define NULL 0
#endif
#define NYEARS	(1970 - 1900)
#define TOFFSET (60*60*24*(365*NYEARS + (NYEARS/4)))
extern errno;

static void do_close();

rtime(addrp, timep, timeout)
	struct sockaddr_in *addrp;
	struct timeval *timep;
	struct timeval *timeout;
{
	int s;
	fd_set readfds;
	int res;
	unsigned long thetime;
	struct sockaddr_in from;
	int fromlen;
	int type;

	if (timeout == NULL) {
		type = SOCK_STREAM;
	} else {
		type = SOCK_DGRAM;
	}
	s = socket(AF_INET, type, 0);
	if (s < 0) {
		return(-1);
	}
	addrp->sin_family = AF_INET;
	addrp->sin_port = htons(IPPORT_TIMESERVER);
	if (type == SOCK_DGRAM) {
		res = sendto(s, (char *)&thetime, sizeof(thetime), 0, 
			     (struct sockaddr *)addrp, sizeof(*addrp));
		if (res < 0) {
			do_close(s);
			return(-1);	
		}
		do {
			FD_ZERO(&readfds);
			FD_SET(s, &readfds);
			res = select(_rpc_dtablesize(), &readfds, (int *)NULL, 
				     (int *)NULL, timeout);
		} while (res < 0 && errno == EINTR);
		if (res <= 0) {
			if (res == 0) {
				errno = ETIMEDOUT;
			}
			do_close(s);
			return(-1);	
		}
		fromlen = sizeof(from);
		res = recvfrom(s, (char *)&thetime, sizeof(thetime), 0, 
			       (struct sockaddr *)&from, &fromlen);
		do_close(s);
		if (res < 0) {
			return(-1);	
		}
	} else {
		if (connect(s, (struct sockaddr *)addrp, sizeof(*addrp)) < 0) {
			do_close(s);
			return(-1);
		}
		res = read(s, (char *)&thetime, sizeof(thetime));
		do_close(s);
		if (res < 0) {
			return(-1);
		}
	}
	if (res != sizeof(thetime)) {
		errno = EIO;
		return(-1);	
	}
	thetime = ntohl(thetime);
	timep->tv_sec = thetime - TOFFSET;
	timep->tv_usec = 0;
	return(0);
}

static void
do_close(s)
	int s;
{
	int save;

	save = errno;
	(void) close(s);
	errno = save;
}
