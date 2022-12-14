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
/* $Header: globals.h,v 1.1.2.2 90/05/09 17:43:44 wje Exp $ */
/*
 * Copyright (c) 1983 Regents of the University of California.
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
 *
 *	@(#)globals.h	2.6 (Berkeley) 6/18/88
 */

#include <sys/param.h>
#include <stdio.h>
#include <sys/time.h>
#include <errno.h>
#include <syslog.h>
#include <sys/socket.h>
#include <netinet/in.h>
#include <netdb.h>
#include <arpa/inet.h>

extern int errno;
extern int sock;

#define SAMPLEINTVL	240		/* synch() freq for master, sec */
#define	MAXADJ		20		/* max correction (sec) for adjtime */
/*
 * Parameters for network time measurement
 * of each host using ICMP timestamp requests.
 */
#define RANGE		20		/* best expected round-trip time, ms */
#define MSGS 		5		/* # of timestamp replies to average */
#define TRIALS		10		/* max # of timestamp echos sent */

#define MINTOUT		360
#define MAXTOUT		900

#define GOOD		1
#define UNREACHABLE	2
#define NONSTDTIME	3
#define HOSTDOWN 	0x7fffffff

#define OFF	0
#define ON	1

/*
 * Global and per-network states.
 */
#define NOMASTER 	0		/* no master on any network */
#define SLAVE 		1
#define MASTER		2
#define IGNORE		4
#define ALL		(SLAVE|MASTER|IGNORE)
#define SUBMASTER	(SLAVE|MASTER)

#define NHOSTS		100	/* max number of hosts controlled by timed */

struct host {
	char *name;
	struct sockaddr_in addr;
	long delta;
	u_short seq;
};

struct netinfo {
	struct netinfo *next;
	u_long net;
	u_long mask;
	struct in_addr my_addr;
	struct sockaddr_in dest_addr;	/* broadcast addr or point-point */
	long status;
};

extern struct netinfo *nettab;
extern int status;
extern int trace;
extern int sock;
extern struct sockaddr_in from;
extern struct netinfo *fromnet, *slavenet;
extern FILE *fd;
extern char hostname[];
extern char tracefile[];
extern struct host hp[];
extern int backoff;
extern long delay1, delay2;
extern int slvcount;
extern int nslavenets;		/* Number of nets were I could be a slave */
extern int nmasternets;		/* Number of nets were I could be a master */
extern int nignorednets;	/* Number of ignored nets */
extern int nnets;		/* Number of nets I am connected to */

char *strcpy(), *malloc();
