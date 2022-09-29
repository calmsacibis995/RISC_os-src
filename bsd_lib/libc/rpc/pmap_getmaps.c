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
#ident	"$Header: pmap_getmaps.c,v 1.2.1.2 90/05/07 20:59:14 wje Exp $"
/*
 * @(#)pmap_getmaps.c 1.1 86/09/24 Copyr 1984 Sun Micro
 *
 * pmap_getmaps.c
 * Client interface to pmap rpc service.
 * contains pmap_getmaps, which is only tcp service involved
 */

#include <rpc/rpc.h>
#include <rpc/pmap_prot.h>
#include <rpc/pmap_clnt.h>
#ifdef KERNEL
#include <bsd/sys/time.h>
#include <bsd/sys/ioctl.h>
#else
#ifdef SYSTYPE_BSD43
#include <sys/time.h>
#include <sys/ioctl.h>
#endif
#ifdef SYSTYPE_SYSV
#include <bsd/sys/time.h>
#include <bsd/sys/ioctl.h>
#endif
#endif

#ifdef SYSTYPE_BSD43
#include <sys/socket.h>
#include <netdb.h>
#include <net/if.h>
#endif
#ifdef SYSTYPE_SYSV
#include <bsd/sys/socket.h>
#include <bsd/netdb.h>
#include <bsd/net/if.h>
#endif
#include <stdio.h>
#include <errno.h>

#define NAMELEN 255
#define MAX_BROADCAST_SIZE 1400

extern int errno;

/*
 * Get a copy of the current port maps.
 * Calls the pmap service remotely to do get the maps.
 */
struct pmaplist *
pmap_getmaps(address)
	 struct sockaddr_in *address;
{
	struct pmaplist *head = (struct pmaplist *)NULL;
	int socket = -1;
	struct timeval minutetimeout;
	register CLIENT *client;

	minutetimeout.tv_sec = 60;
	minutetimeout.tv_usec = 0;
	address->sin_port = htons(PMAPPORT);
	client = clnttcp_create(address, PMAPPROG,
	    PMAPVERS, &socket, 50, 500);
	if (client != (CLIENT *)NULL) {
		if (CLNT_CALL(client, PMAPPROC_DUMP, xdr_void, NULL, xdr_pmaplist,
		    &head, minutetimeout) != RPC_SUCCESS) {
			clnt_perror(client, "pmap_getmaps rpc problem");
		}
		CLNT_DESTROY(client);
	}
	(void)close(socket);
	address->sin_port = 0;
	return (head);
}
