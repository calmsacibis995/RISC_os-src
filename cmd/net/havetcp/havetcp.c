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
#ident	"$Header: havetcp.c,v 1.5.2.2 90/05/09 17:04:32 wje Exp $"

/*
 * Determine if the system supports tcp.
 *
 * Written by: Kipp Hickman
 */
#include "signal.h"

#include "bsd/sys/types.h"
#include "bsd/sys/socket.h"
#include "bsd/net/af.h"
#include "bsd/netinet/in.h"

nottcp()
{
	exit(-1);
}

main()
{
	/*
	 * Try to create a socket, catching SIGSYS in case this is a
	 * really old kernel.
	 */
	signal(SIGSYS, nottcp);

	/*
	 * Try to get a socket.  This should always succeed on tcp systems.
	 * For non-tcp systems, it should always fail.
	 */
	if (socket(AF_INET, SOCK_STREAM, 0) < 0)
		exit(-1);
	exit(0);
}
