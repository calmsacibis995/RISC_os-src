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
#ident	"$Header: udp_request.c,v 1.1.1.2 90/05/09 17:33:54 wje Exp $"

/*
 *	$Header: udp_request.c,v 1.1.1.2 90/05/09 17:33:54 wje Exp $
 *	Author: J. Davin
 *	Copyright 1988, 1989, Massachusetts Institute of Technology
 *	See permission and disclaimer notice in file "notice.h"
 */

#include	<notice.h>

#include	<sys/types.h>
#include	<sys/socket.h>
#include	<netinet/in.h>

#include	<ctypes.h>
#include	<debug.h>
#include	<local.h>
#include	<udp_request.h>

typedef		struct			UdpTag {

		int			udpSocket;
		struct	sockaddr	udpSockAddr;
		CIntfType		udpRefCnt;

		}			UdpType;

typedef		UdpType		*UdpPtrType;

SmpStatusType	udpSend (udp, bp, n)

SmpSocketType		udp;
CBytePtrType		bp;
CIntfType		n;

{
	UdpPtrType		tp;
	int			result;

	if (udp == (SmpSocketType) 0) {
		return (errBad);
	}

	DEBUG0 ("udpSend:\n");
	DEBUGBYTES (bp, n);
	DEBUG0 ("\n");

	tp = (UdpPtrType) udp;
	do {	
		result = sendto (tp->udpSocket, (char *) bp,
			(int) n, (int) 0,
			& (tp->udpSockAddr), sizeof (struct sockaddr_in));
		n -= result;
		bp += result;

	} while ((result > 0) && (n > 0));

	if (result < 0) {
		perror ("udpSend");
		return (errBad);
	}
	else {
		return (errOk);
	}
}

SmpSocketType	udpNew (so, host, port)

int			so;
u_long			host;
u_short			port;

{
	UdpPtrType		tp;
	struct	sockaddr_in	*sin;

	tp = (UdpPtrType) malloc ((unsigned) sizeof (*tp));
	if (tp != (UdpPtrType) 0) {
		(void) bzero ((char *) tp, (int) sizeof (*tp));
		tp->udpSocket = so;
		tp->udpRefCnt = 1;
		sin = (struct sockaddr_in *) & tp->udpSockAddr;
		sin->sin_family = AF_INET;
		sin->sin_port = port;
		sin->sin_addr.s_addr = host;
	}

	return ((SmpSocketType) tp);
}

SmpSocketType	udpFree (udp)

SmpSocketType	udp;

{
	UdpPtrType		tp;

	if (udp != (SmpSocketType) 0) {
		tp = (UdpPtrType) udp;
		if (--tp->udpRefCnt <= 0) {
			(void) free ((char *) tp);
		}
	}
	return ((SmpSocketType) 0);
}

