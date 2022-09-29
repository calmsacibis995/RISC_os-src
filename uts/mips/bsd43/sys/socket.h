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
/* $Header: socket.h,v 1.7.1.2 90/05/10 04:55:24 wje Exp $ */
/*
 * Copyright (c) 1982,1985, 1986 Regents of the University of California.
 * All rights reserved.  The Berkeley software License Agreement
 * specifies the terms and conditions for redistribution.
 *
 *	@(#)socket.h	7.1 (Berkeley) 6/4/86
 */
/*----- COPYRIGHT (END) ------------------------------------------------------*/

#ifndef BSD43_
#    include <bsd43/bsd43_.h>
#endif BSD43_


/*
 * Definitions related to sockets: types, address families, options.
 */

/*
 * Types
 */
#define	BSD43_SOCK_STREAM	1		/* stream socket */
#define	BSD43_SOCK_DGRAM	2		/* datagram socket */
#define	BSD43_SOCK_RAW	3		/* raw-protocol interface */
#define	BSD43_SOCK_RDM	4		/* reliably-delivered message */
#define	BSD43_SOCK_SEQPACKET	5		/* sequenced packet stream */

/*
 * Option flags per-socket.
 */
#define	BSD43_SO_DEBUG	0x0001		/* turn on debugging info recording */
#define	BSD43_SO_ACCEPTCONN	0x0002		/* socket has had listen() */
#define	BSD43_SO_REUSEADDR	0x0004		/* allow local address reuse */
#define	BSD43_SO_KEEPALIVE	0x0008		/* keep connections alive */
#define	BSD43_SO_DONTROUTE	0x0010		/* just use interface addresses */
#define	BSD43_SO_BROADCAST	0x0020		/* permit sending of broadcast msgs */
#define	BSD43_SO_USELOOPBACK	0x0040		/* bypass hardware when possible */
#define	BSD43_SO_LINGER	0x0080		/* linger on close if data present */
#define	BSD43_SO_OOBINLINE	0x0100		/* leave received OOB data in line */

/*
 * Additional options, not kept in so_options.
 */
#define BSD43_SO_SNDBUF	0x1001		/* send buffer size */
#define BSD43_SO_RCVBUF	0x1002		/* receive buffer size */
#define BSD43_SO_SNDLOWAT	0x1003		/* send low-water mark */
#define BSD43_SO_RCVLOWAT	0x1004		/* receive low-water mark */
#define BSD43_SO_SNDTIMEO	0x1005		/* send timeout */
#define BSD43_SO_RCVTIMEO	0x1006		/* receive timeout */
#define	BSD43_SO_ERROR	0x1007		/* get error status and clear */
#define	BSD43_SO_TYPE		0x1008		/* get socket type */

/*
 * Structure used for manipulating linger option.
 */
struct	bsd43_(linger) {
	int	l_onoff;		/* option on/off */
	int	l_linger;		/* linger time */
};

/*
 * Level number for (get/set)sockopt() to apply to socket itself.
 */
#define	BSD43_SOL_SOCKET	0xffff		/* options for socket level */

/*
 * Address families.
 */
#define	BSD43_AF_UNSPEC	0		/* unspecified */
#define	BSD43_AF_UNIX		1		/* local to host (pipes, portals) */
#define	BSD43_AF_INET		2		/* internetwork: UDP, TCP, etc. */
#define	BSD43_AF_IMPLINK	3		/* arpanet imp addresses */
#define	BSD43_AF_PUP		4		/* pup protocols: e.g. BSP */
#define	BSD43_AF_CHAOS	5		/* mit CHAOS protocols */
#define	BSD43_AF_NS		6		/* XEROX NS protocols */
#define	BSD43_AF_NBS		7		/* nbs protocols */
#define	BSD43_AF_ECMA		8		/* european computer manufacturers */
#define	BSD43_AF_DATAKIT	9		/* datakit protocols */
#define	BSD43_AF_CCITT	10		/* CCITT protocols, X.25 etc */
#define	BSD43_AF_SNA		11		/* IBM SNA */
#define BSD43_AF_DECnet	12		/* DECnet */
#define BSD43_AF_DLI		13		/* Direct data link interface */
#define BSD43_AF_LAT		14		/* LAT */
#define	BSD43_AF_HYLINK	15		/* NSC Hyperchannel */
#define	BSD43_AF_APPLETALK	16		/* Apple Talk */
#define BSD43_AF_NIT          17              /* Network Interface Tap */

#define	BSD43_AF_MAX		18

/*
 * Structure used by kernel to store most
 * addresses.
 */
struct bsd43_(sockaddr) {
	u_short	sa_family;		/* address family */
	char	sa_data[14];		/* up to 14 bytes of direct address */
};

/*
 * Structure used by kernel to pass protocol
 * information in raw sockets.
 */
struct bsd43_(sockproto) {
	u_short	sp_family;		/* address family */
	u_short	sp_protocol;		/* protocol */
};

/*
 * Protocol families, same as address families for now.
 */
#define	BSD43_PF_UNSPEC	BSD43_AF_UNSPEC
#define	BSD43_PF_UNIX		BSD43_AF_UNIX
#define	BSD43_PF_INET		BSD43_AF_INET
#define	BSD43_PF_IMPLINK	BSD43_AF_IMPLINK
#define	BSD43_PF_PUP		BSD43_AF_PUP
#define	BSD43_PF_CHAOS	BSD43_AF_CHAOS
#define	BSD43_PF_NS		BSD43_AF_NS
#define	BSD43_PF_NBS		BSD43_AF_NBS
#define	BSD43_PF_ECMA		BSD43_AF_ECMA
#define	BSD43_PF_DATAKIT	BSD43_AF_DATAKIT
#define	BSD43_PF_CCITT	BSD43_AF_CCITT
#define	BSD43_PF_SNA		BSD43_AF_SNA
#define BSD43_PF_DECnet	BSD43_AF_DECnet
#define BSD43_PF_DLI		BSD43_AF_DLI
#define BSD43_PF_LAT		BSD43_AF_LAT
#define	BSD43_PF_HYLINK	BSD43_AF_HYLINK
#define	BSD43_PF_APPLETALK	BSD43_AF_APPLETALK
#define BSD43_PF_NIT          BSD43_AF_NIT

#define	BSD43_PF_MAX		BSD43_AF_MAX

/*
 * Maximum queue length specifiable by listen.
 */
#define	BSD43_SOMAXCONN	5

/*
 * Message header for recvmsg and sendmsg calls.
 */
struct bsd43_(msghdr) {
	caddr_t	msg_name;		/* optional address */
	int	msg_namelen;		/* size of address */
	struct	bsd43_(iovec) *msg_iov;		/* scatter/gather array */
	int	msg_iovlen;		/* # elements in msg_iov */
	caddr_t	msg_accrights;		/* access rights sent/received */
	int	msg_accrightslen;
};

#define	BSD43_MSG_OOB		0x1		/* process out-of-band data */
#define	BSD43_MSG_PEEK	0x2		/* peek at incoming message */
#define	BSD43_MSG_DONTROUTE	0x4		/* send without using routing tables */

#define	BSD43_MSG_MAXIOVLEN	16

/*----- NUMIPS: STRIP MACRO PREFIXES (BEGIN) ---------------------------------*/
/* 
 * Strip off "BSD43_" and "bsd43_" for use with standard BSD code.
 * (GENERATED AUTOMATICALLY -- DON'T EDIT BY HAND)
 */
#ifdef SYSTYPE_BSD43
#   define AF_APPLETALK BSD43_AF_APPLETALK
#   define AF_CCITT BSD43_AF_CCITT
#   define AF_CHAOS BSD43_AF_CHAOS
#   define AF_DATAKIT BSD43_AF_DATAKIT
#   define AF_DECnet BSD43_AF_DECnet
#   define AF_DLI BSD43_AF_DLI
#   define AF_ECMA BSD43_AF_ECMA
#   define AF_HYLINK BSD43_AF_HYLINK
#   define AF_IMPLINK BSD43_AF_IMPLINK
#   define AF_INET BSD43_AF_INET
#   define AF_LAT BSD43_AF_LAT
#   define AF_MAX BSD43_AF_MAX
#   define AF_NBS BSD43_AF_NBS
#   define AF_NIT BSD43_AF_NIT
#   define AF_NS BSD43_AF_NS
#   define AF_PUP BSD43_AF_PUP
#   define AF_SNA BSD43_AF_SNA
#   define AF_UNIX BSD43_AF_UNIX
#   define AF_UNSPEC BSD43_AF_UNSPEC
#   define MSG_DONTROUTE BSD43_MSG_DONTROUTE
#   define MSG_MAXIOVLEN BSD43_MSG_MAXIOVLEN
#   define MSG_OOB BSD43_MSG_OOB
#   define MSG_PEEK BSD43_MSG_PEEK
#   define PF_APPLETALK BSD43_PF_APPLETALK
#   define PF_CCITT BSD43_PF_CCITT
#   define PF_CHAOS BSD43_PF_CHAOS
#   define PF_DATAKIT BSD43_PF_DATAKIT
#   define PF_DECnet BSD43_PF_DECnet
#   define PF_DLI BSD43_PF_DLI
#   define PF_ECMA BSD43_PF_ECMA
#   define PF_HYLINK BSD43_PF_HYLINK
#   define PF_IMPLINK BSD43_PF_IMPLINK
#   define PF_INET BSD43_PF_INET
#   define PF_LAT BSD43_PF_LAT
#   define PF_MAX BSD43_PF_MAX
#   define PF_NBS BSD43_PF_NBS
#   define PF_NIT BSD43_PF_NIT
#   define PF_NS BSD43_PF_NS
#   define PF_PUP BSD43_PF_PUP
#   define PF_SNA BSD43_PF_SNA
#   define PF_UNIX BSD43_PF_UNIX
#   define PF_UNSPEC BSD43_PF_UNSPEC
#   define SOCK_DGRAM BSD43_SOCK_DGRAM
#   define SOCK_RAW BSD43_SOCK_RAW
#   define SOCK_RDM BSD43_SOCK_RDM
#   define SOCK_SEQPACKET BSD43_SOCK_SEQPACKET
#   define SOCK_STREAM BSD43_SOCK_STREAM
#   define SOL_SOCKET BSD43_SOL_SOCKET
#   define SOMAXCONN BSD43_SOMAXCONN
#   define SO_ACCEPTCONN BSD43_SO_ACCEPTCONN
#   define SO_BROADCAST BSD43_SO_BROADCAST
#   define SO_DEBUG BSD43_SO_DEBUG
#   define SO_DONTROUTE BSD43_SO_DONTROUTE
#   define SO_ERROR BSD43_SO_ERROR
#   define SO_KEEPALIVE BSD43_SO_KEEPALIVE
#   define SO_LINGER BSD43_SO_LINGER
#   define SO_OOBINLINE BSD43_SO_OOBINLINE
#   define SO_RCVBUF BSD43_SO_RCVBUF
#   define SO_RCVLOWAT BSD43_SO_RCVLOWAT
#   define SO_RCVTIMEO BSD43_SO_RCVTIMEO
#   define SO_REUSEADDR BSD43_SO_REUSEADDR
#   define SO_SNDBUF BSD43_SO_SNDBUF
#   define SO_SNDLOWAT BSD43_SO_SNDLOWAT
#   define SO_SNDTIMEO BSD43_SO_SNDTIMEO
#   define SO_TYPE BSD43_SO_TYPE
#   define SO_USELOOPBACK BSD43_SO_USELOOPBACK
#endif SYSTYPE_BSD43
/*----- NUMIPS: STRIP MACRO PREFIXES (END) -----------------------------------*/


