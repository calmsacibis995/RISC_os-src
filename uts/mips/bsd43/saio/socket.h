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
/* $Header: socket.h,v 1.7.1.2 90/05/10 04:47:37 wje Exp $ */
#ifndef BSD43_
#    include <bsd43/bsd43_.h>
#endif BSD43_


/*
 * socket.h -- definitions for prom sockets
 */

#ifdef PROM
#define	BSD43_NSO_TABLE	2		/* only 2 ethernet connections */
#else
#define	BSD43_NSO_TABLE	5
#endif

struct bsd43_(so_table) {
	int st_count;			/* reference count */
	u_short st_udpport;		/* port socket is bound to */
	struct bsd43_(mbuf) *st_mbuf;		/* packets recv on this port */
};

extern struct bsd43_(so_table) *bsd43_(_get_socket)();
extern struct bsd43_(so_table) *bsd43_(_find_socket)();
extern struct bsd43_(so_table) bsd43_(_so_table)[];

/*
 * Structure used by kernel to store most
 * addresses.
 */
struct bsd43_(sockaddr) {
	u_short	sa_family;		/* address family */
	char	sa_data[14];		/* up to 14 bytes of direct address */
};

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

#define	BSD43_AF_MAX		12

/*----- NUMIPS: STRIP MACRO PREFIXES (BEGIN) ---------------------------------*/
/* 
 * Strip off "BSD43_" and "bsd43_" for use with standard BSD code.
 * (GENERATED AUTOMATICALLY -- DON'T EDIT BY HAND)
 */
#ifdef SYSTYPE_BSD43
#   define AF_CCITT BSD43_AF_CCITT
#   define AF_CHAOS BSD43_AF_CHAOS
#   define AF_DATAKIT BSD43_AF_DATAKIT
#   define AF_ECMA BSD43_AF_ECMA
#   define AF_IMPLINK BSD43_AF_IMPLINK
#   define AF_INET BSD43_AF_INET
#   define AF_MAX BSD43_AF_MAX
#   define AF_NBS BSD43_AF_NBS
#   define AF_NS BSD43_AF_NS
#   define AF_PUP BSD43_AF_PUP
#   define AF_SNA BSD43_AF_SNA
#   define AF_UNIX BSD43_AF_UNIX
#   define AF_UNSPEC BSD43_AF_UNSPEC
#   define NSO_TABLE BSD43_NSO_TABLE
#endif SYSTYPE_BSD43
/*----- NUMIPS: STRIP MACRO PREFIXES (END) -----------------------------------*/


