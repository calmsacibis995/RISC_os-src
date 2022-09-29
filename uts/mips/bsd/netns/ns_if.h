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
/* $Header: ns_if.h,v 1.5.4.2 90/05/10 04:33:14 wje Exp $ */

#ifndef	_BSD_NETNS_NS_IF_
#define	_BSD_NETNS_NS_IF_	1


/*
 * Copyright (c) 1985, 1986 Regents of the University of California.
 * All rights reserved.  The Berkeley software License Agreement
 * specifies the terms and conditions for redistribution.
 *
 *	@(#)ns_if.h	7.1 (Berkeley) 6/5/86
 */

/*
 * Interface address, xerox version.  One of these structures
 * is allocated for each interface with an internet address.
 * The ifaddr structure contains the protocol-independent part
 * of the structure and is assumed to be first.
 */

struct ns_ifaddr {
	struct	ifaddr ia_ifa;		/* protocol-independent info */
#define	ia_addr	ia_ifa.ifa_addr
#define	ia_broadaddr	ia_ifa.ifa_broadaddr
#define	ia_dstaddr	ia_ifa.ifa_dstaddr
#define	ia_ifp		ia_ifa.ifa_ifp
	union	ns_net	ia_net;		/* network number of interface */
	int	ia_flags;
	struct	ns_ifaddr *ia_next;	/* next in list of internet addresses */
};

/*
 * Given a pointer to an ns_ifaddr (ifaddr),
 * return a pointer to the addr as a sockadd_ns.
 */

#define	IA_SNS(ia) ((struct sockaddr_ns *)(&((struct ns_ifaddr *)ia)->ia_addr))
/*
 * ia_flags
 */
#define	IFA_ROUTE	0x01		/* routing entry installed */

/* This is not the right place for this but where is? */
#define	ETHERTYPE_NS	0x0600

#ifdef	NSIP
struct nsip_req {
	struct sockaddr rq_ns;	/* must be ns format destination */
	struct sockaddr rq_ip;	/* must be ip format gateway */
	short rq_flags;
};
#endif

#ifdef	KERNEL
struct	ns_ifaddr *ns_ifaddr;
struct	ns_ifaddr *ns_iaonnetof();
struct	ifqueue	nsintrq;	/* XNS input packet queue */
#endif

#endif	_BSD_NETNS_NS_IF_
