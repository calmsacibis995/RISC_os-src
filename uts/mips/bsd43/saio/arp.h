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
/* $Header: arp.h,v 1.7.1.2 90/05/10 04:45:42 wje Exp $ */
#ifndef BSD43_
#    include <bsd43/bsd43_.h>
#endif BSD43_


/*
 * arp.h -- arp and ethernet definitions
 */

/*
 * Structure of a 10Mb/s Ethernet header.
 */
struct	bsd43_(ether_header) {
	u_char	ether_dhost[6];
	u_char	ether_shost[6];
	u_short	ether_type;
};

#define	BSD43_ETHERPUP_PUPTYPE	0x0200		/* PUP protocol */
#define	BSD43_ETHERPUP_IPTYPE		0x0800		/* IP protocol */
#define BSD43_ETHERPUP_ARPTYPE	0x0806		/* Addr. resolution protocol */

/*
 * The ETHERPUP_NTRAILER packet types starting at ETHERPUP_TRAIL have
 * (type-ETHERPUP_TRAIL)*512 bytes of data followed
 * by a PUP type (as given above) and then the (variable-length) header.
 */
#define	BSD43_ETHERPUP_TRAIL		0x1000		/* Trailer PUP */
#define	BSD43_ETHERPUP_NTRAILER	16

#define	BSD43_ETHERMTU	1500
#define	BSD43_ETHERMIN	(60-14)

/*
 * Structure defining a network interface.
 */
struct bsd43_(ifnet) {
	short	if_unit;		/* sub-unit for lower level driver */
	short	if_mtu;			/* maximum transmission unit */
#ifdef notdef
	int	if_net;			/* network number of interface */
	int	if_host[2];		/* local net host number */
#endif
	struct	bsd43_(sockaddr) if_addr;	/* address of interface */
	union {
		struct	bsd43_(sockaddr) ifu_broadaddr;
		struct	bsd43_(sockaddr) ifu_dstaddr;
	} if_ifu;
	int	(*if_output)();		/* output routine */
};

/*
 * short hand
 */
#define	bsd43_if_broadaddr	if_ifu.ifu_broadaddr	/* broadcast address */
#define	bsd43_if_dstaddr	if_ifu.ifu_dstaddr	/* other end of p-to-p link */

/*
 * Ethernet Address Resolution Protocol.
 *
 * See RFC 826 for protocol description.  Structure below is adapted
 * to resolving internet addresses.  Field names used correspond to 
 * RFC 826.
 */
struct	bsd43_(ether_arp) {
	u_short	bsd43_arp_hrd;	/* format of hardware address */
#define BSD43_ARPHRD_ETHER 	1	/* ethernet hardware address */
	u_short	bsd43_arp_pro;	/* format of proto. address (ETHERPUP_IPTYPE) */
	u_char	bsd43_arp_hln;	/* length of hardware address (6) */
	u_char	bsd43_arp_pln;	/* length of protocol address (4) */
	u_short	bsd43_arp_op;
#define	BSD43_ARPOP_REQUEST	1	/* request to resolve address */
#define	BSD43_ARPOP_REPLY	2	/* response to previous request */
	u_char	arp_sha[6];	/* sender hardware address */
	u_char	arp_spa[4];	/* sender protocol address */
	u_char	arp_tha[6];	/* target hardware address */
	u_char	arp_tpa[4];	/* target protocol address */
};

/*
 * Structure shared between the ethernet driver modules and
 * the address resolution code.  For example, each ec_softc or il_softc
 * begins with this structure.
 */
struct	bsd43_(arpcom) {
	struct 	bsd43_(ifnet) ac_if;	/* network-visible interface */
	u_char	ac_enaddr[6];	/* ethernet hardware address */
};

#ifdef KERNEL
struct	bsd43_(in_addr) bsd43_(arpmyaddr)();
struct	bsd43_(arptab) *bsd43_(arptnew)();
#endif

#define	BSD43_ARP_TRIES	3	/* number of attempts to resolve address */
#define	BSD43_ARP_TIME	2	/* seconds before re-attempting arp request */

/*----- NUMIPS: STRIP MACRO PREFIXES (BEGIN) ---------------------------------*/
/* 
 * Strip off "BSD43_" and "bsd43_" for use with standard BSD code.
 * (GENERATED AUTOMATICALLY -- DON'T EDIT BY HAND)
 */
#ifdef SYSTYPE_BSD43
#   define ARPHRD_ETHER BSD43_ARPHRD_ETHER
#   define ARPOP_REPLY BSD43_ARPOP_REPLY
#   define ARPOP_REQUEST BSD43_ARPOP_REQUEST
#   define ARP_TIME BSD43_ARP_TIME
#   define ARP_TRIES BSD43_ARP_TRIES
#   define ETHERMIN BSD43_ETHERMIN
#   define ETHERMTU BSD43_ETHERMTU
#   define ETHERPUP_ARPTYPE BSD43_ETHERPUP_ARPTYPE
#   define ETHERPUP_IPTYPE BSD43_ETHERPUP_IPTYPE
#   define ETHERPUP_NTRAILER BSD43_ETHERPUP_NTRAILER
#   define ETHERPUP_PUPTYPE BSD43_ETHERPUP_PUPTYPE
#   define ETHERPUP_TRAIL BSD43_ETHERPUP_TRAIL
#   define if_broadaddr bsd43_if_broadaddr
#   define if_dstaddr bsd43_if_dstaddr
#endif SYSTYPE_BSD43
/*----- NUMIPS: STRIP MACRO PREFIXES (END) -----------------------------------*/


