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
/* $Header: if_ether.h,v 1.10.1.2 90/05/10 04:27:08 wje Exp $ */

#ifndef	_BSD_NETINET_IF_ETHER_
#define	_BSD_NETINET_IF_ETHER_	1


/*
 * Copyright (c) 1982, 1986 Regents of the University of California.
 * All rights reserved.  The Berkeley software License Agreement
 * specifies the terms and conditions for redistribution.
 *
 *	@(#)if_ether.h	7.1 (Berkeley) 6/5/86
 */

/*
 * Ethernet address - 6 octets
 */
struct ether_addr {
	u_char	ether_addr_octet[6];
};

/*
 * Structure of a 10Mb/s Ethernet header.
 */
struct	ether_header {
	u_char	ether_dhost[6];
	u_char	ether_shost[6];
	u_short	ether_type;
};

#define	ETHERTYPE_PUP	0x0200		/* PUP protocol */
#define	ETHERTYPE_IP	0x0800		/* IP protocol */
#define ETHERTYPE_ARP	0x0806		/* Addr. resolution protocol */
#define	ETHERTYPE_RARP	0x8035		/* Reverse ARP */
#define ETHERTYPE_REVARP 0x8035		/* Reverse ARP; NFS 4.0 Reference Port */

#define ETHERTYPE_DECRESV0 	0x6000	/* DECnet unasigned */
#define ETHERTYPE_MOPDL 	0x6001	/* DECnet MOP Dump/Load Assistance */
#define ETHERTYPE_MOPRC 	0x6002	/* DECnet MOP Remote Console */
#define ETHERTYPE_DECIV 	0x6003	/* DECnet phase IV */
#define ETHERTYPE_LAT 		0x6004	/* DECnet Local Area Transport (LAT) */
#define ETHERTYPE_DECDIAG 	0x6005	/* DECnet diagnostic protocol */
#define ETHERTYPE_DECCUSTOMER 	0x6006	/* DECnet customer protocol */
#define ETHERTYPE_LAVC 		0x6007	/* DECnet Local Area VAX Cluster (LAVC) */
#define ETHERTYPE_DECRESV1 	0x6008	/* DECnet unassigned */
#define ETHERTYPE_DECRESV2 	0x6009	/* DECnet unassigned */


/*
 * The ETHERTYPE_NTRAILER packet types starting at ETHERTYPE_TRAIL have
 * (type-ETHERTYPE_TRAIL)*512 bytes of data followed
 * by an ETHER type (as given above) and then the (variable-length) header.
 */
#define	ETHERTYPE_TRAIL		0x1000		/* Trailer packet */
#define	ETHERTYPE_NTRAILER	16

#define	ETHERMTU	1500
#define	ETHERMIN	(60-14)

/*
 * Ethernet Address Resolution Protocol.
 *
 * See RFC 826 for protocol description.  Structure below is adapted
 * to resolving internet addresses.  Field names used correspond to 
 * RFC 826.
 */
struct	ether_arp {
	struct	arphdr ea_hdr;	/* fixed-size header */
	u_char	arp_sha[6];	/* sender hardware address */
	u_char	arp_spa[4];	/* sender protocol address */
	u_char	arp_tha[6];	/* target hardware address */
	u_char	arp_tpa[4];	/* target protocol address */
};
#define	arp_hrd	ea_hdr.ar_hrd
#define	arp_pro	ea_hdr.ar_pro
#define	arp_hln	ea_hdr.ar_hln
#define	arp_pln	ea_hdr.ar_pln
#define	arp_op	ea_hdr.ar_op


/*
 * Structure shared between the ethernet driver modules and
 * the address resolution code.  For example, each ec_softc or il_softc
 * begins with this structure.
 */
struct	arpcom {
	struct 	ifnet ac_if;		/* network-visible interface */
	u_char	ac_enaddr[6];		/* ethernet hardware address */
	struct in_addr ac_ipaddr;	/* copy of ip address- XXX */
};

/*
 * Internet to ethernet address resolution table.
 */
struct	arptab {
	struct	in_addr at_iaddr;	/* internet address */
	u_char	at_enaddr[6];		/* ethernet address */
	u_char	at_timer;		/* minutes since last reference */
	u_char	at_flags;		/* flags */
	struct	mbuf *at_hold;		/* last packet until resolved/timeout */
};

#ifdef	KERNEL
u_char etherbroadcastaddr[6];
struct	arptab *arptnew();
char *ether_sprintf();
#define	ARPTAB_BSIZ	9		/* bucket size */
#define	ARPTAB_NB	19		/* number of buckets */
#define	ARPTAB_SIZE	(ARPTAB_BSIZ * ARPTAB_NB)
#endif

#endif	_BSD_NETINET_IF_ETHER_
