#ident "$Header: arp.h,v 1.6 90/03/27 18:11:37 zach Exp $"
/*	%Q%	%I%	%M%	*/
/* $Copyright$ */

/*
 * Copyright 1985 by MIPS Computer Systems, Inc.
 */

/*
 * arp.h -- arp and ethernet definitions
 */

/*
 * Structure of a 10Mb/s Ethernet header.
 */
struct	ether_header {
	u_char	ether_dhost[6];
	u_char	ether_shost[6];
	u_short	ether_type;
};

#define	ETHERPUP_PUPTYPE	0x0200		/* PUP protocol */
#define	ETHERPUP_IPTYPE		0x0800		/* IP protocol */
#define ETHERPUP_ARPTYPE	0x0806		/* Addr. resolution protocol */
#define ETHERPUP_RARPTYPE	0x8035		/* Reverse addr. res. proto */

/*
 * The ETHERPUP_NTRAILER packet types starting at ETHERPUP_TRAIL have
 * (type-ETHERPUP_TRAIL)*512 bytes of data followed
 * by a PUP type (as given above) and then the (variable-length) header.
 */
#define	ETHERPUP_TRAIL		0x1000		/* Trailer PUP */
#define	ETHERPUP_NTRAILER	16

#define	ETHERMTU	1500
#define	ETHERMIN	(60-14)

/*
 * Structure defining a network interface.
 */
struct ifnet {
	char	*if_name;		/* name, e.g. ``en'' or ``lo'' */
        short	if_unit;		/* sub-unit for lower level driver */
	short	if_mtu;			/* maximum transmission unit */
	int	if_flags;		/* up/down, broadcast, etc. */
#ifdef notdef
	int	if_net;			/* network number of interface */
	int	if_host[2];		/* local net host number */
#endif
	struct	sockaddr if_addr;	/* address of interface */
	union {
		struct	sockaddr ifu_broadaddr;
		struct	sockaddr ifu_dstaddr;
	} if_ifu;
	struct	ifqueue {
		struct 	mbuf *ifq_head;
		struct	mbuf *ifq_tail;
		int	ifq_len;
		int	ifq_maxlen;
		int	ifq_drops;
	} if_snd;			/* outout queue */
	int	(*if_init)();		/* init routine */
	int	(*if_output)();		/* output routine */
	int	(*if_ioctl)();		/* ioctl routine */
	int	(*if_reset)();		/* bus reset routine */
	int	(*if_watchdog)();	/* timer routine */
/* generic interface statictics */
	int	if_ipackets;		/* packets received on interface */
	int	if_ierrors;		/* input errors on interface */
	int	if_opackets;		/* packets sent on interface */
	int	if_oerrors;		/* output errors on interface */
	int	if_collisions;		/* collisions on interface */
};

/* if_flags defines */
#define	IFF_UP		0x1		/* interface is up */
#define	IFF_BROADCAST	0x2		/* broadcast address valid */
#define	IFF_DEBUG	0x4		/* turn on debugging */
#define	IFF_LOOPBACK	0x8		/* is a loopback net */
#define	IFF_POINTOPOINT	0x10		/* interface is point-to-point link */
#define	IFF_NOTRAILERS	0x20		/* avoid use of trailers */
#define	IFF_RUNNING	0x40		/* resources allocated */
#define	IFF_NOARP	0x80		/* no address resolution protocol */
/* next two not supported now, but reserved: */
#define	IFF_PROMISC	0x100		/* receive all packets */
#define	IFF_ALLMULTI	0x200		/* receive all multicast packets */
/* flags set internally only: */
#define	IFF_CANTCHANGE	(IFF_BROADCAST | IFF_POINTOPOINT | IFF_RUNNING)

/*
 * Output queues (ifp->if_snd) and internetwork datagram level (pup level 1)
 * input routines have queues of messages stored on ifqueue structures
 * (defined above).  Entries are added to and deleted from these structures
 * by these macros, which should be called with ipl raised to splimp().
 */
#define	IF_QFULL(ifq)		((ifq)->ifq_len >= (ifq)->ifq_maxlen)
#define	IF_DROP(ifq)		((ifq)->ifq_drops++)
#define	IF_ENQUEUE(ifq, m) { \
	(m)->m_act = 0; \
	if ((ifq)->ifq_tail == 0) \
		(ifq)->ifq_head = m; \
	else \
		(ifq)->ifq_tail->m_act = m; \
	(ifq)->ifq_tail = m; \
	(ifq)->ifq_len++; \
}
#define	IF_PREPEND(ifq, m) { \
	(m)->m_act = (ifq)->ifq_head; \
	if ((ifq)->ifq_tail == 0) \
		(ifq)->ifq_tail = (m); \
	(ifq)->ifq_head = (m); \
	(ifq)->ifq_len++; \
}
/*
 * Packets destined for level-1 protocol input routines
 * have a pointer to the receiving interface prepended to the data.
 * IF_DEQUEUEIF extracts and returns this pointer when dequeueing the packet.
 * IF_ADJ should be used otherwise to adjust for its presence.
 */
#define	IF_ADJ(m) { \
	(m)->m_off += sizeof(struct ifnet *); \
	(m)->m_len -= sizeof(struct ifnet *); \
	if ((m)->m_len == 0) { \
		struct mbuf *n; \
		MFREE((m), n); \
		(m) = n; \
	} \
}
#define	IF_DEQUEUEIF(ifq, m, ifp) { \
	(m) = (ifq)->ifq_head; \
	if (m) { \
		if (((ifq)->ifq_head = (m)->m_act) == 0) \
			(ifq)->ifq_tail = 0; \
		(m)->m_act = 0; \
		(ifq)->ifq_len--; \
		bcopy(mtod((m), struct ifnet **), &(ifp), sizeof((ifp))); \
		IF_ADJ(m); \
	} \
}
#define	IF_DEQUEUE(ifq, m) { \
	(m) = (ifq)->ifq_head; \
	if (m) { \
		if (((ifq)->ifq_head = (m)->m_act) == 0) \
			(ifq)->ifq_tail = 0; \
		(m)->m_act = 0; \
		(ifq)->ifq_len--; \
	} \
}

#define	IFQ_MAXLEN	50
#define	IFNET_SLOWHZ	1		/* granularity is 1 second */

/*
 * short hand
 */
#define	if_broadaddr	if_ifu.ifu_broadaddr	/* broadcast address */
#define	if_dstaddr	if_ifu.ifu_dstaddr	/* other end of p-to-p link */

/*
 * Ethernet Address Resolution Protocol.
 *
 * See RFC 826 for protocol description.  Structure below is adapted
 * to resolving internet addresses.  Field names used correspond to 
 * RFC 826.
 */
struct	ether_arp {
	u_short	arp_hrd;	/* format of hardware address */
#define ARPHRD_ETHER 	1	/* ethernet hardware address */
	u_short	arp_pro;	/* format of proto. address (ETHERPUP_IPTYPE) */
	u_char	arp_hln;	/* length of hardware address (6) */
	u_char	arp_pln;	/* length of protocol address (4) */
	u_short	arp_op;
#define	ARPOP_REQUEST	1	/* request to resolve address */
#define	ARPOP_REPLY	2	/* response to previous request */
#define	RARPOP_REQUEST	3	/* request for reverse ARP*/
#define	RARPOP_REPLY	4	/* response to reverse ARP request */
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
struct	arpcom {
	struct 	ifnet ac_if;	/* network-visible interface */
	u_char	ac_enaddr[6];	/* ethernet hardware address */
};

#ifdef KERNEL
struct	in_addr arpmyaddr();
struct	arptab *arptnew();
#endif

#define	ARP_TRIES	3	/* number of attempts to resolve address */
#define	ARP_TIME	2	/* seconds before re-attempting arp request */

/* if_flags defines */
#define	IFF_UP		0x1		/* interface is up */
#define	IFF_BROADCAST	0x2		/* broadcast address valid */
#define	IFF_DEBUG	0x4		/* turn on debugging */
#define	IFF_LOOPBACK	0x8		/* is a loopback net */
#define	IFF_POINTOPOINT	0x10		/* interface is point-to-point link */
#define	IFF_NOTRAILERS	0x20		/* avoid use of trailers */
#define	IFF_RUNNING	0x40		/* resources allocated */
#define	IFF_NOARP	0x80		/* no address resolution protocol */
/* next two not supported now, but reserved: */
#define	IFF_PROMISC	0x100		/* receive all packets */
#define	IFF_ALLMULTI	0x200		/* receive all multicast packets */
/* flags set internally only: */
#define	IFF_CANTCHANGE	(IFF_BROADCAST | IFF_POINTOPOINT | IFF_RUNNING)
