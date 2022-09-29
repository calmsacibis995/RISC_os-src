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
/* $Header: netman.h,v 1.4.1.2 90/05/10 06:30:07 wje Exp $ */

/*
 * Data structures for MIPS Network Management
 */

#ifndef MIB
#define MIB

typedef int		Integer;
typedef int		Object_Identifier;
typedef int		Gauge;
typedef int		Counter;
typedef char *		Octet_String;
typedef unsigned long	Time_Ticks;
typedef unsigned long	NetworkAddr;
typedef unsigned long	IpAddress;

/* Declare the actual space at the end */

/* System Group */
typedef struct sysinfo {
	char			sys_id[SYS_NMLN * 13];
	int			sys_objidlen;
	char			sys_objid[32];
	Time_Ticks		sys_lastinit;
	} Sysinfo;

/* Interface Group */

typedef struct ifentry {
	Integer		if_index;
	int		if_descrlen;
	char		if_descr[128];
	Integer		if_type;	/* other, enet, fddi, etc */
	Integer		if_mtu;
	Gauge		if_speed;	/* bandwidth in bits/sec */
	int		if_physaddrlen;
	char		if_physaddr[32];
	Integer		if_adminstat;	/* desired up, down, testing */
	Integer		if_operstat;	/* actual up, down, testing */
	Time_Ticks	if_uptime;	/* Last_init @ enter of cur state */
	Counter		if_rbyte;	/* receive stats */
	Counter		if_rdirpkt;	/* unicasts */
	Counter		if_rcastpkt;	/* broadcasts and multicasts */
	Counter		if_rdiscard;
	Counter		if_rerr;
	Counter		if_rbadproto;	/* unrecognized protocol */
	Counter		if_xbyte;	/* transmit stats */
	Counter		if_xdirpkt;	/* unicasts */
	Counter		if_xcastpkt;	/* broadcasts and multicasts */
	Counter		if_xdiscard;
	Counter		if_xerr;
	Gauge		if_xqlen;	/* current len out output queue */
	} IfEntry;

/* Address Translation Group */
typedef struct atentry {
	Integer		at_index;
	int		at_physaddr_size;
	char		at_physaddr[32];	/* media dependent address */
	NetworkAddr	at_netaddr;	/* ip addr */
	} AtEntry;

/* IP group */

typedef struct ipaddrtab {
	IpAddress	ipa_addr;
	Integer		ipa_ifindex;	/* back index to the if */
	IpAddress	ipa_mask;
	IpAddress	ipa_bcast;
	} IpAddrTab;

typedef struct iproutab {
	IpAddress	ipr_dest;
	Integer		ipr_ifindex;	/* back index to the if */
	Integer		ipr_metric1;
	Integer		ipr_metric2;
	Integer		ipr_metric3;
	Integer		ipr_metric4;
	IpAddress	ipr_nexthop;
	Integer		ipr_type;	/* other, invalid, direct, remote */
	Integer		ipr_proto;	/* routing proto that gave us route */
	Integer		ipr_age;	/* seconds since verified */
	} IpRouTab;

typedef struct ipinfo {
	Integer		ip_forwarding;	/* gateway or host */
	Integer		ip_defttl;
	Counter		ip_rpkts;
	Counter		ip_rhdrerr;
	Counter		ip_raddrerr;
	Counter		ip_forwarded;
	Counter		ip_badproto;
	Counter		ip_rdiscard;
	Counter		ip_delivers;
	Counter		ip_xpkts;
	Counter		ip_xdiscard;
	Counter		ip_noroute;
	Integer		ip_reasm_timeout;
	Counter		ip_reasm_req;
	Counter		ip_reasm_ok;
	Counter		ip_reasm_fail;
	Counter		ip_frag_ok;	/* # of fragmented datagrams */
	Counter		ip_frag_fail;
	Counter		ip_frag;	/* # of resulting fragments */
	} Ipinfo;

/* ICMP Group */
typedef struct icmpinfo {
	Counter		icmp_rpkt;
	Counter		icmp_rerr;
	Counter		icmp_rdest_unreach;
	Counter		icmp_rtime_exceed;
	Counter		icmp_rparm_prob;
	Counter		icmp_rsrc_quench;
	Counter		icmp_rredirect;
	Counter		icmp_recho;
	Counter		icmp_recho_rep;
	Counter		icmp_rtimes;	/* time stamp receives */
	Counter		icmp_rtimes_rep;
	Counter		icmp_raddrmask;
	Counter		icmp_raddrmask_rep;
	Counter		icmp_xpkt;
	Counter		icmp_xerr;
	Counter		icmp_xdest_unreach;
	Counter		icmp_xtime_exceed;
	Counter		icmp_xparm_prob;
	Counter		icmp_xsrc_quench;
	Counter		icmp_xredirect;
	Counter		icmp_xecho;
	Counter		icmp_xecho_rep;
	Counter		icmp_xtimes;	/* time stamp sends */
	Counter		icmp_xtimes_rep;
	Counter		icmp_xaddrmask;
	Counter		icmp_xaddrmask_rep;
	} Icmpinfo;

/* Tcp Group */
typedef struct tcpcontab {
	Integer		tcpc_state;
	IpAddress	tcpc_laddr;
	Integer		tcpc_lport;
	IpAddress	tcpc_raddr;
	Integer		tcpc_rport;
	} TcpConTab;

typedef struct tcpinfo {
	Integer		tcp_rtalg;	/* Round Trip Timeout Algorithm */
	Integer		tcp_rtmin;	/* min allowed rt timeout in ms */
	Integer		tcp_rtmax;	/* max allowed rt timeout in ms */
	Integer		tcp_maxcon;	/* max simultaneous connections */
	Counter		tcp_actopen;	/* active open occurances */
	Counter		tcp_pasopen;	/* passive open occurances */
	Counter		tcp_fails;
	Counter		tcp_est_reset;	/* (EST || CLOSE_WAIT) -> CLOSED */
	Gauge		tcp_estab;	/* current cons in EST or CLOSE_WAIT */
	Counter		tcp_rpkt;
	Counter		tcp_xpkt;
	Counter		tcp_retrans;	/* retransmitted segments */
	} Tcpinfo;

/* UDP Group */
typedef struct udpinfo {
	Counter		udp_rpkt;
	Counter		udp_noport;
	Counter		udp_rerr;
	Counter		udp_xpkt;
	} Udpinfo;

#define ISO_ORG_DOD_INTERNET_MGMT_MIB_SYSTEM				1
#define ISO_ORG_DOD_INTERNET_MGMT_MIB_INTERFACES_IFNUMBER 		2
#define ISO_ORG_DOD_INTERNET_MGMT_MIB_INTERFACES_IFTABLE_IFENTRY 	3
#define ISO_ORG_DOD_INTERNET_MGMT_MIB_AT_ATTABLE_ATENTRY		4
#define ISO_ORG_DOD_INTERNET_MGMT_MIB_IP				5
#define ISO_ORG_DOD_INTERNET_MGMT_MIB_IP_IPADDRTABLE_IPADDRENTRY	6
#define ISO_ORG_DOD_INTERNET_MGMT_MIB_IP_IPROUTINGTABLE_IPROUTEENTRY	7
#define ISO_ORG_DOD_INTERNET_MGMT_MIB_ICMP				8
#define ISO_ORG_DOD_INTERNET_MGMT_MIB_TCP				9
#define ISO_ORG_DOD_INTERNET_MGMT_MIB_TCP_TCPCONNTABLE_TCPCONNENTRY	10
#define ISO_ORG_DOD_INTERNET_MGMT_MIB_UDP				11
#define ISO_ORG_DOD_INTERNET_MGMT_MIB_EGP				12
#define ISO_ORG_DOD_INTERNET_MGMT_MIB_EGP_EGPNEIGHTABLE_EGPNEIGHENTRY	13

#endif MIB
