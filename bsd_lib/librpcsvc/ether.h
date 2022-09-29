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
/* $Header: ether.h,v 1.2.1.2 90/05/09 14:42:48 wje Exp $ */

/*	@(#)ether.h 1.1 86/09/25 SMI */

/*
 *	1985 by Sun Microsystems Inc.
 *
 */

#define NPROTOS 6
#define NDPROTO 0
#define ICMPPROTO 1
#define UDPPROTO 2
#define TCPPROTO 3
#define ARPPROTO 4
#define OTHERPROTO 5

#define NBUCKETS 16
#define MINPACKETLEN 60
#define MAXPACKETLEN 1514
#define BUCKETLNTH ((MAXPACKETLEN - MINPACKETLEN + NBUCKETS - 1)/NBUCKETS)

#define HASHSIZE 256

#define ETHERSTATPROC_GETDATA 1
#define ETHERSTATPROC_ON 2
#define ETHERSTATPROC_OFF 3
#define ETHERSTATPROC_GETSRCDATA 4
#define ETHERSTATPROC_GETDSTDATA 5
#define ETHERSTATPROC_SELECTSRC 6
#define ETHERSTATPROC_SELECTDST 7
#define ETHERSTATPROC_SELECTPROTO 8
#define ETHERSTATPROC_SELECTLNTH 9
#define ETHERSTATPROG 100010
#define ETHERSTATVERS 1

int xdr_etherstat();
int xdr_etherhbody();
int xdr_etherhmem();
int xdr_etherhtable();
int xdr_etheraddrs();
int xdr_addrmask();

/*
 * all ether stat's except src, dst addresses
 */
struct etherstat {
	struct timeval	e_time;
	unsigned long	e_bytes;
	unsigned long	e_packets;
	unsigned long	e_bcast;
	unsigned long	e_size[NBUCKETS];
	unsigned long	e_proto[NPROTOS];
};

/*
 * member of address hash table
 */
struct etherhmem {
	int h_addr;
	unsigned h_cnt;
	struct etherhmem *h_nxt;
};

/*
 * src, dst address info
 */
struct etheraddrs {
	struct timeval	e_time;
	unsigned long	e_bytes;
	unsigned long	e_packets;
	unsigned long	e_bcast;
	struct etherhmem **e_addrs;
};

/*
 * for size, a_addr is lowvalue, a_mask is high value
 */
struct addrmask {
	int a_addr;
	int a_mask;		/* 0 means wild card */
};

extern char *protoname[];
extern int if_fd;
