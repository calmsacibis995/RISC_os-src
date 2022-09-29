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
/* $Header: protocol.h,v 1.1.2.2 90/05/07 19:40:58 wje Exp $ */

/*
 * protocol.h -- definitions for serial line protocol
 */

/*
 * Max/min packet size parameters
 */
#define	MAXPACKET	1023
#define	MINPACKET	32
#define	MAXPKTOVERHEAD	10	/* max bytes in pkt header/trailer */

/*
 * Parameters for line quality adaptive transmission routine (proto_pktsize)
 */
#define	MIN_XMIT_PKTS	10	/* min # of pkts before calc new pkt size */
#define	MAX_ACK_THRESH	95	/* % good pkts to trigger size increase */
#define	MIN_ACK_THRESH	85	/* % good pkts to trigger size decrease */

/*
 * Ascii characters that are special to protocol
 */
#define	SYN		0x16
#define	DLE		0x10

/*
 * Packet protocol retransmit time
 */
#ifdef DEBUG
#define	REXMIT_TIME	100000
#else
#define	REXMIT_TIME	3
#endif

/*
 * Packet type definitions
 */
#define	MASK_PKTTYPE	0x20

#define	DATA_PKTTYPE	0
#define	ACK_PKTTYPE	0x20
#define	ANY_PKTTYPE	0x100
