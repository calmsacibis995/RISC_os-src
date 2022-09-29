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
/* $Header: protocol.h,v 1.6.3.2 90/05/10 04:47:03 wje Exp $ */
#ifndef BSD43_
#    include <bsd43/bsd43_.h>
#endif BSD43_


/*
 * protocol.h -- definitions for serial line protocol
 */

/*
 * Max/min packet size parameters
 */
#define	BSD43_MAXPACKET	1023
#define	BSD43_MINPACKET	32
#define	BSD43_MAXPKTOVERHEAD	10	/* max bytes in pkt header/trailer */

/*
 * Parameters for line quality adaptive transmission routine (proto_pktsize)
 */
#define	BSD43_MIN_XMIT_PKTS	10	/* min # of pkts before calc new pkt size */
#define	BSD43_MAX_ACK_THRESH	95	/* % good pkts to trigger size increase */
#define	BSD43_MIN_ACK_THRESH	85	/* % good pkts to trigger size decrease */

/*
 * Ascii characters that are special to protocol
 */
#define	BSD43_SYN		0x16
#define	BSD43_DLE		0x10

/*
 * Packet protocol retransmit time
 * NOTE: if these are too short, you'll see frequent "bad seq" messages
 * do to premature retransmissions.
 */
#ifdef DEBUG
#define	BSD43_REXMIT_TIME	100000
#else
#define	BSD43_REXMIT_TIME	3
#endif

/*
 * Packet type definitions
 */
#define	BSD43_MASK_PKTTYPE	0x20

#define	BSD43_DATA_PKTTYPE	0
#define	BSD43_ACK_PKTTYPE	0x20
#define	BSD43_ANY_PKTTYPE	0x100

/*----- NUMIPS: STRIP MACRO PREFIXES (BEGIN) ---------------------------------*/
/* 
 * Strip off "BSD43_" and "bsd43_" for use with standard BSD code.
 * (GENERATED AUTOMATICALLY -- DON'T EDIT BY HAND)
 */
#ifdef SYSTYPE_BSD43
#   define ACK_PKTTYPE BSD43_ACK_PKTTYPE
#   define ANY_PKTTYPE BSD43_ANY_PKTTYPE
#   define DATA_PKTTYPE BSD43_DATA_PKTTYPE
#   define DLE BSD43_DLE
#   define MASK_PKTTYPE BSD43_MASK_PKTTYPE
#   define MAXPACKET BSD43_MAXPACKET
#   define MAXPKTOVERHEAD BSD43_MAXPKTOVERHEAD
#   define MAX_ACK_THRESH BSD43_MAX_ACK_THRESH
#   define MINPACKET BSD43_MINPACKET
#   define MIN_ACK_THRESH BSD43_MIN_ACK_THRESH
#   define MIN_XMIT_PKTS BSD43_MIN_XMIT_PKTS
#   define REXMIT_TIME BSD43_REXMIT_TIME
#   define SYN BSD43_SYN
#endif SYSTYPE_BSD43
/*----- NUMIPS: STRIP MACRO PREFIXES (END) -----------------------------------*/


