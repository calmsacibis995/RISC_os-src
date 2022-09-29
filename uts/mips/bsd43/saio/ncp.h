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
/* $Header: ncp.h,v 1.6.3.2 90/05/10 04:46:50 wje Exp $ */
#ifndef BSD43_
#    include <bsd43/bsd43_.h>
#endif BSD43_


/*
 * ncp.h -- definitions for network console protocol
 */

/*
 * misc NCP constants
 */
#define	BSD43_NCP_MAXDATA	1024	/* max data length for ncp packet */
#define	BSD43_NCP_UNITS	2	/* max number of network consoles */
#define	BSD43_NCPPORT_CONS	2000	/* udp port base for network consoles */
#define	BSD43_NCP_REV		1	/* initial protocol revision */
#define	BSD43_NCP_MAXCREDITS	3	/* # of xmit packets w/o further credits */

/*
 * format of a network console protocol header
 */
struct bsd43_(ncphdr) {
	char nh_rev;		/* protocol rev */
	char nh_type;		/* packet type */
	short nh_credits;	/* xmit credits */
	short nh_datalen;	/* data length */
};

/*
 * ncp_packet -- network console protocol packet format
 */
struct bsd43_(ncp_packet) {
	struct bsd43_(ncphdr) np_ncp;
	char np_data[BSD43_NCP_MAXDATA];
};

/*
 * ncp packet types
 */
#define	BSD43_NCPTYPE_OPEN	1	/* ask if we can converse */
#define	BSD43_NCPTYPE_CLOSE	2	/* end of conversation */
#define	BSD43_NCPTYPE_DATA	3	/* body of conversation */
#define	BSD43_NCPTYPE_RESET	4	/* rudely interrupt */
#define	BSD43_NCPTYPE_ERROR	5	/* note a faux pas, error msg in data */
#define	BSD43_NCPTYPE_START	6	/* start xmit if stopped */
#define	BSD43_NCPTYPE_STOP	7	/* stop xmit */

/*----- NUMIPS: STRIP MACRO PREFIXES (BEGIN) ---------------------------------*/
/* 
 * Strip off "BSD43_" and "bsd43_" for use with standard BSD code.
 * (GENERATED AUTOMATICALLY -- DON'T EDIT BY HAND)
 */
#ifdef SYSTYPE_BSD43
#   define NCPPORT_CONS BSD43_NCPPORT_CONS
#   define NCPTYPE_CLOSE BSD43_NCPTYPE_CLOSE
#   define NCPTYPE_DATA BSD43_NCPTYPE_DATA
#   define NCPTYPE_ERROR BSD43_NCPTYPE_ERROR
#   define NCPTYPE_OPEN BSD43_NCPTYPE_OPEN
#   define NCPTYPE_RESET BSD43_NCPTYPE_RESET
#   define NCPTYPE_START BSD43_NCPTYPE_START
#   define NCPTYPE_STOP BSD43_NCPTYPE_STOP
#   define NCP_MAXCREDITS BSD43_NCP_MAXCREDITS
#   define NCP_MAXDATA BSD43_NCP_MAXDATA
#   define NCP_REV BSD43_NCP_REV
#   define NCP_UNITS BSD43_NCP_UNITS
#endif SYSTYPE_BSD43
/*----- NUMIPS: STRIP MACRO PREFIXES (END) -----------------------------------*/


