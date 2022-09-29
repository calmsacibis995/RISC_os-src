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
/* $Header: bfs.h,v 1.7.1.2 90/05/10 04:45:49 wje Exp $ */
#ifndef BSD43_
#    include <bsd43/bsd43_.h>
#endif BSD43_


/*
 * Definitions for boot file server
 */

#define	BSD43_BFSREV		1		/* protocol revision */

/*
 * NOTE: BFS_MAXPATH+ BFS_MAXDATA + sizeof(struct bfshdr) must
 * be less than ETHERMTU (1500 bytes)
 */
#define	BSD43_BFS_MAXPATH	255		/* max bfs pathname length */
#define	BSD43_BFS_MAXDATA	1024		/* max data in a bfs packet */
#define	BSD43_BFS_MAXSERVER	15		/* max length of server name */
#define	BSD43_BFS_MAXTRIES	4		/* max attempts to read a block */
#ifdef DEBUG
/* need some time to play around when debugging protocol */
#define	BSD43_BFS_REXMIT	400		/* timeout period */
#else
#define	BSD43_BFS_REXMIT	3		/* timeout period */
#endif

struct bsd43_(bfshdr) {
	char	bh_rev;				/* protocol revision */
	char	bh_type;			/* packet type */
	u_short bh_pathlen;			/* pathname length */
	short	bh_datalen;			/* data length */
	short	bh_pad;				/* pad to int boundry */
	u_int	bh_offset;			/* file offset */
	int	bh_flags;			/* open flags to use */
	char	bh_server[BSD43_BFS_MAXSERVER+1];	/* responding server */
};

/*
 * bfs packet types
 * (writes currently aren't fully implemented)
 */
#define	BSD43_BFSPT_ENQUIRE	1		/* enquire for file availability */
#define	BSD43_BFSPT_ENQRPY	2		/* reply to enquire */
#define	BSD43_BFSPT_READ	3		/* request for data read */
#define	BSD43_BFSPT_RDRPY	4		/* reply to data read request */
#define	BSD43_BFSPT_WRITE	5		/* request to write data */
#define	BSD43_BFSPT_WTRPY	6		/* reply to write data request */
#define	BSD43_BFSPT_ERROR	7		/* error message reply */
#define	BSD43_BFSPT_ENQFWD	8		/* gateway'ed ENQUIRE request */

/*
 * bfs udp ports
 * (ports less than IPPORT_RESERVED require bfsd server to run setuid root)
 */
#define	BSD43_BFSPORT_PROM	2200
#define	BSD43_BFSPORT_SERVER	2201

/*----- NUMIPS: STRIP MACRO PREFIXES (BEGIN) ---------------------------------*/
/* 
 * Strip off "BSD43_" and "bsd43_" for use with standard BSD code.
 * (GENERATED AUTOMATICALLY -- DON'T EDIT BY HAND)
 */
#ifdef SYSTYPE_BSD43
#   define BFSPORT_PROM BSD43_BFSPORT_PROM
#   define BFSPORT_SERVER BSD43_BFSPORT_SERVER
#   define BFSPT_ENQFWD BSD43_BFSPT_ENQFWD
#   define BFSPT_ENQRPY BSD43_BFSPT_ENQRPY
#   define BFSPT_ENQUIRE BSD43_BFSPT_ENQUIRE
#   define BFSPT_ERROR BSD43_BFSPT_ERROR
#   define BFSPT_RDRPY BSD43_BFSPT_RDRPY
#   define BFSPT_READ BSD43_BFSPT_READ
#   define BFSPT_WRITE BSD43_BFSPT_WRITE
#   define BFSPT_WTRPY BSD43_BFSPT_WTRPY
#   define BFSREV BSD43_BFSREV
#   define BFS_MAXDATA BSD43_BFS_MAXDATA
#   define BFS_MAXPATH BSD43_BFS_MAXPATH
#   define BFS_MAXSERVER BSD43_BFS_MAXSERVER
#   define BFS_MAXTRIES BSD43_BFS_MAXTRIES
#   define BFS_REXMIT BSD43_BFS_REXMIT
#endif SYSTYPE_BSD43
/*----- NUMIPS: STRIP MACRO PREFIXES (END) -----------------------------------*/


