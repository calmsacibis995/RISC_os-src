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
/* $Header: slip.h,v 1.2.2.2 90/05/10 06:37:35 wje Exp $ */


/*
 * Header file for streams SLIP module
 *
 * Derived from work done by Rayan Sachariassen and Doug Kingston.
 */

#ifndef	_SLIP
#define	_SLIP

#define	SLIPIFNAME	"slip"

#define	SLIOGUNIT	_IOR(t, 62, int)		/* get SLIP unit no. */
#define	SLIOGSTATS	_IOR(t, 63, struct slipstat)	/* get statistics */
#define	SLIOSFLAGS	_IOW(t, 64, int)		/* set flags */

struct slipstat {
	uint		sl_ibytes;	/* total number of data bytes in */
	uint		sl_ipackets;	/* total number of data packets in */
	uint		sl_ierrors;	/* total number of input errors */
	uint		sl_obytes;	/* total number of data bytes out */
	uint		sl_opackets;	/* total number of data packets out */
	uint		sl_oerrors;	/* total number of output errors */
};

/* end of what the user-level process should care about */

/* See RFC1055 for the origin of the following magic numbers */

#define SLIPMTU		1006	/* this is the IP-level MTU */

#define	END	0300		/* a frame just finished */
#define	ESC	0333		/* introduces an escape sequence */
#define	ESC_END	0334		/* the data contained an END */
#define	ESC_ESC	0335		/* the data contained an ESC */


#ifdef INKERNEL

#undef	SLIP_FASTECHO	/* give RLOGIN/TELNET packets priority */
#undef	SLIP_UCACHE	/* micro-optimization for CPUs with small cache */
#define	SLIP_STATS	/* maintain statistics that can be read by user */
#undef  SLIP_CLUSTERS	/* use 4.3 style mbuf clusters */
			/* (SLIP_CLUSTERS not supported yet */
#define	SLIP_RTMAINT	/* delete routes through interface when it goes down */
#define	INET

struct slipdata {
	struct ifnet	ifnet;		/* interface definition */
	queue_t		*q;		/* local message queue */
	u_char		*buf;		/* mbuf cluster base */
	u_char		*dp;		/* where incoming bytes are stuffed */
	uint		inlen;		/* how many bytes accumulated */
	short		sawescape;	/* flag: saw ESC in input */
	short		overrun;	/* flag: inlen exceeded SLIPMTU */
	short		userflags;	/* user-settable flag bits */
	short		sawhangup;	/* set when hangup seen */
#ifdef	SLIP_FASTECHO
	mblk_t		*lastpriopkt;	/* last priority interactive message */
#endif	/* SLIP_FASTECHO */
#ifdef	SLIP_STATS
	struct slipstat	stats;		/* statistics */
#endif	/* SLIP_STATS */
};

#endif /* INKERNEL */

#endif	/* !_SLIP */
