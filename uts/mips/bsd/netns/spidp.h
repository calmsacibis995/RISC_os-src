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
/* $Header: spidp.h,v 1.5.4.2 90/05/10 04:33:33 wje Exp $ */

#ifndef	_BSD_NETNS_SPIDP_
#define	_BSD_NETNS_SPIDP_	1


/*
 * Copyright (c) 1984, 1985, 1986 Regents of the University of California.
 * All rights reserved.  The Berkeley software License Agreement
 * specifies the terms and conditions for redistribution.
 *
 *	@(#)spidp.h	7.1 (Berkeley) 6/5/86
 */

/*
 * Definitions for NS(tm) Internet Datagram Protocol
 * containing a Sequenced Packet Protocol packet.
 */
struct spidp {
	struct idp	si_i;
	struct sphdr 	si_s;
};
struct spidp_q {
	struct spidp_q	*si_next;
	struct spidp_q	*si_prev;
};
#define SI(x)	((struct spidp *)x)
#define si_sum	si_i.idp_sum
#define si_len	si_i.idp_len
#define si_tc	si_i.idp_tc
#define si_pt	si_i.idp_pt
#define si_dna	si_i.idp_dna
#define si_sna	si_i.idp_sna
#define si_sport	si_i.idp_sna.x_port
#define si_cc	si_s.sp_cc
#define si_dt	si_s.sp_dt
#define si_sid	si_s.sp_sid
#define si_did	si_s.sp_did
#define si_seq	si_s.sp_seq
#define si_ack	si_s.sp_ack
#define si_alo	si_s.sp_alo

#endif	_BSD_NETNS_SPIDP_
