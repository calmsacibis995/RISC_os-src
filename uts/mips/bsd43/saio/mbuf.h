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
/* $Header: mbuf.h,v 1.6.3.2 90/05/10 04:46:44 wje Exp $ */
#ifndef BSD43_
#    include <bsd43/bsd43_.h>
#endif BSD43_


/*
 * mbuf.h -- definitions for message buffers
 */
#define	BSD43_MMINOFF		0
#define	BSD43_MMAXOFF		100
#define	BSD43_MLEN		(BSD43_ETHERMTU+BSD43_MMAXOFF)

#ifdef PROM
#define	BSD43_MAXMBUFS	1		/* queue up to 1 mbufs per socket */
#else
#define	BSD43_MAXMBUFS	2		/* queue up to 2 mbufs per socket */
#endif
/*
 * NMBUFS = number of sockets * max queued buffs per socket +
 * 1 mbuf to xmit with + 1 mbuf to receive with + 1 mbuf to arp with
 */
#define	BSD43_NMBUFS		(BSD43_NSO_TABLE*BSD43_MAXMBUFS+3)

struct bsd43_(mbuf) {
	short m_len;
	struct bsd43_(sockaddr) m_srcaddr;
	int m_off;
	char bsd43_m_dat[BSD43_MLEN];
	struct	bsd43_(mbuf) *m_act;		/* link in higher-level mbuf list */
};

/*
 * These aren't used in prom version, they're just here
 * so the kernel code works unmodified.
 */
#define	BSD43_M_DONTWAIT	0
#define	BSD43_MT_DATA		0

/* mbuf head, to typed data */
#define	bsd43_mtod(x,t)	((t)((int)((x)->bsd43_m_dat) + (x)->m_off))

extern struct bsd43_(mbuf) *bsd43_(_m_get)();
extern struct bsd43_(mbuf) *bsd43_(_so_remove)();

/*----- NUMIPS: STRIP MACRO PREFIXES (BEGIN) ---------------------------------*/
/* 
 * Strip off "BSD43_" and "bsd43_" for use with standard BSD code.
 * (GENERATED AUTOMATICALLY -- DON'T EDIT BY HAND)
 */
#ifdef SYSTYPE_BSD43
#   define MAXMBUFS BSD43_MAXMBUFS
#   define MLEN BSD43_MLEN
#   define MMAXOFF BSD43_MMAXOFF
#   define MMINOFF BSD43_MMINOFF
#   define MT_DATA BSD43_MT_DATA
#   define M_DONTWAIT BSD43_M_DONTWAIT
#   define NMBUFS BSD43_NMBUFS
#   define mtod bsd43_mtod
#endif SYSTYPE_BSD43
/*----- NUMIPS: STRIP MACRO PREFIXES (END) -----------------------------------*/


