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
/* $Header: if_imphost.h,v 1.8.1.2 90/05/10 04:45:21 wje Exp $ */

/*
 * Copyright (c) 1982, 1986 Regents of the University of California.
 * All rights reserved.  The Berkeley software License Agreement
 * specifies the terms and conditions for redistribution.
 *
 *	@(#)if_imphost.h	7.1 (Berkeley) 6/4/86
 */
/*----- COPYRIGHT (END) ------------------------------------------------------*/

#ifndef _BSD43_NETIMP_IF_IMPHOST_
#define _BSD43_NETIMP_IF_IMPHOST_ 1

#ifndef BSD43_
#    include <bsd43/bsd43_.h>
#endif BSD43_


/*
 * Host structure used with IMP's.
 * Used to hold outgoing packets which
 * would exceed allowed RFNM count.
 *
 * These structures are packed into
 * mbuf's and kept as small as possible.
 */
struct bsd43_(host) {
	struct	mbuf *h_q;		/* holding queue */
	struct	in_addr h_addr;		/* host's address */
	u_char	h_qcnt;          	/* size of holding q */
	u_char	h_timer;		/* used to stay off deletion */
	u_char	h_rfnm;			/* # outstanding rfnm's */
	u_char	h_flags;		/* see below */
};

/*
 * A host structure is kept around (even when there are no
 * references to it) for a spell to avoid constant reallocation
 * and also to reflect IMP status back to sites which aren't
 * directly connected to the IMP.  When structures are marked
 * free, a timer is started; when the timer expires the structure
 * is scavenged.
 */
#define	BSD43_HF_INUSE	0x1
#define	BSD43_HF_DEAD	(1<<BSD43_IMPTYPE_HOSTDEAD)
#define	BSD43_HF_UNREACH	(1<<BSD43_IMPTYPE_HOSTUNREACH)

#define	BSD43_HOSTTIMER	128		/* keep structure around awhile */

/*
 * Host structures, as seen inside an mbuf.
 * Hashing on the host address is used to
 * select an index into the first mbuf.  Collisions
 * are then resolved by searching successive
 * mbuf's at the same index.  Reclamation is done
 * automatically at the time a structure is free'd.
 */
#define	BSD43_HPMBUF	((MLEN - sizeof(int)) / sizeof(struct bsd43_(host)))
#if vax
#define	BSD43_HOSTHASH(a)	((((a).s_addr>>24)+(a).s_addr) % BSD43_HPMBUF)
#endif

/*
 * In-line expansions for queuing operations on
 * host message holding queue.  Queue is maintained
 * as circular list with the head pointing to the
 * last message in the queue.
 */
#define	BSD43_HOST_ENQUE(hp, m) { \
	register struct mbuf *n; \
	(hp)->h_qcnt++; \
	if ((n = (hp)->h_q) == 0) \
		(hp)->h_q = (m)->m_act = (m); \
	else { \
		(m)->m_act = n->m_act; \
		(hp)->h_q = n->m_act = (m); \
	} \
}
#define	BSD43_HOST_DEQUE(hp, m) { \
	if ((m) = (hp)->h_q) { \
		if ((m)->m_act == (m)) \
			(hp)->h_q = 0; \
		else { \
			(m) = (m)->m_act; \
			(hp)->h_q->m_act = (m)->m_act; \
		} \
		(hp)->h_qcnt--; \
		(m)->m_act = 0; \
	} \
}

struct bsd43_(hmbuf) {
	int	hm_count;		/* # of struct's in use */
	struct	bsd43_(host) hm_hosts[BSD43_HPMBUF];	/* data structures proper */
};

#ifdef KERNEL
struct bsd43_(host) *bsd43_(hostlookup)();
struct bsd43_(host) *bsd43_(hostenter)();
struct mbuf *bsd43_(hostdeque)();
#endif

/*----- NUMIPS: STRIP MACRO PREFIXES (BEGIN) ---------------------------------*/
/* 
 * Strip off "BSD43_" and "bsd43_" for use with standard BSD code.
 * (GENERATED AUTOMATICALLY -- DON'T EDIT BY HAND)
 */
#ifdef SYSTYPE_BSD43
#   define HF_DEAD BSD43_HF_DEAD
#   define HF_INUSE BSD43_HF_INUSE
#   define HF_UNREACH BSD43_HF_UNREACH
#   define HOSTHASH BSD43_HOSTHASH
#   define HOSTTIMER BSD43_HOSTTIMER
#   define HOST_DEQUE BSD43_HOST_DEQUE
#   define HOST_ENQUE BSD43_HOST_ENQUE
#   define HPMBUF BSD43_HPMBUF
#endif SYSTYPE_BSD43
/*----- NUMIPS: STRIP MACRO PREFIXES (END) -----------------------------------*/


#endif _BSD43_NETIMP_IF_IMPHOST_
