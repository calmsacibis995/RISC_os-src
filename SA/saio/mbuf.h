#ident "$Header: mbuf.h,v 1.6 90/03/12 18:30:46 chungc Exp $"
/*	%Q%	%I%	%M%	*/
/* $Copyright$ */

/*
 * mbuf.h -- definitions for message buffers
 */
#define	MMINOFF		0
#define	MMAXOFF		100
#define	MLEN		(ETHERMTU+MMAXOFF)

#ifdef PROM
#define	MAXMBUFS	1		/* queue up to 1 mbufs per socket */
#else
#define	MAXMBUFS	2		/* queue up to 2 mbufs per socket */
#endif
/*
 * NMBUFS = number of sockets * max queued buffs per socket +
 * 1 mbuf to xmit with + 1 mbuf to receive with + 1 mbuf to arp with
 */

#ifndef XXX				/* XXX how many do we need? */
#define	NMBUFS		(180)
#else
#define	NMBUFS		(NSO_TABLE*MAXMBUFS+3)
#endif

struct mbuf {
	struct	mbuf *m_next;		/* to next entry in list */
	short m_len;
	struct sockaddr m_srcaddr;
	int m_off;
	char m_dat[MLEN];
	struct	mbuf *m_act;		/* link in higher-level mbuf list */
};

/*
 * These aren't used in prom version, they're just here
 * so the kernel code works unmodified.
 */
#define	M_DONTWAIT	0
#define	MT_DATA		0

/* mbuf head, to typed data */
#define	mtod(x,t)	((t)((int)((x)->m_dat) + (x)->m_off))

extern struct mbuf *_m_get();
extern struct mbuf *_so_remove();
