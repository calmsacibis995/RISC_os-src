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
/* $Header: mbuf.h,v 1.22.1.2 90/05/10 04:36:45 wje Exp $ */

#ifndef _BSD_SYS_MBUF_
#define _BSD_SYS_MBUF_ 1

/*
 * Copyright (c) 1982, 1986 Regents of the University of California.
 * All rights reserved.  The Berkeley software License Agreement
 * specifies the terms and conditions for redistribution.
 *
 *	@(#)mbuf.h	7.1 (Berkeley) 6/4/86
 */

/*
 * Constants related to memory allocator.
 */
#define	MSIZE		128			/* size of an mbuf */
#define	MMINOFF		12			/* mbuf header length */
#define	MTAIL		4
#define	MMAXOFF		(MSIZE-MTAIL)		/* offset where data ends */
#define	MLEN		(MSIZE-MMINOFF-MTAIL)	/* mbuf data length */

#ifndef CLBYTES
#ifndef SYSTYPE_BSD43
#define CLBYTES NBPC
#else SYSTYPE_BSD43
#define CLBYTES NBPG
#endif SYSTYPE_BSD43
#endif CLBYTES

#ifndef KERNEL
#define	NMBCLUSTERS	(2048*1024/CLBYTES)	/* # of virt pages for mbufs */

#define	NMBPCL		(CLBYTES/MSIZE)		/* # mbufs per cluster */
#define MCL_HI		20			/* initial number of clusters */

#ifndef CLSHIFT
#if CLBYTES == 32768
#define CLSHIFT		15
#endif 
#if CLBYTES == 16384
#define CLSHIFT		14
#endif 
#if CLBYTES == 8192
#define CLSHIFT		13
#endif 
#if CLBYTES == 4096
#define CLSHIFT		12
#endif 
#endif CLSHIFT
#endif KERNEL

#define MCLBYTES	1632

/*
 * Macros for type conversion
 */

#ifdef SYSTYPE_BSD43
#define bsd43_mfree mfree
#endif SYSTYPE_BSD43

/* address in mbuf to mbuf head */
#define	dtom(x)		((struct mbuf *)((int)x & ~(MSIZE-1)))

/* mbuf head, to typed data */
#define	mtod(x,t)	((t)((int)(x) + (x)->m_off))

#if defined(mips) && defined(LANGUAGE_C)
struct mbuf {
	struct	mbuf *m_next;		/* next buffer in chain */
	u_long	m_off;			/* offset of data */
	short	m_len;			/* amount of data in this mbuf */
	short	m_type;			/* mbuf type (0 == free) */
	union {
		u_char	mun_dat[MLEN];	/* data storage */
		struct {
			short	mun_cltype;	/* "cluster" type */
			int	(*mun_clfun)(); /* freeing function */
			int	mun_clarg;	/* arg to above */
			int	(*mun_dfun)(); 	/* dup func */
			int	mun_darg;	/* arg to above */
			int	(*mun_clswp)(); /* page swapper */
			int	mun_dma;	/* can dma from here? */
		} mun_cl;
	} m_un;
	struct	mbuf *m_act;		/* link in higher-level mbuf list */
};
#define	m_dat	m_un.mun_dat
#define	m_cltype m_un.mun_cl.mun_cltype
#define	m_clfun	m_un.mun_cl.mun_clfun
#define	m_ffun	m_un.mun_cl.mun_clfun		
#define	m_clarg	m_un.mun_cl.mun_clarg
#define	m_farg	m_un.mun_cl.mun_clarg		
#define	m_clswp	m_un.mun_cl.mun_clswp
#define	m_dfun	m_un.mun_cl.mun_dfun		
#define	m_darg	m_un.mun_cl.mun_darg		
#define m_dma m_un.mun_cl.mun_dma
#endif

/* mbuf types */
#define	MT_FREE		0	/* should be on free list */
#define	MT_DATA		1	/* dynamic (data) allocation */
#define	MT_HEADER	2	/* packet header */
#define	MT_SOCKET	3	/* socket structure */
#define	MT_PCB		4	/* protocol control block */
#define	MT_RTABLE	5	/* routing tables */
#define	MT_HTABLE	6	/* IMP host tables */
#define	MT_ATABLE	7	/* address resolution tables */
#define	MT_SONAME	8	/* socket name */
#define	MT_ZOMBIE	9	/* zombie proc status */
#define	MT_SOOPTS	10	/* socket options */
#define	MT_FTABLE	11	/* fragment reassembly header */
#define	MT_RIGHTS	12	/* access rights */
#define	MT_IFADDR	13	/* interface address */
#ifdef ENPDMA
#define MT_ENPDMA	14	/* part of the enp dma cluster pool */
#endif

/* click mbuf types -- in m_cltype */
#define MCL_NORM	1	/* the old kind, with 1 mapped page */
#define MCL_FUNNY	2	/* the NFS kind, arbitrary mapped area */

/* DMA'ness.  In mun_dma.  Only pages below 8 Mg. can be DMA'ed out of. */
/* This is because the memory boards will not answer to a 24 bit master */
/* above a certain point.  8 Mg. is always safe. */
#define MCL_NODMA	0
#define MCL_DODMA	1

/* flags to m_get */
#define	M_DONTWAIT	0
#define	M_WAIT		1

/* flags to m_pgalloc */
#define	MPG_MBUFS	0		/* put new mbufs on free list */
#define	MPG_CLUSTERS	1		/* put new clusters on free list */
#define	MPG_SPACE	2		/* don't free; caller wants space */

/* length to m_copy to copy all */
#define	M_COPYALL	1000000000

/*
 * m_pullup will pull up additional length if convenient;
 * should be enough to hold headers of second-level and higher protocols. 
 */
#define	MPULL_EXTRA	32

#ifdef KERNEL
#define MCLGET(m) mclget(m)

#endif KERNEL

#ifdef SYSTYPE_SYSV
#include "sys/cmn_err.h"
#endif SYSTYPE_SYSV


#define	MGET(m, i, t) \
	{ int ms = splimp(); \
	  if ((m)=bsd43_mfree) \
		{ if ((m)->m_type != MT_FREE) panic("mget"); \
		  (m)->m_type = t; \
		  mbstat.m_mtypes[MT_FREE]--; mbstat.m_mtypes[t]++; \
		  bsd43_mfree = (m)->m_next; (m)->m_next = 0; \
		  (m)->m_off = MMINOFF; } \
	  else \
		(m) = m_more(i, t); \
	  splx(ms); }
#ifndef ENPDMA
#define	MFREE(m, n) \
	{ int ms = splimp(); \
	  if ((m)->m_type == MT_FREE) panic("mfree"); \
	  mbstat.m_mtypes[(m)->m_type]--; mbstat.m_mtypes[MT_FREE]++; \
	  (m)->m_type = MT_FREE; \
	  if ((m)->m_off >= MSIZE || \
	      (m)->m_off < 0) \
		mclput(m); \
	  (n) = (m)->m_next; (m)->m_next = bsd43_mfree; \
	  (m)->m_off = 0; (m)->m_act = 0; bsd43_mfree = (m); \
	  splx(ms); \
	  if (m_want) { \
		  m_want = 0; \
		  wakeup((caddr_t)&bsd43_mfree); \
	  } \
	}

#else
#define	MFREE(m, n) \
	{ int ms = splimp(); \
	  if ((m)->m_type == MT_FREE) panic("mfree"); \
	  if ((m)->m_type == MT_ENPDMA) {\
		enp_returnmbuf((m));\
		splx(ms);\
		goto out;\
	  }\
	  mbstat.m_mtypes[(m)->m_type]--; mbstat.m_mtypes[MT_FREE]++; \
	  (m)->m_type = MT_FREE; \
	  if ((m)->m_off >= MSIZE ||
	      (m)->m_off < 0) \
		mclput(m); \
	  (n) = (m)->m_next; (m)->m_next = bsd43_mfree; \
	  (m)->m_off = 0; (m)->m_act = 0; bsd43_mfree = (m); \
	  splx(ms); \
	  if (m_want) { \
		  m_want = 0; \
		  wakeup((caddr_t)&bsd43_mfree); \
	  } \
	  out:\
	}
#endif

#if defined(mips) && defined(LANGUAGE_C)
/*
 * Mbuf statistics.
 */
struct mbstat {
	u_long	m_mbufs;	/* mbufs obtained from page pool */
	u_long	m_clusters;	/* clusters obtained from page pool */
        u_long  m_space;        /* interface pages obtained from page pool */
	u_long	m_clfree;	/* free clusters */
	u_long	m_drops;	/* times failed to find space */
        u_long  m_wait;         /* times waited for space */
        u_long  m_drain;        /* times drained protocols for space */
	u_short	m_mtypes[256];	/* type specific mbuf allocations */
};

#ifdef	KERNEL
struct	mbstat mbstat;
int	nmbclusters;
struct	mbuf *bsd43_mfree, *mclfree;
/*
 * The way we use mclrefcnt is a bit weird.  The array starts as all
 * zero.  A vaddr with no phys memory has a refcnt of zero.  A vaddr
 * which has phys memory goes to 1, and may have more refs if copied,
 * dup'ed, etc.  A free vaddr with phys memory has a refcnt of 1, not
 * zero as in BSD.
 */
extern char	mclrefcnt[];
int	m_want;
struct	mbuf *m_get(),*m_getclr(),*m_free(),*m_more(),*m_copy(),*m_pullup();
struct	mbuf *m_clget();
caddr_t	m_clalloc();
#endif KERNEL
#endif mips

#endif	_BSD_SYS_MBUF_
