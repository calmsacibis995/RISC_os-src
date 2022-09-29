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
 * |         950 DeGuigne Drive                                |
 * |         Sunnyvale, CA 94086                               |
 * |-----------------------------------------------------------|
 */
#ident	"$Header: kern_mbuf.c,v 1.39.1.6.1.1.1.2 90/10/05 09:50:33 beacker Exp $"

/*
 * Copyright (c) 1982, 1986, 1988 Regents of the University of California.
 * All rights reserved.
 *
 * Redistribution and use in source and binary forms are permitted
 * provided that this notice is preserved and that due credit is given
 * to the University of California at Berkeley. The name of the University
 * may not be used to endorse or promote products derived from this
 * software without specific prior written permission. This software
 * is provided ``as is'' without express or implied warranty.
 *
 *	@(#)uipc_mbuf.c	7.4.1.2 (Berkeley) 2/8/88
 */

#include "../tcp-param.h"
#include "sys/sbd.h"
#include "sys/cpu_board.h"
#include "sys/types.h"
#include "sys/param.h"
#include "sys/sysmacros.h"
#include "sys/tuneable.h"
#include "sys/systm.h"
#include "sys/map.h"
#include "sys/cmn_err.h"
#include "sys/immu.h"
#include "sys/region.h"
#include "sys/mbuf.h"
#include "sys/cmn_err.h"
#include "sys/debug.h"
#include "sys/domain.h"
#include "sys/protosw.h"

static ulong mbuf_sptmap;		/* first slot reserved for mbufs */
static caddr_t mbuf_vbase;		/* limit of system mbufs */
static caddr_t cluster_vbase;		/* limit of system mbufs */

/* CLBYTES-sized blocks of mbufs backed at boot */
extern int mbuf_small_prealloc;
/* CLBYTES-sized blocks of clusters backed at boot */
extern int mbuf_cluster_prealloc;

static int mbx_rover = 0;		/* allocation speedup */

extern	int  dcache_size;

/* total VM given to clusters, backed as needed with physical memory */
int	mbuf_cluster_count;
extern	int  mbuf_limit;
extern  int  mbuf_small_limit;
int	mbuf_small_count;

/* desired number of clusters on free list */
extern int mbuf_des_clusters;

/* number of cluster mbuf's per allocation cluster */
#define MCL_PER_CL (CLBYTES / MCLBYTES)

/* constants for system analysis programs */
int	mbuf_clbytes = CLBYTES;
int	mbuf_mclbytes = MCLBYTES;

/* convert a cluster ptr to an index into the cluster reference counters */
#define cltox(p) ((((caddr_t)(p) - cluster_vbase) / CLBYTES) * MCL_PER_CL)

/* convert reference counter index into a cluster pointer */
#define xtocl(x) (xtomcl(x))

/* convert a cluster mbuf ptr to an index into the cluster reference counters */
#if (MCL_PER_CL * MCLBYTES) == CLBYTES
#define mcltox(p) (((caddr_t)(p) - cluster_vbase) / MCLBYTES)
#else
#define mcltox(p) (cltox(p) \
		    + ((((caddr_t)(p) - cluster_vbase) % CLBYTES) / MCLBYTES))
#endif

/* convert reference counter index into a cluster buf pointer */
#if (MCL_PER_CL * MCLBYTES) == CLBYTES
#define xtomcl(x) ((struct mbuf*)(cluster_vbase + (x)*MCLBYTES))
#else
#define xtomcl(x) ((struct mbuf*)((cluster_vbase + ((x) / MCL_PER_CL)*CLBYTES) \
			+ (((x) % MCL_PER_CL)*MCLBYTES)))
#endif

/* assert that an mbuf pointer is 'good' */
#define M_GOODM(m) (mbuf_vbase<=(caddr_t)(m) && (caddr_t)(m)<mbuf_vbase+mbuf_limit)
/* assert that a cluster pointer is 'good' */
#define M_GOODC(c) (M_GOODM(c) && 0 == ((ulong)(c) % CLBYTES))

#undef NO_CACHE				/* cache mbufs on intrepid */

int	mb16_drops; /* backward compatibility */

mbinit()
{
	int s;
	int i;

	s = splimp();
	if (!mbuf_sptmap) {		/* do it only once */
		mbuf_small_limit -= (mbuf_small_limit % CLBYTES);
		mbuf_limit -= (mbuf_limit % CLBYTES);
		mbuf_cluster_count = (mbuf_limit - mbuf_small_limit) / CLBYTES;
		mbuf_small_count = mbuf_small_limit / CLBYTES;
		if (mbuf_des_clusters > mbuf_cluster_count)
			mbuf_des_clusters = mbuf_cluster_count;
		if (mbuf_cluster_prealloc > mbuf_cluster_count)
			mbuf_cluster_prealloc = mbuf_cluster_count;
		if (mbuf_small_prealloc > mbuf_small_count)
			mbuf_small_prealloc = mbuf_small_count;

		/* malloc kernel virtual space for mbufs and clusters */
		mbuf_sptmap = malloc(sptmap, btoc(mbuf_limit));
		if (!mbuf_sptmap)
			cmn_err(CE_PANIC, "not enough vm for MBUFs");

		/* vbase is the caddr_t of the base of k2 mbuf space */
		mbuf_vbase = (caddr_t)ctob(mbuf_sptmap);
		cluster_vbase = (caddr_t)(ctob(mbuf_sptmap) + (mbuf_small_count * CLBYTES));
		/*
		 * For the m120 or m20, we need to uncache the mbufs so that
		 * when the lance writes them, we do not have to
		 * explicitly force to read to memory as opposed to
		 * the cache.
		 *
		 * The cleaner solution for this whole problem is to come
		 * up with MTU size mbufs separate from these mbufs
		 * which are used for other network cards.
		 */
#ifdef NO_CACHE
		if (IS_R2400 || IS_R3030) {
			for (i=0; i<btoc(mbuf_limit); i++) {
				pg_setnoncache(kvtokptbl(ctob(mbuf_sptmap+i)));
			}
		}
#endif NO_CACHE
		if (m_clalloc(mbuf_small_prealloc, MPG_MBUFS, M_DONTWAIT) == 0)
			goto bad;
		if (m_clalloc(mbuf_cluster_prealloc, MPG_CLUSTERS, M_DONTWAIT) == 0)
			goto bad;
	}
	splx(s);
	return;
bad:
	cmn_err(CE_PANIC, "mbinit");
}

/*
 * Must be called at splimp.
 */
/* ARGSUSED */
caddr_t
m_clalloc(ncl, how, canwait)
	register int ncl;
	int how;
{
	int mbx;
	register struct mbuf *m;
	register int i;


	switch(how) {
	case MPG_CLUSTERS:
	case MPG_MBUFS:
		break;
	default:
		if (ncl > 1)
			return(NULL);
		break;
	};

	m = NULL;
	for (; ncl > 0; ncl--) {
		m = m_clget(1, canwait, how);
		if (m == NULL)
			return(NULL);

		switch (how) {
		case MPG_CLUSTERS:
			for (i = 0; i < MCL_PER_CL; i++) {
				m->m_off = 0;
				m->m_next = mclfree;
				mclfree = m;
				m = (struct mbuf *) (((caddr_t) m) + MCLBYTES);
				mbstat.m_clfree++;
			}
			mbstat.m_clusters += MCL_PER_CL;
			break;

		case MPG_MBUFS:
			for (i = (CLBYTES / sizeof (*m)); i > 0; i--) {
				m->m_off = 0;
				m->m_type = MT_DATA;
				mbstat.m_mtypes[MT_DATA]++;
				mbstat.m_mbufs++;
				(void) m_free(m);
				m++;
			}
			break;

		case MPG_SPACE:
			mbstat.m_space++;
			break;
		default:
			cmn_err(CE_WARN,"m_clalloc:  unknown type");
			break;
		}
	};
	return ((caddr_t)m);
}


/*
 * Allocate a page of memory.  Make it addressable by the kernel, and
 * then set its reference count to one.
 */
struct mbuf *
m_clget(ncl, canwait, type)
	int ncl;
	int canwait;				/* 1=ok to sleep */
	int type;				/* either cluster or little */
{
	register int mbx, cnt;
	register struct mbuf *p;
	caddr_t r;
	register int s = splhigh();
	static int mbuf_index=0;		/* index into mbuf k2 that we */
	register pde_t *pde;
	register int i;

	if (!mbuf_sptmap) {		/* are we initialized? */
		if (!canwait)		/* no, die if we cannot do it now */
			cmn_err(CE_PANIC, "MBUFs not initialized");
		mbinit();
	}

	if (canwait)
		memlock();
	else if (! memlock_i())
		goto drop_and_return;

	switch (type) {
		case MPG_MBUFS:
			if (mbuf_index == mbuf_small_count) 
				goto unlock_and_return;
			p = (struct mbuf *)(mbuf_vbase + (mbuf_index*CLBYTES));
			mbuf_index++;
			break;

		case MPG_CLUSTERS:
			mbx = mbx_rover;	/* scan for a free cluster */
			cnt = 0;
			for (;;) {
				if (++cnt >= mbuf_cluster_count) 
					goto unlock_and_return;
				if (mbx >= (mbuf_cluster_count * MCL_PER_CL))
					mbx = 0;
				if (mclrefcnt[mbx] == 0) {
					p = xtocl(mbx);
#ifdef DEBUG
					pde = kvtokptbl(p);
					ASSERT(pde->pgm.pg_vr == 0) ;
#endif DEBUG
					break;
				};
				mbx += MCL_PER_CL;
			}
			mbx_rover = mbx;	/* look from here next time */

			for (i = 0; i < MCL_PER_CL; i++) {
				ASSERT(mclrefcnt[mbx + i] == 0);
				mclrefcnt[mbx + i] = 1;
						/* get it now to prevent races*/
			};
			break;

		default:
			cmn_err(CE_WARN,"m_clget: invalid type");
			goto unlock_and_return;
	} /* end switch */

	if ((availsmem - CLBYTES/NBPC < tune.t_minasmem) ||
	    (availrmem - CLBYTES/NBPC < tune.t_minarmem)) 
		goto release_and_return;
	availsmem -= CLBYTES/NBPC;
	availrmem -= CLBYTES/NBPC;
	pde = kvtokptbl(p);
	if (ptmemall(&sysreg,pde,CLBYTES/NBPC, 
		     ((IS_R2400 || IS_R3030) ? PAGE_UNDER_16M : PAGE_COLOR) |
				REGION_NOT_LOCKED | NO_CHANGE_NVALID,
		     !canwait,
		     btoct(p))) {
		availsmem += CLBYTES/NBPC;
		availrmem += CLBYTES/NBPC;
		goto release_and_return;
	}

	for (i = 0; i < CLBYTES/NBPC; i++) {		
		pg_setglob((pde + i));
		pg_setmod((pde + i));
		pg_setvalid((pde + i));
	};

	memunlock();
	splx(s);
	return p;

release_and_return:
	switch (type) {
		case MPG_CLUSTERS:
			for (i = 0; i < MCL_PER_CL; i++) {
				ASSERT(mclrefcnt[mbx + i] == 1);
				mclrefcnt[mbx + i] = 0;
			};
	}
unlock_and_return:
	memunlock();
drop_and_return:
	mbstat.m_drops++;
	splx(s);
	return 0;
}

m_pgfree(addr, n)
	caddr_t addr;
	int n;
{

#ifdef lint
	addr = addr; n = n;
#endif
}

/*
 * Must be called at splimp.
 */
m_expand(canwait)
	int canwait;
{
	register struct domain *dp;
	register struct protosw *pr;
	int tries;

	for (tries = 0;; ) {
		if (m_clalloc(1, MPG_MBUFS, canwait))
			return (1);
		if (canwait == 0 || tries++)
			return (bsd43_mfree != NULL);

		/* ask protocols to free space */
		for (dp = domains; dp; dp = dp->dom_next)
			for (pr = dp->dom_protosw; pr < dp->dom_protoswNPROTOSW;
			    pr++)
				if (pr->pr_drain)
					(*pr->pr_drain)();
		mbstat.m_drain++;
	}
}

/*
 * Space allocation routines.
 * These are also available as macros
 * for critical paths.
 */
struct mbuf *
m_get(canwait, type)
	int canwait, type;
{
	register struct mbuf *m;

	MGET(m, canwait, type);
	return (m);
}

struct mbuf *
m_getclr(canwait, type)
	int canwait, type;
{
	register struct mbuf *m;

	MGET(m, canwait, type);
	if (m == 0)
		return (0);
	bzero(mtod(m, caddr_t), MLEN);
	return (m);
}

struct mbuf *
m_free(m)
	struct mbuf *m;
{
	register struct mbuf *n;

	MFREE(m, n);
	return (n);
}

/*
 * Get more mbufs; called from MGET macro if mfree list is empty.
 * Must be called at splimp.
 */
/*ARGSUSED*/
struct mbuf *
m_more(canwait, type)
	int canwait, type;
{
	register struct mbuf *m;

	while (m_expand(canwait) == 0) {
		if (canwait == M_WAIT) {
			mbstat.m_wait++;
			m_want++;
			sleep((caddr_t)&bsd43_mfree, PZERO - 1);
		} else {
			mbstat.m_drops++;
			return (NULL);
		}
	}
#define m_more(x,y) (cmn_err(CE_PANIC,"m_more"), (struct mbuf *)0)
	MGET(m, canwait, type);
#undef m_more
	return (m);
}

m_freem(m)
	register struct mbuf *m;
{
	register struct mbuf *n;
	register int s;

	if (m == NULL)
		return;
	s = splimp();
	do {
		MFREE(m, n);
	} while (m = n);
	splx(s);
}

/*
 * Mbuffer utility routines.
 */

/*
 * Make a copy of an mbuf chain starting "off" bytes from the beginning,
 * continuing for "len" bytes.  If len is M_COPYALL, copy to end of mbuf.
 * Should get M_WAIT/M_DONTWAIT from caller.
 */
struct mbuf *
m_copy(m, off, len)
	register struct mbuf *m;
	int off;
	register int len;
{
	register struct mbuf *n, **np;
	struct mbuf *top, *p;

	if (len == 0)
		return (0);
	if (off < 0 || len < 0)
		cmn_err(CE_PANIC, "m_copy");
	while (off > 0) {
		if (m == 0)
			cmn_err(CE_PANIC, "m_copy");
		if (off < m->m_len)
			break;
		off -= m->m_len;
		m = m->m_next;
	}
	np = &top;
	top = 0;
	while (len > 0) {
		if (m == 0) {
			if (len != M_COPYALL)
				cmn_err(CE_PANIC, "m_copy");
			break;
		}
		MGET(n, M_DONTWAIT, m->m_type);
		*np = n;
		if (n == 0)
			goto nospace;
		n->m_len = MIN(len, m->m_len - off);
		if (m->m_off > MMAXOFF && 
			(m->m_cltype == MCL_FUNNY || n->m_len > MLEN)) {
			if (mcldup(m, n, off)) {
				n->m_off += off;
			} else {
				m_free(n);
				goto nospace;
			}
		} else
			bcopy(mtod(m, caddr_t)+off, mtod(n, caddr_t),
			    (unsigned)n->m_len);
		if (len != M_COPYALL)
			len -= n->m_len;
		off = 0;
		m = m->m_next;
		np = &n->m_next;
	}
	return (top);
nospace:
	m_freem(top);
	return (0);
}

m_cat(m, n)
	register struct mbuf *m, *n;
{
	while (m->m_next)
		m = m->m_next;
	while (n) {
		if (m->m_off >= MMAXOFF ||
		    m->m_off + m->m_len + n->m_len > MMAXOFF) {
			/* just join the two chains */
			m->m_next = n;
			return;
		}
		/* splat the data from one into the other */
		bcopy(mtod(n, caddr_t), mtod(m, caddr_t) + m->m_len,
		    (u_int)n->m_len);
		m->m_len += n->m_len;
		n = m_free(n);
	}
}

m_adj(mp, len)
	struct mbuf *mp;
	register int len;
{
	register struct mbuf *m;
	register count;

	if ((m = mp) == NULL)
		return;
	if (len >= 0) {
		while (m != NULL && len > 0) {
			if (m->m_len <= len) {
				len -= m->m_len;
				m->m_len = 0;
				m = m->m_next;
			} else {
				m->m_len -= len;
				m->m_off += len;
				break;
			}
		}
	} else {
		/*
		 * Trim from tail.  Scan the mbuf chain,
		 * calculating its length and finding the last mbuf.
		 * If the adjustment only affects this mbuf, then just
		 * adjust and return.  Otherwise, rescan and truncate
		 * after the remaining size.
		 */
		len = -len;
		count = 0;
		for (;;) {
			count += m->m_len;
			if (m->m_next == (struct mbuf *)0)
				break;
			m = m->m_next;
		}
		if (m->m_len >= len) {
			m->m_len -= len;
			return;
		}
		count -= len;
		/*
		 * Correct length for chain is "count".
		 * Find the mbuf with last data, adjust its length,
		 * and toss data from remaining mbufs on chain.
		 */
		for (m = mp; m; m = m->m_next) {
			if (m->m_len >= count) {
				m->m_len = count;
				break;
			}
			count -= m->m_len;
		}
		while (m = m->m_next)
			m->m_len = 0;
	}
}

/*
 * Rearange an mbuf chain so that len bytes are contiguous
 * and in the data area of an mbuf (so that mtod and dtom
 * will work for a structure of size len).  Returns the resulting
 * mbuf chain on success, frees it and returns null on failure.
 * If there is room, it will add up to MPULL_EXTRA bytes to the
 * contiguous region in an attempt to avoid being called next time.
 */
struct mbuf *
m_pullup(n, len)
	register struct mbuf *n;
	int len;
{
	register struct mbuf *m;
	register int count;
	int space;

	if (n->m_off + len <= MMAXOFF && n->m_next) {
		m = n;
		n = n->m_next;
		len -= m->m_len;
	} else {
		if (len > MLEN)
			goto bad;
		MGET(m, M_DONTWAIT, n->m_type);
		if (m == 0)
			goto bad;
		m->m_len = 0;
	}
	space = MMAXOFF - m->m_off;
	do {
		count = MIN(MIN(space - m->m_len, len + MPULL_EXTRA), n->m_len);
		bcopy(mtod(n, caddr_t), mtod(m, caddr_t)+m->m_len,
		  (unsigned)count);
		len -= count;
		m->m_len += count;
		n->m_len -= count;
		if (n->m_len)
			n->m_off += count;
		else
			n = m_free(n);
	} while (len > 0 && n);
	if (len > 0) {
		(void) m_free(m);
		goto bad;
	}
	m->m_next = n;
	return (m);
bad:
	m_freem(n);
	return (0);
}

/*
 * m_fillup() -- Takes as input a chain of mbufs and compacts the chain
 *		 down so that each mbuf has at least len bytes.  This
 *		 is needed for ethernet packets which are required to
 *		 be of a certain length.  The last buffer may have less
 *		 than len bytes and it is up to the user to take care
 *		 of that in the ethernet transmission routine.
 *		 The variable noncache specifies whether new mbufs
 *		 cache bit is set or not set in the TLB.  noncache true
 *		 mean make the mbuf non-cached.
 */
struct mbuf *
m_fillup(m, len, noncache)
	register struct mbuf *m;
	int len, noncache;
{
	register struct mbuf *n, *q, *nxt, *prev;
	register int count;

	ASSERT(M_GOODM(m));
	if (len > MLEN) {
		cmn_err(CE_WARN,"m_fillup: invalid length");
		return(m);
	}
	n = m;
	while (n) {
		nxt = n->m_next;
		if (nxt && nxt->m_len == 0) {
			n->m_next = nxt->m_next;
			m_free(nxt);
			continue;
		}
		if (nxt && n->m_len < len) {
			if (n->m_off > MMAXOFF) {
				/* Shouldn't copy into nfs clusters
				 */
				MGET(q, M_DONTWAIT, n->m_type);
				if (q == NULL) {
					cmn_err(CE_WARN, 
						"m_fillup: out of mbufs");
					goto bad;
				}
				if (noncache) {
					pg_setnoncache(kvtokptbl(q));
				}
				bcopy(mtod(n, caddr_t), mtod(q, caddr_t),
					n->m_len);
				q->m_len = n->m_len;
				q->m_next = nxt;
				m_free(n);
				if (n == m)
					m = q;
				else
					prev->m_next = q;
				n = q;
			}
			ASSERT(MMAXOFF - MLEN >= MMINOFF);
			if (n->m_off > MMAXOFF-len) {
				/* ASSERT: bcopy correctly handles this type
				 * of overlapped copy.
				 */
				bcopy(mtod(n, caddr_t), (caddr_t)n + MMINOFF,
					n->m_len);
				n->m_off = MMINOFF;
			}
			count = MIN(len - n->m_len, nxt->m_len);
			bcopy(mtod(nxt, caddr_t), mtod(n, caddr_t)+n->m_len,
				count);
			n->m_len += count;
			nxt->m_len -= count;
			nxt->m_off += count;
		} else if (IS_KSEG2(mtod(n, int))
			   && btoct(mtod(n, int))
			   != btoct(mtod(n, int)+n->m_len-1)
			   && kvtokptbl(mtod(n, int))->pgm.pg_pfn
			   != kvtokptbl(mtod(n, int)+n->m_len)->pgm.pg_pfn-1) {
					/* Cluster crosses page boundary */
			caddr_t	lim;

			lim = (caddr_t)((mtod(n, int) + NBPC) & ~(NBPC-1));
			count = n->m_len - (lim - mtod(n, caddr_t));
			MGET(q, M_DONTWAIT, n->m_type);
			if (q == NULL) {
				cmn_err(CE_WARN, 
					"m_fillup(2): out of mbufs");
				goto bad;
			}
			if (count > MLEN && !mclget(q)) {
				m_free(q);
				cmn_err(CE_WARN, 
					"m_fillup: out of cluster mbufs");
				goto bad;
			}
			if (noncache) {
				pg_setnoncache(kvtokptbl(mtod(q, int)));
			}
			bcopy(lim, mtod(q, caddr_t), count);
			n->m_len -= count;
			q->m_len = count;
			q->m_next = nxt;
			n->m_next = q;
		} else {
			prev = n;
			n = nxt;
		}
	}
	return(m);

bad:
	return(0);
}

/*
 * m_goodm(m) -- This is merely a functional equivalent of the macro
 *		 M_GOODM(m) which will return true on a good mbuf, false
 *		 on a bad one.  This function is currently being used
 *		 in the lance driver for mbuf sanity checks.
 */
int
m_goodm(m)
struct mbuf *m; 
{
	return(M_GOODM(m));
}

/*
 * Given an mbuf, allocate and attach a cluster mbuf to it.
 * Return 1 if successful, 0 otherwise.
 * NOTE: m->m_len is set to MCLBYTES!
 */
mclget(m)
	register struct mbuf *m;
{
	int ms;
	register struct mbuf *p;

	ms = splimp();
	if (mclfree == 0) {
		p = (struct mbuf *)m_clalloc(1, MPG_CLUSTERS, M_DONTWAIT);
		if (p == NULL) {
			(void) splx(ms);
			return 0;
		}
	}
	p = mclfree;
	if (mclrefcnt[mcltox(p)]++ != 1)
		cmn_err(CE_PANIC,"Bad mclget()");
	mbstat.m_clfree--;
	mclfree = p->m_next;
	p->m_next = 0;
	m->m_len = MCLBYTES;
	m->m_off = (int)p - (int)m;
	m->m_cltype = MCL_NORM;
	(void) splx(ms);
	return (p ? 1 : 0);
}

/* 
 * Allocate an NFS style click mbuf.  That is, an mbuf which does not
 * use either the m_data area nor the normal mbuf page pool for its data,
 * but rather points to some arbitrary data area.  These are used for
 * NFS, where the data you want in the mbuf can be up to UDPMAXSIZE,
 * which is much bigger than your average memory page.  The fun() function
 * is used to free the space, since an arbitrary memory allocator may
 * have been used to get it in the first place.  The dfun() function
 * is used by mcldup() to duplicate the region.
 *
 */
struct mbuf *
mclgetx(fun, arg, dfun, darg, addr, len, wait)
	int (*fun)(), (*dfun)(), darg, arg, len, wait;
	caddr_t addr;
{
	register struct mbuf *m;

	MGET(m, wait, MT_DATA);
	if (m == 0)
		return (0);
	m->m_off = (int)addr - (int)m;
	m->m_len = len;
	m->m_cltype = MCL_FUNNY;
	m->m_clfun = fun;
	m->m_clarg = arg;
	m->m_clswp = NULL;
	m->m_dfun = dfun;
	m->m_darg = darg;
	return (m);
}

/* 
 * Generic cluster mbuf unallocator -- invoked from within MFREE 
 */
mclput(m)
	register struct mbuf *m;
{
	int s;
	register int	i;
	int	mclx;
	int	clx;
	register int *dp;

	switch (m->m_cltype) {
	case MCL_NORM:
		/* we know how to free it, since we allocated it */
		dp = mtod(m,int *);
		mclx = mcltox(dp);
		m = xtomcl(mclx);
		s = splhigh();
		if (--mclrefcnt[mclx] == 1) {
			m->m_next = mclfree;
			mclfree = m;
			mbstat.m_clfree++;

			clx = mclx - (mclx % MCL_PER_CL);
			for (i = 0; i < MCL_PER_CL; i++) 
				if (mclrefcnt[clx + i] != 1)
					break;
			if (i == MCL_PER_CL &&
			    mbstat.m_clfree > mbuf_des_clusters &&
			    memlock_i()) {
				register struct	mbuf *last_m;
				register struct	mbuf *next_m;

				/* 
				 * take all the cluster mbufs in this cluster 
				 * off of the free list
				 */
				for(last_m = NULL, next_m = mclfree;
				    next_m != NULL;) 
					if (cltox(next_m) == clx) {
						next_m = next_m->m_next;
						if (last_m != NULL)
							last_m->m_next = next_m;
						else
							mclfree = next_m;
						mbstat.m_clfree--;
					} else {
						last_m = next_m;
						next_m = next_m->m_next;
					};
							
				/*
				 * mark the virtual space unused 
				 */
				for (i = 0; i < MCL_PER_CL; i++)
					mclrefcnt[clx + i] = 0;
				mbstat.m_clusters -= MCL_PER_CL;

				/*
				 * release the pages
				 */
				kvirt_inval(pnum(m), CLBYTES/NBPC);
				pfree(NULL,kvtokptbl((char *)m),NULL,
					CLBYTES/NBPC);
				availsmem += CLBYTES/NBPC;

				availrmem += CLBYTES/NBPC;
				memunlock();
			};
		}
		splx(s);
		break;

	case MCL_FUNNY:
		/* got to let him do it, since we don't know where the */
		/* memory came from originally */
		(*m->m_clfun)(m->m_clarg);
		break;

	default:
		cmn_err(CE_WARN,"mclput: invalid type");
	}
}


/*
 * Generic cluster mbuf duplicator
 * which duplicates <m> into <n>.
 * If <m> is a regular cluster mbuf, mcldup simply
 * bumps the reference count and ignores <off>.
 * If <m> is a funny mbuf, mcldup allocates a chunk of
 * kernel memory and makes a copy, starting at <off>.
 *
 * NOTE: caller must set m_len in <n> 
 */
mcldup(m, n, off)
	register struct mbuf *m, *n;
	int off;
{
	register struct mbuf *p;
	register caddr_t copybuf;
	int s;

	switch (m->m_cltype) {
	case MCL_NORM:
		/* use the normal mbuf refcnt mechanism */
		p = mtod(m, struct mbuf *);
		n->m_off = (int)p - (int)n;
		n->m_cltype = MCL_NORM;
		s = splimp();
		mclrefcnt[mcltox(p)]++;
		splx(s);
		break;
	case MCL_FUNNY:
		copybuf = mtod(m, caddr_t);
		n->m_off = (int)copybuf - (int)n;
		n->m_cltype = m->m_cltype;
		n->m_clfun = m->m_clfun;
		n->m_clarg = m->m_clarg;
		n->m_dfun = m->m_dfun;
		n->m_darg = m->m_darg;
		(*m->m_dfun)(m->m_darg);
		n->m_clswp = m->m_clswp;
		break;
	default:
		cmn_err(CE_WARN,"mcldup: invalid type");
	}
	return(1);
}

/*
 * Move an mbuf chain to contiguous locations.
 * Checks for possibility of page exchange to accomplish move.
 * Free chain when moved.
 */
m_movtoc(m, to, count)
	register struct mbuf *m;
	register caddr_t to;
	register int count;
{
	register struct mbuf *m0;
	register caddr_t from;
	register int i;

	while (m != NULL) {
		i = MIN(m->m_len, count);
		from = mtod(m, caddr_t);
		while (i >= NBPC && m->m_cltype == MCL_FUNNY && m->m_clswp &&
		    (((int)from | (int)to) & (NBPC-1)) == 0 &&
		    (*m->m_clswp)(m->m_clarg, from, to)) {
			i -= NBPC;
			from += NBPC;
			to += NBPC;
		}
		if (i > 0) {
			bcopy(from, to, (unsigned)i);
			count -= i;
			to += i;
		}
		m0 = m;
		MFREE(m0, m);
	}
	return (count);
}

