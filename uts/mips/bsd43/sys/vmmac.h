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
/* $Header: vmmac.h,v 1.7.1.2 90/05/10 05:00:18 wje Exp $ */
/*
 * Copyright (c) 1982, 1986 Regents of the University of California.
 * All rights reserved.  The Berkeley software License Agreement
 * specifies the terms and conditions for redistribution.
 *
 *	@(#)vmmac.h	7.1 (Berkeley) 6/4/86
 */
/*----- COPYRIGHT (END) ------------------------------------------------------*/

#ifndef BSD43_
#    include <bsd43/bsd43_.h>
#endif BSD43_


/*
 * Virtual memory related conversion macros
 */

/* Core clicks to number of pages of page tables needed to map that much */
#define	bsd43_ctopt(x)	(((x)+BSD43_NPTEPG-1)/BSD43_NPTEPG)

#ifdef vax
/* Virtual page numbers to text|data|stack segment page numbers and back */
#define	bsd43_vtotp(p, v)	((int)(v))
#define	bsd43_vtodp(p, v)	((int)((v) - bsd43_stoc(bsd43_ctos((p)->p_tsize))))
#define	bsd43_vtosp(p, v)	((int)(BTOPUSRSTACK - 1 - (v)))
#define	bsd43_tptov(p, i)	((unsigned)(i))
#define	bsd43_dptov(p, i)	((unsigned)(bsd43_stoc(bsd43_ctos((p)->p_tsize)) + (i)))
#define	bsd43_sptov(p, i)	((unsigned)(BTOPUSRSTACK - 1 - (i)))

/* Tell whether virtual page numbers are in text|data|stack segment */
#define	bsd43_isassv(p, v)	((v) >= BTOPUSRSTACK - (p)->p_ssize)
#define	bsd43_isatsv(p, v)	((v) < (p)->p_tsize)
#define	bsd43_isadsv(p, v)	((v) >= bsd43_stoc(bsd43_ctos((p)->p_tsize)) && \
	(v) < (p)->p_tsize + (p)->p_dsize)
#endif vax

#ifdef mips
/* Virtual page numbers to text|data|stack segment page numbers and back */
#define	bsd43_vtotp(p, v)	((int)(v) - bsd43_btop((p)->p_textstart))
#define bsd43_vtodp(p, v)	((int)(v) - bsd43_btop((p)->p_datastart))
#define	bsd43_vtosp(p, v)	((int)(bsd43_btop(BSD43_USRSTACK) - 1 - (v)))
#define	bsd43_tptov(p, i)	((unsigned)(i) + bsd43_btop((p)->p_textstart))
#define	bsd43_dptov(p, i)	((unsigned)(i) + bsd43_btop((p)->p_datastart))
#define	bsd43_sptov(p, i)	((unsigned)(bsd43_btop(BSD43_USRSTACK) - 1 - (i)))

/* Tell whether virtual page numbers are in text|data|stack segment */
#define	bsd43_isassv(p, v)	(((v) >= (bsd43_btop(BSD43_USRSTACK) - (p)->p_ssize)) && \
				((v) < bsd43_btop(BSD43_USRSTACK)))
#define	bsd43_isatsv(p, v)	((((v) - bsd43_btop((p)->p_textstart)) < (p)->p_tsize) && \
				(((v) >= bsd43_btop((p)->p_textstart))))
#define	bsd43_isadsv(p, v)	((((v) - bsd43_btop((p)->p_datastart)) < (p)->p_dsize) && \
				(((v) >= bsd43_btop((p)->p_datastart))))
#endif mips

/* Tell whether pte's are text|data|stack */
#ifdef vax
#define	bsd43_isaspte(p, pte)		((pte) > bsd43_sptopte(p, (p)->p_ssize))
#define	bsd43_isatpte(p, pte)		((pte) < bsd43_dptopte(p, 0))
#define	bsd43_isadpte(p, pte)		(!bsd43_isaspte(p, pte) && !bsd43_isatpte(p, pte))
#endif vax

#ifdef mips
#define	bsd43_isaspte(p, pte)		(((pte) > bsd43_sptopte(p, (p)->p_ssize)) && \
					((pte) <= bsd43_sptopte(p, 0)))
#define	bsd43_isatpte(p, pte)		(((pte) < bsd43_tptopte(p, (p)->p_tsize)) && \
					((pte) >= bsd43_tptopte(p, 0)))
#define	bsd43_isadpte(p, pte)		(((pte) < bsd43_dptopte(p, (p)->p_dsize)) && \
					((pte) >= bsd43_dptopte(p, 0)))
#endif mips

/* Text|data|stack pte's to segment page numbers and back */
#ifdef vax
#define	bsd43_ptetotp(p, pte)		((pte) - (p)->p_p0br)
#define	bsd43_ptetodp(p, pte)		(((pte) - (p)->p_p0br) - (p)->p_tsize)
#define	bsd43_ptetosp(p, pte)		(((p)->p_addr - (pte)) - 1)
#endif vax

#ifdef mips
#define	bsd43_ptetotp(p, pte)		((pte) - (p)->p_textbr)
#define	bsd43_ptetodp(p, pte)		((pte) - (p)->p_databr)
/*
 * u block not located in users virtual address space.
 */
#define	bsd43_ptetosp(p, pte)	\
	(((p)->p_stakbr + (p)->p_stakpt*BSD43_NPTEPG - BSD43_REDZONEPAGES - 1) - (pte))
#endif mips

#ifdef vax
#define	bsd43_tptopte(p, i)		((p)->p_p0br + (i))
#define	bsd43_dptopte(p, i)		((p)->p_p0br + ((p)->p_tsize + (i)))
#define	bsd43_sptopte(p, i)		((p)->p_addr - (1 + (i)))

/* Convert a virtual page number to a pte address. */
#define bsd43_vtopte(p, v) \
	(((v) < (p)->p_tsize + (p)->p_dsize) ? ((p)->p_p0br + (v)) : \
	((p)->p_addr - (BTOPUSRSTACK - (v))))
#endif vax

#ifdef mips
#define	bsd43_tptopte(p, i)		((p)->p_textbr + (i))
#define	bsd43_dptopte(p, i)		((p)->p_databr + (i))
#define	bsd43_sptopte(p, i)		\
	(((p)->p_stakbr + (p)->p_stakpt*BSD43_NPTEPG - BSD43_REDZONEPAGES - 1) - (i))

struct	bsd43_(pte) *bsd43_vtopte();
#endif mips

/* Bytes to pages without rounding, and back */
#define	bsd43_btop(x)		(((unsigned)(x)) >> BSD43_PGSHIFT)
#define	bsd43_ptob(x)		((caddr_t)((x) << BSD43_PGSHIFT))

/* Turn virtual addresses into kernel map indices */
#define	bsd43_kmxtob(a)	(bsd43_(usrpt) + (a) * BSD43_NPTEPG)
#define	bsd43_btokmx(b)	(((b) - bsd43_(usrpt)) / BSD43_NPTEPG)

/* User area address and pcb bases */
#ifdef vax
#define	bsd43_uaddr(p)	(&((p)->p_p0br[(p)->p_szpt * BSD43_NPTEPG - BSD43_UPAGES]))
#endif
#ifdef mips
#define	bsd43_uaddr(p)	((p)->p_addr)
#endif
#ifdef vax
#define	bsd43_pcbb(p)		((p)->p_addr[0].pg_pfnum)
#endif
#ifdef mips
#define	bsd43_pcbb(p)		(p)
#endif

/* Average new into old with aging factor time */
#define	bsd43_ave(smooth, cnt, time) \
	smooth = ((time - 1) * (smooth) + (cnt)) / (time)

/* Abstract machine dependent operations */
#ifdef vax
#define	bsd43_setp0br(x)	(bsd43_u.u_pcb.pcb_p0br = (x), bsd43_(mtpr)(P0BR, x))
#define	bsd43_setp0lr(x)	(bsd43_u.u_pcb.pcb_p0lr = \
			    (x) | (bsd43_u.u_pcb.pcb_p0lr & AST_CLR), \
			 bsd43_(mtpr)(P0LR, x))
#define	bsd43_setp1br(x)	(bsd43_u.u_pcb.pcb_p1br = (x), bsd43_(mtpr)(P1BR, x))
#define	bsd43_setp1lr(x)	(bsd43_u.u_pcb.pcb_p1lr = (x), bsd43_(mtpr)(P1LR, x))
#define	bsd43_initp1br(x)	((x) - P1PAGES)
#endif vax

#define	bsd43_outofmem()	bsd43_(wakeup)((caddr_t)&bsd43_(proc)[2]);

/*
 * Page clustering macros.
 * 
 * dirtycl(pte)			is the page cluster dirty?
 * anycl(pte,fld)		does any pte in the cluster has fld set?
 * zapcl(pte,fld) = val		set all fields fld in the cluster to val
 * distcl(pte)			distribute high bits to cluster; note that
 *				distcl copies everything but pg_pfnum,
 *				INCLUDING pg_m!!!
 *
 * In all cases, pte must be the low pte in the cluster, even if
 * the segment grows backwards (e.g. the stack).
 */
#define	BSD43_H(pte)	((struct bsd43_(hpte) *)(pte))

#if BSD43_CLSIZE==1
#define	bsd43_dirtycl(pte)	bsd43_dirty(pte)
#define	bsd43_anycl(pte,fld)	((pte)->fld)
#define	bsd43_zapcl(pte,fld)	(pte)->fld
#define	bsd43_distcl(pte)
#endif

#if BSD43_CLSIZE==2
#define	bsd43_dirtycl(pte)	(bsd43_dirty(pte) || bsd43_dirty((pte)+1))
#define	bsd43_anycl(pte,fld)	((pte)->fld || (((pte)+1)->fld))
#define	bsd43_zapcl(pte,fld)	(pte)[1].fld = (pte)[0].fld
#endif

#if BSD43_CLSIZE==4
#define	bsd43_dirtycl(pte) \
    (bsd43_dirty(pte) || bsd43_dirty((pte)+1) || bsd43_dirty((pte)+2) || bsd43_dirty((pte)+3))
#define	bsd43_anycl(pte,fld) \
    ((pte)->fld || (((pte)+1)->fld) || (((pte)+2)->fld) || (((pte)+3)->fld))
#define	bsd43_zapcl(pte,fld) \
    (pte)[3].fld = (pte)[2].fld = (pte)[1].fld = (pte)[0].fld
#endif

#ifndef bsd43_distcl
#define	bsd43_distcl(pte)	bsd43_zapcl(BSD43_H(pte),pg_high)
#endif

/*
 * Lock a page frame.
 */
#define BSD43_MLOCK(c) { \
	while ((c)->c_lock) { \
		(c)->c_want = 1; \
		bsd43_(sleep)((caddr_t)(c), BSD43_PSWP+1); \
	} \
	(c)->c_lock = 1; \
}
/*
 * Unlock a page frame.
 */
#define BSD43_MUNLOCK(c) { \
	if (c->c_want) { \
		bsd43_(wakeup)((caddr_t)c); \
		c->c_want = 0; \
	} \
	c->c_lock = 0; \
}

/*----- NUMIPS: STRIP MACRO PREFIXES (BEGIN) ---------------------------------*/
/* 
 * Strip off "BSD43_" and "bsd43_" for use with standard BSD code.
 * (GENERATED AUTOMATICALLY -- DON'T EDIT BY HAND)
 */
#ifdef SYSTYPE_BSD43
#   define H BSD43_H
#   define MLOCK BSD43_MLOCK
#   define MUNLOCK BSD43_MUNLOCK
#   define anycl bsd43_anycl
#   define ave bsd43_ave
#   define btokmx bsd43_btokmx
#   define btop bsd43_btop
#   define ctopt bsd43_ctopt
#   define dirtycl bsd43_dirtycl
#   define distcl bsd43_distcl
#   define dptopte bsd43_dptopte
#   define dptov bsd43_dptov
#   define initp1br bsd43_initp1br
#   define isadpte bsd43_isadpte
#   define isadsv bsd43_isadsv
#   define isaspte bsd43_isaspte
#   define isassv bsd43_isassv
#   define isatpte bsd43_isatpte
#   define isatsv bsd43_isatsv
#   define kmxtob bsd43_kmxtob
#   define outofmem bsd43_outofmem
#   define pcbb bsd43_pcbb
#   define ptetodp bsd43_ptetodp
#   define ptetosp bsd43_ptetosp
#   define ptetotp bsd43_ptetotp
#   define ptob bsd43_ptob
#   define setp0br bsd43_setp0br
#   define setp0lr bsd43_setp0lr
#   define setp1br bsd43_setp1br
#   define setp1lr bsd43_setp1lr
#   define sptopte bsd43_sptopte
#   define sptov bsd43_sptov
#   define tptopte bsd43_tptopte
#   define tptov bsd43_tptov
#   define uaddr bsd43_uaddr
#   define vtodp bsd43_vtodp
#   define vtopte bsd43_vtopte
#   define vtosp bsd43_vtosp
#   define vtotp bsd43_vtotp
#   define zapcl bsd43_zapcl
#endif SYSTYPE_BSD43
/*----- NUMIPS: STRIP MACRO PREFIXES (END) -----------------------------------*/


