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
/* $Header: pfdat.h,v 1.17 90/07/25 14:03:22 jj Exp $ */

#ifndef	_SYS_PFDAT_
#define	_SYS_PFDAT_	1

#include <sys/immu.h>
#include <sys/region.h>

/*	Copyright (c) 1984 AT&T	*/
/*	  All Rights Reserved  	*/

/*	THIS IS UNPUBLISHED PROPRIETARY SOURCE CODE OF AT&T	*/
/*	The copyright notice above does not evidence any   	*/
/*	actual or intended publication of such source code.	*/



typedef struct pfdat {
	struct	dbd	pf_dbd;		/* copy of dbd for	*/
					/* current use of this 	*/
					/* page			*/
	ulong		pf_nodeid;	/* unique id for file 	*/
					/* within this devid	*/
	ulong		pf_devid;	/* unique id for dev/fs	*/
	cnt_t		pf_use;		/* share use count	*/
	unsigned short	pf_flags;	/* page flags		*/
	short		pf_rawcnt;	/* Cnt of processes	*/
					/* doing raw I/O to 	*/
					/* page.		*/
	short		pf_waitcnt;	/* Number of processes	*/
					/* waiting for PG_DONE	*/
	struct pfdat	*pf_next;	/* Next free pfdat.	*/
	struct pfdat	*pf_prev;	/* Previous free pfdat.	*/
	struct pfdat	*pf_hchain;	/* Hash chain link.	*/
	union {
		pguselink_t	pfu_use_link;
					/* list of pde's which	*/
					/* use this page	*/
		reg_t	*pfu_region;	/* region or regions which use this */
		reg_t	**pfu_regions;	/* page (if pf_type == DBD_PDE) */
	} pf_u;
#ifdef CACHETRICKS
unsigned short	c_icachecnt,	/* icache stamp when page free'd */
		c_dcachecnt;	/* dcache stamp when page free'd */
#endif /* CACHETRICKS */
} pfd_t;

#define pf_type pf_dbd.dbd_type
#define pf_swpi pf_dbd.dbd_swpi
#define pf_blkno pf_dbd.dbd_blkno
#define pf_use_link pf_u.pfu_use_link
#define pf_region pf_u.pfu_region
#define pf_regions pf_u.pfu_regions

#define	P_QUEUE		0x01	/* Page on free queue		*/
#define	P_BAD		0x02	/* Bad page (parity error, etc.)*/
#define	P_HASH		0x04	/* Page on hash queue		*/
#define P_DONE		0x08	/* I/O to read page is done	*/
#define	P_SWAP		0x10	/* Page on swap (not file).	*/
#define	P_DUMPED	0x20	/* Page has been dumped. 	*/
				/* (Only used when dumping core)*/
#define P_LOCKED	0x40	/* locked for raw I/O or pagein */
#define	P_CLEARED	0x80	/* Page has been cleared.  No need
				   need to clear on fault of zero-fill-
				   on-demand page. */
#define P_WANTED	0x100	/* process is waiting for lock */

extern struct pfdat phead;
/* The free list has been enhanced.
 * All pages from phead until *pclearp are not associated with disk and have
 * already been cleared (during idle() or initialization).  Pages from *pclearp
 * until the hashed pages still need to be cleared when faulted in as a
 * zero-fill-on-demand page.
 */
extern struct pfdat *pclearp;

struct pclearstat {
	unsigned int	p_cnt;	/* # pages idle() successfully cleared */
	unsigned int	p_hit;	/* # DZERO pages cleared by idle() */
	unsigned int	p_miss;	/* # DZERO pages that vfault() had to clear */
};
extern struct pclearstat pclearstat;

extern struct pfdat pbad;
extern struct pfdat *pfdat;
extern struct pfdat **phash;
extern struct pfdat ptfree;
extern int phashmask;
extern struct pfdat	*pfind();


#define BLKNULL		0	/* pf_blkno null value		*/

extern int	mem_lock;
extern int	memlock();
extern int	memunlock();
extern int	memlocked();

#ifdef CACHETRICKS
extern unsigned short icachecnt[];/* cacheflush count for each icache pg */
extern unsigned short dcachecnt[];/* cacheflush count for each dcache pg */
extern unsigned int icachemask;	/* mask with pfnum to get icachecnt index */
extern unsigned int dcachemask;	/* mask with pfnum to get dcachecnt index */
#endif /* CACHETRICKS */

/* bit definitions of flag passed to ptmemall:
 * 	ptmemall(rp, base, size, flag, nosleep, vpn)
 */
#define VALIDATE	0x0000001	/* validate this page */
#define PAGE_COLOR	0x0000002	/* try to get "nice" cache behavior */
#define REGION_NOT_LOCKED 0x0000004	/* region supplied is not locked */
#define PAGE_UNDER_16M	0x0000008	/* allocate page under 16M byte addr */
#define NO_CHANGE_NVALID 0x0000010	/* do not increment r_nvalid */
#define CONTIGUOUS_PAGES 0x0000020	/* Allocate physically contiguous memory */

/* Symbolic constants */
#define	MEM16M	0x1000000		/* 16 Mbytes memory */

/* maximum pages to scan on the free list looking for "nice" cache location */
#define MAXCACHECOLOR	64

#ifdef KERNEL
/*
 * Lock a page frame.
 */
#define MLOCK(c) { \
	while ((c)->pf_flags & P_LOCKED) { \
		(c)->pf_flags |= P_WANTED; \
		sleep((caddr_t)(c), PSWP+1); \
	} \
	(c)->pf_flags |= P_LOCKED; \
}
/*
 * Lock a page frame, without waiting
 */
#define MLOCK_NOWAIT(c)  \
	(((c)->pf_flags & P_LOCKED) \
	 	? 0 \
		: ((c)->pf_flags |= P_LOCKED, 1))

/*
 * Unlock a page frame.
 */
#define MUNLOCK(c) { \
	if (c->pf_flags & P_WANTED) { \
		wakeup((caddr_t)c); \
		c->pf_flags &= ~P_WANTED; \
	} \
	c->pf_flags &= ~P_LOCKED; \
}

/*
 * Test if page frame is locked
 */
#define MISLOCKED(c) \
	((c)->pf_flags & P_LOCKED)

/*
 * Wait for page frame be unlocked
 */
#define MUNLOCK_WAIT(c) { \
	while ((c)->pf_flags & P_LOCKED) { \
		(c)->pf_flags |= P_WANTED; \
		sleep((caddr_t)(c), PSWP+1); \
	} \
}

/*
 * Mark I/O on a page frame done
 */
#define MDONE(c) { \
	c->pf_flags |= P_DONE; \
	if (c->pf_waitcnt) { \
		c->pf_waitcnt = 0; \
		wakeup(c); \
	} \
}

/*
 * Test if I/O on a page frame is done
 */
#define MISDONE(c) ((pfd->pf_flags & P_DONE) != 0)

/*
 * Wait for I/O on a page frame to be done
 */
#define MWAIT(c) { \
	while (!(c->pf_flags & P_DONE)) { \
		c->pf_waitcnt++; \
		sleep(c, PZERO); \
	} \
}

#endif KERNEL

#endif	_SYS_PFDAT_
