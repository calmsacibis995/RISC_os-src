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
#ident	"$Header: rd.c,v 1.6.4.3 90/05/10 05:27:55 wje Exp $"


/*
 * VMEBUS disk driver for ram disk device.
 * System V version.
 */
#include "sys/sbd.h"
#include "sys/types.h"
#include "sys/immu.h"
#include "sys/param.h"
#include "sys/systm.h"
#include "sys/map.h"
#include "sys/buf.h"
#include "sys/dir.h"
#include "sys/pcb.h"
#include "sys/signal.h"
#include "sys/user.h"
#include "sys/region.h"
#include "sys/proc.h"
#include "sys/conf.h"
#include "sys/errno.h"
#include "sys/cmn_err.h"
#include "sys/sysmacros.h"
#include "sys/debug.h"
#include "sys/vmevar.h"

#define NRD	1
#define NRDMAP	64

#ifdef notdef
struct vme_ctlr *rcinfo[NRC];
struct vme_device *rdinfo[NRD];
int *rcstd[] = { (int *)0 };
int rdprobe(), rdslave(), rdattach();

struct vme_driver rcdriver = {
	rdprobe, rdslave, rdattach, (unsigned *)rcstd, "rd", rdinfo, "rc", rcinfo
};
#endif

extern pde_t *vtop();

struct vme_driver scdriver;

/* like on 4.2, define minor to be   unit:5, slice:3 */
#define dkunit(bp) (minor((bp)->b_dev)>>5)
#define dkslice(bp) (minor((bp)->b_dev)&07)
#define dkblock(bp) ((bp)->b_blkno)

/* THIS SHOULD BE READ OFF THE PACK, PER DRIVE */
struct	size {
	daddr_t	blockoff;		/* starting block in slice */
	daddr_t	nblocks;		/* number of blocks in slice */
} rd_sizes[8] = {
#ifndef SABLE
	    0,	     12096,		/* A=cyl   0 thru 397 */
	 12096,	     2048,		/* B=cyl 398 thru 510 */
	    0,	     4096,		/* C=cyl   0 thru 511 */
	    0,		0,		/* D=cyl 398 thru 510 */
	    0,          0,		/* F= Not Defined     */
	    0,	        0,		/* G=cyl   0 thru 510 */
	    0,          0,		/* H= Not Defined     */
#else
	    0,	     4096,		/* A=cyl   0 thru 511 */
	 4096,	     4096,		/* B=cyl 511 thru 1023 */
	    0,	     8192,		/* C=cyl   0 thru 1023 */
	    0,		0,		/* D=cyl 398 thru 510 */
	    0,          0,		/* F= Not Defined     */
	    0,	        0,		/* G=cyl   0 thru 510 */
	    0,          0,		/* H= Not Defined     */
#endif
};
/* END OF STUFF WHICH SHOULD BE READ IN PER DISK */


/* buffers for raw IO */
struct	buf	rrdbuf[NRD];

#define RDISK_BASE	0xa0a00000
static struct rddevice *rdp = (struct rddevice *)RDISK_BASE;

#define	b_cylin b_resid

rdprint(dev,str)
dev_t dev;
char * str;
{
	cmn_err(CE_NOTE,"rd: dev %d, %s\n",dev, str);
}

rdintr()
{
	cmn_err(CE_PANIC,"rdintr");
}

rdopen(dev)
	dev_t dev;
{
	register int unit = minor(dev) >> 3;

	if (unit >= NRD) {
		u.u_error = ENXIO;
		return ;
	}
	return ;
}

rdstrategy(bp)
	register struct buf *bp;
{
	register int drive, i;
	int partition = minor(bp->b_dev) & 07, s;
	long blocknumber, sz;
	unsigned base, offset, v;
	int resid,npf, err;
	struct proc *rp;
	register pde_t *pdeptr;
	pde_t rd_map[NRDMAP];
	char *dbaddr, *phaddr;
	int count;
	unsigned sable = 0;

	/* check for valid drive number of the controller */
	drive = dkunit(bp);
	if (drive >= NRD)
		goto bad;

	/* calc number of 512byte blocks in transfer */
	/* make sure the request is contained in the partition */
	sz = (bp->b_bcount+511) >> 9;
	if (bp->b_blkno < 0 ||
	    (blocknumber = dkblock(bp))+sz > rd_sizes[partition].nblocks)
		goto bad;

	blocknumber += rd_sizes[partition].blockoff;
	dbaddr = (char *)RDISK_BASE + (blocknumber*512);

	/* calc number of 4k pages in transfer */
	v = btoct(bp->b_un.b_addr);
	offset = (int)bp->b_un.b_addr & (NBPC - 1);
	npf = bp->b_bcount + offset + (NBPC -1);
	npf >>= PNUMSHFT;
	if (npf <= 0)
		goto bad;


	/* 
	 *	Determine the page map for this transaction.
	 *	The transaction can be:
	 *	Swap request (b_flags & B_SWAP)
	 *		(from proc 0 (sched) or proc 2 (vhand) or vfault):
	 *		b_un.b_addr is physical address.
	 *	Physical to user's buffer: use vtop to get pde's
	 *	To kernel buffer in k0seg (for now): make pde's
	 *	To kernel u_area for current process.
	 *	Other types not used in Sys V: page tables.
	 */


	if(bp->b_flags & B_SWAP) {
		/* swap requests come one-at-a-time */
		ASSERT(npf == 1);
		rd_map[0].pgi.pg_pde = (unsigned long)bp->b_un.b_addr;
	} else if(IS_KUSEG(bp->b_un.b_addr)) {
		/* physical I/O to current process */
		pdeptr = vtop((unsigned)bp->b_un.b_addr, curproc);
		for (i=0; i<npf; i++) {
			rd_map[i] = *pdeptr++;
		}
	} else if(IS_UAREA(bp->b_un.b_addr)) {
		pdeptr = u.u_procp->p_ubptbl + 
			(int)btoc(bp->b_un.b_addr - (caddr_t)&u);
		for (i=0; i<npf; i++) {
			rd_map[i] = *pdeptr++;
		}
#ifdef SABLE
	} else if (IS_KSEG2(bp->b_un.b_addr)) {
		/* I/O to kernel buffer */
		for (i=0; i<npf; i++) {
			rd_map[i].pgm.pg_pfn = v;
			v += 1;
		}
		sable = 1;
#else
	} else {
		/* I/O to kernel buffer */
		register pfn = svtopfn(bp->b_un.b_addr);
		for (i=0; i<npf; i++) {
			rd_map[i].pgi.pg_pde = mkpde(0,pfn);
			pfn++;
		}
#endif
	}
		
	resid = bp->b_bcount;
	pdeptr = rd_map;
	do {
		if (!sable)
			phaddr = (char *)PHYS_TO_K1(ctob(pdeptr->pgm.pg_pfn));
		else
			phaddr = (char *)(ctob(pdeptr->pgm.pg_pfn));
		phaddr += offset;
		count = NBPC - offset;
		if (count > resid)
			count = resid;
		if (bp->b_flags & B_READ) {
			bcopy(dbaddr, phaddr, count);
		} else {
			bcopy(phaddr, dbaddr, count);
		}
		pdeptr++;
		offset = 0;
		dbaddr += count;
		resid -= count;
	} while (resid > 0);

	iodone(bp);
	return;
bad:
	bp->b_flags |= B_ERROR;
	iodone(bp);
	return;
}

rdread(dev)
	dev_t dev;
{
	register int unit = minor(dev) >> 3;

	if (unit >= NRD) {
		u.u_error = ENXIO;
		return;
	}
	return (physio(rdstrategy, &rrdbuf[unit], dev, B_READ));
}

rdwrite(dev)
	dev_t dev;
{
	register int unit = minor(dev) >> 3;

	if (unit >= NRD) {
		u.u_error = ENXIO;
		return;
	}
	return (physio(rdstrategy, &rrdbuf[unit], dev, B_WRITE));
}

rdsize(dev)
	dev_t dev;
{
	register int unit = minor(dev) >> 3;

	if (unit >= NRD)
		return (-1);
	return (rd_sizes[minor(dev) & 07].nblocks);
}
#ifndef STANDALONE
extern struct devtable *Devboot;
extern struct devtable Dev_rd[];
int has_rd()
{
	Devboot = Dev_rd;
	return(1);
}
#endif
