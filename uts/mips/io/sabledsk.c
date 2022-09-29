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
#ident	"$Header: sabledsk.c,v 1.5.3.3 90/05/10 05:28:08 wje Exp $"

/*
 * VMEBUS disk driver for sable virtual disk device.
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
#include "bsd/sys/time.h"
#include "sys/user.h"
#include "sys/region.h"
#include "sys/proc.h"
#include "sys/conf.h"
#include "sys/errno.h"
#include "sys/cmn_err.h"
#include "sys/sysmacros.h"
#include "sys/debug.h"
#include "sys/sabledsk.h"
#include "sys/vmevar.h"
#ifndef STANDALONE
#include "sys/dkio.h"
#endif

/*
 *	Defining ASYNCDISK makes the driver not finish I/O on
 *	blocks until sdservice is called from idle. This is
 *	done to allow reasonable simulation of a real disk with
 *	latency for testing the vm code.
 */

#define ASYNCDISK 1

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
} sd_sizes[8] = {
	    0,	    16384,		/* partition 0: 8MB */
        16384,	    32768,		/* 	     1: 8MB */
	    0,	     4096,		/* 	     2: 2MB from beginning */
	    0,		0,
	    0,          0,
	    0,	        0,
	    0,          0,
};
/* END OF STUFF WHICH SHOULD BE READ IN PER DISK */


/* buffers for raw IO */
struct	buf	rsdbuf[NSD];
static struct sddevice *sdp = (struct sddevice *)SDISK_BASE;

#define	b_cylin b_resid

int logoffset, logblock, logcount;

sdprint(dev,str)
dev_t dev;
char * str;
{
	cmn_err(CE_NOTE,"sd: dev %d, %s\n",dev, str);
	cmn_err(CE_CONT,"last offset: %d, blk: %d, cnt: %d\n",logoffset,
		logblock, logcount);
}

sdintr()
{
	cmn_err(CE_PANIC,"sdintr");
}

sdopen(dev)
	dev_t dev;
{
	register int unit = minor(dev) >> 3;

	/* printf ("sd%d: open\n", unit);  JRH */
	if (unit >= NSD)
		return (ENXIO);
	return (0);
}

#ifdef ASYNCDISK
struct buf sdtab;
extern int idleflag;
int sdserv = 0;
#endif

sdstrategy(bp)
	register struct buf *bp;
{
	register int drive, i;
	int partition = minor(bp->b_dev) & 07, s;
	long blocknumber, sz;
	unsigned base, offset;
	int npf;
	caddr_t dmaaddr;

	/* check for valid drive number of the controller */
	drive = dkunit(bp);
	if (drive >= NSD)
		goto bad;

	/* calc number of 512byte blocks in transfer */
	/* make sure the request is contained in the partition */
	sz = (bp->b_bcount+511) >> 9;
	if (bp->b_blkno < 0 ||
	    (blocknumber = dkblock(bp))+sz > sd_sizes[partition].nblocks)
		goto bad;

	/* calc number of 4k pages in transfer */
	offset = (int)bp->b_un.b_addr & 4095;
	npf = bp->b_bcount + offset + SD_PGOFFSET;
	npf >>= SD_PGSHIFT;
	if (npf > SD_NMAP)
		goto bad;

	iomap(bp);
	dmaaddr = bp->b_dmaaddr;
	for(i = 0; i < npf; i++){
	    sdp->sd_map[i] = (kvtokptbl(dmaaddr)->pgm.pg_pfn <<BPTSHFT)
				+ ((unsigned long)dmaaddr & (NBPP-1));
	    dmaaddr += 4096;
	}

	/* fill in the disk controller registers */
	sdp->sd_blocknumber = logblock =
		blocknumber + sd_sizes[partition].blockoff;
	sdp->sd_byteoffset = logoffset = offset;
	sdp->sd_bytecount = logcount = bp->b_bcount;

	sdp->sd_command = bp->b_flags&B_READ ? SD_READ : SD_WRITE;
	if (sdp->sd_status == SD_ERR)
		goto bad;
#ifdef ASYNCDISK
	/*
	 * Link buf onto list of outstanding blocks 
	 * iodone will be called by sdservice from idle()
	 * (Note that they are currently linked in backwards order)
	 */
	bp->av_forw = sdtab.av_forw;
	sdtab.av_forw = bp;
	sdserv = 1;
#else
	iounmap(bp);
	iodone(bp);
#endif ASYNCDISK
	return;
bad:
	bp->b_flags |= B_ERROR;
	iounmap(bp);
	iodone(bp);
	return;
}

#ifdef ASYNCDISK
sdservice()
{
	struct buf *p, *q;
	for(p = sdtab.av_forw; p ; p = q) {
		q = p->av_forw; /* have to get it before iodone clobbers */
		iounmap(p);
		iodone(p);
	}
	sdserv = 0;
	sdtab.av_forw = 0;
	/* have to act like we had an interrupt to jog us out of idle loop */
	idleflag = 0;
}
#endif

sdread(dev, uio)
	dev_t dev;
	struct uio *uio;
{
	register int unit = minor(dev) >> 3;

	if (unit >= NSD)
		return (ENXIO);
	return (physio(sdstrategy, &rsdbuf[unit], dev, B_READ));
}

sdwrite(dev, uio)
	dev_t dev;
	struct uio *uio;
{
	register int unit = minor(dev) >> 3;

	if (unit >= NSD)
		return (ENXIO);
	return (physio(sdstrategy, &rsdbuf[unit], dev, B_WRITE));
}

/*ARGSUSED*/
sddump(dev)
	dev_t dev;
{
	ASSERT(0);

	/* don't think there is room on swap for it anyway. */
}

sdsize(dev)
	dev_t dev;
{
	register int unit = minor(dev) >> 3;

	if (unit >= NSD)
		return (-1);
	return (sd_sizes[minor(dev) & 07].nblocks);
}
#ifndef STANDALONE
extern struct devtable *Devboot;
extern struct devtable Dev_dksd[];
int has_dksd()
{
	Devboot = Dev_dksd;
	return(1);
}
#endif
