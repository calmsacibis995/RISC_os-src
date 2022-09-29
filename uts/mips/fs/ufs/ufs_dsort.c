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
#ident	"$Header: ufs_dsort.c,v 1.3.1.2 90/05/10 05:08:54 wje Exp $"


#include "sys/param.h"
#include "sys/types.h"
#include "sys/buf.h"
#include "sys/iobuf.h"
#include "sys/debug.h"

#define	b_cyl	b_resid

int maxlistsize = 8;

/*
 * Seek sort for disks.  The driver has already decomposed the disk address
 * into b_cyl.
 *
 * The argument dp structure holds a av_forw activity chain pointer
 * on which we keep N queues, sorted in ascending cylinder order.
 * The first queue holds those requests which are positioned after
 * the current cylinder (in the first request); the rest holds
 * requests which came in after their cylinder number was passed.
 * Thus we implement a one way scan, retracting after reaching the
 * end of the drive to the first request on the second queue,
 * at which time it becomes the first queue.
 *
 * A one-way scan is natural because of the way UNIX read-ahead
 * blocks are allocated.
 */
disksort(dp, bp, cur_cyl)
	register struct iobuf *dp;
	register struct buf *bp;
        register cur_cyl;		/* Current cylinder for this unit */
{
	register struct buf *ap;
	register listsize = maxlistsize;/* Maximum sorted table size	 */
	register num = 1;		/* Used to count number in list	 */
	register numlists = 1;		/* Used to count number of lists */
	register struct buf *zp;	/* Temporary pointer             */

	/*
	 * Sanity check maxlistsize, and reset if bad!
	 */
	if (maxlistsize < 1)
		maxlistsize = listsize = 1;

	/*
	 * If nothing on the activity queue, then
	 * we become the only thing.
	 */
	ap = dp->b_actf;
	if(ap == NULL) {
		dp->b_actf = bp;
		dp->b_actl = bp;
		bp->av_forw = NULL;
		XPR3(XPR_DISK,"dsortR: cyl %d blk %d\n",bp->b_resid,
		     bp->b_blkno);
		return;
	}

#ifdef XPR_DEBUG
	if (xpr_flags & XPR_DISK) {
		XPR1(XPR_DISK,"before: ");
		zp = dp->b_actf->av_forw;
	        XPR2(XPR_DISK,"%x,",cur_cyl);
		while (zp) {
		    XPR2(XPR_DISK,"%x,",zp->b_cyl);
		    zp = zp->av_forw;
		}
		XPR1(XPR_DISK,"\n");
	}
#endif

	/*
	 * First we are going to search the entire list and place the
	 * new bp in the last list.  We also keep count of how many
	 * bp's are in the last list.
	 */
	zp = ap;
	while (ap->av_forw) {
	    num += 1;
	    
	    /* The next check determines if we have found a new list.  An
	     * 'inversion' in the cylinder numbers means a new list
	     */
	    if (ap->av_forw->b_cyl < (ap==dp->b_actf ? cur_cyl : ap->b_cyl)) {
		num = 1;
		numlists += 1;
		zp = ap;
	    }
	    ap = ap->av_forw;
	}
	XPR4(XPR_DISK,"dsort: cyl 0x%x numlists: 0x%x num: 0x%x\n",
		bp->b_cyl,numlists,num);
	/*
	 * Now we have a pointer to the beginning of the last list in zp.
	 * We two special cases here:
	 *
	 *   o We only have one list, and the bp->b_cyl < cur_cyl.
	 *     Normally this would be sorted into this list, but the
	 *     first list is a special case, and we must create a
	 *     second list because we do not want to reseek until we
	 *     hit the next list.
	 *   o If there are already too many entries in this list, create
	 *     new list.
	 *
	 * Otherwise we just sort into this list in order of the cylinder
	 * number.
	 */

	if ( ((numlists == 1) && (bp->b_cyl <= cur_cyl)) ||
	    (num >= listsize)) {
	    XPR4(XPR_DISK,"dsort: listsize: 0x%x cur_cyl 0x%x b_cyl 0x%x\n",
		 listsize,cur_cyl,bp->b_cyl);
	    goto insert;
	}

	/*
	 * Gotten rid of special cases, so just sort normally.
	 */

	ap = zp;			/* Head of list */
	while (ap->av_forw) {
	    if (bp->b_cyl < ap->av_forw->b_cyl)
	      break;
	    ap = ap->av_forw;
	}

insert:
	bp->av_forw = ap->av_forw;
	ap->av_forw = bp;

#ifdef XPR_DEBUG
	if (xpr_flags & XPR_DISK) {
		XPR1(XPR_DISK,"after: ");
		zp = dp->b_actf->av_forw;
	        XPR2(XPR_DISK,"%x,",cur_cyl);
		while (zp) {
		    XPR2(XPR_DISK,"%x,",zp->b_cyl);
		    zp = zp->av_forw;
		}
		XPR1(XPR_DISK,"\n");
	}
#endif
	    
	if (ap == dp->b_actl)
		dp->b_actl = bp;
}

