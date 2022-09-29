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
#ident	"$Header: sgset.c,v 1.3.4.2 90/05/10 05:30:05 wje Exp $"

#include "sys/sbd.h"
#include "sys/types.h"
#include "sys/immu.h"
#include "sys/param.h"
#include "sys/systm.h"
#include "sys/map.h"
#include "sys/buf.h"
#include "sys/dir.h"
#include "sys/pcb.h"
#include "sys/file.h"
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
#include "sys/vmevar.h"
#include "sys/vmereg.h"

#include "sys/sg.h"

# define PHYSADDR(x)	(pttob(kvtokptbl(x)->pgm.pg_pfn)+poff(x))

/*
 * set up the scatter / gather vector for a data transfer.
 * return the actual number of sg entries used.  if more than
 * maxvec are needed, pass back the leftover count in *_resid.
 *
 * it is up to the caller to convert the generic scatter / gather
 * vector to the appropriate device specific form.
 */
int
sgset(bp, vec, maxvec, _resid)
	struct buf *bp;
	struct sg *vec;
	int maxvec;
	unsigned (*_resid);
{
	register struct sg *sg;
	register unsigned count;
	register unsigned long vaddr;
	int nvec;

	/*
	 * for now, make sure it's mapped in to kernel space
	 * so the PHYSADDR macro will work.  this may sleep!
	 * if this is a raw i/o
	 */
	iomap(bp);

	/*
	 * for each piece of contiguous physical memory,
	 * setup one scatter / gather descriptor.
	 * for now, ignore the fact that pages may be
	 * coincidentally contiguous.
	 */
	count = bp->b_resid;
	vaddr = (unsigned long) bp->b_dmaaddr + bp->b_bcount - count;
	sg = vec;
	for (nvec = 0; nvec < maxvec; nvec++) {
		if (count == 0)
			break;

		if ((sg->sg_bcount = NBPP - poff(vaddr)) > count)
			sg->sg_bcount = count;

		sg->sg_ioaddr = PHYSADDR(vaddr);
		count -= sg->sg_bcount;
		vaddr += sg->sg_bcount;
		sg++;
	}

	*_resid = count;
	iounmap(bp);
	return nvec;
}

/*
 * display the scatter / gather vector, for debugging
 */
sgprint(vec, nvec)
	struct sg *vec;
	int nvec;
{
	register struct sg *sg;
	register int i;
	register unsigned long count;

	count = 0;
	sg = vec;
	for (i = 0; i < nvec; i++)
		count += sg++ ->sg_bcount;

	printf(" {%d:", count);
	sg = vec;
	for (i = 0; i < nvec; i++) {
		printf(" 0x%x/%d", sg->sg_ioaddr, sg->sg_bcount);
		sg++;
	}
	printf("}\n");
}
