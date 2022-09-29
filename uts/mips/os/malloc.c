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
#ident	"$Header: malloc.c,v 1.6.1.2 90/05/10 05:50:28 wje Exp $"

/*	Copyright (c) 1984 AT&T	*/
/*	  All Rights Reserved  	*/

/*	THIS IS UNPUBLISHED PROPRIETARY SOURCE CODE OF AT&T	*/
/*	The copyright notice above does not evidence any   	*/
/*	actual or intended publication of such source code.	*/

#include "sys/param.h"
#include "sys/types.h"
#include "sys/systm.h"
#include "sys/sysmacros.h"
#include "sys/map.h"
#include "sys/sbd.h"
#include "sys/immu.h"
#include "sys/cmn_err.h"
#include "sys/debug.h"

/*
 * Allocate 'size' units from the given map.
 * Return the base of the allocated space.
 * In a map, the addresses are increasing and the
 * list is terminated by a 0 size.
 * The swap map unit is 512 bytes.
 * Algorithm is first-fit.
 */
unsigned int
malloc(mp, size)
register struct map *mp;
register int size;
{
	register unsigned int a;
	register struct map *bp;

	if (size == 0)
		return(NULL);
	ASSERT(size > 0);
	for (bp = mapstart(mp); bp->m_size; bp++) {
		if (bp->m_size >= size) {
			a = bp->m_addr;
			bp->m_addr += size;
			if ((bp->m_size -= size) == 0) {
				do {
					bp++;
					(bp-1)->m_addr = bp->m_addr;
				} while ((bp-1)->m_size = bp->m_size);
				mapsize(mp)++;
			}
			ASSERT(bp->m_size < 0x80000000);
			return(a);
		}
	}
	return(0);
}


#ifdef PERFECT_COLORING

/* Allocate 'size' units from the given map,
 * returning the base of an area which starts on the same secondary
 * cache color boundary as 'vpn'.
 * Algorithm is first-fit.
 */

malloc_scc(mp, size, vpn)
struct map *mp;
{
	register int a, b;
	register int gap, tail;
	register struct map *bp;
	register unsigned int pg_color;
	
	extern unsigned int scachemask;

	ASSERT(size >= 0);
	pg_color = vpn & scachemask;
	
	for (bp = mapstart(mp); bp->m_size; bp++) {
		if (bp->m_size >= size) {
			a = bp->m_addr;
			b = a;
			while ( (b & scachemask) != pg_color)
			  b++;	/* move to next page */
			gap = b - a;
			if (bp->m_size < (gap + size))
				continue;
			if (gap != 0) {
				tail = bp->m_size - size - gap;
				bp->m_size = gap;
				if (tail) 
					mfree(mp, tail, bp->m_addr+gap+size);
			} else {
				bp->m_addr += size;
				if ((bp->m_size -= size) == 0) {
					do {
						bp++;
						(bp-1)->m_addr = bp->m_addr;
					} while ((bp-1)->m_size = bp->m_size);
					mapsize(mp)++;
				}
			}
			ASSERT(bp->m_size < 0x80000000);
			return(b);
		}
	}
	return(0);
}
#endif PERFECT_COLORING


/*
 * Free the previously allocated space a
 * of size units into the specified map.
 * Sort a into map and combine on
 * one or both ends if possible.
 */
mfree(mp, size, a)
register struct map *mp;
register int	size;
register unsigned int a;
{
	register struct map *bp;
	register unsigned int t;

	if (size == 0)
		return;
	ASSERT(size > 0);
	bp = mapstart(mp);
	for (; bp->m_addr<=a && bp->m_size!=0; bp++);
	if (bp>mapstart(mp) && (bp-1)->m_addr+(bp-1)->m_size == a) {
		(bp-1)->m_size += size;
		if(bp->m_addr){		/* m_addr==0 end of map table */
			ASSERT(a+size <= bp->m_addr);
			if (a+size == bp->m_addr) { 

				/* compress adjacent map addr entries */
				(bp-1)->m_size += bp->m_size;
				while (bp->m_size) {
					bp++;
					(bp-1)->m_addr = bp->m_addr;
					(bp-1)->m_size = bp->m_size;
				}
				mapsize(mp)++;
			}
		}
	} else {
		if (a+size == bp->m_addr && bp->m_size) {
			bp->m_addr -= size;
			bp->m_size += size;
#ifdef DEBUG
			{
			int testmap = 
			bp == mapstart(mp)  ||
				((bp - 1)->m_addr + (bp - 1)->m_size) <
				bp->m_addr;
			ASSERT(testmap);
			}
#endif
		} else {
			ASSERT(size);
			if (mapsize(mp) == 0) {
				cmn_err(CE_WARN,"mfree map overflow %x.\
	Lost %d items at %d\n",mp,size,a) ;
				return;
			}
			do {
				t = bp->m_addr;
				bp->m_addr = a;
				a = t;
				t = bp->m_size;
				bp->m_size = size;
				bp++;
			} while (size = t);
			mapsize(mp)--;
		}
	}
	if (mapwant(mp)) {
		mapwant(mp) = 0;
		wakeup((caddr_t)mp);
	}
}

#ifdef	u3b2
/*
 * Allocate `size' clicks of memory in segment sized chunks.
 * If successful, return 1 and set the segment descriptors correctly.
 * If not successful, return 0 without having allocated any memory.
 */
segall(segments, size, access)
register sde_t * segments;
int size;
int access;
{

	register sde_t *segp;
	int nseg;
	unsigned click;
	register unsigned total;

	segp = segments;
	nseg = ctos(size);	/* number of segments */

	total = size;
	while (nseg--) {

		if (0 == (click=malloc(coremap,total>NCPS? NCPS:total))) {
			/* memory not available */
			while (segments < segp) {
				mfree(coremap, motoc(segments->maxoff),
					btoc(segments->wd2.address));
				segments->flags &= ~SDE_V_bit;
				segments++;
			}
			return(0);
		}

		segp->flags |= SDE_flags ;
		segp->access = access;
		segp->maxoff = total>NCPS? ctomo(NCPS) : ctomo(total);
		segp->wd2.address = ctob(click);

		++segp;
		total -= NCPS;
	}
	return(1);
}
#endif
