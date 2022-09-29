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
#ident	"$Header: mem.c,v 1.18.1.3 90/05/10 05:22:50 wje Exp $"

/*	Copyright (c) 1984 AT&T	*/
/*	  All Rights Reserved  	*/

/*	THIS IS UNPUBLISHED PROPRIETARY SOURCE CODE OF AT&T	*/
/*	The copyright notice above does not evidence any   	*/
/*	actual or intended publication of such source code.	*/


#include "sys/types.h"
#include "sys/sbd.h"
#include "sys/param.h"
#include "sys/psw.h"
#include "sys/pcb.h"
#include "sys/immu.h"
#include "sys/vmereg.h"
#include "sys/sysmacros.h"
#include "sys/systm.h"
#include "sys/dir.h"
#include "sys/signal.h"
#include "sys/user.h"
#include "sys/errno.h"
#include "sys/firmware.h"
#include "sys/var.h"

#define CKPHYS(addr) ((ulong)(addr) >= ((ulong)ctob(physmem)))

#define READ 	1
#define WRITE 	2

/* mmopen() and mmclose() do nothing but have to be around for lboot */
mmopen()
{
}

mmclose()
{
}

mmread(dev)
{
	mmrw(dev, READ);
}

mmwrite(dev)
{
	mmrw(dev, WRITE);
}

/* check the validity of addresses */
static int				/* 0=ok ENXIO=bad address */
mmck(off,len)
register int off;
register int len;
{
	while (len > 3 && !(off & 3)) { /* first try word accesses */
		if (badaddr(off,4))
			return ENXIO;
		len -= 4;
		off += 4;
	}
	while (len > 1 && !(off & 1)) { /* then check the half word accesses */
		if (badaddr(off,2)) 
			return ENXIO;
		len -= 2;
		off += 2;
	}

	while (len > 0) {		/* then bytes */
		if (badaddr(off,1)) 
			return ENXIO;
		len--;
		off++;
	}

	return 0;
}

mmrw(dev,rw)
{
	dev = minor(dev);

	/*
	 * Access /dev/null
	 */
	if (dev == 2) {
		if (rw == WRITE) {
		    u.u_offset += u.u_count;
		    u.u_base += u.u_count;
		    u.u_count = 0;
		}
		return;
	} 

	/*
	 * Access /dev/mem or /dev/kmem one page at a time.
	 */
	while(u.u_error == 0 && u.u_count != 0)  {
		register ulong n, offset;
		register int rval;

		offset = u.u_offset;
		n = ctob(1) - (offset % ctob(1));
		if (n > (ulong)u.u_count)
			n = u.u_count;
		u.u_offset += n;
		u.u_count -= n;

		/*
		 * Check address validity.
		 */
		switch(dev) {
		case 0:
			/*
			 * For /dev/mem, only allow access to real core.
			 * This may need to be more liberal.
			 */
			if (CKPHYS(offset)) {
				u.u_error = ENXIO;
				return;
			}
			offset = PHYS_TO_K1(offset);	/* Uncached copy. */
			writeback_cache( offset,n );
			break;

		case 1:
			/*
			 * For /dev/kmem, any access to anything valid
			 * is allowed.
			 */

			/*
			 * Allow user to specify kmem addresses as starting
			 * at 0 or 0x80000000. File system will disallow 
			 * the later. 
			 */
			offset |= K0BASE;

			/* In KSEG2, allow simple access only to cached pages.
			 * This is because uncached pages are likely to be
			 * devices which are likely to bus error.
			 *
			 * Accesses to uncached KSEG2 as well as to VME 
			 * addreses in KSEG1 are converted into simple validity
			 * tests.
			 */
			if (IS_KSEG1(offset)) {
				register int phys = K1_TO_PHYS(offset);
				writeback_cache( offset,n );
				if (IS_VME_A16S(phys)
				    || IS_VME_A16NP(phys)
				    || IS_VME_A24S(phys)) {
					u.u_error = mmck(offset,n);
					continue;
#ifdef TODO
/* XXX should detect and reject invalid KSEG1 addresses, such as the hole 
 * around the I.D. PROM */
				} else {
					u.u_error = ENXIO;
					return;
#endif
				}
			} else if (IS_KSEG2(offset)) {
				register pde_t *pde;

				if (pnum(offset - K2SEG) >= syssegsz) {
					u.u_error = ENXIO;
					return;
				}
				
				pde = kvtokptbl(offset);
				if (!pde->pgm.pg_vr) {	/* fail on bad addr */
					u.u_error = ENXIO;
					return;
				}

				if (pde->pgm.pg_n &&	/* if uncached, only */
				    pde->pgm.pg_pfn >= physmem) {
					u.u_error = mmck(offset,n);
					continue;	/* check validity */
				}

			/* notice bad physical addresses */
			} else if (!IS_KSEG0(offset) 
				   || CKPHYS(K0_TO_PHYS(offset))) {
				u.u_error = ENXIO;
				return;
			}
			break;

		default:
			u.u_error = ENXIO;
			return;
		}

		/*
		 * Do the access.
		 */
		if (rw == READ)
			rval = copyout(offset,u.u_base,n);
		else 
			rval = copyin(u.u_base,offset,n);
		u.u_base += n;
		if (rval)
			u.u_error = ENXIO;
	}
}


mmioctl(dev, cmd, arg, mode)
{
	/* Don't have any. */
	u.u_error = EINVAL;
	u.u_roff = -1;
}

mmmmap(dev, off, prot)
{
	if (!suser()) {
		return(-1);
	}
	switch(minor(dev)) {
	case 0:				/* /dev/mem: must be REAL core */
		if (CKPHYS(off)) {
			return(-1);
		}
		/* fall through */
	case 1:				/* /dev/kmem: anything's OK */
		return(pnum(off));
	default:
		return(-1);
	}
}
