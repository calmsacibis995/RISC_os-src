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
#ident	"$Header: r6000mem.c,v 1.2.1.3.1.1.1.2 90/10/26 16:12:16 beacker Exp $"

/*
 * Memory Error Correcting Code handler.
 * RC62x0
 *
 * To run the system without printing lots of ecc error messages to the console,
 * use the boot-time switch ecc_noprint.
 * This is intended to let you run with a dead bit if that is important to you.
 * ECC is still enabled and causes traps, but error messages
 * are not printed for single-bit errors.
 *
 */

#include "sys/sbd.h"
#include "sys/types.h"
#include "sys/cmn_err.h"
#include "sys/bc.h"
#include "sys/ioa.h"

int		lmem_errcount;	/* system-total of ecc errors		*/

extern int	ecc_noprint;	/* if nonzero, don't print single-bit errs */

extern int	memory_ctlspace_vaddr[];
extern int	memory_count;



/*
 * This routine defines the maximum size of memory that should
 * be probed for valid addresses in the startup routine.
 * Until we support more than 384MB, we'll set this to 384MB.  That's the
 * base of the third IOC's GBA0 space.
 */
void
lmem_setmax()
{
	extern char	*mem_test_max;	/* max to test in startup	*/

	mem_test_max = (char *)(IOA3 - K1BASE);
}


void
lmem_err_scan()
{
}


/*
 * ECC error interrupt handler
 *
 * All memory cards share a single interrupt vector for simplicity
 * so look at all the cards and see where the error is.
 */
void
mem_error_intr(ep)
int *ep;
{
    int		memory;
    register 	MemCtl memctl;		/* SBC mem control reg */
    register	MemEcc memecc;		/*     ecc register */
    register	CtlMisc *ctl_misc;	/*     misc control reg */


    for (memory = 1; memory <= memory_count; memory++) {
	memctl = *(MemCtl *)(memory_ctlspace_vaddr[memory-1] + SBC_MEMCTL);
	if (memctl.wd & (SingleBitErr | MultiBitErr)) {

	    lmem_errcount++;

	    memecc = *(MemEcc *)(memory_ctlspace_vaddr[memory-1] + SBC_MEMECC);

	    ctl_misc = (CtlMisc *)(memory_ctlspace_vaddr[memory-1]
					 + SBC_CTLMISC);

	    if (memctl.wd & MultiBitErr)
		cmn_err(CE_WARN,"Mem:  ECC MULTI-BIT error: slot %d"
			,ctl_misc->f.sltno);

	    if (memctl.wd & SingleBitErr  &&  ecc_noprint == 0) {
		cmn_err(CE_WARN,"Mem:  ECC SINGLE-BIT error: slot %d, bank 0x%x, syndrome 0x%x"
		       ,ctl_misc->f.sltno, memecc.f.bank & 0x7, memecc.f.synd);
	    }

	    /* get ready for next error */
	    memctl.wd &= ~(SingleBitErr | MultiBitErr);	/* reset */
	    *(volatile MemCtl *)(memory_ctlspace_vaddr[memory-1] + SBC_MEMCTL)
		    = memctl;			/* and write back */
	} /* detected error */
    } /* for each memory board */

}
