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
#ident	"$Header: vme.c,v 1.14.3.2 90/05/10 05:37:35 wje Exp $"


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
#include "sys/reg.h"
#include "sys/debug.h"
#include "sys/cmn_err.h"
#include "sys/vmereg.h"
#include "sys/edt.h"


#define VME_MINIPL 1		/* lowest interrupt level */
#define VME_MAXIPL 7		/* highest interrupt level */

/* We have few devices, and they are well distributed among the IPL levels,
 * so we caching rather than a simple array to map interrupts to functions.
 * This scheme is faster than the previous implementation using an array,
 * tho the array could have been made faster.
 */
#define VME_CACHE(ipl,vec) (vec_cache[ipl])
#define VME_CACHE_HIT(ipl,vec,ptr) ((ptr)->v_vec == (vec))
#define VME_ISR_CONTENTS (*(volatile unsigned char *)PHYS_TO_K1(VME_ISR)) 
static struct vme_intrs *vec_cache[VME_MAXIPL];

vme_intr(ep)
uint *ep;
{
	register unsigned char previpl;
	register int req;

	/* vme interrupt mask */
	previpl = *(volatile unsigned char *)PHYS_TO_K1(VME_IMR);

#ifdef TODO
	ffintr should not know the bit positions in the cause register 
	of the interrupt pending bits
#endif
	/* make sure we do not-- oh what the hell you figure out
	 *	just kidding-- make sure we do not process a
	 *	interrupt that is masked coming into this
	 *	routine. so mask out non-enable bits with previpl.
	 */
	while (req = (ffintr((VME_ISR_CONTENTS & previpl) << CAUSE_IPSHIFT))) {
		register unsigned char vec;
		register struct vme_intrs *intr;

		/* ffintr returns a number from 8 to 1 and we want
		 *	the number 0-based so we can shift a '1' to the
		 *	correct bit location.
		 */
		req--;
		*(volatile unsigned char *)PHYS_TO_K1(VME_IMR) = 0xfe << req;
		wbflush();

/* The following statement can generate a bus error if the 
 *	interrupting device does not respond.  A routine similar to
 *	badaddr() could be used, but would be too expensive.  Instead, 
 *	we will document the panic that results from such sick devices.
 */
		vec = (unsigned char)0xff;	/* Initial to something bad */
		vec = *(u_short *)PHYS_TO_K1(VME_IACK-2+req*2);

		intr = VME_CACHE(req,vec);
		for (;;) {
			if (!intr) {
#ifdef DEBUG
				printf ("Stray at vec 0x%x, ipl %d\n",
					vec, req);
#endif					
				stray(ep);
				break;
			}
			if (VME_CACHE_HIT(req,vec,intr)) {
				intr->v_vintr(intr->v_unit);
				break;
			}
			intr = intr->v_link;
		}
	}
	*(volatile unsigned char *)PHYS_TO_K1(VME_IMR) = previpl;
	wbflush();
}

vme_init()
{
	register int i;

	/* clear VME bus interrupts */
	for (i = VME_MINIPL; i <= VME_MAXIPL; i++) {
		badaddr((unsigned short *)PHYS_TO_K1(VME_IACK-2+i*2),2);
	} /* for */

/* Reset the 0xff to 0xfe to mask out MipsInt always since it is a floating
   signal and may become active */

	*(volatile unsigned char *)PHYS_TO_K1(VME_IMR) = 0xfe;
	wbflush();

	/* create the VME interrupt table */
	for (i = 0; i < nedt; i++) {
		if (edt[i].e_intr_info != NULL) {
			register struct vme_intrs *vme_intr;
			register struct vme_intrs **ip;
			for (vme_intr = edt[i].e_intr_info;
				      vme_intr->v_vintr != NULL;
				      vme_intr++) {
				ip = &VME_CACHE(vme_intr->v_brl,vme_intr->vec);
				vme_intr->v_link = *ip;
				*ip = vme_intr;
			}
		}
	}

}

#ifdef notdef
struct vme_amtok1 {
	unsigned va_am;		/* VME address modifier */
	unsigned va_k1addr;	/* k1 address to generate am */
	unsigned va_maxaddr;	/* max address for am */
} vme_amtok1[] = {
	{ VME_A16NPAMOD, VME_A16NPBASE, 0xffff },
	{ VME_A16SAMOD, VME_A16SBASE, 0xffff },
	{ VME_A24SAMOD, VME_A24SBASE, 0xffffff },
	{ VME_A32NPAMOD, 0, 0x1fffffff },
	{ 0, 0, 0 }
};

vme_to_k1(am, addr)
unsigned addr;
{
	register struct vme_amtok1 *va;

	for (va = vme_amtok1; va->va_maxaddr; va++) {
		if (va->va_am == am) {
			if (addr > va->va_maxaddr) {
				cmn_err(CE_NOTE,"Addr 0x%x unaccessable via am 0x%x\n",
				    addr, am);
				return(0);
			}
			return (PHYS_TO_K1(va->va_k1addr + addr));
		}
	}
	cmn_err(CE_NOTE,"Address modifier 0x%x can not be generated\n", am);
	return(0);
}
#endif
