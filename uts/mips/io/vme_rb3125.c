/* ------------------------------------------------------------------ */
/* | Copyright Unpublished, MIPS Computer Systems, Inc.  All Rights | */
/* | Reserved.  This software contains proprietary and confidential | */
/* | information of MIPS and its suppliers.  Use, disclosure or     | */
/* | reproduction is prohibited without the prior express written   | */
/* | consent of MIPS.                                               | */
/* ------------------------------------------------------------------ */
/* $Header: vme_rb3125.c,v 1.2.2.1.1.1 90/06/28 15:36:33 root Exp $ */

#ident	"$Header: vme_rb3125.c,v 1.2.2.1.1.1 90/06/28 15:36:33 root Exp $"

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
#include "sys/cpu_board.h"


#define VME_MINIPL 1		/* lowest interrupt level */
#define VME_MAXIPL 7		/* highest interrupt level */

/* We have few devices, and they are well distributed among the IPL levels,
 * so we caching rather than a simple array to map interrupts to functions.
 * This scheme is faster than the previous implementation using an array,
 * tho the array could have been made faster.
 */
#define VME_CACHE(ipl,vec) (vec_cache[ipl])
#define VME_CACHE_HIT(ipl,vec,ptr) ((ptr)->v_vec == (vec))
#define VME_ISR_CONTENTS (*(volatile unsigned short *)PHYS_TO_K1(VME_ISR_RB3125)) 
#define	MASK_VALUE	0x1dfe
static struct vme_intrs *vec_cache[VME_MAXIPL];

extern	int (*rb3125_ints[])();

vme_intr(ep)
uint *ep;
{
	register unsigned short isr;
	register int (*fptr)();
	register int req;

#ifdef TODO
	ffintr should not know the bit positions in the cause register 
	of the interrupt pending bits
#endif
	isr = VME_ISR_CONTENTS & MASK_VALUE;
	while (isr) {
		/* Handle RB3125 non-vme interrupts */
		while((req = ffintr(isr))) {
			--req;
			if((fptr = rb3125_ints[req]) != 0) {
				(*fptr)();
			} else {
#ifdef DEBUG
				cmn_err(CE_NOTE,"Stray with req %d\n",req + 8);
#endif					
				stray(ep);
			}
			isr &= ~(1 << (req + CAUSE_IPSHIFT));
		}

	
		isr <<= CAUSE_IPSHIFT;
		while (req = ffintr(isr)) {
			register unsigned char vec;
			register struct vme_intrs *intr;

			/* ffintr returns a number from 8 to 1 and we want
			 *	the number 0-based so we can shift a '1' to the
			 *	correct bit location.
			 */
			req--;

/* The following statement can generate a bus error if the 
 *	interrupting device does not respond.  A routine similar to
 *	badaddr() could be used, but would be too expensive.  Instead, 
 *	we will document the panic that results from such sick devices.
 */
			vec = *(u_short *)PHYS_TO_K1(VME_IACK-2+req*2);

			intr = VME_CACHE(req,vec);
			for (;;) {
				if (!intr) {
#ifdef DEBUG
					cmn_err(CE_NOTE,"Stray at vec 0x%x, ipl %d\n",
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
			isr &= ~(1 << (req + CAUSE_IPSHIFT));
		}
		isr = VME_ISR_CONTENTS & MASK_VALUE;
	}
}

vme_init()
{
	register int i;

	/* clear VME bus interrupts */
	for (i = VME_MINIPL; i <= VME_MAXIPL; i++) {
		badaddr((unsigned short *)PHYS_TO_K1(VME_IACK-2+i*2),2);
	} /* for */

	/* Stop masking the interrupts we will handlle */

	*(volatile unsigned short *)PHYS_TO_K1(VME_IMR_RB3125) = MASK_VALUE;
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

int	cpu_con_shadow;

/*	set_cpucon -- set bit in cpu control register.
 *	Bits in the write only cpu control register are set or cleared.
 */

set_cpu_con(mask, value)

int	mask;		/* Bits in this word will be effected */
int	value;		/* These values will be set in the effected bits */
{
	register int level;

	level = splhi();
	cpu_con_shadow &= ~mask;
	cpu_con_shadow |= (value & mask);
	*((long *)PHYS_TO_K1(CPU_CON_RB3125)) = cpu_con_shadow;
	splx(level);
}
