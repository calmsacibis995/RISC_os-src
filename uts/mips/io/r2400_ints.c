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
#ident	"$Header: r2400_ints.c,v 1.7.3.3 90/05/10 05:25:46 wje Exp $"


#include "sys/sbd.h"
#include "sys/cpu_board.h"
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

/*
 * R2400 series (Intrepid) level 0 interrupt routines.
 *
 * This file contains r2400_intr which is called upon all level 0
 * interrupts.  It is the job of this routine to dispatch interrupts
 * to the approriate devices on this machines.
 *
 * The list of devices this routine must support are:
 *
 *	SCSI, Lance, DMA, ATBUS, Frame Buffer
 *
 */

#define MAX_INT_IRQ	5		/* Maximum number of ATBUS ints */
                                        /* allowed at the same IRQ      */
struct atbus_info {
	int irq;
	int mask;
	int (*irq_service[MAX_INT_IRQ])();
} atbus_info[] = {

	{  9, INT_IRQ9   },
	{ 10, INT_IRQ10  },
	{ 11, INT_IRQ11  },
	{ 12, INT_IRQ12  },
	{ 14, INT_IRQ14  },
	{ 15, INT_IRQ15  },
	{  3, INT_IRQ3   },
	{  4, INT_IRQ4   },
	{  5, INT_IRQ5   },
	{  6, INT_IRQ6   },
	{  7, INT_IRQ7   }
};

int num_atbus_info = (sizeof(atbus_info) / sizeof(struct atbus_info));

extern struct edt edt[];
extern int nedt;

extern int (*MBUS_interrupt)(), (*LANCE_interrupt)(), (*SPC_interrupt)();
extern int (*UDC_interrupt)();

/*
 * r2400_intr -- Level 0 interrupt handler for Intrepid (R2400).  This
 *		 as you will notice is a very simple routine.  We basically
 *		 walk through the ISR and call interrupt routines.  And
 *		 when we are done, we return.  The ATBUS info structure was
 *		 set up by the r2400_init routine, and if we get an ATBUS
 *		 interrupt, we will simply call all the interrupts
 *		 associated with that IRQ
 */

r2400_intr(ep)
uint *ep;
{
	register volatile u_short intrpend, previmr;
	register i, j;

	/* previous interrupt mask */
	previmr = *(volatile u_short *)PHYS_TO_K1(IMR);
	/* current pending interrupts */
	intrpend = *(volatile u_short *)PHYS_TO_K1(ISR) & previmr;
	if(!intrpend)
	    printf("called r2400_intr without isr set\n");

	if (intrpend & INT_ENET)	/* Lance interrupt */
	  (*LANCE_interrupt)();

	if (intrpend & INT_SCSI)	/* SPC interrupt */
	  (*SPC_interrupt)();

	if (intrpend & INT_DMA)		/* UDC interrupt */
	  (*UDC_interrupt)();

	if (intrpend & INT_MBUS)	/* MBUS interrupt (frame buffers) */
	  (*MBUS_interrupt)();

	for (i = 0; i < num_atbus_info; i++) {

		if ( (intrpend & INT_ATMASK) == 0 )	/* No more */
		  break;

		if (intrpend & atbus_info[i].mask) {

			if (!atbus_info[i].irq_service[0]) {
				printf("r2400_intr: Stray intr: irq 0x%x\n",
				       atbus_info[i].irq);
				break;
			}

			for (j = 0; j < MAX_INT_IRQ; j++) {
			  if (!atbus_info[i].irq_service[j])
			    break;		/* Were Done */
			  (*atbus_info[i].irq_service[j])(atbus_info[i].irq);
			}

			intrpend &= ~atbus_info[i].mask;
		}
	}
}

/*
 * r2400_init -- Initialize data struture so that fielding of these
 *		 interrupts will be eased.  What we do is first walk
 *		 through the edt[] table looking for ATBUS devices.
 *		 These are found by a non-zero pointer in the
 *		 e_atbusintr_info field.  If this data is non-zero,
 *		 there it points to an array of ATBUS interrupt specifications.
 *		 We then walk through that list, matching up IRQ levels
 *		 with that of the pre-built priority table.  When we
 *		 find a match, we add the interrupt routine to the list
 *		 of interrupt routines to call when an interrupt of
 *		 that level is received.  All ATBUS interrupts are fielded
 *		 after the more important interrupts (Lance, SCSI, UDC, MBUS).
 */

r2400_init()
{
	register int i, j, k;
	volatile u_short *tmpscr, tmpimr;
	register struct atbus_intrs *intrs_list;
	
	tmpscr = (volatile u_short *)PHYS_TO_K1(SCR);
	*tmpscr = *tmpscr | SCR_NORSTPCATBUS;		/* Start PCAT bus */
	wbflush();

	/*
	 * Mask ALL interrupts because we are NOT ready to handle any.
	 * r2400_add_onboard() which is called later will handle this.
	 */
	*(volatile u_short *)PHYS_TO_K1(IMR) = 0;
	wbflush();

	for (i = 0; i < nedt; i++ ) {

		if (!edt[i].e_atbusintr_info)
		  continue;

		intrs_list = edt[i].e_atbusintr_info;

		while (intrs_list->a_aintr) {
			for (j = 0; j < num_atbus_info; j++)
			  if (atbus_info[j].irq == intrs_list->a_irq) {
				  for (k = 0; k < MAX_INT_IRQ; k++)
				    if (atbus_info[j].irq_service[k] == 0) {
					    atbus_info[j].irq_service[k] =
					      intrs_list->a_aintr;
					    j = num_atbus_info; /* Force out */
					    break;
				    }
			  }
			intrs_list++;
		}
	}

}

r2400_no_interrupt()
{
	printf("r2400_no_interrupt: Stray interrupt\n");
}

/*
 * r2400_add_onboard() -- This routine will unmask the interrupts for all
 *	of the MIPS specific devices that we have on board.  The reason for
 *	this is because when we run r2400_init, it is too early to enable
 *	interrupts because if the system has just been rebooted, residule
 *	interrupts may be lying around, and you end up taking one before you
 *	are ready.  So the r2400_init routine masks ALL interrupts, this
 *	routine will unmask onboard interrupts, and other drivers will
 *	unmask their interrupts when they are ready to handle them. 
 */

r2400_add_onboard()
{
	volatile ushort tmpimr = 0;
	int s;

	if ( (int)LANCE_interrupt != (int)r2400_no_interrupt )
		tmpimr |= INT_ENET;

	if ( (int)SPC_interrupt != (int)r2400_no_interrupt )
		tmpimr |= INT_SCSI;

	if ( (int)UDC_interrupt != (int)r2400_no_interrupt )
		tmpimr |= INT_DMA;

	if ( (int)MBUS_interrupt != (int)r2400_no_interrupt )
		tmpimr |= INT_MBUS;

	s = splclock();
	*(volatile ushort *)PHYS_TO_K1(IMR) |= tmpimr;
	wbflush();
	splx(s);
	if (showconfig)
		printf("IMR: 0x%x (on board interrupts enabled)\n", 
			*(volatile u_short *)PHYS_TO_K1(IMR)); 

}

/*
 * The following routines allow a user to selectiviely mask/unmask certain
 * IRQ levels for the ATBUS.  The irq_unmask() routine should be used in
 * a driver init routine once state has been set up enough to accept
 * interrupts.  One might use irq_mask() to mask off interrupts to perhaps
 * take a driver out of service for some period of time.
 */

irq_unmask(irq_level)
int irq_level; {

	register int i, s;
	register volatile u_short tmpimr;

	tmpimr = *(volatile u_short *)PHYS_TO_K1(IMR); /* Grab a copy... */

	for (i = 0; i < num_atbus_info; i++)
		if (irq_level == atbus_info[i].irq) {
			tmpimr |= atbus_info[i].mask;
			s = splclock();
				*(volatile u_short *)PHYS_TO_K1(IMR) = tmpimr;
				wbflush();
			splx(s);
			return(1);
		}

	printf("irq_unmask: did not find mask for irq %d\n",irq_level);
	return(0);
}
		
irq_mask(irq_level)
int irq_level; {

	register int i, s;
	register volatile u_short tmpimr;

	tmpimr = *(volatile u_short *)PHYS_TO_K1(IMR); /* Grab a copy... */

	for (i = 0; i < num_atbus_info; i++)
		if (irq_level == atbus_info[i].irq) {
			tmpimr &= ~(atbus_info[i].mask);
			s = splclock();
				*(volatile u_short *)PHYS_TO_K1(IMR) = tmpimr;
				wbflush();
			splx(s);
			return(1);
		}

	printf("irq_mask: did not find mask for irq %d\n",irq_level);
	return(0);
}
			
/* Stubs for the 3030 */
void
interrupt_at(time, routine, arg)
unsigned long time;
void (*routine)();
char *arg;
{
}

unsigned long
rambodelay(time)
unsigned long time;
{
	return ((unsigned long)0);
}
