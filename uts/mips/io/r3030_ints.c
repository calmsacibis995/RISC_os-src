/* ------------------------------------------------------------------ */
/* | Copyright Unpublished, MIPS Computer Systems, Inc.  All Rights | */
/* | Reserved.  This software contains proprietary and confidential | */
/* | information of MIPS and its suppliers.  Use, disclosure or     | */
/* | reproduction is prohibited without the prior express written   | */
/* | consent of MIPS.                                               | */
/* ------------------------------------------------------------------ */
/* $Header: r3030_ints.c,v 1.2.1.5.1.2 90/07/11 18:33:33 hawkes Exp $ */

#ident	"$Header: r3030_ints.c,v 1.2.1.5.1.2 90/07/11 18:33:33 hawkes Exp $"

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
 * R3030 series level 0 interrupt routines.
 *
 * This file contains r3030_intr which is called upon all level 0
 * interrupts.  It is the job of this routine to dispatch interrupts
 * to the approriate devices on this machines.
 *
 * The list of devices this routine must support are:
 *
 *	SCSI, Lance, ATBUS, Frame Buffer
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

extern int (*MBUS_interrupt)(), (*LANCE_interrupt)(), (*ASC_interrupt)();
extern int (*SCC_interrupt)(), (*KBD_interrupt)();
extern int (*RAMBO_interrupt)();

/*
 * r3030_intr -- Level 0 interrupt handler for R3030.  This
 *		 as you will notice is a very simple routine.  We basically
 *		 walk through the ISR and call interrupt routines.  And
 *		 when we are done, we return.  The ATBUS info structure was
 *		 set up by the r3030_init routine, and if we get an ATBUS
 *		 interrupt, we will simply call all the interrupts
 *		 associated with that IRQ
 */

#define	INT_REG	0x19800000

#define	INT_LANCE	(1<<4)
#define	INT_NCR		(1<<3)
#define	INT_SCC		(1<<2)
#define	INT_KBD		(1<<1)
#define	INT_SLOT	(1<<0)


r3030_intr(ep)
uint *ep;
{
	register i, j;
	volatile int *intreg = (int *)(PHYS_TO_K1(INT_REG));

     i = ~(*intreg);

	if (i & INT_SCC)
		(*SCC_interrupt)();
#if !SABLE
	if (i & INT_LANCE)
		(*LANCE_interrupt)();
	if (i & INT_KBD)
		(*KBD_interrupt)();
#endif

	if (i & INT_SLOT){
		for (i = 0; i < num_atbus_info; i++) {
			for (j = 0; j < MAX_INT_IRQ; j++) {
				if (!atbus_info[i].irq_service[j])
				  break;		/* Were Done */
				(*atbus_info[i].irq_service[j])();
			}
		}
	}
}

r3030_intr1(ep)
uint *ep;
{
	register i, j;
	volatile int *intreg = (int *)(PHYS_TO_K1(INT_REG));

	i = ~(*intreg);

	if (i & INT_NCR)
		(*ASC_interrupt)();
	else
		(*RAMBO_interrupt)();

}

/*
 * r3030_init -- Initialize data struture so that fielding of these
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

r3030_init()
{
	register int i, j, k;
	volatile u_short *tmpscr, tmpimr;
	register struct atbus_intrs *intrs_list;
	
	/* Start PCAT bus */

	/*
	 * Mask ALL interrupts because we are NOT ready to handle any.
	 * r3030_add_onboard() which is called later will handle this.
	 */

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

r3030_no_interrupt()
{
	printf("r3030_no_interrupt: Stray interrupt\n");
}

/*
 * r3030_add_onboard() -- This routine will unmask the interrupts for all
 *	of the MIPS specific devices that we have on board.  The reason for
 *	this is because when we run r3030_init, it is too early to enable
 *	interrupts because if the system has just been rebooted, residule
 *	interrupts may be lying around, and you end up taking one before you
 *	are ready.  So the r3030_init routine masks ALL interrupts, this
 *	routine will unmask onboard interrupts, and other drivers will
 *	unmask their interrupts when they are ready to handle them. 
 */

r3030_add_onboard()
{
	volatile ushort tmpimr = 0;
	int s;

	if ( (int)LANCE_interrupt != (int)r3030_no_interrupt )
		tmpimr |= INT_ENET;
/*
	if ( (int)SPC_interrupt != (int)r3030_no_interrupt )
		tmpimr |= INT_SCSI;
 */
	if ( (int)MBUS_interrupt != (int)r3030_no_interrupt )
		tmpimr |= INT_MBUS;

	s = splclock();
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

#ifdef	DOINTDEBUG
/*
 * this is the companion routine to those in spl_r3030.s
 *
 * This routines takes two arguments, the old sr, the new sr and the
 * ra where it was called from.
 * we keep a stack of 20 elements for each interrupt level, and push/pop them
 * according to the spl/ail/iil we receive.
 */
struct	intdebugs	{
	unsigned long	ra;
	unsigned long	t;
};

static char	*intlevels[] = {
	"SW 0 (softclock)   ",
	"SW 1 (softnet)     ",
	"HW 0 (ethernet..)  ",
	"HW 1 (SCSI/RAMBO)  ",
	"HW 2 (timer)       ",
	"HW 3 (FPU)         ",
	"HW 4 (Floppy)      ",
	"HW 5 (Video/parity)"
};

#define	NUM_LEVEL	5

struct	intdebugs	intdebug_disable[8][NUM_LEVEL];
struct	intdebugs	intdebug_enable[8][NUM_LEVEL];
unsigned long		intdebug_level[8];
unsigned long		intdebug_bad = 0;

#define	INT_LEVEL1	(SR_IMASK0 ^ SR_IMASK1)
#define	INT_LEVEL2	(SR_IMASK1 ^ SR_IMASK2)
#define	INT_LEVEL3	(SR_IMASK2 ^ SR_IMASK3)
#define	INT_LEVEL4	(SR_IMASK3 ^ SR_IMASK4)
#define	INT_LEVEL5	(SR_IMASK4 ^ SR_IMASK5)
#define	INT_LEVEL6	(SR_IMASK5 ^ SR_IMASK6)
#define	INT_LEVEL7	(SR_IMASK6 ^ SR_IMASK7)
#define	INT_LEVEL8	(SR_IMASK7 ^ SR_IMASK8)
#define	TIME		(unsigned)62500

intdebug(old, new, ra)
int	old, new, ra;
{
	register int	enable;
	register int	disable;
	register int	count;
	register int	mask;
	register unsigned long t = *(unsigned long *)(0xbc000c00);
#define	multiuser	(*(unsigned long *)0x800003d8)

	enable = ~old & new;
	disable = old & ~new;
	for(count = 0, mask = 0x100; count != 8; count++, mask <<= 1) {
		if (disable & mask){
			if (intdebug_level[count]) {
#if 0
				printf("Re-disabled %s at 0x%x (old=0x%x)\n",
					intlevels[count], ra,
					intdebug_disable[count][intdebug_level[count]-1].ra);
#endif
			}
			if (intdebug_level[count] < NUM_LEVEL) {
				intdebug_disable[count][intdebug_level[count]].ra = ra;
				intdebug_disable[count][intdebug_level[count]].t = t;
				intdebug_level[count]++;
			}
		}
		if (enable & mask) {
			if (intdebug_level[count] == 0) {
#if 0
				printf("Re-enabled %s at 0x%x\n", intlevels[count], ra);
#endif
			} else {
				--intdebug_level[count];
				intdebug_enable[count][intdebug_level[count]].ra = ra;
				intdebug_enable[count][intdebug_level[count]].t = t;
				intdebug_bad++;
#if 0
				if ((t - intdebug_disable[count][intdebug_level[count]].t) > TIME)
			 	{
					printf("%s disabled 0x%x:0x%x, enabled 0x%x:0x%x, diff = %d\n",
					intlevels[count],
					intdebug_disable[count][intdebug_level[count]].ra,
					intdebug_disable[count][intdebug_level[count]].t,
					intdebug_enable[count][intdebug_level[count]].ra,
					t,t - intdebug_disable[count][intdebug_level[count]].t);
				}
#endif
			}
		}
	}
}
#endif	DOINTDEBUG
