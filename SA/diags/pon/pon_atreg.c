#ident "$Header: pon_atreg.c,v 1.6.7.1 90/07/18 14:29:19 huang Exp $"
/* $Copyright$ */

#include "sys/types.h"
#include "machine/cpu_board.h"
#include "machine/cpu.h"
#include "pon.h"

extern char success[], failure[];

/*
 * This diag walk's one's through the the PCAT register and verifies the data 
 */
Pon_Atreg()

{
	register volatile u_short *at = (u_short*)PHYS_TO_K1(PCAT);
	register u_short i;

	if ((machine_type != BRDTYPE_R2400) && (machine_type != BRDTYPE_M180)){
		return(PASS);
	}

	pon_set_leds(PON_ATREG);
	pon_puts("AT Register Test...");
	
#ifdef	SABLE				/* sable doesn't support AT yet */
	pon_puts(success);
	return(PASS);
#endif
	SetSR(GetSR() & SR_BEV);		/* disable interrupts */
	SetSCR(GetSCR() & ~SCR_NORSTPCATBUS);	/* hold PCAT Bus in reset */

	for (i = 1; i <= 16; i = (i << 1)) {
		*at = i;
		FlushWB();
		if (*at != i) {
			*at = 0;
			FlushWB();
			pon_puts(failure);
			FastFlash(PON_ATREG);
			pon_set_leds(PON_ATREG);
			SetDepend(PON_FAULT_AT);
			return(FAIL);
		}
	}

	*at = 0;
	FlushWB();

	pon_puts(success);
	return(PASS);
}
