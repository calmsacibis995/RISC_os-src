#ident "$Header: pon_imr.c,v 1.6.1.1 90/07/18 14:31:45 huang Exp $"
/* $Copyright: |
 * |-----------------------------------------------------------|
 * | Copyright (c) 1988, 1990 MIPS Computer Systems, Inc.      |
 * | All Rights Reserved                                       |
 * |-----------------------------------------------------------|
 * |          Restrictive Rights Legend                        |
 * | Use, duplication, or disclosure by the Government is      |
 * | subject to restrictions as set forth in                   |
 * | subparagraph (c)(1)(ii) of the Rights in Technical        |
 * | Data and Computer Software Clause of DFARS 252.227-7013.  |
 * |         MIPS Computer Systems, Inc.                       |
 * |         928 Arques Avenue                                 |
 * |         Sunnyvale, CA 94086                               |
 * |-----------------------------------------------------------|
 *  $ */

#include "sys/types.h"
#include "mips/cpu.h"
#include "mips/cpu_board.h"
#include "pon.h"

extern char success[], failure[], skipped[];


Pon_Imr()

{
	register volatile u_short *addr = (u_short *)PHYS_TO_K1(IMR);
	register int error;
	register u_int i;
	register u_short j;

	if ((machine_type != BRDTYPE_R2400) && (machine_type != BRDTYPE_M180)){
		return(PASS);
	}

	pon_set_leds(PON_IMR);
	pon_puts("IMR Test...");

	error = 0;
	SetSR(GetSR() & SR_BEV);		/* disable interrupts */
	for (i = 1; i < (1 << 16); i <<= 1) {
		*addr = i;
		j = *addr;
		if (i != j) {
			error++;
#ifdef	DEBUG_C
			printf("IMR: expect %x, actual %x\n", i, j);
#endif	DEBUG_C
		}
	}

	if (error) {
		pon_puts(failure);
		FastFlash(PON_IMR);
		pon_set_leds(PON_IMR);
		SetDepend(PON_FAULT_IMR);
		return(FAIL);
	}

	pon_puts(success);
	return(PASS);
}
