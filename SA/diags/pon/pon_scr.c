#ident "$Header: pon_scr.c,v 1.7.1.1 90/07/18 14:32:50 huang Exp $"
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
#include "machine/cpu.h"
#include "machine/cpu_board.h"
#include "pon.h"

#define	SCR_MASK	0xff00

u_short pats[] = {
	SCR_PAREN,
	SCR_FORCEPAR,
	SCR_SLOWUDCEN,
	SCR_ATTC,
	SCR_NORSTPCATBUS,
	SCR_EOP9516,
	SCR_SCSIHIN,
	SCR_RSTSCSI,
};

#define	SIZE		(sizeof(pats) / sizeof(u_short))

extern char success[], failure[], skipped[];


Pon_Scr()

{
	register volatile u_short *scr = (u_short *)PHYS_TO_K1(SCR);
	register int error;
	register u_short read;
	register u_short write;
	register u_int i;

	if ((machine_type != BRDTYPE_R2400) && (machine_type != BRDTYPE_M180)){
		return(PASS);
	}

	pon_set_leds(PON_SCR);
	pon_puts("SCR Test...");
	error = 0;
	for (i = 0; i < SIZE; i++) {
		write = pats[i];
		*scr = write;
		read = *scr;
		*scr = 0;			/* in case it's a parity bit */
		if (write != (read & SCR_MASK)) {
			error++;
		}
	}

	*scr = 0;
	FlushWB();

	if (error) {
		pon_puts(failure);
		pon_set_leds(PON_SCR);
		FastFlash(PON_SCR);
		SetDepend(PON_FAULT_SCR);
		return(FAIL);
	}

	pon_puts(success);
	return(PASS);
}
