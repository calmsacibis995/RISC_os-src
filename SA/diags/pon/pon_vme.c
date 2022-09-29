#ident "$Header: pon_vme.c,v 1.5.3.1 90/07/18 14:34:07 huang Exp $"
/* $Copyright: |
 * |-----------------------------------------------------------|
 * | Copyright (c) 1989, 1990 MIPS Computer Systems, Inc.      |
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

/* ------------------------------------------------------------------ */
/* | Copyright Unpublished, MIPS Computer Systems, Inc.  All Rights | */
/* | Reserved.  This software contains proprietary and confidential | */
/* | information of MIPS and its suppliers.  Use, disclosure or     | */
/* | reproduction is prohibited without the prior express written   | */
/* | consent of MIPS.                                               | */
/* ------------------------------------------------------------------ */

#include "sys/types.h"
#include "machine/cpu.h"
#include "machine/cpu_board.h"
#include "machine/mem_board.h"
#include "machine/r3250reg.h"
#include "mipsvme/vmereg.h"
#include "pon_ecc.h"
#include "pon.h"

#define	VMESPACE 0x100000

extern char success[], failure[], skipped[];
char f_slot[8];


Pon_Vme()

{
	register u_short read;
	register u_short write,*n;
	register u_int i,ar,ar_save,*m;
	register char brd_count,k,*p;

	if ((machine_type != BRDTYPE_R3200) && (machine_type != BRDTYPE_RB3125))
	{
		return(PASS);
	}

	pon_set_leds(PON_VME);
	pon_puts("VME Test...");

	if (GetDepend() & (PON_FAULT_CACHE | PON_FAULT_MEM)) {
		pon_puts(skipped);
		goto norun;
	}

	for(i=0; i<8; i++)
		f_slot[i] = 0xff;

	brd_count = dmemconfig();

	for(i=0; i< brd_count; i++){

		if(f_slot[i] != 0xff || f_slot[i] != 0)
			continue;
		ar_save = ar = GetMemIoReg(MEM_ADDR,f_slot[i]);

		/* create new value for the address register */
			ar = (ar_save & 0xff00) | ((ar_save >> 8) ^ 0x10);

		/* write new value */
			PutMemIoReg(MEM_ADDR,f_slot[i],ar);

		/* determine type of memory board */

k = ((GetMemIoReg(MEM_PROMID,f_slot[i]) & SIZE_MASK) == SIZE_16MEG) ? 2 : 4;

		while (k--){
		     m = (u_int*)(((ar & 0xff00) << 16) | VMESPACE | (k << 23));
			m = (u_int *)PHYS_TO_VME(m);
			n = (u_short *)m;
			p = (char *)m;

			for(i=0; i< 0x3ff; i++){
				m[i] = (u_int)&m[i];
				if(m[i] != (u_int)&m[i])
					goto failed;
			}
			for(i=0; i< 0x3ff; i++){
				n[i] = i;
				if(n[i] != i)
					goto failed;
			}
			for(i=0; i< 0x3ff; i++){
				p[i] = ~i;
				if(p[i] != ~i)
					goto failed;
			}

		/* restore boards memory address register value */

			PutMemIoReg(MEM_ADDR,f_slot[i],ar_save);

		}
	}
	pon_puts(success);
	return(PASS);

failed:
/* restore boards memory address register value */

	PutMemIoReg(MEM_ADDR,f_slot[i],ar_save);

	pon_set_leds(PON_VME);
	pon_puts(failure);
	return(FAIL);


norun:
	  SetDepend(PON_FAULT_VME);
	  return(FAIL);
}
