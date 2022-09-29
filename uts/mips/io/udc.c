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
#ident	"$Header: udc.c,v 1.1.4.2 90/05/10 05:37:27 wje Exp $"
#include "sys/debug.h"
#include "sys/types.h"
#include "sys/param.h"
#include "sys/errno.h"
#include "sys/sbd.h"
#include "sys/cpu_board.h"
#include "sys/am9516.h"

#undef DEBUGRPH

#define AMD_BASE	(*(struct am9516 *)PHYS_TO_K1(AMD_BASE_R2400))
#define AMD_ADDR	(&AMD_BASE)
#define AMD_REG		struct am9516

udcinit()
{
	register AMD_REG *amd = AMD_ADDR;
	register u_short *syscon = (u_short *)PHYS_TO_K1(SCR);
	register zero = 0;

#ifndef SABLE
	if (badaddr(&amd->base, sizeof(amd->base))) {
		printf("amd udc chip not addressable at 0x%x\n", &amd->base);
		return(-1); /* catastrophic failure (panic) */
	}
#endif SABLE
	/* initialize the UDC
	 */
	amd->ptr = COMMAND1;  wbflush(); /* select ch 1 command reg */
	amd->base = RESET_CMD; wbflush(); /* and reset it */
	DELAY(100); /* allow ~100us for the reset to take hold */
	amd->ptr = MASTER_MODE; wbflush(); /* select reg */
	if (amd->base) {
		printf("after reset UDC mode was 0x%x it should be 0\n",
							amd->base);
/*
		goto bad;
 */
	}
#ifdef notdef
	amd->ptr = STATUS1; wbflush(); /* select ch 1 status reg */
	if ((amd->base) != RESET_STATUS) {
		printf("after reset ch 1 status was 0x%x should be 0x%x\n",
							amd->base,RESET_STATUS);
	}
#endif notdef
	amd->ptr = MASTER_MODE; wbflush(); /* select reg */
	amd->base = CHIP_ENABLE|CHIP_WAIT_EN|CHIP_NOVEC;
#ifdef DEBUGRPH
	printf("after 9516 initialization\n");
	udc_regs(); /* dump AMD register values */
#endif DEBUGRPH
}
udcintr()
{
	printf("phantom UDC interrupt\n");
}
