#ident "$Header: pon_chain2.c,v 1.7.5.1 90/07/18 14:30:23 huang Exp $"
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

#define	NOREGS

#include "sys/types.h"
#include "mips/cpu_board.h"
#include "mips/cpu.h"
#include "saio/saioctl.h"
#include "mips/am9516.h"
#include "mips/m120scsi.h"
#include "pon.h"

extern char success[], failure[], skipped[];

/* 
 * This diag does a chain operation from both channels. The values loaded into
 * the registers are read back and verified where possible. 
 */
Pon_Chain2()

{
	register volatile struct am9516 *udc = (struct am9516 *)PHYS_TO_K1(AMD_BASE_R2400);
	register volatile struct scsisge *buf;
	register volatile u_char delay; 
	register volatile u_short pattern, pattern1;

	if ((machine_type != BRDTYPE_R2400) && (machine_type != BRDTYPE_M180)){
		return(PASS);
	}

	pon_puts("UDC Channel 2 Chain Test...");

#ifndef	SABLE

	if (GetDepend() & (PON_FAULT_MEM | PON_FAULT_SCR | PON_FAULT_UDC)) {
		pon_puts(skipped);
		goto norun;			/* dependency check failed */
	}

	pon_set_leds(PON_CHAIN2);
	pattern = 0xaaaa;
	pattern1 = 0xface;
	buf = (struct scsisge *)0xa0500000;

/* Set up the Master Mode register */

	udc->ptr = MASTER_MODE;
	udc->base = CHIP_ENABLE | CHIP_INTLV_EN | CHIP_WAIT_EN | CHIP_NOVEC;

/* Set up the chain buffer in memory */

	buf->reload_word = LD_CUR_ARA | LD_CUR_OPCNT | LD_CHAIN_ADDR | LD_CHAN_MODE;
	buf->mem_ptr[0] = pattern;
	buf->mem_ptr[1] = ~pattern;
	buf->count = pattern1;
	buf->chanmode[0] = DD_BUSREL | NOFLIP | WORD_FLYBY | NOFLIP;
	buf->chanmode[1] = 0;
	buf->next_blk[0] = buf->next_blk[1] = 0;

/* Load chain address register */

	buf = (struct scsisge *)((int)buf & 0x1fffffff);
	udc->ptr = CAR2_HI;
	udc->base = (u_short)((((int)buf >> 8) & 0xff00) | ONEWAIT);
	FlushWB();
	udc->ptr = CAR2_LO;
	udc->base = (u_short)buf;
	FlushWB();

/* Start chain */

	udc->ptr = COMMAND2;
	udc->base = START_CHAIN2;
	FlushWB();

/* Wait a short while for chain to complete */

	delay = 0x5;
	while (delay--)
		;

/* Check registers */
	
	udc->ptr = CARA2_HI;
	FlushWB();
	if (udc->base != pattern) {
#ifdef	DEBUG
		pon_puts("FAILED UDC Channel 2 Current Address Register High\n\r");
#endif	DEBUG
		goto failed;
	}

	udc->ptr = CARA2_LO;
	FlushWB();
	if (udc->base != (~pattern & 0xffff)) {
#ifdef	DEBUG
		pon_puts("FAILED UDC Channel 2 Current Address Register Low\n\r");
#endif	DEBUG
		goto failed;
	}

	udc->ptr = COC2; 
	FlushWB();
	if (udc->base != pattern1) {
#ifdef	DEBUG
		pon_puts("FAILED UDC Channel 2 Current Operation Count Register\n\r");
#endif	DEBUG
		goto failed;
	}

	udc->ptr = CMR2_HI; 
	SetSCR(GetSCR() | SCR_SLOWUDCEN); 
	FlushWB();
	if (udc->base != (0x1f & (DD_BUSREL | NOFLIP | WORD_FLYBY | NOFLIP))) {
#ifdef	DEBUG
		pon_puts("FAILED UDC Channel 2 Channel Mode Register High\n\r");
#endif	DEBUG
		goto failed;
	}

	udc->ptr = CMR2_LO; 
	FlushWB();
	if (udc->base != 0) {
#ifdef	DEBUG
		pon_puts("FAILED UDC Channel 1 Channel Mode Register Low\n\r");
#endif	DEBUG
		goto failed;
	}

	SetSCR(GetSCR() & ~SCR_SLOWUDCEN); 

	pon_puts(success);
	ClearRegs();
	return(PASS);

failed:
	pon_puts(failure);
	ClearRegs();
	FastFlash(PON_CHAIN2);
	pon_set_leds(PON_CHAIN2);

norun:
	SetDepend(PON_FAULT_UDC);
	return(FAIL);
#else SABLE
	pon_puts(success);
	return(PASS);
#endif SABLE
}


ClearRegs()

{
	register volatile struct am9516 *udc = (struct am9516 *)PHYS_TO_K1(AMD_BASE_R2400);

	udc->ptr = MASTER_MODE;
	udc->base = 0;
	FlushWB();
	udc->ptr = COMMAND2;
	udc->base = 0;
	FlushWB();
	udc->ptr = CAR2_HI;
	udc->base = 0;
	FlushWB();
	udc->ptr = CAR2_LO;
	udc->base = 0;
	FlushWB();
	udc->ptr = CARA2_HI;
	udc->base = 0;
	FlushWB();
	udc->ptr = CARA2_LO;
	udc->base = 0;
	FlushWB();
	udc->ptr = COC2; 
	udc->base = 0;
	FlushWB();
	udc->ptr = CMR2_HI; 
	udc->base = 0;
	FlushWB();
	udc->ptr = CMR2_LO; 
	udc->base = 0;
	FlushWB();
}
