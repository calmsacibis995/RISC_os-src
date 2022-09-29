#ident "$Header: pon_udcslave.c,v 1.6.1.1 90/07/18 14:33:51 huang Exp $"
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

#include "sys/param.h"
#include "mips/cpu.h"
#include "mips/cpu_board.h"
#include "mips/am9516.h"
#include "pon.h"

extern int machine_type;

struct udctbl {
	u_short addr;
	u_short mask;
};

static struct udctbl udcfast[] = {
	0x38,	0x000f,				/* MMR */
	0x1a,	0xffde,				/* CAR-A channel 1 hi */
	0x0a,	0xffff,				/* CAR-A channel 1 lo */
	0x18,	0xffde,				/* CAR-A channel 2 hi */
	0x08,	0xffff,				/* CAR-A channel 2 lo */
	0x12,	0xffde,				/* CAR-B channel 1 hi */
	0x02,	0xffff,				/* CAR-B channel 1 lo */
	0x10,	0xffde,				/* CAR-B channel 2 hi */
	0x00,	0xffff,				/* CAR-B channel 2 lo */
	0x1e,	0xffde,				/* BAR-A channel 1 hi */
	0x0e,	0xffff,				/* BAR-A channel 1 lo */
	0x1c,	0xffde,				/* BAR-A channel 2 hi */
	0x0c,	0xffff,				/* BAR-A channel 2 lo */
	0x16,	0xffde,				/* BAR-B channel 1 hi */
	0x06,	0xffff,				/* BAR-B channel 1 lo */
	0x14,	0xffde,				/* BAR-B channel 2 hi */
	0x04,	0xffff,				/* BAR-B channel 2 lo */
	0x32,	0xffff,				/* COC channel 1 */
	0x30,	0xffff,				/* COC channel 2 */
	0x36,	0xffff,				/* BOC channel 1 */
	0x34,	0xffff,				/* BOC channel 2 */
	0x26,	0xff06,				/* CHAR channel 1 hi */
	0x22,	0xffff,				/* CHAR channel 1 lo */
	0x24,	0xff06,				/* CHAR channel 2 hi */
	0x20,	0xffff,				/* CHAR channel 2 lo */
};

#define	FASTSIZE		(sizeof(udcfast) / sizeof(struct udctbl))

static struct udctbl udcslow[] = {
	0x4a,	0xffff,				/* PAT channel 1 */
	0x48,	0xffff,				/* PAT channel 2 */
	0x4e,	0xffff,				/* MASK channel 1 */
	0x4c,	0xffff,				/* MASK channel a */
	0x5a,	0xffff,				/* IV channel 1 */
	0x58,	0xffff,				/* IV channel 2 */
	0x52,	0xffff,				/* CMR channel 1 lo */
	0x50,	0xffff,				/* CMR channel 2 lo */
};

#define	SLOWSIZE		(sizeof(udcslow) / sizeof(struct udctbl))

extern char success[], failure[], skipped[];


Pon_UdcSlave()

{
	register volatile struct am9516 *udc = (struct am9516 *)PHYS_TO_K1(AMD_BASE_R2400);
	register u_int error;
	register u_int i;
	register u_int mask;
	register u_int read;

	if ((machine_type != BRDTYPE_R2400) && (machine_type != BRDTYPE_M180)){
		return(PASS);
	}

	pon_set_leds(PON_UDCSLAVE);
	pon_puts("UDC Slave TEST...");

	if (GetDepend() & PON_FAULT_SCR) {
		pon_puts(skipped);
		goto norun;
	}

	error = 0;

	/*
	 * Set command registers (which are write only).
	 */
	udc->ptr = 0x2e;
	FlushWB();
	udc->base = RESET_CMD;
	FlushWB();
	udc->ptr = 0x2c;
	FlushWB();
	udc->base = RESET_CMD;
	FlushWB();

	SetSCR(GetSCR() & ~SCR_SLOWUDCEN);

	for (i = 0; i < FASTSIZE; i++) {
		mask = udcfast[i].mask;
		udc->ptr = udcfast[i].addr;
		FlushWB();

		udc->base = 0x5555 & mask;
		if ((read = udc->base) != (mask & 0x5555)) {
			error++;
#ifdef	DEBUG_C
			printf("reg %02x: wrote %04x, read %04x\n", udcfast[i].addr, mask & 0x5555, read);
#endif	DEBUG_C
		}

		udc->base = 0xaaaa & mask;
		if ((read = udc->base) != (mask & 0xaaaa)) {
			error++;
#ifdef	DEBUG_C
			printf("reg %02x: wrote %04x, read %04x\n", udcfast[i].addr, mask & 0xaaaa, read);
#endif	DEBUG_C
		}

		udc->base = 0;			/* clear out register */
	}

	SetSCR(GetSCR() | SCR_SLOWUDCEN);

	for (i = 0; i < SLOWSIZE; i++) {
		mask = udcslow[i].mask;
		udc->ptr = udcslow[i].addr;
		FlushWB();

		udc->base = 0x5555 & mask;
		if ((read = udc->base) != (mask & 0x5555)) {
			error++;
#ifdef	DEBUG_C
			printf("reg %02x: wrote %04x, read %04x\n", udcslow[i].addr, mask & 0x5555, read);
#endif	DEBUG_C
		}

		udc->base = 0xaaaa & mask;
		if ((read = udc->base) != (mask & 0xaaaa)) {
			error++;
#ifdef	DEBUG_C
			printf("reg %02x: wrote %04x, read %04x\n", udcslow[i].addr, mask & 0xaaaa, read);
#endif	DEBUG_C
		}

		udc->base = 0;			/* clear out register */
	}

	SetSCR(GetSCR() & ~SCR_SLOWUDCEN);

	if (error) {
		pon_puts(failure);
		FastFlash(PON_UDCSLAVE);
		pon_set_leds(PON_UDCSLAVE);

norun:
		SetDepend(PON_FAULT_UDC);
		return(FAIL);
	}

	pon_puts(success);
	return(PASS);
}
