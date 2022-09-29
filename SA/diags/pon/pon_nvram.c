#ident "$Header: pon_nvram.c,v 1.12.3.1 90/07/18 14:32:23 huang Exp $"
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

#include "sys/param.h"
#include "machine/regdef.h"
#include "machine/asm.h"
#include "machine/cpu_board.h"
#include "machine/cpu.h"
#include "machine/asm.h"
#include "machine/mk48t02.h"
#include "pon.h"
#include "prom.h"

#define	FIVES			0x55
#define	ACES			0xaa
#define	REALTESTCOUNT		50
#define	READONLY_SIZE		1024		/* size of M20 read-only section of NVRAM */

extern char success[], failure[], crlf[];


Pon_NVram()

{
	register volatile char *addr;

#ifndef	R3030
	pon_set_leds(PON_NVRAM);
#endif	!R3030
	pon_puts("Battery Check Test...");

	/*
	 * BATTERY check test
	 */
	if (bat_chk()) {
		pon_puts(failure);
	}
	else {
		pon_puts(success);
	}

	/*
	 * Actual NVRAM test.
	 */
	pon_puts("NVRAM Test...");
	if (IS_R2400 || IS_R3200 || IS_RB3125 || IS_R6300) {
		addr = (char *)get_nvramaddr(0);
		if (nvramchk(addr, TODC_NVRAM_SIZE)) {
			goto failed;
		}
		else {
			pon_puts(success);
		}
	}
	else if (IS_R3030) {
		addr = (char *)get_nvramaddr(0);
		if (nvramchk(addr + (4 * READONLY_SIZE), TODC_NVRAM_SIZE - READONLY_SIZE)) {
			goto failed;
		}
		else {
			pon_puts(success);
		}
	}
	else {
		addr = (char *)get_nvramaddr(0);
		if (nvramchk(addr, REALTESTCOUNT)) {
			goto failed;
		}
		else {
			pon_puts(success);
		}
	}

	return(PASS);

failed:
	FastFlash(PON_NVRAM);
#ifndef	R3030
	pon_set_leds(PON_NVRAM);
#endif	!R3030
	SetDepend(PON_FAULT_NVRAM);
	return(FAIL);
}


static nvramchk(startaddr, ramsz)

u_char *startaddr;
register int ramsz;

{
	register volatile u_char *nvaddr;
	char tmpstore;
	int err_count = 0, err_detected, i;
	int tmp = 0;

	nvaddr = startaddr;

	for (i = 0; i < ramsz; i++) {
		err_detected = 0;
		tmpstore = *nvaddr;
		*nvaddr = FIVES;
		FlushWB();
		if (i < REALTESTCOUNT) {
			DELAY(8);
		}

		tmp = *nvaddr;
		if (tmp != FIVES) {
			err_detected++;
		}

		if (i < REALTESTCOUNT) {
			DELAY(8);
		}

		*nvaddr = ACES;
		FlushWB();
		if (i < REALTESTCOUNT) {
			DELAY(8);
		}

		tmp = *nvaddr;
		if (tmp != ACES) {
			err_detected++;
		}

		if (i < REALTESTCOUNT) {
			DELAY(8);
		}

		*nvaddr = tmpstore;
		FlushWB();
		if (i < REALTESTCOUNT) {
			DELAY(8);
		}

		if (err_detected) {
		    err_count++;
		    if (err_count == 1)
			pon_puts(failure);
		    if (err_count <= 5) {
			pon_puts("  Address: ");
			pon_puthex((u_int)nvaddr);
			pon_puts("\r\n");
		    }
		}

		nvaddr += 4;
	}

	if (err_count) {
		if (err_count >= 6) {
			pon_puts("  (0x");
			pon_puthex(err_count);
			pon_puts(" errors total)\r\n");
		}
		return(1);
	}
	else {
		return(0);
	}
}


static bat_chk()

{
	char *nvstate;
	extern char *get_nvram();
	int  return_value = 0;	/* default "success" */

	DELAY(128);
	nvstate = get_nvram(NVADDR_STATE, NVLEN_STATE);
#ifdef	DEBUG
	pon_puts("state ");
	pon_puthex(*nvstate);
	pon_puts(crlf);
#endif	DEBUG

#ifdef	DEBUG_C
	printf("nvstate value = %c\n", *nvstate);
#endif

	if ( !(*nvstate && NVSTATE_TODVALID) ) {
		return_value = 1;	/* failed */
		pon_puts("TOD invalid...");
	}
	if ( !(*nvstate && NVSTATE_RAMVALID) ) {
		return_value = 1;	/* failed */
		pon_puts("RAM invalid...");
	}
	return( return_value );
}
