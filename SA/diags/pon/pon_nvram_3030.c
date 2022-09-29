#ident "$Header: pon_nvram_3030.c,v 1.2.1.1 90/07/18 14:32:31 huang Exp $"
/*	%Q%	%I%	%M%	*/
/* $Copyright 
 * |-----------------------------------------------------------|
 * | Copyright (c) 1987, 1990 MIPS Computer Systems, Inc.      |
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
#include "machine/cpu_board.h"
#include "machine/cpu.h"
#include "machine/mk48t02.h"
#include "pon.h"
#include "prom.h"

#define	READONLY_SIZE		1024		/* size of M20 read-only section of NVRAM */

extern char success[], failure[], crlf[];


Pon_NVram()

{
	register volatile char *addr;

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
	addr = (char *)get_nvramaddr(0);
	if (nvramchk(addr + (4 * READONLY_SIZE), TODC_NVRAM_SIZE - READONLY_SIZE)) {
		goto failed;
	}
	else {
		pon_puts(success);
	}

	return(PASS);

failed:
	FastFlash();
	SetDepend(PON_FAULT_NVRAM);
	return(FAIL);
}


static nvramchk(startaddr, ramsz)

u_char *startaddr;
register int ramsz;

{
	register volatile u_char *nvaddr;
	register int i;
	register int tmp = 0;
	register int err_count = 0;
	u_char save[TODC_NVRAM_SIZE - READONLY_SIZE];

	/*
	 * Save and initialize the data as address in address.
	 */
	for (i = 0, nvaddr = startaddr; i < ramsz; i++, nvaddr += 4) {
		save[i] = *nvaddr;
		*nvaddr = i;
	}

	for (i = 0, nvaddr = startaddr; i < ramsz; i++, nvaddr += 4) {
		if (*nvaddr != (i & 0xff)) {
			if (++err_count == 1) {
				pon_puts(failure);
			}

			if (++err_count <= 5) {
				pon_puts("  Address: ");
				pon_puthex((u_int)nvaddr);
				pon_puts(crlf);
			}
		}
	}

	/*
	 * Initialize the data as inverse address in address.
	 */
	for (i = 0, nvaddr = startaddr; i < ramsz; i++, nvaddr += 4) {
		*nvaddr = ~i;
	}

	for (i = 0, nvaddr = startaddr; i < ramsz; i++, nvaddr += 4) {
		if (*nvaddr != (~i & 0xff)) {
			if (++err_count == 1) {
				pon_puts(failure);
			}

			if (++err_count <= 5) {
				pon_puts("  Address: ");
				pon_puthex((u_int)nvaddr);
				pon_puts(crlf);
			}
		}
	}

	/*
	 * Restore the NVRAM contents;
	 */
	for (i = 0, nvaddr = startaddr; i < ramsz; i++, nvaddr += 4) {
		*nvaddr = save[i];
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

	Pon_Delay(128);
	nvstate = get_nvram(NVADDR_STATE, NVLEN_STATE);
#ifdef	DEBUG
	pon_puts("state ");
	pon_puthex(*nvstate);
	pon_puts(crlf);
#endif	DEBUG

#ifdef	DEBUG_C
	printf("nvstate value = %c\n", *nvstate);
#endif	DEBUG_C

	if (!(*nvstate && NVSTATE_TODVALID)) {
		return_value = 1;	/* failed */
		pon_puts("TOD invalid...");
	}
	if (!(*nvstate && NVSTATE_RAMVALID)) {
		return_value = 1;	/* failed */
		pon_puts("RAM invalid...");
	}
	return(return_value);
}
