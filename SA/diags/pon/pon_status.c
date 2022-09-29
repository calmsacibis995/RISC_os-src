#ident "$Header: pon_status.c,v 1.2.1.1 90/07/18 14:33:01 huang Exp $"
/*	%Q%	%I%	%M%	*/
/* $Copyright
 * |-----------------------------------------------------------|
 * | Copyright (c) 1990       MIPS Computer Systems, Inc.      |
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
#include "machine/param.h"
#include "machine/cpu.h"
#include "prom/prom.h"
#include "pon.h"

/*
 * Optional tests.
 */
#define	NO_OPTION			0
#define	FP_OPTION			1
#define	COLOR_OPTION			2
#define	AT_OPTION			3

static char *status_list[] = {
	" PASSED ",
	"*FAILED*",
};

struct test_type {
	char *desc;
	int bit_mask;
	int option;
};

static struct test_type tests[] = {
	"Keyboard Controller Tests....",	PON_FAULT_KEYBOARD,	NO_OPTION,
	"Instruction Cache Tests......",	PON_FAULT_ICACHE,	NO_OPTION,
	"Data Cache Tests.............",	PON_FAULT_DCACHE,	NO_OPTION,
	"Main Memory Test.............",	PON_FAULT_MEM,		NO_OPTION,
	"Memory Parity Test...........",	PON_FAULT_FAULT,	NO_OPTION,
	"ID PROM Test.................",	PON_FAULT_IDPROM,	NO_OPTION,
	"Write Buffer Test............",	PON_FAULT_WB,		NO_OPTION,
	"SCC Tests....................",	PON_FAULT_DUARTS,	NO_OPTION,
	"Timer Test...................",	PON_FAULT_TIMER,	NO_OPTION,
	"Time-of-Day Test.............",	PON_FAULT_TOD,		NO_OPTION,
	"Color Frame Buffer Tests.....",	PON_FAULT_COLORVIDEO,	COLOR_OPTION,
	"Floppy Disk Controller Test..",	PON_FAULT_FDC,		NO_OPTION,
	"System Control Register Test.",	PON_FAULT_SCR,		NO_OPTION,
	"DMA Channel 1 Test...........",	PON_FAULT_DMA_CHAN1,	NO_OPTION,
	"DMA Channel 2 Test...........",	PON_FAULT_DMA_CHAN2,	NO_OPTION,
	"SCSI Controller Chip Test....",	PON_FAULT_SCSI,		NO_OPTION,
	"R3000 TLB Tests..............",	PON_FAULT_TLB,		NO_OPTION,
	"R3000 Exceptions Test........",	PON_FAULT_EXCEPT,	NO_OPTION,
	"DMA Parity Checking Test.....",	PON_FAULT_DMA_PARITY,	NO_OPTION,
	"NVRAM Tests..................",	PON_FAULT_NVRAM,	NO_OPTION,
	"R3010 Floating Point Tests...",	PON_FAULT_FP,		FP_OPTION,
	"LANCE Tests..................",	PON_FAULT_LANCE,	NO_OPTION,
	"AT Serial Board Tests........",	PON_FAULT_AT,		AT_OPTION,
};

#define	TESTS_SIZE		(sizeof(tests) / sizeof(struct test_type))


PonStatus()

{
	register int display;
	register u_int depend;
	register u_int i;
	register u_int j;
	register u_int mask;
	char c;

	/*
	 * Check if PON was executed.
	 */
	depend = GetPonEnviron();
	ResetPonEnviron();
#ifdef	DEBUG
	printf("PON Environment %02x, bootmode %s\n", depend, get_nvram(NVADDR_BOOTMODE, NVLEN_BOOTMODE));
#endif	DEBUG
	if (!(depend & PON_ENTERED)) {
		return;
	}

	display = TRUE;
	c = *(char *)get_nvram(NVADDR_CONSOLE, NVLEN_CONSOLE);
	switch (c) {
	case '0':
	case '1':
	case 't':
	case 'r':
	case 'a':
#ifdef	DEBUG
		display = TRUE;
#else	DEBUG
		display = FALSE;
#endif	DEBUG
		break;

	case 'm':
	case 'c':
	case 'l':
	case 'g':
		if (!(depend & PON_KBD_NOT_PRESENT)) {
			display = TRUE;
		}
		else {
#ifdef	DEBUG
			display = TRUE;
#else	DEBUG
			display = FALSE;
#endif	DEBUG
		}
		break;
	}

	if (display == TRUE) {
		depend = GetDepend();

		/*
		 * Create mask for dependency bits.
		 */
		mask = 0;
		for (i = 0; i < TESTS_SIZE; i++) {
			mask |= tests[i].bit_mask;
		}

		i = depend & mask;
		printf("\nPower-On Diagnostics.............%s\n",
		    i ? status_list[1] : status_list[0]);

		/*
		 * List all PON tests only if there were any failures.
		 */
		if (i != 0) {
			j = 0;
			for (i = 0; i < TESTS_SIZE; i++) {
				/*
				 * Display message for standard components.  If the test is
				 * for an optional component, then first check if that
				 * component exist.
				 *
				 * NOTE: 
				 *
				 * If NVRAM is bad or the routines that detect for the
				 * component is in error
				 */
				switch (tests[i].option) {
				case NO_OPTION:
					break;

				case FP_OPTION:
					if (FindFpu()) {
						continue;
					}
					break;

				case COLOR_OPTION:
					if (!CfbPresent()) {
						continue;
					}
					break;

				case AT_OPTION:
					if(CfbPresent() || !DigiPresent()) {
						continue;
					}
					break;
				}

				printf("  %s%s", tests[i].desc,
				    depend & tests[i].bit_mask ? status_list[1] : status_list[0]);

				if ((j++ % 2) != 0) {
					putchar('\n');
				}
			}

			if ((j % 2) != 0) {
				putchar('\n');
			}
		}
		else {
			/*
			 * Indicate that PON passed.
			 */
			Pon_Buzzer();
		}
	}

	ResetDepend();
}
