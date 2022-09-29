#ident "$Header: pon_buffer.c,v 1.2.1.1 90/07/18 14:29:32 huang Exp $"

/* ------------------------------------------------------------------ */
/* | Copyright Unpublished, MIPS Computer Systems, Inc.  All Rights | */
/* | Reserved.  This software contains proprietary and confidential | */
/* | information of MIPS and its suppliers.  Use, disclosure or     | */
/* | reproduction is prohibited without the prior express written   | */
/* | consent of MIPS.                                               | */
/* ------------------------------------------------------------------ */

/******************************************************************************
 *
 *	AINA - Address In Address
 *
 *  This is a traditional, hueristic, rule-of-thumb, "address-in-address"
 *  memory test.  It makes one pass writing the address, followed by one read
 *  pass.  Both writes and reads are in ascending order.
 *
 *  Nearly immediately after detecting a miscompare, the MemBoard's LEDs are
 *  written to to provide a logic-analyzer/scope trigger.  The LED pattern
 *  written is unique for each pass.
 *
 *  INPUTS:  first_address - address of the first meory location to be tested
 *           last_address - address of the last 32 bit word to be tested.
 *           hoe_flag - flag indicating whether to halt on error, or not
 *  OUTPUTS: expected_data - the expected data
 *           actual_data - the data value actually returned
 *           ret_error_addr - addreess at an error occured
 *  RETURNS: 0 if the memory has passed the test
 *          -1 if an error was detected and the test is to halt-on-error
 *           N if an error was detected and the test is to report-error and
 *             continue running (N = the number of errors detected)
 *  GLOBALS AFFECTED: None
 *
 *****************************************************************************/

#include "sys/types.h"
#include "machine/cpu_board.h"
#include "machine/cpu.h"
#include "prom/prom.h"
#include "pon.h"

extern char success[], failure[], skipped[];

Pon_Lance_Buffer()
{
	register u_int *first_addr;
	register u_int *last_addr;
	register volatile u_int last_value_read;
	register volatile u_int *ptr;
	register u_short cntrl,pattern;
	int error;

	if(machine_type != BRDTYPE_RB3125){
		return(PASS);
	}

	pon_set_leds(PON_LANCE_BUFF);
	pon_puts("Lance Buffer Test...");

	if(GetDepend() & PON_FAULT_MEM){
		pon_puts(skipped);
		return(FAIL);
	}
	first_addr = (u_int*)PHYS_TO_K1(LANCE_BUFFER_RB3125);
	last_addr = (u_int*)PHYS_TO_K1(LANCE_BUFFER_RB3125 + 0xffffc);
	error = 0;

	/*
	 *  Set all locations to A's.
	 */
	for (ptr = first_addr; ptr <= last_addr; ) {
		*ptr++ = 0xaaaaaaaa;
        }

	/*
	 *  Address-In-Address; Ascending Stores; Ascending Check.
	 */
	for (ptr = first_addr; ptr <= last_addr;) {
		*ptr++ = (u_int)ptr;
	}

	for (ptr = first_addr; ptr <= last_addr;) {
		if ((last_value_read = *ptr) != (u_int)ptr) {
			error = 1;
			goto done;
		}

		++ptr;
	}

done:
	if (error ) {
		pon_puts(failure);
		return(FAIL);
	}
	pon_puts(success);
	return(PASS);
}


void Pon_Delay(count)
int count;
{
int i;
	for (i = 0; i < count; i++) ;
}
