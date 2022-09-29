#ident "$Header: pon_parity_3030.c,v 1.2.1.1 90/07/18 14:32:38 huang Exp $"
/*	%Q%	%I%	%M%	*/
/* $Copyright: |
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
#include "machine/cpu.h"
#include "machine/cpu_board.h"
#include "saio/setjmp.h"
#include "pon.h"

#define	BPAT		0x37

extern int *nofault;
extern char success[], failure[], skipped[], crlf[];

static u_int error_id;
static int cause;

static jmp_buf fault_buf;


/*
 * Checks ForceBadParity and ParityEnable bits in SYSCON, parity logic for RAM
 * memory, FaultAddress register and FaultID register.
 */
Pon_Parity()

{
	register volatile u_int *addr = (u_int *)PHYS_TO_K1(PON_SCRATCHMEM + 0x43678);
	register volatile u_char *addrb = (u_char *)PHYS_TO_K1(PON_SCRATCHMEM + 0x43678);
	register u_int error;
	register u_int temp;
	register volatile u_char *realaddr;
	register u_int errorid;
	register u_int last_addr;
	register u_int offset;

	pon_puts("Parity Test...");

#ifdef	SABLE
	pon_puts(success);			/* sable doesn't simulate memory parity */
	return(PASS);
#else	SABLE

	if (GetDepend() & (PON_FAULT_FAULT | PON_FAULT_MEM | PON_FAULT_KEYBOARD)) {
		pon_puts(skipped);
		goto norun;
	}

	error = 0;

	last_addr = SizeMemory();
#ifdef	DEBUG
	pon_puts("Memory size ");
	pon_puthex(last_addr);
	pon_puts(crlf);
#endif	DEBUG
#ifdef	TEST
	last_addr = 0x800000;
#endif	TEST
	last_addr = PHYS_TO_K1(last_addr);
	for ( ; (u_int)addrb < (u_int)last_addr; addrb = (u_char *)((int)addrb + 0x800000)) {
		for (offset = 0; offset < 8; offset += 1) {
			realaddr = (u_char *)((int)addrb + offset);

#ifdef	DEBUG
			pon_puts("Realaddr ");
			pon_puthex(realaddr);
			pon_puts(crlf);
#endif	DEBUG

			error_id = 0;
			nofault = &fault_buf[0];
			if (setjmp(fault_buf)) {
				cause = GetCause();
#ifdef	DEBUG
				pon_puts("Fault 1\n\r");
				pon_puts("Cause ");
				pon_puthex(cause);
				pon_puts(crlf);
#endif	DEBUG
			}
			else {
				SetSCtlR(CR_ENA_BUZZER_B | CR_CLR_ERR_REG);
						/* disable parity checking */
				if (SetBadParity() == BAD) {
						/* force bad parity on write */
					error++;
					goto done;
				}

				*realaddr = BPAT;

				if (UnsetBadParity() == BAD) {
					error = 1;
					goto done;
				}

				SetSCtlR(CR_ENA_BUZZER_B | CR_CLR_ERR_REG);
				temp = *realaddr;
						/* should not get parity error */
			}

			/*
			 * Check if there was an exception.  Should be none since parity error
			 * is not enabled.
			 */
			if (error_id) {
#ifdef	DEBUG
				pon_puts("Parity disabled and got an exception\r\n");
#endif	DEBUG
				error++;
				goto done;
			}

			if (temp != BPAT) {	/* bad data read */
#ifdef	DEBUG
				pon_puts("Bad word data read: ");
				pon_puthex(temp);
				pon_puts(crlf);
#endif	DEBUG
				error++;
				goto done;
			}

			error_id = 0;

			if (setjmp(fault_buf)) {
				cause = GetCause();
#ifdef	DEBUG
				pon_puts("Fault 2\n\r");
				pon_puts("Cause ");
				pon_puthex(cause);
				pon_puts(crlf);
#endif	DEBUG
				error_id = GetSErrR();
#ifdef	DEBUG
				pon_puts("System Error Register: ");
				pon_puthex(error_id);
				pon_puts(crlf);
#endif	DEBUG
			}
			else {
				SetSCtlR(CR_ENA_BUZZER_B | CR_ENA_PAR_CHK);
						/* enable parity checking */
				temp = *realaddr;
						/* should get parity error */
			}

			if (!error_id) {
#ifdef	DEBUG
				pon_puts("Expected exception did not happen\r\n");
#endif	DEBUG
				error++;
				goto done;
			}

			/*
			 * Check cause register.  Should get just DBE and no interrupt level 5.
			 */
			if ((cause & (CAUSE_IP8 | CAUSE_EXCMASK)) != EXC_DBE) {
#ifdef	DEBUG
				pon_puts("Got DBE and interrupt level 5 for word\r\n");
				pon_puthex(cause);
#endif	DEBUG
				error++;
				goto done;
			}

			/*
			 * Check fault address.
			 */
			if ((error_id & ER_ADDR_MASK) != ((u_int)realaddr & ER_ADDR_MASK)) {
#ifdef	DEBUG
				pon_puts("Bad fault address\r\n");
#endif	DEBUG
				error++;
				goto done;
			}

			/*
			 * Check parity error bits.
			 */
			switch (offset & 3) {
			case 0:
				errorid = ER_PARERR0;
				break;

			case 1:
				errorid = ER_PARERR1;
				break;

			case 2:
				errorid = ER_PARERR2;
				break;

			case 3:
				errorid = ER_PARERR3;
				break;
			}

			if ((error_id & ER_PAR_ERR_MASK) != errorid) {
#ifdef	DEBUG
				pon_puts("Bad fault error bits\r\n");
#endif	DEBUG
				error++;
				goto done;
			}

			error_id = 0;

			if (setjmp(fault_buf)) {
				cause = GetCause();
#ifdef	DEBUG
				pon_puts("Fault 3\n\r");
				pon_puthex(cause);
#endif	DEBUG
				error_id = GetSErrR();
			}
			else {
				/*
				 * Correct bad data and check it.
				 */
				*realaddr = BPAT;
				temp = *realaddr;
			}

			if (temp != BPAT) {	/* bad data read */
#ifdef	DEBUG
				pon_puts("Bad word data read, parity enabled ");
				pon_puthex(temp);
				pon_puts(crlf);
#endif	DEBUG
				error++;
				goto done;
			}

			/*
			 * Check if there was an exception.  Should not get one.
			 */
			if (error_id) {
#ifdef	DEBUG
				pon_puts("Got an unexpected exception\r\n");
#endif	DEBUG
				error++;
				goto done;
			}

			if (error) {
				break;
			}
		}
	}

done:
	UnsetBadParity();
	*realaddr = 0;
	SetSCtlR(CR_ENA_BUZZER_B | CR_CLR_ERR_REG);
	SetSCtlR(CR_ENA_BUZZER_B);

	if (error) {
		pon_puts(failure);
		FastFlash();
norun:
		SetDepend(PON_FAULT_FAULT);
		return(FAIL);
	}

	pon_puts(success);
	return(PASS);
#endif	SABLE
}
