#ident "$Header: pon_parity.c,v 1.9.5.1 90/07/18 14:32:36 huang Exp $"

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
#include "saio/setjmp.h"
#include "pon.h"

#define	WPAT		0x12345678
#define	BPAT0		0x12
#define	BPAT1		0x56

#define	ADDR_MASK	0x1fffffff
#undef	FID_MASK
#define	FID_MASK	(FID_PROCBRD | FID_TIMEOUT | FID_MREAD | FID_ACCTYPEB | FID_PARERB)
#define	EXP_FID		((FID_PROCBRD | FID_MREAD | (ACCTYPEB_WORD << ACCTYPEB_SHIFT)) & ~(FID_TIMEOUT | FID_PARERB))
#define	EXP_FID1	((FID_PROCBRD | FID_MREAD | FID_ACCTYPEB | FID_PARER3B | FID_PARER2B | FID_PARER1B) & ~(FID_TIMEOUT | FID_PARER0B))

extern int machine_type;
extern int *nofault;
extern char success[], failure[], skipped[];

volatile u_int *far = (u_int *)PHYS_TO_K1(FAR);
volatile u_short *fid = (u_short *)PHYS_TO_K1(FID);
int cause;
volatile u_short error_id;
volatile u_int error_addr;

jmp_buf fault_buf;


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

	/*
	 * If machine_type is not Intrepid, exit without executing the test.
	 */
	if ((machine_type != BRDTYPE_R2400) && (machine_type != BRDTYPE_M180)){
		return(PASS);
	}

	pon_set_leds(PON_PARITY);
	pon_puts("Parity Test...");

	/* sable doesn't simulate m120 memory parity */
#ifdef SABLE
	pon_puts(success);
	return(PASS);
#else SABLE

	if (GetDepend() & PON_FAULT_MEM) {
		pon_puts(skipped);
		goto norun;
	}

	temp = *far;			/* clear any pending faults */

	SetSR(GetSR() & SR_BEV);
	error = 0;
	error_id = 0;
	error_addr = 0;
	nofault = &fault_buf[0];

	if (setjmp(fault_buf)) {
		cause = GetCause();
#ifdef	DEBUG
		pon_puts("Fault 1\n\r");
		pon_puthex(cause);
#endif	DEBUG
		error_id = *fid;
		error_addr = *far;
	}
	else {
		SetSCR((GetSCR() | SCR_FORCEPAR) & ~SCR_PAREN);
					/* force bad parity on write */
		FlushWB();
		*addr = WPAT;
		FlushWB();
		SetSCR(GetSCR() & ~SCR_FORCEPAR);
		FlushWB();
		temp = *addr;		/* should not get parity error */
	}

	/*
	 * Check if there was an exception.  Should be none since parity error
	 * is not enabled.
	 */
	if (error_id || error_addr) {
#ifdef	DEBUG
		pon_puts("Parity disabled and got an exception\r\n");
#endif	DEBUG
		error++;
		goto done;
	}

	if (temp != WPAT) {		/* bad data read */
#ifdef	DEBUG
		pon_puts("Bad word data read\r\n");
#endif	DEBUG
		error++;
		goto done;
	}

	if (setjmp(fault_buf)) {
		cause = GetCause();
#ifdef	DEBUG
		pon_puts("Fault 2\n\r");
		pon_puthex(cause);
#endif	DEBUG
		error_id = *fid;
		error_addr = *far;
#ifdef	DEBUG
		pon_puts("Fault ID: \n\r");
		pon_puthex(error_id);
		pon_puts("Fault address \n\r");
		pon_puthex(error_addr);
#endif	DEBUG
	}
	else {
		SetSCR(GetSCR() | SCR_PAREN);
					/* enable parity checking */
		FlushWB();
		temp = *addr;		/* should get parity error */
	}

	/*
	 * Check if there was an exception.  Should get one.
	 */
	if (!(error_id || error_addr)) {
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
	if (error_addr != ((u_int)addr & ADDR_MASK)) {
#ifdef	DEBUG
		pon_puts("Bad fault address\r\n");
#endif	DEBUG
		error++;
		goto done;
	}

	/*
	 * Check parity error bits, IBus valid, processor board error,
	 * timeout, read request, access type and parity error.
	 */
	if ((error_id & FID_MASK) != EXP_FID) {
#ifdef	DEBUG
		pon_puts("Bad fault ID\r\n");
#endif	DEBUG
		error++;
		goto done;
	}

	error_id = 0;
	error_addr = 0;

	if (setjmp(fault_buf)) {
		cause = GetCause();
#ifdef	DEBUG
		pon_puts("Fault 3\n\r");
		pon_puthex(cause);
#endif	DEBUG
		error_id = *fid;
		error_addr = *far;
	}
	else {
		*addr = WPAT;
		temp = *addr;
	}

	if (temp != WPAT) {		/* bad data read */
#ifdef	DEBUG
		pon_puts("Bad word data read, Parity enabled\r\n");
#endif	DEBUG
		error++;
		goto done;
	}

	/*
	 * Check if there was an exception.  Should not get one.
	 */
	if (error_id || error_addr) {
#ifdef	DEBUG
		pon_puts("Got an unexpected exception\r\n");
#endif	DEBUG
		error++;
		goto done;
	}

	nofault = 0;
	SetSCR(GetSCR() & ~(SCR_FORCEPAR | SCR_PAREN));
	FlushWB();
	*addr = WPAT;			/* good background pattern */
	FlushWB();
	SetSCR((GetSCR() | SCR_FORCEPAR) & ~SCR_PAREN);
					/* force bad parity on write */
	FlushWB();
	*addrb = BPAT0;
	FlushWB();
	SetSCR(GetSCR() & ~SCR_FORCEPAR);
	FlushWB();

	if (testb(addrb, BPAT0, EXP_FID1) != PASS) {
		error++;
		goto done;
	}

	if (testb(addrb + 2, BPAT1, EXP_FID1) != PASS) {
		error++;
	}

done:
	SetSCR(GetSCR() & ~(SCR_PAREN | SCR_FORCEPAR));
					/* disable parity checking */
	FlushWB();
	*addr = 0;			/* fix bad parity location */
	FlushWB();
	temp = *far;			/* clear any pending faults */

	if (error) {
		pon_puts(failure);
		FastFlash(PON_PARITY);
		pon_set_leds(PON_PARITY);

norun:
		SetDepend(PON_FAULT_FAULT);
		return(FAIL);
	}

	pon_puts(success);
	return(PASS);
#endif SABLE
}


static testb(addr, bpat, expmask)

register u_char *addr;
register u_char bpat;
register u_int expmask;

{
	register u_char temp;

	cause = 0;
	error_id = 0;
	error_addr = 0;
	nofault = &fault_buf[0];

	SetSCR(GetSCR() & ~SCR_PAREN);	/* disable parity checking */
	FlushWB();
	temp = *far;			/* clear any pending faults */
	FlushWB();

	if (setjmp(fault_buf)) {
		cause = GetCause();
#ifdef	DEBUG
		pon_puts("Fault 4\n\r");
		pon_puthex(cause);
#endif	DEBUG
		error_id = *fid;
		error_addr = *far;
	}
	else {
		temp = *addr;		/* should not get parity error	*/
	}

	/*
	 * Check if there was an exception.  Should be none since parity error
	 * is disabled.
	 */
	if (error_id || error_addr) {
#ifdef	DEBUG
		pon_puts("Got an unexpected exception\r\n");
#endif	DEBUG
		goto failed;
	}

	if (temp != bpat) {		/* bad data read */
#ifdef	DEBUG
		pon_puts("Bad byte data read, Parity enabled\n\r");
#endif	DEBUG
		goto failed;
	}

	SetSCR(GetSCR() | SCR_PAREN);	/* enable parity checking */
	FlushWB();
	if (setjmp(fault_buf)) {
		cause = GetCause();
#ifdef	DEBUG
		pon_puts("Fault 5\n\r");
		pon_puthex(cause);
#endif	DEBUG
		error_id = *fid;
		error_addr = *far;
	}
	else {
		temp = *addr;		/* should get parity error */
	}

	/*
	 * Check if there was an exception.  Should get one.
	 */
	if (!(error_id || error_addr)) {
#ifdef	DEBUG
		pon_puts("Expected exception did not happen\r\n");
#endif	DEBUG
		goto failed;
	}

	/*
	 * Check cause register.  Should get just DBE and no interrupt level 5.
	 */
	if ((cause & (CAUSE_IP8 | CAUSE_EXCMASK)) != EXC_DBE) {
#ifdef	DEBUG
		pon_puts("Got DBE and interrupt level 5 for byte\r\n");
		pon_puthex(cause);
#endif	DEBUG
		goto failed;
	}

	/*
	 * Check fault address.
	 */
	if (error_addr != ((u_int)addr & ADDR_MASK)) {
#ifdef	DEBUG
		pon_puts("Bad fault address\r\n");
#endif	DEBUG
		goto failed;
	}

	/*
	 * Check parity error bits, IBus valid, processor board error,
	 * timeout, read request, access type and parity error.
	 */
	if ((error_id & FID_MASK) != expmask) {
#ifdef	DEBUG
		pon_puts("Bad fault ID\r\n");
#endif	DEBUG
		goto failed;
	}

	return(PASS);

failed:
	SetSCR(GetSCR() & ~SCR_PAREN);	/* disable parity checking */
	FlushWB();
	return(FAIL);
}
