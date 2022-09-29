#ident "$Header: pon_scctest.c,v 1.2.1.1 90/07/18 14:32:48 huang Exp $"
/* $Copyright$ */

#include "sys/types.h"
#include "machine/cpu.h"
#include "machine/cpu_board.h"
#include "machine/scc_cons.h"
#include "pon.h"

#define	DELAY_COUNT		25000
#define	DEBUG

extern void Pon_Delay();
extern char success[], failure[];

struct scc {
	u_int ptr_b;
	u_int data_b;
	u_int ptr_a;
	u_int data_a;
};


static loopback_a_test()
{
#ifdef	R3030
register volatile struct scc *sccp = (struct scc *)PHYS_TO_K1(SCC_BASE_R3030);
#else	R3030
register volatile struct scc *sccp = (struct scc *)PHYS_TO_K1(SCC_BASE_RB3125);
#endif	R3030
register int data;
register int i, counter;

	for(i = 'a'; i <= 'z'; i++) {
		counter = 0;
		while(!(sccp->ptr_a & SCCR_STATUS_TxReady)) {
			if((++counter) >= DELAY_COUNT) {
#ifdef	DEBUG
				Pon_Scc();
				pon_puts("transmit(A) timeout.");
#endif	DEBUG
				return(FAIL);
			}
		}

		sccp->data_a = (i & 0xff); /* send data */
		Pon_Delay(100);

		counter = 0;
		while(!(sccp->ptr_a & SCCR_STATUS_RxDataAvailable)) {
			if((++counter) >= DELAY_COUNT) {
#ifdef	DEBUG
				Pon_Scc();
				pon_puts("receive(A) timeout.");
#endif	DEBUG
				return(FAIL);
			}
		}

		data = (sccp->data_a & 0xff); /* get data */
		Pon_Delay(100);

		if (data != i) {
#ifdef	DEBUG
			Pon_Scc();
			pon_puts("compare(A) error.");
			printf("Act = 0x%x, Exp = 0x%x\n",data & 0xff,i);
#endif	DEBUG
			return(FAIL);
		}

		if(scc_a_send('\b') == FAIL) return(FAIL);
		if(scc_a_receive() == FAIL) return(FAIL);
		if(scc_a_send(' ') == FAIL) return(FAIL);
		if(scc_a_receive() == FAIL) return(FAIL);
		if(scc_a_send('\b') == FAIL) return(FAIL);
		if(scc_a_receive() == FAIL) return(FAIL);
	}

	return(PASS);
}


static loopback_b_test()
{
#ifdef	R3030
register volatile struct scc *sccp = (struct scc *)PHYS_TO_K1(SCC_BASE_R3030);
#else	R3030
register volatile struct scc *sccp = (struct scc *)PHYS_TO_K1(SCC_BASE_RB3125);
#endif	R3030
register int data;
register int i, counter;

	for(i = 'a'; i <= 'z'; i++) {
		counter = 0;
		while(!(sccp->ptr_b & SCCR_STATUS_TxReady)) {
			if((++counter) >= DELAY_COUNT) {
#ifdef	DEBUG
				Pon_Scc();
				pon_puts("transmit(B) timeout.");
#endif	DEBUG
				return(FAIL);
			}
		}

		sccp->data_b = (i & 0xff); /* send data */
		Pon_Delay(100);

		counter = 0;
		while(!(sccp->ptr_b & SCCR_STATUS_RxDataAvailable)) {
			if((++counter) >= DELAY_COUNT) {
#ifdef	DEBUG
				Pon_Scc();
				pon_puts("receive(B) timeout.");
#endif	DEBUG
				return(FAIL);
			}
		}

		data = (sccp->data_b & 0xff); /* get data */
		Pon_Delay(100);

		if (data != i) {
#ifdef	DEBUG
			Pon_Scc();
			pon_puts("compare(B) error.");
			printf("Act = 0x%x, Exp = 0x%x\n",data & 0xff,i);
#endif	DEBUG
			return(FAIL);
		}

		if(scc_b_send('\b') == FAIL) return(FAIL);
		if(scc_b_receive() == FAIL) return(FAIL);
		if(scc_b_send(' ') == FAIL) return(FAIL);
		if(scc_b_receive() == FAIL) return(FAIL);
		if(scc_b_send('\b') == FAIL) return(FAIL);
		if(scc_b_receive() == FAIL) return(FAIL);
	}

	Pon_Scc();
	return(PASS);
}


static scc_int_test()
{
#ifdef	R3030
register volatile struct scc *sccp = (struct scc *)PHYS_TO_K1(SCC_BASE_R3030);
#else	R3030
register volatile struct scc *sccp = (struct scc *)PHYS_TO_K1(SCC_BASE_RB3125);
#endif	R3030
register unsigned int sys_int, cause;
register int count = 0;
int error = 0;

	SetSR(SR_BEV);

	sccp->ptr_a = 1;
	wbflush();
	sccp->ptr_a = 1; /* Ext Int Enable */
	wbflush();
	sccp->ptr_a = 9;
	wbflush();
	sccp->ptr_a = 8; /* MIE */
	wbflush();
	sccp->ptr_a = 15;
	wbflush();
	sccp->ptr_a = 2; /* Zero Count IE */
	wbflush();

#ifdef	R3030
	while(!((cause = GetCause()) & CAUSE_IP3)) {
#else	R3030
	while(!((cause = GetCause()) & CAUSE_IP4)) {
#endif	R3030
		if(count++ > 1000000) {
			error = 1;
#ifdef	DEBUG
			Pon_Scc();
			printf("Cause = 0x%x\n",cause);
#endif	DEBUG
			break;
		}
	}

	Pon_Delay(100);
#ifdef	R3030
	sys_int = GetIntR();
	if (sys_int & IR_SCC_INT_B) {
		error = 1;

#ifdef	DEBUG
		Pon_Scc();
		printf("System interrupt reg = 0x%x\n", sys_int);
#endif	DEBUG
	}

#endif	R3030
	return(error);
}


static scc_a_send(ch)
register unsigned char ch;
{
#ifdef	R3030
register volatile struct scc *sccp = (struct scc *)PHYS_TO_K1(SCC_BASE_R3030);
#else	R3030
register volatile struct scc *sccp = (struct scc *)PHYS_TO_K1(SCC_BASE_RB3125);
#endif	R3030
register int data;
int counter;

	counter = 0;
	while(!((data = sccp->ptr_a) & SCCR_STATUS_TxReady)) {
		if((++counter) >= DELAY_COUNT) {
#ifdef	DEBUG
			Pon_Scc();
			pon_puts("transmit(A) timeout.");
#endif	DEBUG
			return(FAIL);
		}
	}

	sccp->data_a = (ch & 0xff); /* send data */
	Pon_Delay(100);
}


static scc_a_receive()
{
#ifdef	R3030
register volatile struct scc *sccp = (struct scc *)PHYS_TO_K1(SCC_BASE_R3030);
#else	R3030
register volatile struct scc *sccp = (struct scc *)PHYS_TO_K1(SCC_BASE_RB3125);
#endif	R3030
register int data;
int counter;

	counter = 0;
	while(!((data = sccp->ptr_a) & SCCR_STATUS_RxDataAvailable)) {
		if((++counter) >= DELAY_COUNT) {
#ifdef	DEBUG
			Pon_Scc();
			pon_puts("receive(A) timeout.");
#endif	DEBUG
			return(FAIL);
		}
	}

	data = (sccp->data_a & 0xff); /* get data */
}


static scc_b_send(ch)
register unsigned char ch;
{
#ifdef	R3030
register volatile struct scc *sccp = (struct scc *)PHYS_TO_K1(SCC_BASE_R3030);
#else	R3030
register volatile struct scc *sccp = (struct scc *)PHYS_TO_K1(SCC_BASE_RB3125);
#endif	R3030
register int data;
int counter;

	counter = 0;
	while(!((data = sccp->ptr_b) & SCCR_STATUS_TxReady)) {
		if((++counter) >= DELAY_COUNT) {
#ifdef	DEBUG
			Pon_Scc();
			pon_puts("transmit(B) timeout.");
#endif	DEBUG
			return(FAIL);
		}
	}

	sccp->data_b = (ch & 0xff); /* send data */
	Pon_Delay(100);
}


static scc_b_receive()
{
#ifdef	R3030
register volatile struct scc *sccp = (struct scc *)PHYS_TO_K1(SCC_BASE_R3030);
#else	R3030
register volatile struct scc *sccp = (struct scc *)PHYS_TO_K1(SCC_BASE_RB3125);
#endif	R3030
register int data;
int counter;

	counter = 0;
	while(!((data = sccp->ptr_b) & SCCR_STATUS_RxDataAvailable)) {
		if((++counter) >= DELAY_COUNT) {
#ifdef	DEBUG
			Pon_Scc();
			pon_puts("receive(B) timeout.");
#endif	DEBUG
			return(FAIL);
		}
	}

	data = (sccp->data_b & 0xff); /* get data */
}


Pon_SccTest()
{
#ifdef	R3030
	register u_char environ;
#endif	R3030
	int error = 0;

	pon_puts("SCC Test...");

#ifdef	R3030
	environ = GetPonEnviron();
	Pon_Delay(DELAY_COUNT);

	if (environ & (PON_TTY0 | PON_TTY1)) {
		Scc_Local();

		/* channel A internal loopback test */
		if((environ & PON_TTY0) && (loopback_a_test() == FAIL)) {
			error = 1;
		}

		/* channel B internal loopback test */
		if((environ & PON_TTY0) && (loopback_b_test() == FAIL)) {
			error = 1;
		}

		Pon_Scc();
	}
#else	R3030
	Pon_Delay(DELAY_COUNT);
	Scc_Local();

	/* channel A internal loopback test */
	if(loopback_a_test() == FAIL) {
		error |= 1;
	}

	/* channel B internal loopback test */
	if(loopback_b_test() == FAIL) {
		error |= 2;
	}

	Pon_Scc();
#endif	R3030

	/* this doesn't send char to channel A */
	if(scc_int_test()) {
		error |= 1;
	}

	Pon_Delay(DELAY_COUNT);
	Pon_Scc();
	if (error) {
		pon_puts(failure);
#ifdef	R3030
		FastFlash(0);
		SetDepend(PON_FAULT_DUARTS);
#else	R3030
		if (error & 1) {
			FastFlash(PON_DUART1A);
			pon_set_leds(PON_DUART1A);
			SetDepend(PON_FAULT_CONSOLE);
		}

		if (error & 2) {
			FastFlash(PON_DUART1B);
			pon_set_leds(PON_DUART1B);
			SetDepend(PON_FAULT_DUARTS);
		}
#endif	R3030
		return(FAIL);
	}

	pon_puts(success);
	return(PASS);
}
