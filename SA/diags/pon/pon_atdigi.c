#ident "$Header: pon_atdigi.c,v 1.2.1.1 90/07/18 14:29:14 huang Exp $"
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
#include "machine/cpu.h"
#include "machine/cpu_board.h"
#include "saio/setjmp.h"
#include "pon.h"

/* registers */
#define RRDATA 0		/* received data */
#define RTDATA 0		/* transmitted data */
#define RSTATUS 5		/* status */
#define RMstat 6		/* modem status */
#define RMctrl 4		/* modem control */
#define RCtrl 3			/* line control */
#define RIENABL 1		/* interrupt enable */
#define RSPEED 0		/* data rate */
#define RIIR 2			/* interrupt identification */

/* status register bits (both modem status and line status) */
#define SRRDY	0x01		/* received data ready */
#define STRDY	0x20		/* transmitter ready */
#define SOERR	0x02		/* received data overrun */
#define SPERR	0x04		/* received data parity error */
#define SFERR	0x08		/* received data framing error */
#define SDSR	0x80		/* status of DSR (CD) */
#define SCTS	0x10		/* status of CTS */
#define DDSR	0x02		/* delta status of DSR (CD) */
#define DCTS	0x01		/* delta status of CTS */

/* control register (both modem control and line control) */
#define CBITS5	0x00		/* five bit characters */
#define CBITS6	0x01		/* six bit characters */
#define CBITS7	0x02		/* seven bit characters */
#define CBITS8	0x03		/* eight bit characters */
#define CDTR	0x01		/* data terminal ready */
#define CRTS	0x02		/* request to send */
#define CSTOP2	0x04		/* two stop bits */
#define CPARITY	0x08		/* parity on */
#define CEVEN	0x10		/* even parity */
#define CBREAK	0x40		/* send break (space) */
#define	CDLAB	0x80		/* enable divisor latch access */

/* interrupt enable */
#define EXMIT	0x02		/* transmitter ready */
#define ERECV	0x01		/* receiver ready */
#define EMS	0x08		/* modem status change */
#define ENINTR	0x08		/* board interupt enable */
#define LOOPBCK 0x10		/* Loop back mode */

/* Function codes for c8modem */

#define	EIGHTBPC 0x3

extern int *nofault;
extern u_char success[], failure[], skipped[], crlf[];
extern u_int pon_tmp;

/* Define setjmp array */
static jmp_buf fault_buf;

static u_int c8_addr_brd0[] = {
	0x0,
	0x140,
	0x148,
	0x150,
	0x158,
	0x160,
	0x168,
	0x170,
	0x178,
	0x180,
	0x188,
	0x190,
	0x198,
	0x1a0,
	0x1a8,
	0x1b0,
	0x1b8,
	0
};

#define SCRATCH_REG	0x7

/* data rates */

static int c8_speeds[] = {
	/* 9600	*/	12,
};

/* Port Base Addresses */

static int *c8_addr;

static int port_id;
static u_char data;

/*
 * Read a byte from an 8 bit AT device
 */
static u_char RdByte_Io8(ratadr)
register u_char *ratadr;
{
	register u_char *cpuadr;

	cpuadr = (u_char *) ( ((u_int) ratadr << 2) +3);

	cpuadr= (u_char *) (RAT_IO_BASE | PHYS_TO_K1(cpuadr));
#ifdef DEBUG
	printf("RdByte_Io8() Routine: Reading RAT address %x using CPU address %x\n",
		ratadr, cpuadr);
#endif DEBUG
	return(*cpuadr);
}


/*
 * Write a byte to an 8 bit AT device
 */
static void WrtByte_Io8(ratadr, data)
register u_char *ratadr;
register u_char data;
{
	register u_char *cpuadr;

	cpuadr = (u_char *) ( ((u_int)ratadr << 2) +3);

	cpuadr= (u_char *)(RAT_IO_BASE | PHYS_TO_K1(cpuadr));
#ifdef DEBUG
	printf("WrtByte_Io8() Routine: Writing RAT address %x using CPU address %x\n",
		ratadr, cpuadr);
#endif DEBUG

	*cpuadr= data;
}


/* WRITE */
static WrtMemory(adrptr, expected)
register u_char *adrptr;
register u_char expected;
{

	u_int error;
	register u_char actual;

	error= 0;
	if(setjmp(fault_buf) ) {
#ifdef DEBUG
		printf("Bus Error during read/write, address= \n",
			(u_int)adrptr);
#endif DEBUG
		error= 1;
	} else {
#ifdef DEBUG
		printf("addr= %x data= %x\n", adrptr, expected);
#endif DEBUG
		WrtByte_Io8(adrptr, expected);
		if( (actual= RdByte_Io8(adrptr) ) != expected) {

#ifdef DEBUG
			printf("Data Path Error\n\r");
			printf("Addr= %x exp= %x act= %x\n",
				(u_int)adrptr, expected,actual);
#endif DEBUG
			error= 1;
		}
	}
	return(error);
}


/*
 * See if there is a digi board present
 */
DigiPresent()
{
	volatile u_char *ratadr;

	/* Address of scatch register */
	ratadr= (u_char *)(c8_addr_brd0[1] + SCRATCH_REG);

	nofault= &fault_buf[0];

	if( setjmp(fault_buf) ) {
#ifdef DEBUG
		printf("Nobody home\n");
#endif DEBUG
		return(0);	/* Nobody home */

	}
	else {
		WrtByte_Io8(ratadr, 0x55);
		pon_tmp = 0;
		if(RdByte_Io8(ratadr) == 0x55) {
#ifdef DEBUG
			printf("RAT present\n");
#endif DEBUG
			return(1);	/* RAT board present */
		}
		else {
#ifdef DEBUG
			printf("Nobody home today\n");
#endif DEBUG
			return(0);
		}
	}
}


static port_init(polled, numports)
register volatile char polled;
register u_int numports;
{

	register volatile u_char *digi;
	register volatile u_char port;
	int i;

	/* disable all interrupts at start */
#ifdef DEBUG
	printf("Disable all interrupts\n");
#endif DEBUG
	for(port=1; port <= (u_char)numports; port++){
		digi = (u_char *)(c8_addr[port] + RIENABL);
		WrtByte_Io8(digi, 0);
		wbflush();
	}

#ifdef DEBUG
	printf("Write RCtrl\n");
#endif DEBUG
	for(port=1; port <= (u_char)numports; port++){

		/* Set baud rate in line control register */
		digi = (u_char *)(c8_addr[port] + RCtrl);
		WrtByte_Io8(digi, CDLAB | EIGHTBPC);
		wbflush();
		for(i=0; i<200; i++);

		/* Use 9600 baud */
		digi = (u_char *)(c8_addr[port] + RSPEED);
		WrtByte_Io8(digi, c8_speeds[0]);
		wbflush();
		for(i=0; i<200; i++);
		digi = (u_char *)(c8_addr[port] + RSPEED +1);
		WrtByte_Io8(digi, 0);
		wbflush();
		for(i=0; i<200; i++);

		/* Non clear CDLAB bit, 8 bit word */
		digi = (u_char *)(c8_addr[port] + RCtrl);
		WrtByte_Io8(digi, EIGHTBPC);
		wbflush();
		for(i=0; i<200; i++);

		/* Enable all interrupts on data received */
		digi = (u_char *)(c8_addr[port] + RIENABL);
		WrtByte_Io8(digi, ERECV);
		wbflush();
		for(i=0; i<200; i++);

		if(!polled){
			/* Enable interrupts */
			digi = (u_char *)(c8_addr[port] + RMctrl);
			WrtByte_Io8(digi, ENINTR);
			for(i=0; i<200; i++);
			wbflush();
		}
	}
}


static send(c,port)
register volatile u_char c;
register volatile u_char port;
{
	register volatile u_char *digi;

#ifdef DEBUG
	printf("send\n");
#endif DEBUG

	/* Write Modem control for dtr and rts */
	digi = (u_char *)(c8_addr[port] + RMctrl);
	WrtByte_Io8(digi, (RdByte_Io8(digi) | CDTR| LOOPBCK | CRTS) );

	/* Read the line status */
	digi = (u_char *)(c8_addr[port] + RSTATUS);
	if(RdByte_Io8(digi) & STRDY) {
#ifdef DEBUG
		printf("sending\n");
#endif DEBUG

		/* Transmitter empty send the data */
		digi = (u_char *)(c8_addr[port] + RTDATA);
		WrtByte_Io8(digi, c);
		wbflush();
		return(0);
	}
	else {
#ifdef DEBUG
		printf("Data not transmitted, transmitter not ready\n");
		printf("Line Status Register= %x\n", RdByte_Io8(digi));
#endif DEBUG
		return(1);
	}
}


#ifdef	USED
static GetPortCnt()
{
	u_int *addr_ptr;
	u_char index;
	u_int num_of_ports;
	u_char tmpdat;

	/* Write all scratch registers */
	for(index= 1; index <= 16; index++) {
		WrtByte_Io8(c8_addr_brd0[index] + SCRATCH_REG, index);
	}

	/* Now read them back, see how many ports */
	for (index=1; index <= 16; index++) {
		tmpdat= RdByte_Io8(c8_addr_brd0[index] + SCRATCH_REG);
		if (tmpdat != index) {
			break;
		}
	}
	/*
	 * Don't use i for the number of ports since one of the ports may
	 * be bad and will go untested. We either have 8 or 16 ports.
	 */
	if (--index <= 8)
		num_of_ports= 8 ;
	else
		num_of_ports= 16;

#ifdef DEBUG
	printf("num_= %x\n", num_of_ports);
#endif DEBUG
	return(num_of_ports);
}
#endif	USED


static digi_handler()
{

	register volatile u_char *digi;
	register volatile u_char port;
	register volatile u_char c,tee = 0;
	register volatile u_short junk;
	register volatile u_char *clearint= (u_char *) 0xb0400000;

#ifdef M120
	register volatile u_short *isr = (u_short *)PHYS_TO_K1(ISR);

	junk = *isr;

#endif M120
#ifdef M20
	/* See if slot0 interrupt, active low */
	junk = (GetIntR() & 1);
	if(junk) {
#ifdef DEBUG
		printf("Level0 Intr not generated by AT device, IR= %8x\n",
		GetIntR());
#endif DEBUG
		return(1);
	}

	/* clear the interupt */
#ifdef DEBUG
	printf("Clearing interrupt\n");
#endif DEBUG
	*clearint= 0;
#endif M20

	port= port_id;

	/* Check Data set ready */
	digi = (u_char *)(c8_addr[port] + RMstat);
	wbflush();
	tee= RdByte_Io8(digi);
	/* Check clear to send */
	if(tee & SCTS){
		/* observe only the delta CTS which tells whether
			 * CTS changed state after the previous read
			 */
#ifdef DEBUG
		printf(" DSR AND CTS OK (ModemStat : %x) \n",tee);
#endif DEBUG
	}
	else {
#ifdef DEBUG
#ifdef DEBUG
		printf(" CTS FAILURE \n");
		printf("Modem Status Reg.= %x\n", tee);
#endif DEBUG
		digi = (u_char *)(c8_addr[port] + RSTATUS);
		wbflush();
		tee= RdByte_Io8(digi);
#ifdef DEBUG
		printf("Line Status Reg.=%x\n", tee);
#endif DEBUG

		/* Display receive buffer */
		digi = (u_char *)(c8_addr[port] + RRDATA);
		wbflush();
		tee= RdByte_Io8(digi);
#ifdef DEBUG
		printf("Receive Buffer=%x\n", tee);
#endif DEBUG

		/* Display modem status register */
		digi = (u_char *)(c8_addr[port] + RMstat);
		wbflush();
		tee= RdByte_Io8(digi);
#ifdef DEBUG
		printf("Modem Status= %x\n", tee);
#endif DEBUG

		/* Display modem interrupt ID */
		digi = (u_char *)(c8_addr[port] + RIIR);
		wbflush();
		tee= RdByte_Io8(digi);
#ifdef DEBUG
		printf("Interrupt Id Reg.= %x\n", tee);
#endif DEBUG
#endif DEBUG
		return(1);	/* skip last read*/
	}

	/* Check data ready status*/
	digi = (u_char *)(c8_addr[port] + RSTATUS);
	wbflush();
	tee= RdByte_Io8(digi);

	if(tee & SRRDY){
#ifdef DEBUG
		printf("Reading Data\n");
#endif DEBUG
		digi = (u_char *)(c8_addr[port] + RRDATA);
		wbflush();
		data= RdByte_Io8(digi);
		*clearint= 0;
		return(0);		/* skip last read		*/
	}
	/* Error data not ready */
	return(1);
}


static pondigi(com_port, numports)
int com_port;
int numports;
{
	register volatile u_char *digi;
	register volatile u_char pattern;
	register volatile u_char *clearint= (u_char *) 0xb0400000;
	register volatile u_char *digi1;
	register volatile u_char port;
	register volatile int delay ;
	register volatile int hang;
	u_char i = 0;
	u_int error, intrflag;
	u_char tmp;
	volatile u_char j;

#ifdef M120
	register volatile u_short *scr = (u_short *)PHYS_TO_K1(SCR);
	register volatile u_short *imr = (u_short *)PHYS_TO_K1(IMR);
	register volatile u_short *isr = (u_short *)PHYS_TO_K1(ISR);

	*scr = *scr | SCR_NORSTPCATBUS;
#endif M120
	wbflush();


repeat:
	port= com_port;
	port_id= port;
	c8_addr= (int *)c8_addr_brd0;

	/* Initialize all communication ports with rx intr enable*/
	port_init(0, numports);
	for(i=1; i <= numports; i++) {
#ifdef DEBUG
		printf("Reading Data\n");
#endif DEBUG
		digi = (u_char *)(c8_addr[port_id] + RRDATA);
		wbflush();
		tmp= RdByte_Io8(digi);
		*clearint= 0x0;

	}

#ifdef M120
	/* enable interrupts */
	*imr = *imr | INT_IRQ7;
#endif M120

	wbflush();

	for( j= 1, i = 1; j <= 8 ; j++, i <<= 1) {

		intrflag= 0;

		/* send data to the com port selected */
		nofault= &fault_buf[0];
		if(setjmp(fault_buf) ) {
			intrflag=1;
			error= digi_handler();
		}
		else {
			SetSR(GetSR() | SR_IBIT3 | SR_IEC);
			send(i,port);
		}
		for(delay = 0;((delay < 5000 ) && (!intrflag)); delay++);
		if(delay == 5000){
#ifdef DEBUG
			printf("TimeOut waiting for character\n");
#endif DEBUG
			return(1);
		}
		for(hang = 0 ; hang < 200; hang++);	/* delay */
#ifdef DEBUG
		printf("data=%x i=%x\n", data,i);
#endif DEBUG
		if( i != data) {
			/* Test Failure */
#ifdef DEBUG
			printf(" Actual Data read: %x Expected Data : %x\n",
				data,i);
#endif DEBUG
			return(1);
		}
		if(error)
			return(1);
	}

	return(0);
}


/*
 * Test only 8 ports until we find out how to tell the differences between
 * 8 port and 16 port digi boards.
 */
Pon_AtDigi()

{
	register u_char *adrptr;
	register u_char actual;
	register u_char expected;
	register u_char *cpuadr;
	register u_int	i;
	register u_int	port;
		 u_int	passcode= PASS;

	/* Trace mode message */
	pon_puts("AT Serial Board Tests...");

	if(CfbPresent() || !DigiPresent()) {
		pon_puts(skipped);
		return(PASS);
	}

	/* Install fault handler */
	nofault= &fault_buf[0];

	/* Init the address pointer */
	for (port = 1; port <= 16; port++) {
		adrptr= (u_char *)(c8_addr_brd0[port] + SCRATCH_REG);

		/* start with data pattern of zeros */
		expected= 0;
		if (WrtMemory(adrptr, expected))
			goto out;

		/* Try all ones */
		expected= 0xff;
		if (WrtMemory(adrptr, expected))
			goto out;

		/* Try sliding one */
		for( i=1, expected =1; i <= 8; i ++, expected <<= 1) {
			if (WrtMemory(adrptr, expected) ) {
				goto out;
			}
		}
		/* Try sliding zero */
		for( i=1, expected =1; i <= 8; i ++, expected <<= 1) {
			if (WrtMemory(adrptr, ~expected) ) {
				goto out;
			}
		}
	}

out:
	--port;
	if (port == 8) {
		goto done;
	}
	else if (port != 16) {
		passcode = FAIL;
	}

#ifdef	USED
	/* Get num of ports on digi board */
	port = GetPortCnt();
	if(port <= 8) {
		goto done;
	}
#endif	USED

#ifdef DEBUG
	printf("ports to test= %x\n", port);
#endif DEBUG

	for(i = 1; i <= port; i++) {
		if(passcode= pondigi(i, port))
			break;
	}

done:
	if(passcode != PASS) {
		pon_puts(failure);
		FastFlash(0);
		SetDepend(PON_FAULT_AT);
		return(FAIL);
	}
	else {
		pon_puts(success);
		return(PASS);
	}
}
