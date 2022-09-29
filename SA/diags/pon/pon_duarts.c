#ident "$Header: pon_duarts.c,v 1.11.3.1 90/07/18 14:30:38 huang Exp $"
/* $Copyright: |
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

/* ------------------------------------------------------------------ */
/* | Copyright Unpublished, MIPS Computer Systems, Inc.  All Rights | */
/* | Reserved.  This software contains proprietary and confidential | */
/* | information of MIPS and its suppliers.  Use, disclosure or     | */
/* | reproduction is prohibited without the prior express written   | */
/* | consent of MIPS.                                               | */
/* ------------------------------------------------------------------ */

#include "sys/types.h"
#include "machine/duart.h"
#include "machine/asm.h"
#include "machine/cpu_board.h"
#include "pon.h"

extern u_int DUART0[];
extern u_int DUART1[];
extern int machine_type;
extern char success[];

static u_char baud_rate[] = {
	0xcc,	/* 19.2k */
	0xbb,	/* 9600 */
	0xaa,	/* 7200 */
	0x99,	/* 4800 */
	0x88,	/* 2400 */
	0x66,	/* 1200 */
	0x55,	/* 600 */
	0x44	/* 300 */
};

#define BAUD_RATES	sizeof(baud_rate)


Pon_Duarts()

{
	register u_char *duart_reg;
	register u_char ledcode;
	u_char x, y, z, status;

	if ((machine_type == BRDTYPE_R3030) || (machine_type == BRDTYPE_RB3125))
	{
		return(PASS);
	}

	pon_puts("Duart Port Tests...");
	for (x = 0; x < BAUD_RATES; x++) {	/* use 8 baud rates */
		z = baud_rate[x];		/* init the current baud rate */
		y = 0x87;			/* local loopback & 1 stop bit */

		/* DUART 1 channel B local mode diagnostic */

		pon_set_leds(PON_DUART1B);
		duart_reg = (char *)(MACHDEP(DUART0) | 0x20);
		channel_init(duart_reg, y, z);
		status = tx_rx(duart_reg, TBYTE_DATA + x);
		if (status == FAIL) {
			InitDuart1B();
			pon_puts("FAILED Duart 1, Channel B\n\r");
			ledcode = PON_DUART1B;
			goto failed;
		}

		if (!IS_R3200 && !IS_R6300) {

		/* DUART 2 channel A local mode diagnostic */

		pon_set_leds(PON_DUART2A);
		duart_reg = (char *)MACHDEP(DUART1);
		channel_init(duart_reg, y, z);
		status = tx_rx(duart_reg, TBYTE_DATA + x);
		if (status == FAIL) {
			InitDuart1B();
			pon_puts("FAILED Duart 2, Channel A\n\r");
			ledcode = PON_DUART2A;
			goto failed;
		}

		/* DUART 2 channel B local mode diagnostic */

		pon_set_leds(PON_DUART2B);
		duart_reg = (char *)(MACHDEP(DUART1) | 0x20);
		channel_init(duart_reg, y, z);
		status = tx_rx(duart_reg, TBYTE_DATA + x);
		if (status == FAIL) {
			InitDuart1B();
			pon_puts("FAILED Duart 2, Channel B\n\r");
			ledcode = PON_DUART2B;
			goto failed;
		}
		}
	}

	InitDuart1B();
	pon_puts(success);
	return(PASS);

failed:
	FastFlash(ledcode);
	pon_set_leds(ledcode);
	SetDepend(PON_FAULT_DUARTS);
	return(FAIL);
}


/* Initialize the channel */

static channel_init(duart_reg, mode_set, baud_set)

register u_char *duart_reg;
register u_char mode_set;
register u_char baud_set;

{
	duart_reg[8] = 0x2a;		/* reset receiver, disable tx & rx */
	FlushWB();
	duart_reg[8] = 0x3a;		/* reset transmitter, disable tx & rx */
	FlushWB();
	duart_reg[8] = 0x4a;		/* reset error status,disable tx & rx */
	FlushWB();
	duart_reg[8] = 0x1a;		/* reset MR pointer, disable tx & rx */
	FlushWB();
	duart_reg[0] = 0x13;		/* no parity, 8 bits/char */
	FlushWB();
	duart_reg[0] = mode_set;	/* reset_byte[6]; local/remote & stop */
	FlushWB();
	duart_reg[4] = baud_set;	/* reset_byte[7]; set tx & rx clock */
	FlushWB();
	duart_reg[8] = 0x5;		/* enable tx and rx */
	FlushWB();
}


/* Transmit and receive routine */

static tx_rx(duart_reg, data_byte)

register u_char *duart_reg;
register u_char data_byte;

{
	volatile u_char *reg4 = &duart_reg[4];
	volatile u_char *reg12 = &duart_reg[12];
	u_char x, rx_char;
	u_int delay = 0x400;

	/* Check status reg for TX READY */

	while ((*reg4 & SRTXRDY) == FALSE && (delay-- != 0))
		;

	if (!(*reg4 & SRTXRDY)) {
#ifdef	DEBUG
		pon_puts("timed out waiting for TXRDY\n\r");
#endif	DEBUG
		return(FAIL);
	}

	for (x = 0; x < BYTE_CNT; x++) {	/* send out 20 characters */
		delay = 0x4000;			/* reset delay count */
		*reg12 = data_byte + x;		/* RHRA/THRA (Send the byte) */

		/* Check status reg for RX READY */

		while ((*reg4 & SRRXRDY) == FALSE && (delay-- != 0))
			;

		if (!(*reg4 & SRRXRDY)) {
#ifdef	DEBUG
			pon_puts("timed out waiting for RXRDY\n\r");
#endif	DEBUG
			return(FAIL);
		}

		rx_char = *reg12;		/* RHRA/THRA (Receive the byte) */
		if (rx_char != (data_byte + x)) {
#ifdef	DEBUG
			pon_puts("rx'ed character different from that tx'ed\n\r");
#endif	DEBUG

			return(FAIL);
		}
	}

	return(PASS);
}
