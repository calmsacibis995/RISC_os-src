/*
 * |-----------------------------------------------------------|
 * | Copyright (c) 1990 MIPS Computer Systems, Inc.            |
 * | All Rights Reserved                                       |
 * |-----------------------------------------------------------|
 * |          Restricted Rights Legend                         |
 * | Use, duplication, or disclosure by the Government is      |
 * | subject to restrictions as set forth in                   |
 * | subparagraph (c)(1)(ii) of the Rights in Technical        |
 * | Data and Computer Software Clause of DFARS 52.227-7013.   |
 * |         MIPS Computer Systems, Inc.                       |
 * |         928 Arques Avenue                                 |
 * |         Sunnyvale, CA 94086                               |
 * |-----------------------------------------------------------|
 */
/* $Header: mouse.h,v 1.2.2.2 90/05/10 06:29:17 wje Exp $ */
/*
 * $Header: mouse.h,v 1.2.2.2 90/05/10 06:29:17 wje Exp $
 */
/*
 * Header file for Mickey Mouse driver.
 */

/*
 * convert baud rate to in bits per second to a load value for
 * the clock. we assume a baud rate factor (divisor) of 16.
 * input clock is 5mhz and serial unit causes an additional divide of 16 
 * so effective baud clock is 312500 times per second.
 */
#ifdef V50EM
#define BAUD_TO_LOAD(b)	(250000 / b)			/* emulator runs at 8mhz */
#else
#define BAUD_TO_LOAD(b)	(312500 / b)
#endif

struct MouseInterface {
	u_short		m_baud;		/* Contains the baud rate load value
					 * i.e 9600 = BAUD_TO_LOAD(9600). */
	u_short		m_code_reg;	/* length, parity, stop bits */
	u_short		m_status_reg;	/* break, fe, ove, pe, rdrdy, txrdy */
	u_short		m_xmit_char;
	u_short		m_rcv_char;
	u_short		m_command;
	u_short		m_debug_level;
};
typedef struct MouseInterface MouseReg;

/*
 * ioc_statparm values for the mouse driver.
 */
#define M_STAT_OK		0
#define M_STAT_ERR		1

/*
 * m_code_reg:
 * The format of this register is identical to the V50's serial code register,
 * hence the name.  This was done to make the V50's life a little easier.
 */
/* Baud rate factor (not currently used) */
#define M_BRF_SHIFT		0
#define M_BRF_MASK		(3 << M_BRF_SHIFT)
#define M_BRF_16		(2 << M_BRF_SHIFT)
#define M_BRF_64		(3 << M_BRF_SHIFT)

/* Character length */
#define M_CS_SHIFT		2
#define M_CS_MASK		3
#define M_CS7			(2 << M_CS_SHIFT)
#define M_CS8			(3 << M_CS_SHIFT)

/* Parity even/odd */
#define M_PARITY_SHIFT	4
#define M_PARITY_MASK		3
#define M_PARITY_NONE		(0 << M_PARITY_SHIFT)
#define M_PARITY_ODD		(1 << M_PARITY_SHIFT)
#define M_PARITY_EVEN		(3 << M_PARITY_SHIFT)

/* Stop bits */
#define M_STOPBITS_SHIFT	6
#define M_STOPBITS_MASK	3
#define M_STOP1		(1 << M_STOPBITS_SHIFT)
#define M_STOP2		(3 << M_STOPBITS_SHIFT)

/*
 * m_status_reg:
 * The format of this register is identical to the V50' serial status register,
 * hence the name.
 */
/* Transmit data buffer ready */
#define M_XMIT_RDY		(1 << 0)

/* Receive data buffer ready */
#define M_RCV_RDY		(1 << 1)

/* Unused status bit (should always be 1) */
#define M_STATUS_BIT2		(1 << 2)

/* Parity error */
#define M_PARITY_ERROR	(1 << 3)

/* Overrun error */
#define M_OVERRUN_ERROR	(1 << 4)

/* Framing error */
#define M_FRAME_ERROR		(1 << 5)

/* Break detector */
#define M_BREAK_DETECTED	(1 << 6)

/* Command done for M_PARAMS_SET or M_DEBUG_COMMAND */
#define M_CMD_DONE			(1 << 7)

/*
 * m_commands
 * This is a simple interface that should only have a mouse attached.
 * Therefore the command interface is simple.
 * Bit 7 of this register indicates if and interrupt should be sent.
 * Interrupts are caused by:
 *  . Command completion, no exceptions.
 *  . serial status register changes value.
 */
#define M_PARAMS_SET		0x01
#define M_TRANSMIT		0x02
#define M_RCV_ACK		0x03	/* acks error conditions as well */
#define M_DEBUG_COMMAND	0x04
#define M_INTR		0x80
