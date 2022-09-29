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
/* $Header: uartreg.h,v 1.5.1.2 90/05/10 06:42:53 wje Exp $ */
/*
 * $Header: uartreg.h,v 1.5.1.2 90/05/10 06:42:53 wje Exp $
 */
/*
 * structure definitions for generic uart driver for IOP
 * Project: Jupiter Workstation
 * Date: 08-Feb-1988
 */

/*
 * Local structures for uart driver.
 * Note:
 *   Must include termio.h before this file or compiler will barf.
 */
struct uart_s {
	struct async *com_blk;	/* IOP communication block */
	struct ss_line *uart_dpl;/* line information buffer */
	int driver_state;	/* internal driver state */
	int uart_line;		/* Most routines will only get a pointer
				   to a Uart_t.  This is a convenient place
				   to keep it. */
	int xmit_request_size;	/* number of bytes on last xmit */
	int timer_id;		/* Used when setting delays. */
	int intr_mask;		/* interrupt bits or'd in with command */
	int cmd_retry;		/* when iopb is busy command is saved here. */
	caddr_t xmit_buffer;	/* R2000 address of xmit buffer */
	caddr_t rcv_buffer;	/* R2000 address of receive buffer */
};
typedef struct uart_s Uart_t;

struct convert_table {
	int iop_value;		/* This is what the iop wants to see. */
	int termio_value;	/* This is what termio will give us. */
};
typedef struct convert_table Convert_Table_t;

/*
 * Local defines
 */
#define UART_BUF_SIZE	((NBPP/2) - sizeof(long))
#define UART_MAGIC	0x1234bad0
#define UART_MSG_SIZE	4	/* WAG. Should do some testing and see if this
				   is enough. */
#define V50_DEBUG_PORT	1	/* The V50 has a debug monitor that is
				   available on this port by typing Control-X.
				   If someone opens this device this feature
				   is disabled. */

/* driver_state */
#define CLEAN_STATE	(-1)	/* During open we will reset the driver
				   state.  If any bits need to be preserved
				   'or' them in. */
#define UART_IS_OPEN	(1<<0)	/* Driver has been opened and is ready
				   to transmit and receive */
#define UART_WOPEN	(1<<1)	/* Wait for carrier.  Driver is sleeping
				   in the open routine. */
#define UART_TRANSMIT	(1<<2)	/* IOP is transmitting for us.  Don't touch
				   the ring buffer until we get a transmitter
				   ready interrupt. */
#define UART_CARRIER_ON	(1<<3)	/* if set data carrier has been detected
				   on line */
#define UART_STOPPED	(1<<4)	/* The kernel has stopped output for one
				   reason or another.  When this bit is set
				   the IOP should be stopped and uart_start
				   will not restart output until this bit is
				   clear. */
#define UART_XMITTING	(1<<5)	/* The IOP is currently transmitting. */
#define UART_IS_AVAIL	(1<<6)	/* The uart complete uartinit() o.k. */
#define UART_IS_SLEEP	(1<<7)	/* Uart is waiting for a completion
				   interrupt from the IOP */
#define UART_TIMESTAMP	(1<<8)	/* uart input is timestamped */


/*
 * Driver macros
 */
#define UART_LINE(dev) ((dev) & 0x3)
#define UART_MODEM_LINE(dev) ((dev) & 0x80)
#ifndef MIN
#define MIN(a, b)	(((a) < (b)) ? (a) : (b))
#endif MIN
