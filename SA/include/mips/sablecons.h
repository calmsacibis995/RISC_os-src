#ident "$Header: sablecons.h,v 1.5 90/01/23 14:19:54 huang Exp $"
/* $Copyright$ */

/* This structure used to be 5 longs.  The new character addressing
 * is required for Intrepid.  Since it is a simulated device, we
 * might as well put this requirment on other version of hardware also.
 */

struct scons_device {
	unsigned	:24;
	unsigned char	sc_status;	/* status register */
	unsigned	:24;
	unsigned char	sc_command;	/* command register */
	unsigned	:24;
	unsigned char	sc_rx;		/* receiver data register */
	unsigned	:24;
	unsigned char	sc_tx;		/* transmitter data register */
	unsigned	:24;
	unsigned char	sc_txbuf;	/* transmitter data buffer */
};

#define	SC_STAT_RXRDY	1		/* reciever has data available */
#define	SC_CMD_RXIE	1		/* reciever interrupt enable */
#define	SC_CMD_TXFLUSH	2		/* flush any buffered output */

/* These numbers are based on Intrepid SABLE which have moved them
 * from their original location.  Since this is a simulated device
 * it is really not a problem.  We will just move them to the new
 * location everywhere
 */

#define SCONS0_BASE ((struct scons_device *)(0x1f000000+K1BASE))
#define SCONS1_BASE ((struct scons_device *)(0x1f000020+K1BASE))
