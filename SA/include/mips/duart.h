#ident "$Header: duart.h,v 1.2 90/01/23 14:12:30 huang Exp $"
/* $Copyright$ */

/************************************************************************/
/*      duart registers                                                 */
/************************************************************************/

/* channel specific duart registers */

#ifdef LANGUAGE_C
struct duart        /* based at CHABASE or CHBBASE */
   {
   unsigned char pad0[2];       /* to put registers at the odd addresses */
   unsigned char d_mr;       /* channel mode register (1 or 2) */
   unsigned char pad3[3];
   unsigned char d_sr;       /* channel status register (read) */
   unsigned char pad7[3];
   unsigned char d_cr;       /* channel command register */
   unsigned char padb[3];
   unsigned char d_rhr;      /* channel receiver holding register (read) */
   unsigned char padf;
   };

/* non-channel specific duart registers */

struct duint        /* based at CHABASE */
   {
   struct duart channel_a;
   unsigned char pad10[2];    /* duart structure for channel a plus one pad */
   unsigned char d_ipcr;     /* input port change register */
   unsigned char pad13[3];
   unsigned char d_isr;      /* interrupt status register */
   unsigned char pad17[3];
   unsigned char d_ctu;      /* counter/timer upper */
   unsigned char pad1b[3];
   unsigned char d_ctl;      /* counter/timer lower */
   unsigned char pad1f;   /* duart structure for channel b plus one pad */
   struct duart channel_b;
   unsigned char pad20[2];    /* duart structure for channel a plus one pad */
   unsigned char d_ivr;      /* interrupt vector register */
   unsigned char pad23[3];
   unsigned char d_iport;    /* input port */
   unsigned char pad27[3];
   unsigned char d_ccgo;     /* start counter command */
   unsigned char pad2b[3];
   unsigned char d_ccstp;    /* stop counter command */
   unsigned char pad2f;
   };
#endif
#ifdef SABLE
#ifdef LANGUAGE_C
struct scons_device {
	unsigned long	sc_status;	/* status register */
	unsigned long	sc_command;	/* command register */
	unsigned long	sc_rx;		/* receiver data register */
	unsigned long	sc_tx;		/* transmitter data register */
	unsigned long	sc_txbuf;	/* transmitter data buffer */
};
#define SCONS_BASE ((struct scons_device *)(0x1f000000+K1BASE))
#else
#define SCONS_BASE (0x1f000000+K1BASE)
#endif
#define	SC_STATUS	0	/* status register */
#define	SC_COMMAND	4	/* command register */
#define	SC_RX		8	/* receiver data register */
#define	SC_TX		12	/* transmitter data register */
#define	SC_TXBUF	16	/* transmitter data buffer */

#define	SC_STAT_RXRDY	1		/* reciever has data available */
#define	SC_CMD_RXIE	1		/* reciever interrupt enable */
#define	SC_CMD_TXFLUSH	2		/* flush any buffered output */

#endif
#define UART_BASE 0xbe008002

/* aliases for registers with different meanings on read and write */

#define	d_csr	d_sr		/* Clock Select Register (A/B) */
#define	d_thr	d_rhr		/* TX Holding Register (A/B) */
#define	d_acr	d_ipcr		/* Aux Control Register */
#define	d_imr	d_isr		/* Interrupt Mask Register */
#define	d_ctur	d_ctu		/* C/T Upper Register */
#define	d_ctlr	d_ctl		/* C/T Lower Register */
#define	d_opcr	d_iport		/* Output Port Configuration Reg */
#define	d_sopbc	d_ccgo		/* Set Output Port Bits Command */
#define	d_ropbc	d_ccstp		/* Reset Output Port Bits Command */

/************************************************************************/
/*      base io addresses                                               */
/************************************************************************/

#define DUART_BASE (UART_BASE & 0xfffffffc)
#ifdef LANGUAGE_C
#define CHABASE ((struct duart *) DUART_BASE)      /* channel a base addr */
#define CHBBASE ((struct duart *) (DUART_BASE + 0x20)) /* channel b base addr */
#else
#define CHABASE ( DUART_BASE)      /* channel a base addr */
#define CHBBASE ( (DUART_BASE + 0x20)) /* channel b base addr */
#endif
#define D_MR	0x02	/* generic channel mode register (1 or 2) */
#define D_MRA	D_MR	/* channel mode register (1 or 2) */
#define D_SR	0x06	/* generic channel status register (read) */
#define D_SRA	D_SR	/* channel status register (read) */
#define	D_CSR	D_SR	/* generic Clock Select Register (A/B) */
#define	D_CSRA	D_SR	/* channel A Clock Select Register */
#define D_CR	0x0a	/* generic channel command register */
#define D_CRA	D_CR	/* channel command register */
#define D_RHR	0x0e	/* generic channel receiver holding register (read) */
#define D_RHRA	D_RHR	/* channel receiver holding register (read) */
#define	D_THR	D_RHR	/* generic tX Holding Register (A/B) */
#define	D_THRA	D_RHR	/* generic tX Holding Register (A/B) */
#define D_IPCR	0x12	/* input port change register */
#define	D_ACR	D_IPCR	/* Aux Control Register */
#define D_ISR	0x16	/* interrupt status register */
#define	D_IMR	D_ISR	/* Interrupt Mask Register */
#define D_CTU	0x1a	/* counter/timer upper */
#define	D_CTUR	D_CTU	/* C/T Upper Register */
#define D_CTL	0x1e	/* counter/timer lower */
#define	D_CTLR	D_CTL	/* C/T Lower Register */
#define D_MRB	0x22	/* channel B mode register (1 or 2) */
#define D_SRB	0x26	/* channel B status register (read) */
#define	D_CSRB	D_SRB	/* channel B Clock Select Register */
#define D_CRB	0x2a	/* channel B command register */
#define D_RHRB	0x2e	/* channel B receiver holding register (read) */
#define D_THRB	D_RHRB	/* channel B receiver holding register (read) */
#define D_IPORT	0x36	/* input port */
#define	D_OPCR	D_IPORT	/* Output Port Configuration Reg */
#define D_CCGO	0x3a	/* start counter command */
#define	D_SOPBC	D_CCGO	/* Set Output Port Bits Command */
#define D_CCSTP	0x3e	/* stop counter command */
#define	D_ROPBC	D_CCSTP	/* Reset Output Port Bits Command */


/************************************************************************/
/*      bit values for interrupt status register (d_isr)                */
/************************************************************************/

#define IPC      0x80    /* inport status change */
#define B_BREAK  0x40    /* change in channel b break state */
#define B_RXRDY  0x20    /* channel b receiver ready */
#define B_TXRDY  0x10    /* channel b transmitter ready */
#define CTR_RDY  0x08    /* counter done */
#define A_BREAK  0x04    /* change in channel a break state */
#define A_RXRDY  0x02    /* channel a receiver ready */
#define A_TXRDY  0x01    /* channel a transmitter ready */

/*
  Interrupt Mask Register bits control whether the setting of the
  corresponding bit in the ISR generates an interrupt.  If the
  bit in the IMR is reset (off), then no interrupt is generated.
*/

#define DNOINT   0x00    /* IMR value to disable interrupts */

/************************************************************************/
/*      status register bits                                            */
/************************************************************************/

#define SRERRORS 0xf0    /* some kind of error occurred */
#define SRBREAK  0x80    /* received break on this char */
#define SRFRM    0x40    /* framing error on receive */
#define SRPAR    0x20    /* parity error on receive */
#define SROVR    0x10    /* overflow on receive */
#define SREMT    0x08    /* all chars sent */
#define SRTXRDY  0x04    /* can take char to send */
#define SRFULL   0x02    /* fifo is full */
#define SRRXRDY  0x01    /* char available */

/************************************************************************/
/*      commands for command register                                   */
/************************************************************************/

#define STOP_BRK  0x70   /* stop break output on channel */
#define START_BRK 0x60   /* start break output on channel */
#define RES_BRK  0x50    /* reset break indication on channel */
#define RES_ERR  0x40    /* reset error flags in status register */
#define RES_TX   0x30    /* reset transmitter */
#define RES_RX   0x20    /* reset receiver */
#define SEL_MR1  0x10    /* select mr1 register for next mr action */
#define DIS_TX   0x08    /* disable transmitter */
#define EN_TX    0x04    /* enable transmitter */
#define DIS_RX   0x02    /* disable receiver */
#define EN_RX    0x01    /* enable receiver */

/************************************************************************/
/*      input port change register bits                                 */
/************************************************************************/

#define DIP3     0x80    /* delta input 3 */
#define DIP2     0x40    /* delta input 2 */
#define DIP1     0x20    /* delta input 1 */
#define DIP0     0x10    /* delta input 0 */
#define IP5      0x20    /* input 5 */
#define IP4      0x10    /* input 4 */
#define IP3      0x08    /* input 3 */
#define IP2      0x04    /* input 2 */
#define IP1      0x02    /* input 1 */
#define IP0      0x01    /* input 0 */

/************************************************************************/
/*      Clock Select Register                                           */
/************************************************************************/

#define b110     0x11    /* 110 baud */
#define b1200    0x66    /* 1200 baud */
#define b134     0x22    /* 134 baud */
#define b150     0x33    /* 150 baud */
#define b1800    0xaa    /* 1800 baud */
#define b19200   0xcc    /* 19200 baud */
#define b2400    0x88    /* 2400 baud */
#define b300     0x44    /* 300 baud */
#define b4800    0x99    /* 4800 baud */
#define b600     0x55    /* 600 baud */
#define b75      0x00    /* 75 baud */
#define b9600    0xbb    /* 9600 baud */

/************************************************************************/
/*      auxillary control register bits                                 *
/************************************************************************/

#define SET2     0x80	/* Enable baud rate set 2 */

/************************************************************************/
/*      auxillary control register bits                                 *
/************************************************************************/

#define stop2 	 0x0f    /* 2 stop bits */
#define stop1p5  0x08    /* 1.5 stop bits */
#define stop1    0x07    /* 1 stop bit */
