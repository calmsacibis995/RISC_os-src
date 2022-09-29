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
/* $Header: s2681cons.h,v 1.7.1.2 90/05/10 04:42:29 wje Exp $ */
#ifndef BSD43_
#    include <bsd43/bsd43_.h>
#endif BSD43_


/*
 * Templates onto the IO space of the cpu board SCN2681 duart.
 * See the Signetics data book for information.
 */

/*
 * The per channel IO template.
 */
struct bsd43_(cn_chan) {
	u_char pad0[3];u_char	cnc_mode;
	u_char pad1[3];u_char	cnc_stat;
	u_char pad2[3];u_char	cnc_cmd;
	u_char pad3[3];u_char	cnc_data;
};
#define bsd43_cnc_clockselect cnc_stat

/*
 * The entire 2681 IO template.
 */
struct bsd43_(cn_reg) {
	struct	bsd43_(cn_chan) 	cn_chan_a;
	u_char	pad0[3];u_char	cn_inport_change;
	u_char	pad1[3];u_char	cn_isr;
	u_char	pad2[3];u_char	cn_ctupper;
	u_char	pad3[3];u_char	cn_ctlower;
	struct	bsd43_(cn_chan)		cn_chan_b;
	u_char	pad4[3];u_char	cn_snipe;
	u_char	pad5[3];u_char	cn_inport;
	u_char	pad6[3];u_char	cn_startcount;
	u_char	pad7[3];u_char	cn_stopcount;
};
#define	bsd43_cn_auxreg		cn_inport_change
#define	bsd43_cn_imr			cn_isr
#define	bsd43_cn_outport_conf		cn_inport
#define	bsd43_cn_outport_set		cn_startcount
#define	bsd43_cn_outport_clear	cn_stopcount
/*
 *	2681 interrupt auxreg/inport change reg
 *	controls the baud select clock and counter/timer functions.
 */
#define BSD43_CNAUX_ENIP0	0x1	/* interrupt on input 0 status change */
#define BSD43_CNAUX_ENIP1	0x2	/* interrupt on input 1 status change */
#define BSD43_CNAUX_ENIP2	0x4	/* interrupt on input 2 status change */
#define BSD43_CNAUX_ENIP3	0x8	/* interrupt on input 3 status change */
#define BSD43_CNAUX_SET0	0x0	/* select baud rate set 0 */
#define BSD43_CNAUX_SET1	0x80	/* select baud rate set 1 */
/*
 *	2681 interrupt status/mask registers bit definitions.
 *	This is a global register shared between ports A and B.
 */
#define	BSD43_CNIMR_TXRDY_A	0x01	/* channel A Tx interrupt */
#define	BSD43_CNIMR_RXRDY_A	0x02	/* channel A Rx interrupt */
#define	BSD43_CNIMR_BREAK_A	0x04	/* channel A break interrupt */
#define	BSD43_CNIMR_COUNTER	0x08	/* counter interrupt */
#define	BSD43_CNIMR_TXRDY_B	0x10	/* channel B Tx interrupt */
#define	BSD43_CNIMR_RXRDY_B	0x20	/* channel B Rx interrupt */
#define	BSD43_CNIMR_BREAK_B	0x40	/* channel B break interrupt */
#define	BSD43_CNIMR_CD_B	0x80	/* input port. wired to CD for channel B */
/*
 *	2681 status register bit definitions.
 *	One status register per port.
 */
#define	BSD43_CNSTAT_RXRDY	0x01	/* data available in Rx FIFO */
#define	BSD43_CNSTAT_FFULL	0x02	/* Rx FIFO full. (3 bytes) */
#define	BSD43_CNSTAT_TXRDY	0x04	/* Tx will accept another byte */
#define	BSD43_CNSTAT_TXEMPTY	0x08	/* Tx empty. (both buffer and shifter) */
#define	BSD43_CNSTAT_OVERRUN	0x10	/* Rx FIFO overrun */
#define	BSD43_CNSTAT_PARITY	0x20	/* parity error */
#define	BSD43_CNSTAT_FRAME	0x40	/* framing error */
#define	BSD43_CNSTAT_RXBREAK	0x80	/* Rx break detected */
#define	BSD43_CNSTAT_ERROR	(BSD43_CNSTAT_OVERRUN | BSD43_CNSTAT_PARITY | BSD43_CNSTAT_FRAME)
/*
 *	2681 command register bit definitions.
 *	One command register per port.
 */
#define	BSD43_CNCMD_RXENABLE		0x01	/* start looking for data */
#define	BSD43_CNCMD_RXDISABLE		0x02	/* stop accepting for data */
#define	BSD43_CNCMD_TXENABLE		0x04	/* accept new data to send */
#define	BSD43_CNCMD_TXDISABLE		0x08	/* do not accept new data to send */
#define	BSD43_CNCMD_MRRESET		0x10	/* point to MR1 for next mode access */
#define	BSD43_CNCMD_RXRESET		0x20	/* disable Rx, flush FIFO */
#define	BSD43_CNCMD_TXRESET		0x30	/* disable Tx, flush buffer */
#define	BSD43_CNCMD_ERRESET		0x40	/* clear error bits in status reg */
#define	BSD43_CNCMD_BREAKRESET	0x50	/* clear break change status */
#define	BSD43_CNCMD_BREAKSTART	0x60	/* Start sending break after Tx idle */
#define	BSD43_CNCMD_BREAKSTOP		0x70	/* Terminate break condition */
/*
 *	2681 mode register 1 bit definitions.
 *	One mode register 1 per port.
 *	The first access to the mode register after a reset or after a 
 *	reset MR pointer command will access MR1. All other accesses will
 *	be to MR2
 */
#define	BSD43_CNMR1_RXRTS		0x80	/* enable wierd FFULL control flow */
#define	BSD43_CNMR1_RXFULLINTR	0x40	/* interrupt on FFULL, else on RDY */
#define	BSD43_CNMR1_ERRSUM		0x20	/* sumarize all FIFO errors in stat */
#define	BSD43_CNMR1_PARITY		0x00	/* generate and detect parity */
#define	BSD43_CNMR1_FORCEPARITY	0x08	/* generate and detect parity */
#define	BSD43_CNMR1_NOPARITY		0x10	/* do not generate and detect parity */
#define	BSD43_CNMR1_MULTDROP		0x18	/* enter multi-drop mode */
#define	BSD43_CNMR1_EVENPARITY	0x00	/* generate and accept even parity */
#define	BSD43_CNMR1_ODDPARITY		0x04	/* generate and accept odd parity */
#define	BSD43_CNMR1_BITS5		0x01	/* 5 bits per character */
#define	BSD43_CNMR1_BITS6		0x01	/* 6 bits per character */
#define	BSD43_CNMR1_BITS7		0x02	/* 7 bits per character */
#define	BSD43_CNMR1_BITS8		0x03	/* 8 bits per character */
/*
 *	2681 mode register 2 bit definitions
 *	One mode register 2 per port.
 */
#define	BSD43_CNMR2_AUTOECHO		0x40	/* retransmit all recieved data */
#define	BSD43_CNMR2_LOCALLOOP   	0x80	/* internal loopback */
#define	BSD43_CNMR2_REMOTELOOP   	0xC0	/* play echo deamon */
#define	BSD43_CNMR2_TXRTS		0x20	/* enable wierd RTS handshake */
#define	BSD43_CNMR2_TXCTS		0x10	/* honor CTS handshake */
#define	BSD43_CNMR2_STOP1		0x07	/* generate 1 stop bit */
#define	BSD43_CNMR2_STOP2		0x0f	/* generate 2 stop bit */
/*
 *	Global output port bit definitions.
 *	OP0 and OP1 are all that are available.
 */
#define	BSD43_CNOP_DTR_B		0x01	/* assert DTR for channel B */
#define	BSD43_CNOP_RTS_B		0x02	/* assert RTS for channel B */
/*
 *	Global input port bit definitions.
 *	IP2 is the only one available.
 */
#define	BSD43_CNIP_CTS_B		0x04	/* maybe hooked to CTS or.. */
#define	BSD43_CNIP_CD_B		0x04	/* .. maybe to CD. */

/*----- NUMIPS: STRIP MACRO PREFIXES (BEGIN) ---------------------------------*/
/* 
 * Strip off "BSD43_" and "bsd43_" for use with standard BSD code.
 * (GENERATED AUTOMATICALLY -- DON'T EDIT BY HAND)
 */
#ifdef SYSTYPE_BSD43
#   define CNAUX_ENIP0 BSD43_CNAUX_ENIP0
#   define CNAUX_ENIP1 BSD43_CNAUX_ENIP1
#   define CNAUX_ENIP2 BSD43_CNAUX_ENIP2
#   define CNAUX_ENIP3 BSD43_CNAUX_ENIP3
#   define CNAUX_SET0 BSD43_CNAUX_SET0
#   define CNAUX_SET1 BSD43_CNAUX_SET1
#   define CNCMD_BREAKRESET BSD43_CNCMD_BREAKRESET
#   define CNCMD_BREAKSTART BSD43_CNCMD_BREAKSTART
#   define CNCMD_BREAKSTOP BSD43_CNCMD_BREAKSTOP
#   define CNCMD_ERRESET BSD43_CNCMD_ERRESET
#   define CNCMD_MRRESET BSD43_CNCMD_MRRESET
#   define CNCMD_RXDISABLE BSD43_CNCMD_RXDISABLE
#   define CNCMD_RXENABLE BSD43_CNCMD_RXENABLE
#   define CNCMD_RXRESET BSD43_CNCMD_RXRESET
#   define CNCMD_TXDISABLE BSD43_CNCMD_TXDISABLE
#   define CNCMD_TXENABLE BSD43_CNCMD_TXENABLE
#   define CNCMD_TXRESET BSD43_CNCMD_TXRESET
#   define CNIMR_BREAK_A BSD43_CNIMR_BREAK_A
#   define CNIMR_BREAK_B BSD43_CNIMR_BREAK_B
#   define CNIMR_CD_B BSD43_CNIMR_CD_B
#   define CNIMR_COUNTER BSD43_CNIMR_COUNTER
#   define CNIMR_RXRDY_A BSD43_CNIMR_RXRDY_A
#   define CNIMR_RXRDY_B BSD43_CNIMR_RXRDY_B
#   define CNIMR_TXRDY_A BSD43_CNIMR_TXRDY_A
#   define CNIMR_TXRDY_B BSD43_CNIMR_TXRDY_B
#   define CNIP_CD_B BSD43_CNIP_CD_B
#   define CNIP_CTS_B BSD43_CNIP_CTS_B
#   define CNMR1_BITS5 BSD43_CNMR1_BITS5
#   define CNMR1_BITS6 BSD43_CNMR1_BITS6
#   define CNMR1_BITS7 BSD43_CNMR1_BITS7
#   define CNMR1_BITS8 BSD43_CNMR1_BITS8
#   define CNMR1_ERRSUM BSD43_CNMR1_ERRSUM
#   define CNMR1_EVENPARITY BSD43_CNMR1_EVENPARITY
#   define CNMR1_FORCEPARITY BSD43_CNMR1_FORCEPARITY
#   define CNMR1_MULTDROP BSD43_CNMR1_MULTDROP
#   define CNMR1_NOPARITY BSD43_CNMR1_NOPARITY
#   define CNMR1_ODDPARITY BSD43_CNMR1_ODDPARITY
#   define CNMR1_PARITY BSD43_CNMR1_PARITY
#   define CNMR1_RXFULLINTR BSD43_CNMR1_RXFULLINTR
#   define CNMR1_RXRTS BSD43_CNMR1_RXRTS
#   define CNMR2_AUTOECHO BSD43_CNMR2_AUTOECHO
#   define CNMR2_LOCALLOOP BSD43_CNMR2_LOCALLOOP
#   define CNMR2_REMOTELOOP BSD43_CNMR2_REMOTELOOP
#   define CNMR2_STOP1 BSD43_CNMR2_STOP1
#   define CNMR2_STOP2 BSD43_CNMR2_STOP2
#   define CNMR2_TXCTS BSD43_CNMR2_TXCTS
#   define CNMR2_TXRTS BSD43_CNMR2_TXRTS
#   define CNOP_DTR_B BSD43_CNOP_DTR_B
#   define CNOP_RTS_B BSD43_CNOP_RTS_B
#   define CNSTAT_ERROR BSD43_CNSTAT_ERROR
#   define CNSTAT_FFULL BSD43_CNSTAT_FFULL
#   define CNSTAT_FRAME BSD43_CNSTAT_FRAME
#   define CNSTAT_OVERRUN BSD43_CNSTAT_OVERRUN
#   define CNSTAT_PARITY BSD43_CNSTAT_PARITY
#   define CNSTAT_RXBREAK BSD43_CNSTAT_RXBREAK
#   define CNSTAT_RXRDY BSD43_CNSTAT_RXRDY
#   define CNSTAT_TXEMPTY BSD43_CNSTAT_TXEMPTY
#   define CNSTAT_TXRDY BSD43_CNSTAT_TXRDY
#   define cn_auxreg bsd43_cn_auxreg
#   define cn_imr bsd43_cn_imr
#   define cn_outport_clear bsd43_cn_outport_clear
#   define cn_outport_conf bsd43_cn_outport_conf
#   define cn_outport_set bsd43_cn_outport_set
#   define cnc_clockselect bsd43_cnc_clockselect
#endif SYSTYPE_BSD43
/*----- NUMIPS: STRIP MACRO PREFIXES (END) -----------------------------------*/


