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
/* $Header: cdsioreg.h,v 1.4.4.2 90/05/10 06:07:18 wje Exp $ */

#ifndef	_SYS_CDSIOREG_
#define	_SYS_CDSIOREG_	1


/*
 * Definitions for the Central Data 3608 8 port serial i/o interface
 *
 * $Source: /MIPSNET/dish/root/usr4/co-rel4.50/root/usr/src/uts/mips/sys/RCS/cdsioreg.h,v $
 * $Revision: 1.4.4.2 $
 * $Date: 90/05/10 06:07:18 $
 */
#if defined(PM2) || defined(IP2)
/* multibus product */
#define	CD3100	1
#endif

#if defined(mips)
/* vmebus product */
#define	CD3608	1
#endif

#ifdef	INDRIVER
#ifdef	SVR0
typedef	unsigned char	unchar;
#define	volatile
#endif

/*
 * Layout of the dual port ram
 * XXX add in MIPSEL support
 */
struct	device {
#ifdef	MIPSEB
	unchar	outputBuf0[3056];	/* 0000-0BEF: output buffer 0 */
	unchar	outputBuf1[3056];	/* 0BF0-17DF: output buffer 1 */
	unchar	inputBuf[1024];		/* 17E0-1BDF: input buffer */
	unchar	errorBuf[1024];		/* 1BE0-1FDF: error buffer */
	unchar	pad0;			/* 1FE0: not used */
	unchar	cmd;			/* 1FE1: command code */
	unchar	cmdHigh;		/* 1FE2: command data, high byte */
	unchar	cmdLow;			/* 1FE3: command data, low byte */
	unchar	pad1;			/* 1FE4: not used */
	unchar	cmdStatus;		/* 1FE5: command status */
	unchar	pad2;			/* 1FE6: not used */
	unchar	uartStatus;		/* 1FE7: status from uart */
	ushort	inputFillPtr;		/* 1FE8-1FE9: input filling pointer */
	ushort	inputEmptyPtr;		/* 1FEA-1FEB: input emptying pointer */
	ushort	inputOverflows;		/* 1FEC-1FED: input buffer overflows */
	unchar	pad3;			/* 1FEE: not used */
	unchar	intSourceFlag;		/* 1FEF: interrupt source flag */
	unchar	pad4;			/* 1FF0: not used */
	unchar	outputBusy;		/* 1FF1: output busy flag */
	unchar	pad5;			/* 1FF2: not used */
	unchar	outputStopped;		/* 1FF3: output stopped */
	unchar	pad6;			/* 1FF4: not used */
	unchar	parityError;		/* 1FF5: parity error */
	unchar	pad7[6];		/* 1FF6-1FFB: not used */
	ushort	firmwareRev;		/* 1FFC-1FFD: firmware revision */
	unchar	pad8;			/* 1FFE: not used */
	unchar	portIntStatusFlag;	/* 1FFF: port interrupt status flag */
#endif	/* MIPSEB */
#if defined(MIPSEL) || defined(IP2) || defined(PM2)
	unchar	outputBuf0[3056];	/* 0000-0BEF: output buffer 0 */
	unchar	outputBuf1[3056];	/* 0BF0-17DF: output buffer 1 */
	unchar	inputBuf[1024];		/* 17E0-1BDF: input buffer */
	unchar	errorBuf[1024];		/* 1BE0-1FDF: error buffer */
	unchar	cmd;			/* 1FE1: command code */
	unchar	pad0;			/* 1FE0: not used */
	unchar	cmdLow;			/* 1FE3: command data, low byte */
	unchar	cmdHigh;		/* 1FE2: command data, high byte */
	unchar	cmdStatus;		/* 1FE5: command status */
	unchar	pad1;			/* 1FE4: not used */
	unchar	uartStatus;		/* 1FE7: status from uart */
	unchar	pad2;			/* 1FE6: not used */
	ushort	inputFillPtr;		/* 1FE8-1FE9: input filling pointer */
	ushort	inputEmptyPtr;		/* 1FEA-1FEB: input emptying pointer */
	ushort	inputOverflows;		/* 1FEC-1FED: input buffer overflows */
	unchar	intSourceFlag;		/* 1FEF: interrupt source flag */
	unchar	pad3;			/* 1FEE: not used */
	unchar	outputBusy;		/* 1FF1: output busy flag */
	unchar	pad4;			/* 1FF0: not used */
	unchar	outputStopped;		/* 1FF3: output stopped */
	unchar	pad5;			/* 1FF2: not used */
	unchar	parityError;		/* 1FF5: parity error */
	unchar	pad6;			/* 1FF4: not used */
#ifdef	CD3608
	unchar	portIntStatusFlag;	/* 1FF7: port interrupt status flag */
	unchar	pad7;			/* 1FF6: not used */
	unchar	pad8[4];		/* 1FF8-1FFB: not used */
	ushort	pad9;			/* 1FFC-1FFD: not used */
	ushort	firmwareRev;		/* 1FFE-1FFF: firmware revision */
#endif
#ifdef	CD3100
	unchar	pad7[6];		/* 1FF6-1FFB: not used */
	ushort	firmwareRev;		/* 1FFC-1FFD: firmware revision */
	unchar	portIntStatusFlag;	/* 1FFF: port interrupt status flag */
	unchar	pad8;			/* 1FFE: not used */
#endif
#endif	/* MIPSEL || IP2 || PM2 */
};

#ifdef	CD3100
struct	iodevice {
	union {
		char	iodu_cmdPort;		/* one byte only, write */
		char	iodu_hostStatusPort;	/* one byte only, read */
	} iodu;
};
#define	cmdPort		iodu.iodu_cmdPort
#define	hostStatusPort	iodu.iodu_hostStatusPort
#endif

#ifdef	CD3608
/*
 * On the CD3608 board the command port and hostStatusPort overlay the
 * memory space for port 7.
 */
struct	iodevice {
	char	pad[0x1FFC];		/* 0000-1FFB: not used */
	unchar	pad0;			/* 1FFC: not used */
	unchar	hostStatusPort;		/* 1FFD: status port */
	unchar	pad1;			/* 1FFE: not used */
	unchar	cmdPort;		/* 1FFF: command port */
};
#endif

#ifdef	CD3608
/* bits in the errorBuf */
#define	ERRB_FRAMING	0x40		/* framing error */
#define	ERRB_OVERRUN	0x20		/* overrun error */
#define	ERRB_PARITY	0x10		/* parity error */

/* bits in the uartStatus */
#define	SCC_RI_		0x80		/* (active low) RI active */
#define	SCC_DSR_	0x08		/* (active low) DSR active */
#define	SCC_DCD_	0x04		/* (active low) DCD active */
#endif	/* CD3608 */

#ifdef	CD3100
/* bits in the errorBuf */
#define	ERRB_FRAMING	0x20		/* framing error */
#define	ERRB_OVERRUN	0x10		/* overrun error */
#define	ERRB_PARITY	0x08		/* parity error */

/* bits in the uartStatus */
#define	SCC_DSR		0x80		/* (active high) DSR active */
#define	SCC_DCD		0x40		/* (active high) DCD active */
#endif

/* bits in intSourceFlag */
#define	ISF_PARITY	0x08		/* parity error interrupt */
#define	ISF_STATUS	0x04		/* status change interrupt */
#define	ISF_INPUT	0x02		/* input interrupt */
#define	ISF_OUTPUT	0x01		/* output done interrupt */

/* bits in hostStatusPort */
#define	HSP_INT		0x08		/* this board interrupted */
#define	HSP_READY	0x01		/* board is ready for a command */

/* cmd's */
#ifdef	CD3608
#define	CMD_SET_SCC	0x00		/* set scc register */
#define	CMD_SET_BAUD	0x01		/* set baud rate registers */
#endif
#ifdef	CD3100
#define	CMD_SET_MR12	0x00		/* set usart mode registers 1 & 2 */
#define	CMD_SET_CR	0x01		/* set usart command register */
#endif
#define	CMD_SET_XONMASK	0x02		/* set xon/xoff mask/enables */
#define	CMD_SET_XONCHAR	0x03		/* set xon/xoff characters */
#define	CMD_SEND_0	0x04		/* send block 0 */
#define	CMD_SEND_1	0x05		/* send block 1 */
#define	CMD_SEND_NOW	0x06		/* send immediate character */
#define	CMD_SEND_BREAK	0x07		/* send break */
#define	CMD_SET_IDT	0x08		/* set input delay time */
#define	CMD_SET_HWM	0x09		/* set high water mark */
#define	CMD_SET_III	0x0A		/* set immediate input interrupt */
#define	CMD_SET_ISB0	0x0B		/* set interrupt on status bits 0 */
#define	CMD_SET_ISB1	0x0C		/* set interrupt on status bits 1 */
#define	CMD_SET_INPUT	0x0D		/* set input interrupt */
#define	CMD_SET_OUTPUT	0x0E		/* set output interrupt */
#define	CMD_SET_STATUS	0x0F		/* set status interrupt */
#define	CMD_SET_PARITY	0x10		/* set parity interrupt */
#ifdef	CD3100
#define	CMD_CLEARINT	0x11		/* clear interrupt */
#define	CMD_MBUSLOCK	0x12		/* allow multibus lock */
#endif

/* high bits in ipl for CMD_SET_{INPUT/OUTPUT/STATUS/PARITY} */
#define	INT_ENABLE	0x08		/* enable interrupts */
#ifdef	CD3608
#define	INT_ROAK	0x80		/* reset on ack */
#endif
#ifdef	CD3100
#define	INT_BUSVEC	0x80		/* use bus vectored mode */
#endif

#ifdef	CD3608
/*
 * CD3608 uart registers
 */

/* bits in SCC write register 3 (used by CMD_SET_SCC) */
#define	WR3_BPC_5	0x00		/* 5 bits per character */
#define	WR3_BPC_7	0x40		/* 7 bits per character */
#define	WR3_BPC_6	0x80		/* 6 bits per character */
#define	WR3_BPC_8	0xC0		/* 8 bits per character */
#define	WR3_DCD_	0x20		/* (active low) dcd & cts enable */
#define	WR3_RCV		0x01		/* receiver enabled */

/* bits in SCC write register 4 (used by CMD_SET_SCC) */
#define	WR4_CLK_1	0x00		/* clock rate x1 */
#define	WR4_CLK_16	0x40		/* clock rate x16 */
#define	WR4_CLK_32	0x80		/* clock rate x32 */
#define	WR4_CLK_64	0xC0		/* clock rate x64 */
#define	WR4_STOP_NONE	0x00		/* 0 stop bits */
#define	WR4_STOP_1	0x04		/* 1 stop bits */
#define	WR4_STOP_15	0x08		/* 1.5 stop bits */
#define	WR4_STOP_2	0x0C		/* 2 stop bits */
#define	WR4_EVEN	0x02		/* even parity */
#define	WR4_PARITY	0x01		/* enable parity */

/* bits in SCC write register 5 (used by CMD_SET_SCC) */
#define	WR5_DTR		0x80		/* dtr active */
#define	WR5_BPC_5	0x00		/* 5 bits per character */
#define	WR5_BPC_7	0x20		/* 7 bits per character */
#define	WR5_BPC_6	0x40		/* 6 bits per character */
#define	WR5_BPC_8	0x60		/* 8 bits per character */
#define	WR5_BIT3	0x08		/* bit 3 (must be one) */
#define	WR5_RTS_	0x02		/* (active low) rts enable */

/* bits in SCC write register 14 (used by CMD_SET_SCC) */
#define	WR14_LOOP	0x10		/* enable local loopback */
#define	WR14_ECHO	0x08		/* auto echo enable */
#define	WR14_DTRCTL	0x04		/* dtr control */
#define	WR14_BIT1	0x02		/* bit 1 (must be one) */
#define	WR14_BIT0	0x01		/* bit 0 (must be one) */
#endif	/* CD3608 */

#ifdef	CD3100
/*
 * CD3100 uart registers
 */
#define	MR1_STOP_0	0x00		/* 0 stop bits */
#define	MR1_STOP_1	0x40		/* 1 stop bit */
#define	MR1_STOP_15	0x80		/* 1.5 stop bits */
#define	MR1_STOP_2	0xC0		/* 2 stop bits */
#define	MR1_EVEN	0x20		/* even parity */
#define	MR1_PARITY	0x10		/* parity enable */
#define	MR1_BPC_5	0x00		/* 5 bits per character */
#define	MR1_BPC_6	0x04		/* 6 bits per character */
#define	MR1_BPC_7	0x08		/* 7 bits per character */
#define	MR1_BPC_8	0x0C		/* 8 bits per character */
#define	MR1_BIT1	0x02		/* bit 1 (must be one) */

#define	MR2_BIT5	0x20		/* bit 5 (must be one) */
#define	MR2_BIT4	0x10		/* bit 4 (must be one) */
#define	MR2_BAUD_50	0x00		/* 50 baud */
#define	MR2_BAUD_75	0x01		/* 75 baud */
#define	MR2_BAUD_110	0x02		/* 110 baud */
#define	MR2_BAUD_134	0x03		/* 134.5 baud */
#define	MR2_BAUD_150	0x04		/* 150 baud */
#define	MR2_BAUD_300	0x05		/* 300 baud */
#define	MR2_BAUD_600	0x06		/* 600 baud */
#define	MR2_BAUD_1200	0x07		/* 1200 baud */
#define	MR2_BAUD_1800	0x08		/* 1800 baud */
#define	MR2_BAUD_2000	0x09		/* 2000 baud */
#define	MR2_BAUD_2400	0x0A		/* 2400 baud */
#define	MR2_BAUD_3600	0x0B		/* 3600 baud */
#define	MR2_BAUD_4800	0x0C		/* 4800 baud */
#define	MR2_BAUD_7200	0x0D		/* 7200 baud */
#define	MR2_BAUD_9600	0x0E		/* 9600 baud */
#define	MR2_BAUD_19200	0x0F		/* 19200 baud */

#define	CR_NORMAL	0x00		/* normal mode */
#define	CR_AUTOECHO	0x40		/* auto echo */
#define	CR_LOOP		0x80		/* local loopback */
#define	CR_REMOTELOOP	0xC0		/* remote loopback */
#define	CR_RTS		0x20		/* drive RTS */
#define	CR_BIT2		0x04		/* bit 2 (must be one) */
#define	CR_DTR		0x02		/* drive DTR */
#define	CR_BIT0		0x01		/* bit 0 (must be one) */
#endif	/* CD3100 */

#endif	/* INDRIVER */

/*
 * This structure is used to manage the software state of each
 * serial port.
 */
struct	cdport {
	ulong	dp_framingErrors;	/* # of framing errors */
	ulong	dp_overruns;		/* # of overrun errors */
	ulong	dp_allocb_fail;		/* losses due to allocb() failures */

	volatile struct device *dp_device;	/* port device address */
	volatile struct iodevice *dp_iodevice;	/* pointer to i/o port */
	unchar	dp_uartStatus;		/* last scc status */
	unchar	dp_isf;			/* last interrupt source flag */

	unchar	dp_litc;		/* escape next char--e.g. XOFF */
	unchar	dp_stopc;		/* output XOFF character */
	unchar	dp_startc;		/* output XON character	*/
	struct	termio dp_termio;
#define dp_iflag dp_termio.c_iflag	/* use some of the bits (see below) */
#define dp_cflag dp_termio.c_cflag	/* use all of the standard bits */
#define dp_line dp_termio.c_line	/* 'line discipline' */

	unchar	dp_index;		/* port number (0 to MAXPORT) */
	unchar	dp_bit;			/* 1 << port number */
	unchar	dp_rsrv_cnt;		/* input timer count */
	unchar	dp_obuf;		/* which output buffer being used */
	ushort	dp_state;		/* current state */

	queue_t	*dp_rq, *dp_wq;		/* our queues */
	mblk_t	*dp_rmsg, *dp_rmsge;	/* current input message */
	int	dp_rmsg_len;
	int	dp_rbsize;		/* resent msg length */
	mblk_t	*dp_rbp;		/* current buffer */
	mblk_t	*dp_wbp;		/* current output buffer */

	int	dp_tid;			/* (recent) output delay timer ID */
};

/* bits in dp_state */
#define DP_ISOPEN	0x0001		/* device is open */
#define DP_WOPEN	0x0002		/* waiting for carrier */
#define DP_DCD		0x0004		/* we have carrier */
#define DP_TIMEOUT	0x0008		/* delaying */
#define DP_BREAK	0x0010		/* breaking */
#define DP_BREAK_QUIET	0x0020		/* finishing break */
#define DP_TXSTOP	0x0040		/* output stopped by received XOFF */
#define DP_LIT		0x0080		/* have seen literal character */
#define DP_BLOCK	0x0100		/* XOFF sent because input full */
#define DP_TX_TXON	0x0200		/* need to send XON */
#define DP_TX_TXOFF	0x0400		/* need to send XOFF */
#define DP_FLOW		0x4000		/* do hardware flow control */
#define	DP_XMIT		0x8000		/* xmit in progress */

#define	DP_BUSY		(DP_TIMEOUT|DP_BREAK|DP_BREAK_QUIET|DP_XMIT)

#define	CDUPC	8			/* 8 units per controller */

#if defined(SVR3) && defined(INKERNEL)
extern	struct cdport cdport[];
extern	int cdnumports;			/* total ports configured */
extern	int cdnumboards;		/* total boards configured */
#endif

#endif	_SYS_CDSIOREG_
