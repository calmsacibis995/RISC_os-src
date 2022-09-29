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
/* $Header: ikcreg.h,v 1.4.4.2 90/05/10 05:21:09 wje Exp $ */

#ifndef	_IO_IKCREG_
#define	_IO_IKCREG_	1


/*
**	ikcreg.h
**
**	$Source: /MIPSNET/dish/root/usr4/co-rel4.50/root/usr/src/uts/mips/io/RCS/ikcreg.h,v $
**	$Revision: 1.4.4.2 $
**	$Date: 90/05/10 05:21:09 $
*/

#define DEVICE(x)	(minor(x))
#define TEK		0x00		/* minor device number:DMA tektronix  */
#define VERS		0x01		/* minor device number:versatec	      */
#define RAW		0x05		/* minor device number:raw interface  */
#define TEKPOLL		0x06		/* minor device number:tektronix      */
#define TEKDEBUG	0x07		/* minor device number:toggle debug   */
#define VERSPOLL	0x08		/* minor device number:versatec poll  */

#define	FORM		0x0c
#define	BACKSPACE	0x08

/* Registers of the Board */
#define LATCHED		0x00		/* BYTES */
#define INTSTAT		0x01		/* BYTES */
#define PULSED		0x02		/* BYTES */
#define DEVSTAT		0x03		/* BYTES */
#define PIOOUT		0x04		/* BYTES */
#define DIAGOUT		0x05		/* BYTES */
#define ADDMOD		0x06		/* BYTES */
#define INTVECTOR	0x07		/* BYTES */
#define DMAHIGH		0x08
#define DMALOW		0x0a
#define DMACOUNT	0x0c
#define UNUSED		0x0e

/*reads and writes to the registers*/
#define	IK_READ(a)	*( (short *) iksoftc.sc_ioaddr + a)
#define	IK_WRITE(a,d)	*( (short *) iksoftc.sc_ioaddr + a) = d

#define IK_STATUS()	IK_READ(INTSTAT)
#define IK_LATCHED(d)	IK_WRITE(LATCHED,d)
#define IK_PULSED(d)	IK_WRITE(PULSED,d)
#define IK_PUT(d)	IK_WRITE(PIOOUT,d)

/* Interface Status Bits */
#define INT_READY	0x80
#define DEV_READY	0x40
#define DMAON		0x20
#define OPTIONSEL	0x10
#define STREAMING	0x08
#define INT_ON		0x04
#define INT_FLAG	0x02
#define BUSERROR	0x01


#define	SIMULM		0x04

	/* provides a string for printfs based on value of bit */
#define STAT_ON(i,b)	((i&b)?"":"!")

/* Mode bits */
/* Latched Bits */
#define	INT_TEST	0x80		/* Test Online */
#define	VERS_READY	0x40		/* Test No Paper */
#define PRIME_SIG	0x20
#define	OPTION		0x10		/* Select option port */

#define	ENB_STREAMING	0x08		/* data streaming mode */
#define	ENB_INTS	0x04		/* Enable Interrupts */
#define	SIMULT		0x02		/* versatec simultaneous print & plot */
#define	PLOTMODE	0x01		/* plot mode */
#define	TEKMODE		OPTION		/* select tektronix mode */
#define	VERSMODE	0		/* select versatec mode */

/* Command bits */
/* Pulsed Functions */
#define	SOFTACK		0x80		/* software ACK */
#define	RESET		0x40		/* Master Clear */
#define	RESET_INT	0x20		/* Reset Interrupt Flag */
#define	STARTDMA	0x10		/* Start DMA */
#define	VCLEAR		0x08		/* versatec clear */
#define	VRFF		0x04		/* versatec form feed */
#define	VREOT		0x02		/* versatec remote EOT */
#define	VRLF		0x01		/* versatec remote line terminated */

/* Tektronix image states */
#define	TS_COM		0
#define	TS_RAST		1
#define	TS_ERR		(-1)

/* Tektronix constants */
#define	TS_HORZ		1024		/* max pixels per line */
#define	TS_VERT		768		/* max lines per page */
#define	TS_BPIX		2		/* max bytes per pixel */
#define	TS_HSIZ		9		/* tek header size */

/* Tektronix command bytes */
#define	T_NULL		0x00		/* Null				      */
#define	T_EOT		0x01		/* End Of Transmission (page)	      */
#define	T_EOL		0x02		/* End Of Line			      */
#define	T_ABORT		0x03		/* Abort			      */
#define	T_COPY		0x04		/* Copy (print) request		      */
#define	T_RESERVE	0x05		/* Reserve printer		      */
#define	T_BIT		0x07		/* Bit prompt (read next status bit)  */
#define	T_STATUS	0x08		/* Send status from printer	      */

/* tekwait() constants */
#define	WAITOK		1		/* return ok			      */
#define	WAITERR		0		/* return not ok		      */
#define	TIMEOUT1	(45*HZ)		/* 45 seconds (async headcleaning)    */
#define	TIMEOUT2	(2*60*HZ)	/* 2 minutes			      */

#define SC_ALIVE	0x01
#define SC_OPENED	0x02
#define SC_INITED	0x04
#define SC_SETUP	0x08

/* ioctl commands for Versatec */
#define	VLF		0001		/* versatec remote line terminated */
#define	VFF		0002		/* versatec form feed */
#define	VEOT		0004		/* versatec remote EOT */
#define	VPRINT		0100
#define	VPLOT		0200
#define	VPRINTPLOT	0400
#define	VGETSTATE	0
#define	VSETSTATE	1

/* controller state */
struct	ik_softc {
	short	sc_flags;		/* current controller state */
	short	sc_unit;		/* unused */
	short	sc_status;		/* current printer status */
	caddr_t	sc_ioaddr;		/* virtual address of i/o port */
} iksoftc;

#endif	_IO_IKCREG_
