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
/* $Header: i8042.h,v 1.1.1.2 90/05/10 06:21:35 wje Exp $ */

#ifndef _SYS_I8042_
#define _SYS_I8042_

/*
 * defines needed to manipulate the 8042 in a sensible way
 */

/*
 * I 8042 command and data port defines
 */
#define I8042CMD	0x19000004	/* word access bits 0-7 */
#define	I8042DATA	0x19000000	/* word access bits 0-7 */
#define i8042cmd	*((volatile int *)PHYS_TO_K1(I8042CMD))
#define	i8042data	*((volatile int *)PHYS_TO_K1(I8042DATA))

/*
 * I 8042 commands
 */
#define	I8042RCR	0x20		/* read command register */
#define	I8042WCR	0x60		/* write command register */
#define	I8042PST	0xaa		/* perform self test */
#define	I8042TKI	0xab		/* test keyboard interface */
#define I8042DKI	0xad		/* disable keyboard interface */
#define	I8042EKI	0xae		/* enable keyboard interface */
#define	I8042RIP	0xc0		/* read input port */
#define	I8042ROP	0xd0		/* read output port */
#define	I8042WOP	0xd1		/* write output port */
#define	I8042RTI	0xe0		/* read test inputs */
#define	I8042SR		0xf0		/* soft reset - reset CPU */

/*
 * I 8042 bits in the command status register
 */
#define	I8042PE		0x80		/* parity error */
#define	I8042RT		0x40		/* receive timeout */
#define	I8042TT		0x20		/* transmit timeout */
#define	I8042IK		0x10		/* inhibited keyboard */
#define	I8042LA		0x08		/* last access to data register */
#define	I8042SF		0x04		/* system flag */
#define	I8042IBF	0x02		/* input buffer full */
#define	I8042OBF	0x01		/* output buffer full */

/*
 * I 8042 port 2 bit definitions
 */
#define	I8042OddP	0x02		/* Odd parity */
#define	I80424Mb	0x04		/* 4Mb simms installed */
#define	I8042BIG	0x08		/* Big endian mode */
#define	I8042EIC	0x20		/* Expansion Interrupt Control */

#endif _SYS_I8042_
