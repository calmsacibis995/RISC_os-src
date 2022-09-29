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
/* $Header: uart_ioctl.h,v 1.1.2.2 90/05/10 06:42:46 wje Exp $ */

#ifndef	_SYS_UART_IOCTL_
#define	_SYS_UART_IOCTL_	1


/*
 * Uart ioctls
 *
 * $Source: /MIPSNET/dish/root/usr4/co-rel4.50/root/usr/src/uts/mips/sys/RCS/uart_ioctl.h,v $
 * $Revision: 1.1.2.2 $
 * $Date: 90/05/10 06:42:46 $
 */

#define	UTCSETTIMESTAMP		_IO(U,0x7f)	/* uart input timestamped */
#define	UTCCLRTIMESTAMP		_IO(U,0x7e)	/* no uart input timestamped */

/*
 *  Uart Input Timestamping - when turned on all recieved characters
 *  that would normally be received as a single character are received
 *  eight characters in the format of the following structure.
 */

#define UARTSYNCCHAR 0xff
struct utimestamp {
	unsigned char ut_sync[3];	/* 3 sync characters == 0xff */
	unsigned char ut_char;		/* actual timestamped character */
	time_t ut_time;			/* time in HZ since last boot */
};

#endif	_SYS_UART_IOCTL_
