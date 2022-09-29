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
/* $Header: pty_ioctl.h,v 1.6.2.2 90/05/10 06:32:44 wje Exp $ */

#ifndef	_SYS_PTY_IOCTL_
#define	_SYS_PTY_IOCTL_	1


/*
 * Pty ioctls
 *
 * $Source: /MIPSNET/dish/root/usr4/co-rel4.50/root/usr/src/uts/mips/sys/RCS/pty_ioctl.h,v $
 * $Revision: 1.6.2.2 $
 * $Date: 90/05/10 06:32:44 $
 */

#define	PTIOC(x)		(('p'<<8)|(x))
#define	PTIOC_QUEUE		PTIOC(0)	/* mark pty as queued */
/* 
 *  the console output in the kernel may be re-directed to the output
 *  side of a slave tty with PTCSETCON if you are super-user.  PTCCLRCON
 *  reverses this.
 */
#define PTCSETCON		_IO(P,0x7f)	/* set console usage */
#define PTCCLRCON		_IO(P,0x7e)	/* clear console usage */

/*
 * Queue defines...These should be in "device.h"
 */
#define	QPTYCOUNT	3
#define	QPTYOFFSET	2000
#define	QPTY_CANREAD	(2000)	/* can read from slave */
#define	QPTY_STOP	(2001)	/* user typed ^S */
#define	QPTY_START	(2002)	/* user typed ^Q */

#endif	_SYS_PTY_IOCTL_
