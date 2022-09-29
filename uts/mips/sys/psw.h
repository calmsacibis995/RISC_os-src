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
/* $Header: psw.h,v 1.8.2.2 90/05/10 06:32:32 wje Exp $ */

#ifndef	_SYS_PSW_
#define	_SYS_PSW_	1


/*	Copyright (c) 1984 AT&T	*/
/*	  All Rights Reserved  	*/

/*	THIS IS UNPUBLISHED PROPRIETARY SOURCE CODE OF AT&T	*/
/*	The copyright notice above does not evidence any   	*/
/*	actual or intended publication of such source code.	*/



/*
	PSW structure. For Mips the PSW is just the status register
	XXX - can we axe this file?
*/

typedef int psw_t;


/*
 *	The following is the initial psw for process 0.
 *	All interrupts enabled, previously user mode.
 */

#define	IPSW	(SR_IMASK0 | SR_KUP | SR_IEP)

#endif	_SYS_PSW_
