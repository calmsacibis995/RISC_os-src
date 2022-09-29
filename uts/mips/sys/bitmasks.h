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
/* $Header: bitmasks.h,v 1.6.4.2 90/05/10 06:05:47 wje Exp $ */

#ifndef	_SYS_BITMASKS_
#define	_SYS_BITMASKS_	1


/*	Copyright (c) 1984 AT&T	*/
/*	  All Rights Reserved  	*/

/*	THIS IS UNPUBLISHED PROPRIETARY SOURCE CODE OF AT&T	*/
/*	The copyright notice above does not evidence any   	*/
/*	actual or intended publication of such source code.	*/


/*	setmask[i] has the low order i bits set.  For example,
 *	setmask[5] == 0x1F.
 */

extern int setmask[33];

/*	sbittab[i] has bit number i set.  For example,
 *	sbittab[5] == 0x20.
 */

extern int sbittab[];

/*	cbittab[i] has all bits on except bit i which is off.  For example,
 *	cbittab[5] == 0xFFFFFFDF.
 */

extern int cbittab[];

#endif	_SYS_BITMASKS_
