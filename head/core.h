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
/* $Header: core.h,v 1.8.3.2 90/05/10 00:59:15 wje Exp $ */

#ifndef	_CORE_
#define	_CORE_	1


/*	Copyright (c) 1984 AT&T	*/
/*	  All Rights Reserved  	*/

/*	THIS IS UNPUBLISHED PROPRIETARY SOURCE CODE OF AT&T	*/
/*	The copyright notice above does not evidence any   	*/
/*	actual or intended publication of such source code.	*/

/* machine dependent stuff for core files */

#if vax
#define TXTRNDSIZ 512L
#define stacktop(siz) (0x80000000L)
#define stackbas(siz) (0x80000000L-siz)
#endif

#if pdp11
#define TXTRNDSIZ 8192L
#define stacktop(siz) (0x10000L)
#define stackbas(siz) (0x10000L-siz)
#endif


#if u3b
#define TXTRNDSIZ 0x20000
#define stacktop(siz) 0xF00000
#define stackbas(siz) (0xF00000 + siz)
#endif

#if M32 || u3b15 || u3b5 || u3b2
#define TXTRNDSIZ 2048L
#define stacktop(siz) (0xF00000 + siz)
#define stackbas(siz) 0xF00000
#endif

#if mips
#define TXTRNDSIZ 4096
#define stacktop(siz) 0x7FFFF000
#define stackbas(siz) (0x7FFFF000 - siz)
#endif

#endif	_CORE_
