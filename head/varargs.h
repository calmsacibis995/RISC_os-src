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
/* $Header: varargs.h,v 1.8.3.2 90/05/10 01:06:53 wje Exp $ */

#ifndef	_VARARGS_
#define	_VARARGS_	1


/*	Copyright (c) 1984 AT&T	*/
/*	  All Rights Reserved  	*/

/*	THIS IS UNPUBLISHED PROPRIETARY SOURCE CODE OF AT&T	*/
/*	The copyright notice above does not evidence any   	*/
/*	actual or intended publication of such source code.	*/

typedef char *va_list;
#define va_dcl int va_alist;
#define va_start(list) list = (char *) &va_alist
#define va_end(list)
#ifdef u370
#define va_arg(list, mode) ((mode *)(list = \
	(char *) ((int)list + 2*sizeof(mode) - 1 & -sizeof(mode))))[-1]
#else
#ifdef host_mips
#define va_arg(list, mode) ((mode *)(list = \
	(char *) (sizeof(mode) > 4 ? ((int)list + 2*8 - 1) & -8 \
				   : ((int)list + 2*4 - 1) & -4)))[-1]
#else
#define va_arg(list, mode) ((mode *)(list += sizeof(mode)))[-1]
#endif
#endif


#endif	_VARARGS_
