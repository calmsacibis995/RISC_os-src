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
/* $Header: symbol.h,v 1.8.3.2 90/05/10 01:05:28 wje Exp $ */

#ifndef	_SYMBOL_
#define	_SYMBOL_	1


/*	Copyright (c) 1984 AT&T	*/
/*	  All Rights Reserved  	*/

/*	THIS IS UNPUBLISHED PROPRIETARY SOURCE CODE OF AT&T	*/
/*	The copyright notice above does not evidence any   	*/
/*	actual or intended publication of such source code.	*/

/*
 * Structure of a symbol table entry
 */

#ifdef mips
/*
 * Struct symbol in this file has been replaced by struct syment in
 * <syms.h>. We should put struct syment back into this file sometime.
 */
#else
struct	symbol {
	char	sy_name[8];
	char	sy_type;
	int	sy_value;
};
#endif mips

#endif	_SYMBOL_
