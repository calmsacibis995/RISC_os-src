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
#ident	"$Header: scanw.c,v 1.2.1.2 90/05/10 02:19:54 wje Exp $"
/*	Copyright (c) 1984 AT&T	*/
/*	  All Rights Reserved  	*/

/*	THIS IS UNPUBLISHED PROPRIETARY SOURCE CODE OF AT&T	*/
/*	The copyright notice above does not evidence any   	*/
/*	actual or intended publication of such source code.	*/

/*
 * scanw and friends
 *
 */

# include	"curses_inc.h"
# include	<varargs.h>

/*
 *	This routine implements a scanf on the standard screen.
 */
/*VARARGS1*/
scanw(va_alist)
va_dcl
{
	register char	*fmt;
	va_list	ap;

	va_start(ap);
	fmt = va_arg(ap, char *);
	return vwscanw(stdscr, fmt, ap);
}
