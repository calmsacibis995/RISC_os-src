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
#ident	"$Header: V2.__sscans.c,v 1.2.1.2 90/05/09 20:04:04 wje Exp $"
/*	Copyright (c) 1984 AT&T	*/
/*	  All Rights Reserved  	*/

/*	THIS IS UNPUBLISHED PROPRIETARY SOURCE CODE OF AT&T	*/
/*	The copyright notice above does not evidence any   	*/
/*	actual or intended publication of such source code.	*/

# include	"curses_inc.h"
# include	<varargs.h>
#ifdef _VR2_COMPAT_CODE
/*
	This file is provided for compatibility reasons only
	and will go away someday. Programs should reference
	vwscanw() instead.
 */


extern int vwscanw();
__sscans(win, fmt, ap)
WINDOW	*win;
char *fmt;
va_list	ap;
{
	return vwscanw(win, fmt, ap);
}
#endif
