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
#ident	"$Header: vsscanf.c,v 1.2.1.2 90/05/10 02:26:32 wje Exp $"
/*	Copyright (c) 1984 AT&T	*/
/*	  All Rights Reserved  	*/

/*	THIS IS UNPUBLISHED PROPRIETARY SOURCE CODE OF AT&T	*/
/*	The copyright notice above does not evidence any   	*/
/*	actual or intended publication of such source code.	*/

#include "curses_inc.h"
#include <varargs.h>

/*
 *	This routine implements vsscanf (nonportably) until such time
 *	as one is available in the system (if ever).
 */

/*VARARGS2*/
vsscanf(buf, fmt, ap)
char	*buf;
char	*fmt;
va_list	ap;
{
	FILE	junk;
	extern int _doscan();

#ifdef SYSV
	junk._flag = _IOREAD;
	junk._file = -1;
	junk._base = junk._ptr = (unsigned char *) buf;
#else
	junk._flag = _IOREAD|_IOSTRG;
	junk._base = junk._ptr = buf;
#endif
	junk._cnt = strlen(buf);
	return _doscan(&junk, fmt, ap);
}
