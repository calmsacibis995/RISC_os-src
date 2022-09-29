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
#ident	"$Header: fprintf.c,v 1.6.2.3 90/05/10 01:42:44 wje Exp $"

/*	Copyright (c) 1984 AT&T	*/
/*	  All Rights Reserved  	*/

/*	THIS IS UNPUBLISHED PROPRIETARY SOURCE CODE OF AT&T	*/
/*	The copyright notice above does not evidence any   	*/
/*	actual or intended publication of such source code.	*/

/*LINTLIBRARY*/
#include "shlib.h"
#include <stdio.h>
#include <varargs.h>
#include <errno.h>

extern int _doprnt();

/*VARARGS2*/
int
fprintf(iop, format, va_alist)
FILE *iop;
char *format;
va_dcl
{
	register int count;
	va_list ap;

	va_start(ap);
	if (!(iop->_flag & _IOWRT)) {
		/* if no write flag */
		if (iop->_flag & _IORW) {
			/* if ok, cause read-write */
			iop->_flag |= _IOWRT;
		} else {
			/* else error */
#ifdef	SYSTYPE_POSIX
			errno = EBADF;
#endif
			return EOF;
		}
	}
	count = _doprnt(format, ap, iop);
	va_end(ap);
	return(ferror(iop)? EOF: count);
}
