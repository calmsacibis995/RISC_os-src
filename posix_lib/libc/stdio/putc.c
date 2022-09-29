
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
 * |         950 DeGuigne Drive                                |
 * |         Sunnyvale, CA 94086                               |
 * |-----------------------------------------------------------|
 */
#ident	"$Header: putc.c,v 1.1.1.1.1.1.1.1 90/10/05 09:50:00 beacker Exp $"

/*	Copyright (c) 1984 AT&T	*/
/*	  All Rights Reserved  	*/

/*	THIS IS UNPUBLISHED PROPRIETARY SOURCE CODE OF AT&T	*/
/*	The copyright notice above does not evidence any   	*/
/*	actual or intended publication of such source code.	*/

/*LINTLIBRARY*/
/*
 * A subroutine version of the macro putc from stdio.h.
 *   #define putc(x, p)      (--(p)->_cnt < 0 ? \
 *                      _flsbuf((unsigned char) (x), (p)) : \
 *                      (int) (*(p)->_ptr++ = (unsigned char) (x)))
 */
#include "shlib.h"
#include <stdio.h>
#include <errno.h>
#undef putc


int
putc(x,p)
int     x;
FILE	*p;
{
#ifdef	SYSTYPE_POSIX
	/* Check for read only file */
	if (p->_flag & _IOREAD) {
		errno = EBADF;
		return EOF;
	}
#endif

	if (--(p)->_cnt < 0) 
		return(_flsbuf((unsigned char) (x), (p))); 
	else
		return((int) (*(p)->_ptr++ = (unsigned char)(x)));
}
