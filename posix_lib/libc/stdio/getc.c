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
#ident	"$Header: getc.c,v 1.1.1.3 90/05/10 04:17:04 wje Exp $"

/*	Copyright (c) 1984 AT&T	*/
/*	  All Rights Reserved  	*/

/*	THIS IS UNPUBLISHED PROPRIETARY SOURCE CODE OF AT&T	*/
/*	The copyright notice above does not evidence any   	*/
/*	actual or intended publication of such source code.	*/

/*LINTLIBRARY*/
/*
 * A subroutine version of the macro getc.
 */
#include "shlib.h"
#include <stdio.h>
#include <errno.h>
#undef getc

int
getc(p)
FILE	*p;
{
#ifdef	SYSTYPE_POSIX
	/* Check for write only file */
	if (p->_flag & _IOWRT) {
		errno = EBADF;
		return EOF;
	}
#endif

	if (--(p)->_cnt < 0) 
		return(_filbuf(p)); 
	else
		return((int) *(p)->_ptr++);
}
