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
#ident	"$Header: filbuf.c,v 1.6.2.2.1.1.1.2 90/11/15 13:55:58 beacker Exp $"

/*	Copyright (c) 1984 AT&T	*/
/*	  All Rights Reserved  	*/

/*	THIS IS UNPUBLISHED PROPRIETARY SOURCE CODE OF AT&T	*/
/*	The copyright notice above does not evidence any   	*/
/*	actual or intended publication of such source code.	*/

/*LINTLIBRARY*/
#include "shlib.h"
#include <stdio.h>
#include <errno.h>

extern _findbuf();
extern int read();
extern int fflush();
extern FILE *_lastbuf;

int
_filbuf(iop)
register FILE *iop;
{
	register FILE *diop;

	if (iop->_base == NULL)  /* get buffer if we don't have one */
		_findbuf(iop);

	if ( !(iop->_flag & _IOREAD) )
		if (iop->_flag & _IORW)
			iop->_flag |= _IOREAD;
		else {
#ifdef	SYSTYPE_POSIX
		  	errno = EBADF;
#endif
			return(EOF);
		}

	/* if this device is a terminal (line-buffered) or unbuffered, then */
	/* flush buffers of all line-buffered devices currently writing */

	if (iop->_flag & (_IOLBF | _IONBF))
		for (diop = _iob; diop < _lastbuf; diop++ )
			if (diop->_flag & _IOLBF)
				(void) fflush(diop);

	iop->_ptr = iop->_base;
	iop->_cnt = read(fileno(iop), (char *)iop->_base,
	    (unsigned)((iop->_flag & _IONBF) ? 1 : _bufsiz(iop) ));
	if (--iop->_cnt >= 0)		/* success */
		return (*iop->_ptr++);
	if (iop->_cnt != -1)		/* error */
		iop->_flag |= _IOERR;
	else {				/* end-of-file */
		iop->_flag |= _IOEOF;
		if (iop->_flag & _IORW)
			iop->_flag &= ~_IOREAD;
	}
	iop->_cnt = 0;
	return (EOF);
}
