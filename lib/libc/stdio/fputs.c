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
#ident	"$Header: fputs.c,v 1.6.2.3 90/05/10 01:45:52 wje Exp $"

/*	Copyright (c) 1984 AT&T	*/
/*	  All Rights Reserved  	*/

/*	THIS IS UNPUBLISHED PROPRIETARY SOURCE CODE OF AT&T	*/
/*	The copyright notice above does not evidence any   	*/
/*	actual or intended publication of such source code.	*/

/*LINTLIBRARY*/
/*
 * This version writes directly to the buffer rather than looping on putc.
 * Ptr args aren't checked for NULL because the program would be a
 * catastrophic mess anyway.  Better to abort than just to return NULL.
 */
#include "shlib.h"
#include <stdio.h>
#include "stdiom.h"
#include <errno.h>

extern char *memccpy();

int
fputs(ptr, iop)
char *ptr;
register FILE *iop;
{
	register int ndone = 0, n;
	register unsigned char *cptr, *bufend;
	char *p;

	if (_WRTCHK(iop)) {
#ifdef	SYSTYPE_POSIX
		errno = EBADF;
#endif
		return (EOF);
	}
	bufend = _bufend(iop);

	if ((iop->_flag & _IONBF) == 0)  {
		for ( ; ; ptr += n) {
			while ((n = bufend - (cptr = iop->_ptr)) <= 0)  
				/* full buf */
				if (_xflsbuf(iop) == EOF)
					return(EOF);
			if ((p = memccpy((char *) cptr, ptr, '\0', n)) != NULL)
				n = (p - (char *) cptr) - 1;
			iop->_cnt -= n;
			iop->_ptr += n;
			_BUFSYNC(iop);
			ndone += n;
			if (p != NULL)  { 
				/* done; flush buffer if line-buffered */
	       			if (iop->_flag & _IOLBF)
	       				if (_xflsbuf(iop) == EOF)
	       					return(EOF);
	       			return(ndone);
	       		}
		}
	}  else  {
		/* write out to an unbuffered file */
		register unsigned int cnt = strlen(ptr);

#ifdef	SYSTYPE_POSIX
		return (write(iop->_file, ptr, cnt));
#else
		(void)write(iop->_file, ptr, cnt);
		return cnt;
#endif
	}
}
