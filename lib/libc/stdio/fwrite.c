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
#ident	"$Header: fwrite.c,v 1.8.2.3 90/05/10 01:46:41 wje Exp $"

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
 *
 * This version does buffered writes larger than BUFSIZ directly, when
 * the buffer is empty.
 */
#include "shlib.h"
#include <stdio.h>
#include "stdiom.h"
#include <sys/types.h>
#include <errno.h>

#define MIN(x, y)       (x < y ? x : y)

extern char *memcpy();

int
fwrite(ptr, size, count, iop)
char *ptr;
int count;
size_t size; 
register FILE *iop;
{
	register unsigned long nleft;
  	register int n;
  	register unsigned char *cptr, *bufend;
  
	if (count <= 0 || _WRTCHK(iop)) {
#ifdef	SYSTYPE_POSIX
		errno = EBADF;
#endif
  	        return (0);
	}
  
  	bufend = _bufend(iop);
	nleft = (unsigned long) count * size;	/* may overflow */
	if (nleft < count || nleft < size)	/* overflow occured */
		return (0);
  
	/* if the file is unbuffered, or if the iop->ptr = iop->base, and there
	   is > BUFSZ chars to write, we can do a direct write */
	if (iop->_base >= iop->_ptr)  {	/*this covers the unbuffered case, too*/
		if (((iop->_flag & _IONBF) != 0) || (nleft >= BUFSIZ))  {
			if ((n=write(fileno(iop),ptr,nleft)) != nleft) {
				iop->_flag |= _IOERR;
				n = (n >= 0) ? n : 0;
			}
			return n/size;
		}
	}
	/* Put characters in the buffer */
	/* note that the meaning of n when just starting this loop is
	   irrelevant.  It is defined in the loop */
	for (; ; ptr += n) {
	        while ((n = bufend - (cptr = iop->_ptr)) <= 0)  /* full buf */
	                if (_xflsbuf(iop) == EOF)
	                        return (count - (nleft + size - 1)/size);
	        n = MIN(nleft, n);
	        (void) memcpy((char *) cptr, ptr, n);
	        iop->_cnt -= n;
	        iop->_ptr += n;
	        _BUFSYNC(iop);
		/* done; flush if linebuffered with a newline */
	        if ((nleft -= n) == 0)  { 
			if (iop->_flag & (_IOLBF | _IONBF)) {
	               		if ((iop->_flag & _IONBF) || (memchr(cptr,
					'\n',count * size) != NULL))  {
				     	(void) _xflsbuf(iop);
				}
			}
	                return (count);
	        }
	}
}
