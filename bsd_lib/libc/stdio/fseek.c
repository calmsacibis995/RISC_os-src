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
#ident	"$Header: fseek.c,v 1.2.1.2 90/05/07 21:07:14 wje Exp $"

#if defined(LIBC_SCCS) && !defined(lint)
static char sccsid[] = "@(#)fseek.c	5.3 (Berkeley) 3/9/86";
#endif LIBC_SCCS and not lint

/*
 * Seek for standard library.  Coordinates with buffering.
 */

#include	<stdio.h>

long lseek();

fseek(iop, offset, ptrname)
	register FILE *iop;
	long offset;
{
	register resync, c;
	long p = -1;			/* can't happen? */

	iop->_flag &= ~_IOEOF;
	if (iop->_flag&_IOREAD) {
		if (ptrname<2 && iop->_base &&
			!(iop->_flag&_IONBF)) {
			c = iop->_cnt;
			p = offset;
			if (ptrname==0) {
				long curpos = lseek(fileno(iop), 0L, 1);
				if (curpos == -1)
					return (-1);
				p += c - curpos;
			} else
				offset -= c;
			if(!(iop->_flag&_IORW) && c>0&&p<=c
			    && p>=iop->_base-iop->_ptr){
				iop->_ptr += (int)p;
				iop->_cnt -= (int)p;
				return(0);
			}
			resync = offset&01;
		} else 
			resync = 0;
		if (iop->_flag & _IORW) {
			iop->_ptr = iop->_base;
			iop->_flag &= ~_IOREAD;
			resync = 0;
		}
		p = lseek(fileno(iop), offset-resync, ptrname);
		iop->_cnt = 0;
		if (resync && p != -1)
			if (getc(iop) == EOF)
				p = -1;
	}
	else if (iop->_flag & (_IOWRT|_IORW)) {
		p = fflush(iop);
		if (iop->_flag & _IORW) {
			iop->_cnt = 0;
			iop->_flag &= ~_IOWRT;
			iop->_ptr = iop->_base;
		}
		return(lseek(fileno(iop), offset, ptrname) == -1 || p == EOF ?
		    -1 : 0);
	}
	return(p==-1?-1:0);
}