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
#ident	"$Header: fseek.c,v 1.6.3.2 90/05/10 01:46:04 wje Exp $"

/*	Copyright (c) 1984 AT&T	*/
/*	  All Rights Reserved  	*/

/*	THIS IS UNPUBLISHED PROPRIETARY SOURCE CODE OF AT&T	*/
/*	The copyright notice above does not evidence any   	*/
/*	actual or intended publication of such source code.	*/

/*LINTLIBRARY*/
/*
 * Seek for standard library.  Coordinates with buffering.
 */
#include "shlib.h"
#include <stdio.h>

extern long lseek();
extern int fflush();

int
fseek(iop, offset, ptrname)
register FILE *iop;
long	offset;
int	ptrname;
{
	register int c;
	long	p;

	iop->_flag &= ~_IOEOF;
	if(iop->_flag & _IOREAD) {
		if(ptrname < 2 && iop->_base && !(iop->_flag&_IONBF)) {
			c = iop->_cnt;
			p = offset;
			if(ptrname == 0)
				p += (long)c-lseek(fileno(iop), 0L, 1);
			else
				offset -= (long)c;
			if(!(iop->_flag&_IORW) && c > 0 && p <= c &&
					p >= iop->_base - iop->_ptr) {
				iop->_ptr += (int)p;
				iop->_cnt -= (int)p;
				return(0);
			}
		}
		if(iop->_flag & _IORW) {
			iop->_ptr = iop->_base;
			iop->_flag &= ~_IOREAD;
		}
		p = lseek(fileno(iop), offset, ptrname);
		iop->_cnt = 0;
	} else if(iop->_flag & (_IOWRT | _IORW)) {
		(void) fflush(iop);
		iop->_cnt = 0;
		if(iop->_flag & _IORW) {
			iop->_flag &= ~_IOWRT;
			iop->_ptr = iop->_base;
		}
		p = lseek(fileno(iop), offset, ptrname);
	}
	return((p == -1)? -1: 0);
}
