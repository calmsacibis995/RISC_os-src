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
#ident	"$Header: fread.c,v 1.2.1.2 90/05/07 21:06:39 wje Exp $"

/*
 * Copyright (c) 1980 Regents of the University of California.
 * All rights reserved.  The Berkeley software License Agreement
 * specifies the terms and conditions for redistribution.
 */

#if defined(LIBC_SCCS) && !defined(lint)
static char sccsid[] = "@(#)fread.c	5.2 (Berkeley) 3/9/86";
#endif LIBC_SCCS and not lint

#include	<stdio.h>

fread(ptr, size, count, iop)
	register char *ptr;
	unsigned size, count;
	register FILE *iop;
{
	register int s;
	int c;

	s = size * count;
	while (s > 0) {
		if (iop->_cnt < s) {
			if (iop->_cnt > 0) {
				bcopy(iop->_ptr, ptr, iop->_cnt);
				ptr += iop->_cnt;
				s -= iop->_cnt;
			}
			/*
			 * filbuf clobbers _cnt & _ptr,
			 * so don't waste time setting them.
			 */
			if ((c = _filbuf(iop)) == EOF)
				break;
			*ptr++ = c;
			s--;
		}
		if (iop->_cnt >= s) {
			bcopy(iop->_ptr, ptr, s);
			iop->_ptr += s;
			iop->_cnt -= s;
			return (count);
		}
	}
	return (size != 0 ? count - ((s + size - 1) / size) : 0);
}
