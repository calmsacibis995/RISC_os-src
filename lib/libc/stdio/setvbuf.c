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
#ident	"$Header: setvbuf.c,v 1.6.2.2 90/05/10 01:48:23 wje Exp $"

/*	Copyright (c) 1984 AT&T	*/
/*	  All Rights Reserved  	*/

/*	THIS IS UNPUBLISHED PROPRIETARY SOURCE CODE OF AT&T	*/
/*	The copyright notice above does not evidence any   	*/
/*	actual or intended publication of such source code.	*/

/*LINTLIBRARY*/
#include <stdio.h>

extern void free();
extern int isatty();

int
setvbuf(iop, buf, type, size)
register FILE *iop;
register int type;
register char	*buf;
register int size;
{
	char *malloc();

	if ( size < 0 )
		return -1;
	if(iop->_base != NULL && iop->_flag & _IOMYBUF)
		free((char*)iop->_base);
	iop->_flag &= ~(_IOMYBUF | _IONBF | _IOLBF);
	switch (type)  {
	    /*note that the flags are the same as the possible values for type*/
	    case _IONBF:
		/* file is unbuffered */
		iop->_flag |= _IONBF;
		_bufend(iop) = iop->_base = NULL;
		break;
	    case _IOLBF:
	    case _IOFBF:
		/* buffer file */
		iop->_flag |= type;
		/* 
		* need eight characters beyond bufend for stdio slop
		*/
		if (size <= 8) {
		    size = BUFSIZ;
		    buf = NULL;
		}
		if (buf == NULL) {
		    size += 8;
		    iop->_base = (unsigned char *)malloc((unsigned)size);
		}
		else
		    iop->_base = (unsigned char *)buf;
		_bufend(iop) = iop->_base + size -8;
		break;
	    default:
		return -1;
	}
	iop->_ptr = iop->_base;
	iop->_cnt = 0;
	return 0;
}
