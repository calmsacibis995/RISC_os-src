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
#ident	"$Header: ungetc.c,v 1.7.2.2 90/05/10 01:49:28 wje Exp $"

/*	Copyright (c) 1984 AT&T	*/
/*	  All Rights Reserved  	*/

/*	THIS IS UNPUBLISHED PROPRIETARY SOURCE CODE OF AT&T	*/
/*	The copyright notice above does not evidence any   	*/
/*	actual or intended publication of such source code.	*/

/*LINTLIBRARY*/
#include "shlib.h"
#include <stdio.h>

extern _findbuf();

int
ungetc(c, iop)
int	c;
register FILE *iop;
{
	if(c == EOF)   
		return(EOF);
	if((iop->_flag & _IOREAD) == 0 || iop->_ptr <= iop->_base)
		if(iop->_ptr == iop->_base && iop->_cnt == 0)
			++iop->_ptr;
		else
			return(EOF);
if (iop->_base == NULL)
	_findbuf(iop);
	*--iop->_ptr = c;
	++iop->_cnt;
	return(c);
}
