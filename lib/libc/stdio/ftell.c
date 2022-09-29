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
#ident	"$Header: ftell.c,v 1.6.2.2 90/05/10 01:46:10 wje Exp $"

/*	Copyright (c) 1984 AT&T	*/
/*	  All Rights Reserved  	*/

/*	THIS IS UNPUBLISHED PROPRIETARY SOURCE CODE OF AT&T	*/
/*	The copyright notice above does not evidence any   	*/
/*	actual or intended publication of such source code.	*/

/*LINTLIBRARY*/
/*
 * Return file offset.
 * Coordinates with buffering.
 */
#include <stdio.h>

extern long lseek();

long
ftell(iop)
FILE	*iop;
{
	long	tres;
	register int adjust;

	if(iop->_cnt < 0)
		iop->_cnt = 0;
	if(iop->_flag & _IOREAD)
		adjust = - iop->_cnt;
	else if(iop->_flag & (_IOWRT | _IORW)) {
		adjust = 0;
		if(iop->_flag & _IOWRT && iop->_base &&
					(iop->_flag & _IONBF) == 0)
			adjust = iop->_ptr - iop->_base;
	} else
		return(-1);
	tres = lseek(fileno(iop), 0L, 1);
	if(tres >= 0)
		tres += (long)adjust;
	return(tres);
}
