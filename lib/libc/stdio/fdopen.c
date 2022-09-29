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
#ident	"$Header: fdopen.c,v 1.6.2.2 90/05/10 01:44:26 wje Exp $"

/*	Copyright (c) 1984 AT&T	*/
/*	  All Rights Reserved  	*/

/*	THIS IS UNPUBLISHED PROPRIETARY SOURCE CODE OF AT&T	*/
/*	The copyright notice above does not evidence any   	*/
/*	actual or intended publication of such source code.	*/

/*LINTLIBRARY*/
/*
 * Unix routine to do an "fopen" on file descriptor
 * The mode has to be repeated because you can't query its
 * status
 */

#include <stdio.h>
#include <sys/errno.h>

extern long lseek();
extern FILE *_findiop();

FILE *
fdopen(fd, mode)
int	fd;
register char *mode;
{
	register FILE *iop;

	if((iop = _findiop()) == NULL)
		return(NULL);

	iop->_cnt = 0;
	iop->_flag = 0;
	iop->_file = fd;
	_bufend(iop) = iop->_base = iop->_ptr = NULL;
	switch(*mode) {

		case 'r':
			iop->_flag |= _IOREAD;
			break;
		case 'a':
			(void) lseek(fd, 0L, 2);
			/* No break */
		case 'w':
			iop->_flag |= _IOWRT;
			break;
		default:
			return(NULL);
	}

	if(mode[1] == '+') {
		iop->_flag &= ~(_IOREAD | _IOWRT);
		iop->_flag |= _IORW;
	}

	return(iop);
}
