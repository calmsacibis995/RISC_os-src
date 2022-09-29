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
#ident	"$Header: fdfopen.c,v 1.5.2.2 90/05/10 01:08:34 wje Exp $"

/*	Copyright (c) 1984 AT&T	*/
/*	  All Rights Reserved  	*/

/*	THIS IS UNPUBLISHED PROPRIETARY SOURCE CODE OF AT&T	*/
/*	The copyright notice above does not evidence any   	*/
/*	actual or intended publication of such source code.	*/

/*
	Interfaces with /lib/libS.a
	First arg is file descriptor, second is read/write mode (0/1).
	Returns file pointer on success,
	NULL on failure (no file structures available).
*/

# include	"stdio.h"
# include	"sys/types.h"
# include	"macros.h"

FILE *
fdfopen(fd, mode)
register int fd, mode;
{
	if (fstat(fd, &Statbuf) < 0)
		return(NULL);
	if (mode)
		return(fdopen(fd,"w"));
	else
		return(fdopen(fd,"r"));
}
