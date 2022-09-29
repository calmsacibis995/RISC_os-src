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
#ident	"$Header: zread.c,v 1.5.2.2 90/05/10 02:46:46 wje Exp $"

/*	Copyright (c) 1984 AT&T	*/
/*	  All Rights Reserved  	*/

/*	THIS IS UNPUBLISHED PROPRIETARY SOURCE CODE OF AT&T	*/
/*	The copyright notice above does not evidence any   	*/
/*	actual or intended publication of such source code.	*/


/*	read(2) with error checking
	End-Of-File and incomplete reads are considered errors!
*/

#include	"errmsg.h"
#include	<stdio.h>

zread(  severity, fd, buf, count )
int	severity;
int	fd;	/* file descriptor */
char	*buf;
unsigned int	count;
{
	register int	countin;

	if( (countin = read( fd, buf, count )) != count ) {
		_errmsg( "UXzread1", severity,
			"File descriptor %d, asked for %d, got %d.",
			 fd, count, countin );
	}
}
