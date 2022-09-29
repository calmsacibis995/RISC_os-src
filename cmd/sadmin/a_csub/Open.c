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
#ident	"$Header: Open.c,v 1.5.2.2 90/05/09 18:30:16 wje Exp $"

/*	Copyright (c) 1984 AT&T	*/
/*	  All Rights Reserved  	*/

/*	THIS IS UNPUBLISHED PROPRIETARY SOURCE CODE OF AT&T	*/
/*	The copyright notice above does not evidence any   	*/
/*	actual or intended publication of such source code.	*/

/*	
	open(2) with error checking
*/

#include	<cmderr.h>
#include	<stdio.h>


Open( path, oflag, mode )
char	*path;
int	oflag;
int	mode;
{
	register int	fd;	/* file descriptor */

	if( (fd = open( path, oflag, mode )) == -1 ) {
		cmderr( CERROR, "Cannot open '%s'", path );
		exit( 1 );
	}
	return  fd;
}
