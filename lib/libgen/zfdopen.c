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
#ident	"$Header: zfdopen.c,v 1.5.2.2 90/05/10 02:43:58 wje Exp $"

/*	Copyright (c) 1984 AT&T	*/
/*	  All Rights Reserved  	*/

/*	THIS IS UNPUBLISHED PROPRIETARY SOURCE CODE OF AT&T	*/
/*	The copyright notice above does not evidence any   	*/
/*	actual or intended publication of such source code.	*/


/*	fdopen(3S) with error checking
*/

#include	"errmsg.h"
#include	<stdio.h>


FILE *
zfdopen( severity, fildes, type )
int	severity;
int	fildes;
char	*type;
{
	register FILE	*fp;	/* file pointer */

	if( (fp = fdopen( fildes, type )) == NULL ) {
		_errmsg( "UXzfdopen1", severity,
			"Cannot fdopen() file descriptor %d for \"%s\".",
			fildes, type );
	}
	return  fp;
}
