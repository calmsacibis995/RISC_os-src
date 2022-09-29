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
#ident	"$Header: zfreopen.c,v 1.5.2.2 90/05/10 02:44:46 wje Exp $"

/*	Copyright (c) 1984 AT&T	*/
/*	  All Rights Reserved  	*/

/*	THIS IS UNPUBLISHED PROPRIETARY SOURCE CODE OF AT&T	*/
/*	The copyright notice above does not evidence any   	*/
/*	actual or intended publication of such source code.	*/


/*	freopen(3S) with error checking
*/

#include	"errmsg.h"
#include	<stdio.h>


FILE *
zfreopen( severity, path, type, stream )
int	severity;
char	*path;
char	*type;
FILE	*stream;
{
	register FILE	*fp;	/* file pointer */

	if( (fp = freopen( path, type, stream )) == NULL ) {
		_errmsg( "UXzfreopen1", severity,
			"Cannot open file \"%s\".",
			 path );
	}
	return  fp;
}
