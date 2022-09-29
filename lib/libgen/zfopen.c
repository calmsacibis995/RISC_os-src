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
#ident	"$Header: zfopen.c,v 1.5.2.2 90/05/10 02:44:34 wje Exp $"

/*	Copyright (c) 1984 AT&T	*/
/*	  All Rights Reserved  	*/

/*	THIS IS UNPUBLISHED PROPRIETARY SOURCE CODE OF AT&T	*/
/*	The copyright notice above does not evidence any   	*/
/*	actual or intended publication of such source code.	*/


/*	fopen(3S) with error checking
*/

#include	"errmsg.h"
#include	<stdio.h>


FILE *
zfopen( severity, path, type )
int	severity;
char	*path;
char	*type;
{
	register FILE	*fp;	/* file pointer */

	if( (fp = fopen( path, type )) == NULL ) {
		char	*mode;

		if( type[1] == '+' )
			mode = "updating";
		else
			switch( type[0] ) {
			case 'r':
				mode = "reading";
				break;
			case 'w':
				mode = "writing";
				break;
			case 'a':
				mode = "appending";
				break;
			default:
				mode = type;
			}
		_errmsg( "UXzfopen1", severity,
			"Cannot open file \"%s\" for %s.",
			 path, mode );
	}
	return  fp;
}
