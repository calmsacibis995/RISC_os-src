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
#ident	"$Header: basename.c,v 1.5.2.2 90/05/10 02:34:03 wje Exp $"

/*	Copyright (c) 1984 AT&T	*/
/*	  All Rights Reserved  	*/

/*	THIS IS UNPUBLISHED PROPRIETARY SOURCE CODE OF AT&T	*/
/*	The copyright notice above does not evidence any   	*/
/*	actual or intended publication of such source code.	*/


/*
	Return pointer to the last element of a pathname.
*/

#include	<string.h>
#include	"libgen.h"


char *
basename( s )
char	*s;
{
	register char	*p;

	if( !s  ||  !*s )			/* zero or empty argument */
		return  ".";

	p = s + strlen( s );
	while( p != s  &&  *--p == '/' )	/* skip trailing /s */
		;
	
	while( p != s )
		if( *--p == '/' )
			return  ++p;

	return  p;
}
