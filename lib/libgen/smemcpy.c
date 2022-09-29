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
#ident	"$Header: smemcpy.c,v 1.5.3.2 90/05/10 02:39:20 wje Exp $"

/*	Copyright (c) 1984 AT&T	*/
/*	  All Rights Reserved  	*/

/*	THIS IS UNPUBLISHED PROPRIETARY SOURCE CODE OF AT&T	*/
/*	The copyright notice above does not evidence any   	*/
/*	actual or intended publication of such source code.	*/

/*	Safe memory copy.
	Fast if the to and from strings do not overlap,
	slower but safe if they do.
*/

#include	<memory.h>
#include	"libgen.h"


char *
smemcpy( to, from, count )
register char		*to, *from;
register unsigned	count;
{
	char	*savedto;

	if( &to[ count ] <= from  ||  &from[ count ] <= to )
		return  memcpy( to, from, count );

	if( to == from )
		return  to;

	savedto = to;
	if( to < from )
		while( count-- )
			*(to++) = *(from++);
	else {
		to += count;
		from += count;
		while( count-- )
			*(--to) = *(--from);
	}

	return  savedto;
}
