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
#ident	"$Header: numd.c,v 1.5.2.2 90/05/09 18:32:10 wje Exp $"

/*	Copyright (c) 1984 AT&T	*/
/*	  All Rights Reserved  	*/

/*	THIS IS UNPUBLISHED PROPRIETARY SOURCE CODE OF AT&T	*/
/*	The copyright notice above does not evidence any   	*/
/*	actual or intended publication of such source code.	*/

/*	
	true if string is numeric, with or without decimal point
*/

#include	<ctype.h>


numd( string )
register char *string;
{
	register int	seen_a_digit = 0;

	if( !string  ||  !*string )
		return  0;	/* null pointer or null string */
	while( *string != '.' && *string != '\0' )
		if( isdigit( *(string++) ) )
			seen_a_digit = 1;
		else
			return  0;
	if( *string == '.' ) {	/* have found decimal */
		string++;	/* go to char. after decimal */
		while( *string )
			if( isdigit( *(string++) ) )
				seen_a_digit = 1;
			else
				return  0;
	}
	return  seen_a_digit;
}