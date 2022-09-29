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
#ident	"$Header: strecpy.c,v 1.5.2.2 90/05/10 02:39:48 wje Exp $"

/*	Copyright (c) 1984 AT&T	*/
/*	  All Rights Reserved  	*/

/*	THIS IS UNPUBLISHED PROPRIETARY SOURCE CODE OF AT&T	*/
/*	The copyright notice above does not evidence any   	*/
/*	actual or intended publication of such source code.	*/

/*
	strecpy(output, input, except)
	strccpy copys the input string to the output string expanding
	any non-graphic character with the C escape sequence.
	Esacpe sequences produced are those defined in "The C Programming
	Language" pages 180-181.
	Characters in the except string will not be expanded.
*/

#include	<ctype.h>
#include	<string.h>
#include	"libgen.h"


char *
strecpy( pout, pin, except )
register char	*pout;
register char	*pin;
char	*except;
{
	register unsigned	c;
	register char		*output;

	output = pout;
	while( c = *pin++ ) {
		if( !isprint( c )  &&  ( !except  ||  !strchr( except, c ) ) ) {
			*pout++ = '\\';
			switch( c ) {
			case '\n':
				*pout++ = 'n';
				continue;
			case '\t':
				*pout++ = 't';
				continue;
			case '\b':
				*pout++ = 'b';
				continue;
			case '\r':
				*pout++ = 'r';
				continue;
			case '\f':
				*pout++ = 'f';
				continue;
			case '\v':
				*pout++ = 'v';
				continue;
			case '\\':
				continue;
			default:
				sprintf( pout, "%.3o", c );
				pout += 3;
				continue;
			}
		}
		if( c == '\\' )
			*pout++ = '\\';
		*pout++ = c;
	}
	*pout = '\0';
	return  output;
}
