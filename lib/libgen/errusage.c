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
#ident	"$Header: errusage.c,v 1.5.2.2 90/05/10 02:36:40 wje Exp $"

/*	Copyright (c) 1984 AT&T	*/
/*	  All Rights Reserved  	*/

/*	THIS IS UNPUBLISHED PROPRIETARY SOURCE CODE OF AT&T	*/
/*	The copyright notice above does not evidence any   	*/
/*	actual or intended publication of such source code.	*/


#include  "errmsg.h"
#include  <stdio.h>
#include  <varargs.h>

#define   USAGENO  255	/* exit value for usage messages */

/* This routine prints the standard command usage message.
*/

void
errusage( va_alist )
va_dcl
{
	va_list	ap;
	char	*format;

	va_start( ap );
	format = va_arg( ap, char * );
	va_end( ap );

	fputs( "Usage:  ", stderr );
	if( Err.vsource  &&  Err.source ) {
		fputs( Err.source, stderr );
		fputc( ' ', stderr );
	}
	vfprintf( stderr, format, ap );
	fputc( '\n', stderr );

	errexit( USAGENO );
	erraction( EEXIT );
}
