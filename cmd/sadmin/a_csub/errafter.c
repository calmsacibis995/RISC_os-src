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
#ident	"$Header: errafter.c,v 1.5.2.2 90/05/09 18:30:44 wje Exp $"

/*	Copyright (c) 1984 AT&T	*/
/*	  All Rights Reserved  	*/

/*	THIS IS UNPUBLISHED PROPRIETARY SOURCE CODE OF AT&T	*/
/*	The copyright notice above does not evidence any   	*/
/*	actual or intended publication of such source code.	*/

/*	
	Routine called after error message has been printed.
	Command and library version.
*/

#include	"cmderr.h"
#include	<stdio.h>


void
errafter( severity, format, ErrArgList )
int	severity;
char	*format;
int	ErrArgList;
{
	extern int	errno;

	switch( severity ) {
	case CHALT:
	case CERROR:
		if( errno  &&  (int)format != CERRNO ) {
			fputc( '\t', stderr );
			perror("");
		}
	}
	switch( severity ) {
	case CHALT:
		abort();
	case CERROR:
		exit( errexit );
	case CWARN:
	case CINFO:
		break;
	}
	return;
}
