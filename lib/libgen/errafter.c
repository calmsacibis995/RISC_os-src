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
#ident	"$Header: errafter.c,v 1.5.2.2 90/05/10 02:35:15 wje Exp $"

/*	Copyright (c) 1984 AT&T	*/
/*	  All Rights Reserved  	*/

/*	THIS IS UNPUBLISHED PROPRIETARY SOURCE CODE OF AT&T	*/
/*	The copyright notice above does not evidence any   	*/
/*	actual or intended publication of such source code.	*/


/*
	Customized routine called after error message has been printed.
	Command and library version.
	Return a value to indicate action.
*/

#include	"errmsg.h"
#include	<stdio.h>
#include	<varargs.h>

int
errafter( severity, format, print_args )
int	severity;
char	*format;
va_list print_args;
{
	switch( severity ) {
	case EHALT:
		return EABORT;
	case EERROR:
		return EEXIT;
	case EWARN:
		return ERETURN;
	case EINFO:
		return ERETURN;
	}
	return ERETURN;
}
