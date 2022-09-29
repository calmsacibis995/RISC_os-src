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
#ident	"$Header: regerror.c,v 1.5.2.2 90/05/10 02:38:56 wje Exp $"

/*	Copyright (c) 1984 AT&T	*/
/*	  All Rights Reserved  	*/

/*	THIS IS UNPUBLISHED PROPRIETARY SOURCE CODE OF AT&T	*/
/*	The copyright notice above does not evidence any   	*/
/*	actual or intended publication of such source code.	*/

/*
	return the a message explaining a regexp(5) error code
*/

#include "libgen.h"

char *
regerror(code)
int	code;
{
	switch( code ) {
	case 11:
		return "Range endpoint too large.";
	case 16:
		return "Bad number.";
	case 25:
		return "'\\digit' out of range.";
	case 36:
		return "Illegal or missing delimiter.";
	case 41:
		return "No remembered search string.";
	case 42:
		return "'\\( \\)' imbalance.";
	case 43:
		return "Too many '\\('.";
	case 44:
		return "More than 2 numbers given in \\{ \\}.";
	case 45:
		return "} expected after \\.";
	case 46:
		return "First number exceeds second in \\{ \\}.";
	case 49:
		return "[] imbalance.";
	case 50:
		return "Regular expression too long or too complex.";
	}
	return "Unknown code!";
}
