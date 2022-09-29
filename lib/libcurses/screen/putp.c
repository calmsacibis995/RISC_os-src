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
#ident	"$Header: putp.c,v 1.2.1.2 90/05/10 02:18:20 wje Exp $"
/*	Copyright (c) 1984 AT&T	*/
/*	  All Rights Reserved  	*/

/*	THIS IS UNPUBLISHED PROPRIETARY SOURCE CODE OF AT&T	*/
/*	The copyright notice above does not evidence any   	*/
/*	actual or intended publication of such source code.	*/

/*
 * Handy functions to put out a string with padding.
 * These make two assumptions:
 *	(1) Output is via stdio to stdout through putchar.
 *	(2) There is no count of affected lines.  Thus, this
 *	    routine is only valid for certain capabilities,
 *	    i.e. those that don't have *'s in the documentation.
 */
#include	"curses_inc.h"

/*
 * Routine to act like putchar for passing to tputs.
 * _outchar should really be a void since it's used by tputs
 * and tputs doesn't look at return code.  However, tputs also has the function
 * pointer declared as returning an int so we didn't change it.
 */
_outchar(ch)
char	ch;
{
    (void) putchar(ch);
}

/* Handy way to output a string. */

putp(str)
char	*str;
{
    extern	int	_outchar();

    return (tputs(str, 1, _outchar));
}

/* Handy way to output video attributes. */

vidattr(newmode)
chtype	newmode;
{
    extern	int	_outchar();

    return (vidputs(newmode, _outchar));
}
