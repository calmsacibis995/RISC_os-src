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
#ident	"$Header: setcurscreen.c,v 1.2.1.2 90/05/10 02:20:55 wje Exp $"
/*	Copyright (c) 1984 AT&T	*/
/*	  All Rights Reserved  	*/

/*	THIS IS UNPUBLISHED PROPRIETARY SOURCE CODE OF AT&T	*/
/*	The copyright notice above does not evidence any   	*/
/*	actual or intended publication of such source code.	*/

#include "curses_inc.h"

SCREEN	*
setcurscreen(new)
SCREEN	*new;
{
    register	SCREEN	*rv = SP;

    if (new != SP)
    {

#ifdef	DEBUG
	if (outf)
	    fprintf(outf, "setterm: old %x, new %x\n", rv, new);
#endif	/* DEBUG */

	SP = new;
	(void) setcurterm(SP->tcap);
	LINES = SP->lsize;
	COLS = SP->csize;
	TABSIZE = SP->tsize;
	stdscr = SP->std_scr;
	curscr = SP->cur_scr;
	_virtscr = SP->virt_scr;
    }
    return (rv);
}
