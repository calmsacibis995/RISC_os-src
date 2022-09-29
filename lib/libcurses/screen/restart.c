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
#ident	"$Header: restart.c,v 1.2.1.3 90/05/10 02:19:30 wje Exp $"

/*	Copyright (c) 1984 AT&T	*/
/*	  All Rights Reserved  	*/

/*	THIS IS UNPUBLISHED PROPRIETARY SOURCE CODE OF AT&T	*/
/*	The copyright notice above does not evidence any   	*/
/*	actual or intended publication of such source code.	*/

#include	"curses_inc.h"

/*
 * This is useful after saving/restoring memory from a file (e.g. as
 * in a rogue save game).  It assumes that the modes and windows are
 * as wanted by the user, but the terminal type and baud rate may
 * have changed.
 */

extern	char	_called_before;

restartterm(term, filenum, errret)
char	*term;
int	filenum;	/* This is a UNIX file descriptor, not a stdio ptr. */
int	*errret;
{
    int	saveecho = SP->fl_echoit;
    int	savecbreak = cur_term->_fl_rawmode;
    int	savenl;

#ifdef RISCOS
    {
	int	bsd43_bits;

	bsd43_bits = get_bsd43_bits();

	if (bsd43_bits & RAW) 
	    savecbreak = 2;
	savenl = bsd43_bits & CRMOD;
    };
#else RISCOS
#ifdef	SYSV
    savenl = PROGTTY.c_iflag & ONLCR;
#else	/* SYSV */
    savenl = PROGTTY.sg_flags & CRMOD;
#endif	/* SYSV */
#endif RISCOS

    _called_before = 0;
    (void) setupterm(term, filenum, (int *) 0);

    /* Restore curses settable flags, leaving other stuff alone. */
    SP->fl_echoit = saveecho;

    nocbreak();
    noraw();
    if ((savecbreak == 1) ||
	(savecbreak == 3))
	cbreak();
    else
	if (savecbreak == 2)
	    raw();

    if (savenl)
	nl();
    else
	nonl();

    reset_prog_mode();

    LINES = SP->lsize;
    COLS = columns;
    return (OK);
}
