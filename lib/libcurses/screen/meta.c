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
#ident	"$Header: meta.c,v 1.2.1.3 90/05/10 02:14:58 wje Exp $"

/*	Copyright (c) 1984 AT&T	*/
/*	  All Rights Reserved  	*/

/*	THIS IS UNPUBLISHED PROPRIETARY SOURCE CODE OF AT&T	*/
/*	The copyright notice above does not evidence any   	*/
/*	actual or intended publication of such source code.	*/

#include	"curses_inc.h"

/* TRUE => all 8 bits of input character should be passed through. */

_meta(bf)
int	bf;
{
    extern	int	_outch();
    /*
     * Do the appropriate fiddling with the tty driver to make it send
     * all 8 bits through.  On SYSV, refuse to do it if ISTRIP is set.
     * On V7 you have to resort to RAW mode.
     */
#ifdef	SYSV
    if (bf &&
	PROGTTY.c_iflag & ISTRIP)
	    return ERR;
#else	/* SYSV */
    if (bf)
	raw();
    else
	noraw();
    reset_prog_mode();
#endif	/* SYSV */

    /* Do whatever is needed to put the terminal into meta-mode. */

    if (SP->fl_meta = bf)
	tputs(meta_on, 1, _outch);
    else
	tputs(meta_off, 1, _outch);
    (void) fflush(SP->term_file);
    return (OK);
}
