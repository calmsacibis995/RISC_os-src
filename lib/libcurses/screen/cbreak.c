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
#ident	"$Header: cbreak.c,v 1.3.1.3 90/05/10 02:07:48 wje Exp $"

/*	Copyright (c) 1984 AT&T	*/
/*	  All Rights Reserved  	*/

/*	THIS IS UNPUBLISHED PROPRIETARY SOURCE CODE OF AT&T	*/
/*	The copyright notice above does not evidence any   	*/
/*	actual or intended publication of such source code.	*/

/*
 * Routines to deal with setting and resetting modes in the tty driver.
 * See also setupterm.c in the termlib part.
 */
#include "curses_inc.h"

cbreak()
{
    /*
     * This optimization is here because till SVR3.1 curses did not come up in
     * cbreak mode and now it does.  Therefore, most programs when they call
     * cbreak won't pay for it since we'll know we're in the right mode.
     */

    if (cur_term->_fl_rawmode != 1)
    {
#ifdef RISCOS
	change_bsd43_bits(CBREAK,0);
#else RISCOS
#ifdef SYSV
	/*
	 * You might ask why ICRNL has anything to do with cbreak.
	 * The problem is that there are function keys that send
	 * a carriage return (some hp's).  Curses cannot virtualize
	 * these function keys if CR is being mapped to a NL.  Sooo,
	 * when we start a program up we unmap those but if you are
	 * in nocbreak then we map them back.  The reason for that is that
	 * if a getch or getstr is done and you are in nocbreak the tty
	 * driver won't return until it sees a new line and since we've
	 * turned it off any program that has nl() and nocbreak() would
	 * force the user to type a NL.  The problem with the function keys
	 * only gets solved if you are in cbreak mode which is OK
	 * since program taking action on a function key is probably
	 * in cbreak because who would expect someone to press a function
	 * key and then return ?????
	 */

	PROGTTY.c_iflag &= ~ICRNL;
	PROGTTY.c_lflag &= ~ICANON;
	PROGTTY.c_cc[VMIN] = 1;
	PROGTTY.c_cc[VTIME] = 0;
#else
	PROGTTY.sg_flags |= (CBREAK | CRMOD);
#endif

#ifdef DEBUG
# ifdef SYSV
	if (outf)
	    fprintf(outf, "cbreak(), file %x, flags %x\n", cur_term->Filedes, PROGTTY.c_lflag);
# else
	if (outf)
	    fprintf(outf, "cbreak(), file %x, flags %x\n", cur_term->Filedes, PROGTTY.sg_flags);
# endif
#endif
#endif RISCOS
	cur_term->_fl_rawmode = 1;
	cur_term->_delay = -1;
	reset_prog_mode();
#ifdef FIONREAD
	cur_term->_timeout = 0;
#endif /* FIONREAD */
    }
    return (OK);
}
