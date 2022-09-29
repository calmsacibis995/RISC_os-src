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
#ident	"$Header: raw.c,v 1.3.1.3 90/05/10 02:19:05 wje Exp $"

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

raw()
{
#ifdef RISCOS
    change_bsd43_bits(RAW,CBREAK);
#else RISCOS
#ifdef SYSV
    /* Disable interrupt characters */
    PROGTTY.c_lflag &= ~(ISIG|ICANON);
    PROGTTY.c_cc[VMIN] = 1;
    PROGTTY.c_cc[VTIME] = 0;
    PROGTTY.c_iflag &= ~IXON;
#else
    PROGTTY.sg_flags &= ~CBREAK;
    PROGTTY.sg_flags |= RAW;
#endif

#ifdef DEBUG
# ifdef SYSV
    if (outf)
	fprintf(outf, "raw(), file %x, iflag %x, cflag %x\n",
	    cur_term->Filedes, PROGTTY.c_iflag, PROGTTY.c_cflag);
# else
    if (outf)
	fprintf(outf, "raw(), file %x, flags %x\n",
	    cur_term->Filedes, PROGTTY.sg_flags);
# endif /* SYSV */
#endif
#endif RISCOS

    if (!needs_xon_xoff)
	xon_xoff = 0;	/* Cannot use xon/xoff in raw mode */
    cur_term->_fl_rawmode = 2;
    cur_term->_delay = -1;
    reset_prog_mode();
#ifdef FIONREAD
    cur_term->_timeout = 0;
#endif /* FIONREAD */
    return (OK);
}
