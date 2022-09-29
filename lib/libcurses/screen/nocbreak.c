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
#ident	"$Header: nocbreak.c,v 1.3.1.3 90/05/10 02:16:47 wje Exp $"

/*	Copyright (c) 1984 AT&T	*/
/*	  All Rights Reserved  	*/

/*	THIS IS UNPUBLISHED PROPRIETARY SOURCE CODE OF AT&T	*/
/*	The copyright notice above does not evidence any   	*/
/*	actual or intended publication of such source code.	*/

#include	"curses_inc.h"

nocbreak()
{
#ifdef RISCOS
    change_bsd43_bits(0,CBREAK);
#else RISCOS
#ifdef	SYSV
    /* See comment in cbreak.c about ICRNL. */
    PROGTTY.c_iflag |= ICRNL;
    PROGTTY.c_lflag |= ICANON;
    PROGTTY.c_cc[VEOF] = _CTRL('D');
    PROGTTY.c_cc[VEOL] = 0;
#else	/* SYSV */
    PROGTTY.sg_flags &= ~(CBREAK | CRMOD);
#endif	/* SYSV */
#endif RISCOS

#ifdef	DEBUG
#ifdef	SYSV
    if (outf)
	fprintf(outf, "nocbreak(), file %x, flags %x\n",
	    cur_term->Filedes, PROGTTY.c_lflag);
#else	/* SYSV */
    if (outf)
	fprintf(outf, "nocbreak(), file %x, flags %x\n",
	    cur_term->Filedes, PROGTTY.sg_flags);
#endif	/* SYSV */
#endif	/* DEBUG */

    cur_term->_fl_rawmode = FALSE;
    cur_term->_delay = -1;
    reset_prog_mode();
#ifdef	FIONREAD
    cur_term->_timeout = 0;
#endif	/* FIONREAD */
    return (OK);
}
