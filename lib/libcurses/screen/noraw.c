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
#ident	"$Header: noraw.c,v 1.3.1.3 90/05/10 02:16:59 wje Exp $"

/*	Copyright (c) 1984 AT&T	*/
/*	  All Rights Reserved  	*/

/*	THIS IS UNPUBLISHED PROPRIETARY SOURCE CODE OF AT&T	*/
/*	The copyright notice above does not evidence any   	*/
/*	actual or intended publication of such source code.	*/

#include	"curses_inc.h"

noraw()
{
#ifdef RISCOS
    change_bsd43_bits(0,RAW);
#else RISCOS
#ifdef	SYSV
    /* Enable interrupt characters */
    PROGTTY.c_lflag |= (ISIG|ICANON);
    PROGTTY.c_cc[VEOF] = _CTRL('D');
    PROGTTY.c_cc[VEOL] = 0;
    PROGTTY.c_iflag |= IXON;
#else	/* SYSV */
    PROGTTY.sg_flags &= ~(RAW|CBREAK);
#endif	/* SYSV */

#ifdef	DEBUG
#ifdef	SYSV
    if (outf)
	fprintf(outf, "noraw(), file %x, flags %x\n",
	    cur_term->Filedes, PROGTTY.c_lflag);
#else	/* SYSV */
    if (outf)
	fprintf(outf, "noraw(), file %x, flags %x\n",
	    cur_term->Filedes, PROGTTY.sg_flags);
#endif	/* SYSV */
#endif	/* DEBUG */
#endif RISCOS

    cur_term->_fl_rawmode = FALSE;
    cur_term->_delay = -1;
    reset_prog_mode();
#ifdef	FIONREAD
    cur_term->_timeout = 0;
#endif	/* FIONREAD */
    return (OK);
}
