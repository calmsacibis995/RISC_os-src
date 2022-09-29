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
#ident	"$Header: resetty.c,v 1.2.1.2 90/05/10 02:19:24 wje Exp $"
/*	Copyright (c) 1984 AT&T	*/
/*	  All Rights Reserved  	*/

/*	THIS IS UNPUBLISHED PROPRIETARY SOURCE CODE OF AT&T	*/
/*	The copyright notice above does not evidence any   	*/
/*	actual or intended publication of such source code.	*/


#include	"curses_inc.h"

resetty()
{
    if ((_BR(SP->save_tty_buf)) != 0)
    {
	PROGTTY = SP->save_tty_buf;
#ifdef	DEBUG
	if (outf)
#ifdef	SYSV
	    fprintf(outf, "resetty(), file %x, SP %x, flags %x, %x, %x, %x\n",
		cur_term->Filedes, SP, PROGTTY.c_iflag, PROGTTY.c_oflag,
		PROGTTY.c_cflag, PROGTTY.c_lflag);
#else	/* SYSV */
	    fprintf(outf, "resetty(), file %x, SP %x, flags %x\n",
		cur_term->Filedes, SP, PROGTTY.sg_flags);
#endif	/* SYSV */
#endif	/* DEBUG */
	reset_prog_mode();
    }
    return (OK);
}
