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
#ident	"$Header: curs_set.c,v 1.2.1.2 90/05/10 02:08:19 wje Exp $"
/*	Copyright (c) 1984 AT&T	*/
/*	  All Rights Reserved  	*/

/*	THIS IS UNPUBLISHED PROPRIETARY SOURCE CODE OF AT&T	*/
/*	The copyright notice above does not evidence any   	*/
/*	actual or intended publication of such source code.	*/


#include	"curses_inc.h"

/* Change the style of cursor in use. */

curs_set(visibility)
register	int	visibility;
{
    extern	int	_outch();
    int		ret = cur_term->_cursorstate;
    char	**cursor_seq = cur_term->cursor_seq;

    if ((visibility < 0) || (visibility > 2) || (!cursor_seq[visibility]))
	ret = ERR;
    else
	if (visibility != ret)
	    tputs(cursor_seq[cur_term->_cursorstate = visibility], 0, _outch);
    (void) fflush(SP->term_file);
    return (ret);
}
