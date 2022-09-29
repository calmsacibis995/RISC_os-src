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
#ident	"$Header: slk_clear.c,v 1.2.1.2 90/05/10 02:22:18 wje Exp $"
/*	Copyright (c) 1984 AT&T	*/
/*	  All Rights Reserved  	*/

/*	THIS IS UNPUBLISHED PROPRIETARY SOURCE CODE OF AT&T	*/
/*	The copyright notice above does not evidence any   	*/
/*	actual or intended publication of such source code.	*/

#include	"curses_inc.h"

/* Clear the soft labels. */

slk_clear()
{
    extern	int	_outch();
    register	SLK_MAP	*slk;
    register	int	i;

    if ((slk = SP->slk) == NULL)
	return (ERR);

    slk->_changed = 2;	/* This means no more soft labels. */
    if (slk->_win)
    {
	(void) werase(slk->_win);
	(void) wrefresh(slk->_win);
    }
    else
    {
	/* send hardware clear sequences */
	for (i = 0; i < slk->_num; i++)
	    _PUTS(tparm(plab_norm, i + 1, "        "), 1);
	_PUTS(label_off, 1);
	(void) fflush(SP->term_file);
    }

    for (i = 0; i < slk->_num; ++i)
	slk->_lch[i] = FALSE;

    return (OK);
}
