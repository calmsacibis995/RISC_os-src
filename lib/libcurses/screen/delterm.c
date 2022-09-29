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
#ident	"$Header: delterm.c,v 1.2.1.2 90/05/10 02:09:37 wje Exp $"
/*	Copyright (c) 1984 AT&T	*/
/*	  All Rights Reserved  	*/

/*	THIS IS UNPUBLISHED PROPRIETARY SOURCE CODE OF AT&T	*/
/*	The copyright notice above does not evidence any   	*/
/*	actual or intended publication of such source code.	*/

#include "curses_inc.h"

/*
 * Relinquish the storage associated with "terminal".
 */
extern	TERMINAL	_first_term;
extern	char		_called_before;
extern	char		_frst_tblstr[];

delterm(terminal)
register	TERMINAL	*terminal;
{
    if (!terminal)
	return (ERR);
    delkeymap(terminal);
    if (terminal->_check_fd >= 0)
	close(terminal->_check_fd);
    if (terminal == &_first_term)
    {
	/* next setupterm can re-use static areas */
	_called_before = FALSE;
	if (terminal->_strtab != _frst_tblstr)
	    free((char *)terminal->_strtab);
    }
    else
    {
	free((char *)terminal->_bools);
	free((char *)terminal->_nums);
	free((char *)terminal->_strs);
	free((char *)terminal->_strtab);
	free((char *)terminal);
    }
    return (OK);
}
