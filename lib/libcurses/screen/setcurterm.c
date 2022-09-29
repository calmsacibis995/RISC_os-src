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
#ident	"$Header: setcurterm.c,v 1.2.1.2 90/05/10 02:21:01 wje Exp $"
/*	Copyright (c) 1984 AT&T	*/
/*	  All Rights Reserved  	*/

/*	THIS IS UNPUBLISHED PROPRIETARY SOURCE CODE OF AT&T	*/
/*	The copyright notice above does not evidence any   	*/
/*	actual or intended publication of such source code.	*/

#include "curses_inc.h"

/*
 * Establish the terminal that the #defines in term.h refer to.
 */

TERMINAL *
setcurterm(newterminal)
register TERMINAL *newterminal;
{
    register	TERMINAL	*oldterminal = cur_term;

    if (newterminal)
    {
#ifdef	_VR3_COMPAT_CODE
	acs_map = cur_term->_acs32map;
#else	/* _VR3_COMPAT_CODE */
	acs_map = cur_term->_acsmap;
#endif	/* _VR3_COMPAT_CODE */
	cur_bools = newterminal->_bools;
	cur_nums = newterminal->_nums;
	cur_strs = newterminal->_strs;
	cur_term = newterminal;
    }
    return (oldterminal);
}
