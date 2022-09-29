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
#ident	"$Header: delkeymap.c,v 1.2.1.2 90/05/10 02:09:26 wje Exp $"
/*	Copyright (c) 1984 AT&T	*/
/*	  All Rights Reserved  	*/

/*	THIS IS UNPUBLISHED PROPRIETARY SOURCE CODE OF AT&T	*/
/*	The copyright notice above does not evidence any   	*/
/*	actual or intended publication of such source code.	*/

#include	"curses_inc.h"

/*
 *	Delete a key table
 */

void
delkeymap(terminal)
TERMINAL	*terminal;
{
    register	_KEY_MAP	**kpp, *kp;
    int				numkeys = terminal->_ksz;

    /* free key slots */
    for (kpp = terminal->_keys; numkeys-- > 0; kpp++)
    {
	kp = *kpp;
	if (kp->_sends == ((char *) (kp + sizeof(_KEY_MAP))))
	    free(kp);
    }

    if (terminal->_keys != NULL)
    {
	free(terminal->_keys);
	if (terminal->internal_keys != NULL)
	    free(terminal->internal_keys);
    }
    _blast_keys(terminal);
}
