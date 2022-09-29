/*	Copyright (c) 1984 AT&T	*/
/*	  All Rights Reserved  	*/

/*	THIS IS UNPUBLISHED PROPRIETARY SOURCE CODE OF AT&T	*/
/*	The copyright notice above does not evidence any   	*/
/*	actual or intended publication of such source code.	*/

/* $Header: _delch.c,v 1.2.1.1 89/11/27 10:24:46 wje Exp $ */

#define		NOMACROS
#include	"curses_inc.h"

delch()
{
    return (wdelch(stdscr));
}
