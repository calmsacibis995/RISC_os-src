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
#ident	"$Header: curserr.c,v 1.2.1.2 90/05/10 02:08:25 wje Exp $"
/*	Copyright (c) 1984 AT&T	*/
/*	  All Rights Reserved  	*/

/*	THIS IS UNPUBLISHED PROPRIETARY SOURCE CODE OF AT&T	*/
/*	The copyright notice above does not evidence any   	*/
/*	actual or intended publication of such source code.	*/


#include 	"curses_inc.h"

char	*curs_err_strings[] =
{
    "I don't know how to deal with your \"%s\" terminal",
    "I need to know a more specific terminal type than \"%s\"",	/* unknown */
#ifdef	DEBUG
    "malloc returned NULL in function \"%s\"",
#else	/* DEBUG */
    "malloc returned NULL",
#endif	/* DEBUG */
};

void
curserr()
{
    fprintf(stderr, "Sorry, ");
    fprintf(stderr, curs_err_strings[curs_errno], curs_parm_err);
    fprintf(stderr, ".\r\n");
}
