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
#ident	"$Header: pair_cont.c,v 1.2.1.2 90/05/10 02:17:49 wje Exp $"
/*	Copyright (c) 1984 AT&T	*/
/*	  All Rights Reserved  	*/

/*	THIS IS UNPUBLISHED PROPRIETARY SOURCE CODE OF AT&T	*/
/*	The copyright notice above does not evidence any   	*/
/*	actual or intended publication of such source code.	*/


#include "curses_inc.h"

pair_content(pair, f, b)
short pair, *f, *b;
{
    register _Color_pair *ptp;

    if (pair < 1 || pair >= COLOR_PAIRS)
	return (ERR);

    ptp = SP->_pairs_tbl + pair;

    if (!ptp->init)
        return (ERR);

    *f = ptp->foreground;
    *b = ptp->background;
    return (OK);
}
