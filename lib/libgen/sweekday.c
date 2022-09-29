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
#ident	"$Header: sweekday.c,v 1.5.2.2 90/05/10 02:40:32 wje Exp $"

/*	Copyright (c) 1984 AT&T	*/
/*	  All Rights Reserved  	*/

/*	THIS IS UNPUBLISHED PROPRIETARY SOURCE CODE OF AT&T	*/
/*	The copyright notice above does not evidence any   	*/
/*	actual or intended publication of such source code.	*/

/* sweekday -- return string name of day of the week
**
** This routine returns the name of the day of the week that
** corresponds to an absolute date.  Day 1 is Saturday.  The
** static array therefore begins with Friday at [0].
*/

#include	"libgen.h"

/* Table of names of days.  Day 1 is a Saturday. */

static char * dayname[] = {
	"Friday", "Saturday", "Sunday", "Monday",
	"Tuesday", "Wednesday", "Thursday"
};

char *
sweekday(day)
long day;				/* absolute date */
{
    return( dayname[day%7] );
}
