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
#ident	"$Header: tconv.c,v 1.5.2.2 90/05/10 02:40:38 wje Exp $"

/*	Copyright (c) 1984 AT&T	*/
/*	  All Rights Reserved  	*/

/*	THIS IS UNPUBLISHED PROPRIETARY SOURCE CODE OF AT&T	*/
/*	The copyright notice above does not evidence any   	*/
/*	actual or intended publication of such source code.	*/

/* tconv -- time convert
**
** This routine converts year, month, day, hour, minute, second
** to UNIX's internal time format.  If the input is bad,
** -1 is returned.
*/


#include	"libgen.h"
#include	<time.h>

extern long	time();

long
tconv(year, month, day, hour, minute, second)
int year, month, day;			/* date parameters */
int hour, minute, second;		/* time parameters */
{
    long utime = 0;			/* initial time */
    long date = to_day(month, day, year);
					/* get date (-1 if bad) */
    
    if (date < 0)
	return( -1L );			/* indicate bad input */

    /* the last time UNIX can handle is 1/18/2038; for simplicity,
    ** stop at 2038
    */
    if (year < 1970 || year > 2037)	/* UNIX can't handle these */
	return( -1L );

    utime = date - to_day(1, 1, 1970);	/* day zero is 1/1/1970 */
    if (0 <= hour && hour <= 23)
	utime = utime * 24 + hour;
    else
	return( -1L );			/* bad hour */
    
    if (0 <= minute && minute <= 59)
	utime = utime * 60 + minute;
    else
	return( -1L );			/* bad minute */
    
    if (second < 0 || second >= 60)
	return( -1L );			/* bad seconds */

    utime = utime * 60 + second;		/* include seconds */

    tzset();				/* set time zone information */
    utime += timezone;			/* adjust time for timezone */

    /* Correct for daylight savings time.  Since the number of timezone
    ** hours is one larger during DST, we must subtract an hour's worth
    ** of seconds to compensate (since the GMT was unchanged).
    */

    if (localtime(&utime)->tm_isdst != 0)
	utime -= 1*60*60;

    return( utime );
}
