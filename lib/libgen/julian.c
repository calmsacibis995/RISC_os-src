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
#ident	"$Header: julian.c,v 1.5.2.2 90/05/10 02:37:42 wje Exp $"

/*	Copyright (c) 1984 AT&T	*/
/*	  All Rights Reserved  	*/

/*	THIS IS UNPUBLISHED PROPRIETARY SOURCE CODE OF AT&T	*/
/*	The copyright notice above does not evidence any   	*/
/*	actual or intended publication of such source code.	*/

/*
	julian is sent three arguments: yy or yyyy, mm, dd that comprise a
	gregorian date.  it returns the number of julian days: ddd
*/

#include <stdio.h>
#include	"libgen.h"


#define	LEAP	((year % 4 == 0 && year % 100 != 0) || ( year % 400 \
 == 0 )) ? 1 : 0
 
int
julian( year, month, day )
int	year, month, day;
{
	register	mo_indx;
	int		julday = 0;
	register	leapyr;
	
	static	int	day_tab[] = {0, 31, 28, 31, 30, 31, 30, 31, 31, 30,
					31, 30 };
	
	if ( year < 0 ) {
		fprintf( stderr, "julian: year must be greater than 0\n" );
		exit( 1 );
	}
	leapyr = LEAP;
	if ( month < 1  ||  month > 12 ) {
		fprintf( stderr, "julian: month must be between 1 and");
		fprintf( stderr, " 12\n" );
		exit( 1 );
	} else if ( day > day_tab[month]  || day < 1 ) {
		if ( month == 2  &&  day == 29  &&  leapyr )
			;
		else {
			fprintf( stderr, "julian: wrong number of" );
			fprintf( stderr, " days for month\n" );
			exit( 1 );
		}
	}
	for ( mo_indx = 1; mo_indx <= month; mo_indx++ )
		julday += day_tab[ mo_indx - 1 ];
	julday += day;
	if( leapyr  &&  month > 2 )
		julday++;
	return julday;
}
