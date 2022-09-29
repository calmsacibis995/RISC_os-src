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
#ident	"$Header: tmsecs.c,v 1.1.2.2 90/05/09 15:07:52 wje Exp $"
/*	Copyright (c) 1984 AT&T	*/
/*	  All Rights Reserved  	*/

/*	THIS IS UNPUBLISHED PROPRIETARY SOURCE CODE OF AT&T	*/
/*	The copyright notice above does not evidence any   	*/
/*	actual or intended publication of such source code.	*/


/*
 *	tmsecs returns number of seconds from t1 to t2,
 *	times expressed in localtime format.
 *	assumed that t1 <= t2, and are in same day.
 */

#include <time.h>
long
tmsecs(t1, t2)
register struct tm *t1, *t2;
{
	return((t2->tm_sec - t1->tm_sec) +
		60*(t2->tm_min - t1->tm_min) +
		3600L*(t2->tm_hour - t1->tm_hour));
}
