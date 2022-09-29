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
#ident	"$Header: statlog.c,v 1.5.2.2 90/05/10 00:40:21 wje Exp $"

/*	Copyright (c) 1984 AT&T	*/
/*	  All Rights Reserved  	*/

/*	THIS IS UNPUBLISHED PROPRIETARY SOURCE CODE OF AT&T	*/
/*	The copyright notice above does not evidence any   	*/
/*	actual or intended publication of such source code.	*/


#include "uucp.h"

/*
	Report and log file transfer rate statistics.
	This is ugly because we are not using floating point.
*/

void
statlog( direction, bytes, millisecs )
char		*direction;
unsigned long	bytes;
time_t		millisecs;
{
	char		text[ 100 ];
	unsigned long	bytes1000;

	if (millisecs == 0) {
		millisecs = 1;	/* avoid divide by zero */
	}
	bytes1000 = bytes * 1000;
	(void) sprintf(text, "%s %lu / %lu.%.3u secs, %lu bytes/sec",
		direction, bytes, millisecs/1000, millisecs%1000,
		bytes1000/millisecs );
	CDEBUG(4, "%s\n", text);
	syslog(text);
}
