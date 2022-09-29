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
#ident	"$Header: isrequest.c,v 1.4.2.2 90/05/09 16:27:13 wje Exp $"

/*	Copyright (c) 1984 AT&T	*/
/*	  All Rights Reserved  	*/

/*	THIS IS UNPUBLISHED PROPRIETARY SOURCE CODE OF AT&T	*/
/*	The copyright notice above does not evidence any   	*/
/*	actual or intended publication of such source code.	*/

/*
 *	isrequest(s, dest, seqno) - predicate which returns true if
 *		s is a syntactically correct request id as returned by lp(1).
 *		No check is made to see if it is in the output queue.
 *		if s is a request id, then
 *			dest is set to the destination
 *			seqno is set to the sequence number
 */

#include	"lp.h"


isrequest(s, dest, seqno)
char *s;
char *dest;
int *seqno;
{
	char *strchr(), *p, *strncpy();

	if((p = strchr(s, '-')) == NULL || p == s || p - s > DESTMAX ||
	    (*seqno = atoi(p + 1)) <= 0 || *seqno >= SEQMAX)
		return(FALSE);
	strncpy(dest, s, p - s);
	*(dest + (p - s)) = '\0';
	return(isdest(dest));
}
