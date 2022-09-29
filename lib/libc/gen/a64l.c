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
#ident	"$Header: a64l.c,v 1.6.2.3 90/05/10 01:27:56 wje Exp $"

/*	Copyright (c) 1984 AT&T	*/
/*	  All Rights Reserved  	*/

/*	THIS IS UNPUBLISHED PROPRIETARY SOURCE CODE OF AT&T	*/
/*	The copyright notice above does not evidence any   	*/
/*	actual or intended publication of such source code.	*/

/*LINTLIBRARY*/
/*
 * convert base 64 ascii to long int
 * char set is [./0-9A-Za-z]
 * This will convert up-to the first
 * six characters of a string, as documented.
 *
 */

#define BITSPERCHAR	6 /* to hold entire character set */

long
a64l(s)
register char *s;
{
	register int i, c;
	register int cnt;	/* count chars to stop at 6 if necessary */
	long lg = 0;

	for (i = 0, cnt = 0 ;
	      (c = *s++) != '\0' && cnt < 6;
	      i += BITSPERCHAR, cnt++) {
		if (c > 'Z')
			c -= 'a' - 'Z' - 1;
		if (c > '9')
			c -= 'A' - '9' - 1;
		lg |= (long)(c - ('0' - 2)) << i;
	}
	return (lg);
}
