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
#ident	"$Header: assert.c,v 1.7.1.2 90/05/10 01:28:19 wje Exp $"

/*	Copyright (c) 1984 AT&T	*/
/*	  All Rights Reserved  	*/

/*	THIS IS UNPUBLISHED PROPRIETARY SOURCE CODE OF AT&T	*/
/*	The copyright notice above does not evidence any   	*/
/*	actual or intended publication of such source code.	*/

/*LINTLIBRARY*/
/*
 *	called from "assert" macro; prints without printf or stdio.
 */

#define WRITE(s, n)	(void) write(2, (s), (n))
#define WRITESTR(s1, n, s2)	WRITE((s1), n), \
				WRITE((s2), (unsigned) strlen(s2))

void
_assert(assertion, filename, line_num)
char *assertion;
char *filename;
int line_num;
{
	static char linestr[] = ", line NNNNN\n";
	register char *p = &linestr[7];
	register int div, digit;

	WRITESTR("Assertion failed: ", 18, assertion);
	WRITESTR(", file ", 7, filename);
	for (div = 10000; div != 0; line_num %= div, div /= 10)
		if ((digit = line_num/div) != 0 || p != &linestr[7] || div == 1)
			*p++ = digit + '0';
	*p++ = '\n';
	*p = '\0';
	WRITE(linestr, (unsigned) strlen(linestr));
	(void) abort();
}
