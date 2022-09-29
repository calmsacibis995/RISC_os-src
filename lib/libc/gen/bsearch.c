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
#ident	"$Header: bsearch.c,v 1.6.2.2 90/05/10 01:28:42 wje Exp $"

/*	Copyright (c) 1984 AT&T	*/
/*	  All Rights Reserved  	*/

/*	THIS IS UNPUBLISHED PROPRIETARY SOURCE CODE OF AT&T	*/
/*	The copyright notice above does not evidence any   	*/
/*	actual or intended publication of such source code.	*/

/*LINTLIBRARY*/
/*
 * Binary search algorithm, generalized from Knuth (6.2.1) Algorithm B.
 *
 */

typedef char *POINTER;

POINTER
bsearch(key, base, nel, width, compar)
POINTER	key;			/* Key to be located */
POINTER	base;			/* Beginning of table */
unsigned nel;			/* Number of elements in the table */
unsigned width;			/* Width of an element (bytes) */
int	(*compar)();		/* Comparison function */
{
	int two_width = width + width;
	POINTER last = base + width * (nel - 1); /* Last element in table */

	while (last >= base) {

		register POINTER p = base + width * ((last - base)/two_width);
		register int res = (*compar)(key, p);

		if (res == 0)
			return (p);	/* Key found */
		if (res < 0)
			last = p - width;
		else
			base = p + width;
	}
	return ((POINTER) 0);		/* Key not found */
}
