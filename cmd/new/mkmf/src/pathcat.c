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
#ident	"$Header: pathcat.c,v 1.1.1.2 90/05/09 18:02:52 wje Exp $"

/*
 * Author: Peter J. Nicklin
 */

/*
 * pathcat() concatenates path components p1 and p2 into character buffer
 * p1_p2. Returns p1_p2.
 */
#include <stdio.h>
#include "path.h"

extern char *PGN;			/* program name */

char *
pathcat(p1_p2, p1, p2)
	register char *p1;
	register char *p2;
	register char *p1_p2;
{
	register int plen;		/* maximum pathname length */
	char *sp1_p2;			/* start of p1_p2 */

	sp1_p2 = p1_p2;
	for (plen = PATHSIZE; plen > 0; plen--, p1_p2++, p1++)
		if ((*p1_p2 = *p1) == '\0')
			break;
	if (*p2 != '\0' && plen > 0)
		{
		if (p1_p2 != sp1_p2 && p1_p2[-1] != _PSC)
			{
			*p1_p2++ = _PSC;
			plen--;
			}
		for (; plen > 0; plen--, p1_p2++, p2++)
			if ((*p1_p2 = *p2) == '\0')
				break;
		}
	if (plen == 0)
		{
		*--p1_p2 = '\0';
		if (*PGN != '\0')
			fprintf(stderr, "%s: ", PGN);
		fprintf(stderr, "pathname too long\n");
		}
	return(sp1_p2);
}
