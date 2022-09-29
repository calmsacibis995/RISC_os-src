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
#ident	"$Header: warns.c,v 1.1.1.2 90/05/09 18:05:39 wje Exp $"

/*
 * Author: Peter J. Nicklin
 */

/*
 * warns() places an error message and a string argument on the
 * standard error output stream stderr.
 */
#include <stdio.h>
#include "null.h"

extern char *PGN;			/* program name */

warns(m, s)
	char *m;			/* warning message */
	char *s;			/* string argument */
{
	if (PGN != NULL && *PGN != '\0')
		fprintf(stderr, "%s: ", PGN);
	fprintf(stderr, m, s);
	fprintf(stderr, "\n");
}
