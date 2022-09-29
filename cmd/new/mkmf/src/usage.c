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
#ident	"$Header: usage.c,v 1.1.1.2 90/05/09 18:04:56 wje Exp $"

/*
 * Author: Peter J. Nicklin
 */

/*
 * usage() places a usage error message on the standard error
 * output stream stderr.
 */
#include <stdio.h>
#include "null.h"

extern char *PGN;			/* program name */

usage(m)
	char *m;			/* usage error message */
{
	fprintf(stderr, "usage: ");
	if (PGN != NULL && *PGN != '\0')
		fprintf(stderr, "%s ", PGN);
	fprintf(stderr, "%s\n", m);
}
