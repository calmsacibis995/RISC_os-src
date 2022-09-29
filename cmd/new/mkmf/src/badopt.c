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
#ident	"$Header: badopt.c,v 1.1.1.2 90/05/09 17:59:52 wje Exp $"

/*
 * Author: Peter J. Nicklin
 */

/*
 * badopt() places a "bad option" error message on the standard error
 * output stream stderr.
 */
#include <stdio.h>
#include "null.h"

extern char *PGN;			/* program name */

badopt(sign, c)
	char c;				/* offending option */
	char sign;			/* '+' or '-' sign preceding option */
{
	if (PGN != NULL && *PGN != '\0')
		fprintf(stderr, "%s: ", PGN);
	fprintf(stderr, "bad option %c%c\n", sign, c);
}
