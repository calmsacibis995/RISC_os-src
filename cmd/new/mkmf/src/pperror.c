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
#ident	"$Header: pperror.c,v 1.1.1.2 90/05/09 18:03:04 wje Exp $"

/*
 * Author: Peter J. Nicklin
 */

/*
 * pperror() writes a system error message to standard error output,
 * preceded by the name of the program and message.
 */
#include <stdio.h>

pperror(message)
	char *message;			/* error message */
{
	extern char *PGN;		/* program name */

	fprintf(stderr, "%s: ", PGN);
	perror(message);
}
