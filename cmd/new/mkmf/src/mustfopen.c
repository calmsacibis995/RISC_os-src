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
#ident	"$Header: mustfopen.c,v 1.1.1.2 90/05/09 18:01:59 wje Exp $"

/*
 * Author: Peter J. Nicklin
 */

/*
 * mustfopen() opens a file in the manner of fopen(3). However, if the file
 * cannot be accessed, exit(1) is called.
 */
#include <stdio.h>

extern char *PGN;		/* program name */

FILE *
mustfopen(filename,mode)
	char *filename;
	char *mode;
{
	FILE *stream;			/* file stream */

	if ((stream = fopen(filename,mode)) == NULL)
		{
		if (*PGN != '\0')
			fprintf(stderr, "%s: ", PGN);
		fprintf(stderr, "can't open %s\n",filename);
		exit(1);
		}
	return(stream);
}
