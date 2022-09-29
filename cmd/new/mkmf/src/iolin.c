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
#ident	"$Header: iolin.c,v 1.1.1.2 90/05/09 18:01:36 wje Exp $"

/*
 * Author: Peter J. Nicklin
 */
#include <stdio.h>
#include "yesno.h"

char IOBUF[BUFSIZ];			/* I/O line buffer */
short CONTINUE;				/* does the line continue? */

/*
 * getlin() stores a line from input stream in IOBUF. The string is terminated
 * by a newline character which is replaced by a null character. getlin()
 * returns IOBUF, or null pointer upon end of file.
 */
char *
getlin(stream)
	register FILE *stream;		/* input stream */
{
	register int c;			/* current character */
	register char *iop;		/* IOBUF pointer */

	iop = IOBUF;
	while ((c = getc(stream)) != '\n' && c != EOF)
		*iop++ = c;
	if (c == EOF && iop == IOBUF)
		return(NULL);
	if (iop != IOBUF && iop[-1] == '\\')
		{
		iop[-1] = '\0';
		CONTINUE = YES;
		}
	else	{
		iop[0] = '\0';
		CONTINUE = NO;
		}
	return(IOBUF);
}



/*
 * purgcontinue() eats up continuation lines from an input stream.
 */
void
purgcontinue(stream)
	register FILE *stream;		/* input stream */
{
	register int c;			/* current character */
	register int lastc;		/* previous character */

	if (CONTINUE == YES)
		{
		for (;;)
			{
			while ((c = getc(stream)) != '\n' && c != EOF)
				lastc = c;
			if (c == EOF || (c == '\n' && lastc != '\\'))
				break;
			}
		CONTINUE = NO;
		}
}



/*
 * putlin() writes IOBUF to stream and appends a newline character. If
 * IOBUF holds a CONTINUE line, a `\' precedes the newline.
 */
void
putlin(stream)
	register FILE *stream;		/* output stream */
{
	register int c;			/* current character */
	register char *iop;		/* IOBUF pointer */

	iop = IOBUF;
	while (c = *iop++)
		putc(c, stream);
	if (CONTINUE == YES)
		putc('\\', stream);
	putc('\n', stream);
}
