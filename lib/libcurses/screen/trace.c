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
#ident	"$Header: trace.c,v 1.2.1.2 90/05/10 02:25:13 wje Exp $"
/*	Copyright (c) 1984 AT&T	*/
/*	  All Rights Reserved  	*/

/*	THIS IS UNPUBLISHED PROPRIETARY SOURCE CODE OF AT&T	*/
/*	The copyright notice above does not evidence any   	*/
/*	actual or intended publication of such source code.	*/

#include "curses_inc.h"

traceon()
{
#ifdef DEBUG
    if (outf == NULL)
    {
	outf = fopen("trace", "a");
	if (outf == NULL)
	{
	    perror("trace");
	    exit(-1);
	}
	fprintf(outf, "trace turned on\n");
    }
#endif /* DEBUG */
    return (OK);
}

traceoff()
{
#ifdef DEBUG
    if (outf != NULL)
    {
	fprintf(outf, "trace turned off\n");
	fclose(outf);
	outf = NULL;
    }
#endif /* DEBUG */
    return (OK);
}

#ifdef DEBUG
#include <ctype.h>

char *
_asciify(str)
register char *str;
{
    static	char	string[1024];
    register	char	*p1 = string;
    register	char	*p2;
    register	char	c;

    while (c = *str++)
    {
	p2 = unctrl(c);
	while (*p1 = *p2++)
	    p1++;
    }
    return string;
}
#endif /* DEBUG */
