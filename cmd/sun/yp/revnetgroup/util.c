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
#ident	"$Header: util.c,v 1.1.1.2 90/05/09 19:30:23 wje Exp $"
#ifndef lint
static char sccsid[] = 	"@(#)util.c	1.1 88/03/07 4.0NFSSRC Copyr 1988 Sun Micro";
#endif

#include <stdio.h>
#include "util.h"




/*
 * This is just like fgets, but recognizes that "\\n" signals a continuation
 * of a line
 */
char *
getline(line,maxlen,fp)
	char *line;
	int maxlen;
	FILE *fp;
{
	register char *p;
	register char *start;


	start = line;

nextline:
	if (fgets(start,maxlen,fp) == NULL) {
		return(NULL);
	}	
	for (p = start; ; p++) {
		if (*p == '\n') {       
			if (*(p-1) == '\\') {
				start = p - 1;
				goto nextline;
			} else {
				return(line);	
			}
		}
	}
}	




	
void
fatal(message)
	char *message;
{
	(void) fprintf(stderr,"fatal error: %s\n",message);
	exit(1);
}
