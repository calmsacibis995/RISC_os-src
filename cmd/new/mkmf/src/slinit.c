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
#ident	"$Header: slinit.c,v 1.1.1.2 90/05/09 18:03:32 wje Exp $"

/*
 * Author: Peter J. Nicklin
 */

/*
 * slinit() returns a pointer to the head block of a new list, or null
 * pointer if out of memory.
 */
#include <stdio.h>
#include "null.h"
#include "slist.h"

extern char *PGN;			/* program name */

SLIST *
slinit()
{
	char *malloc();			/* memory allocator */
	SLIST *slist;			/* pointer to list head block */

	if ((slist = (SLIST *) malloc(sizeof(SLIST))) == NULL)
		{
		if (*PGN != '\0')
			fprintf(stderr, "%s: ", PGN);
		fprintf(stderr, "out of memory\n");
		return(NULL);
		}
	slist->nk = 0;
	slist->maxkey = 0;
	slist->head = slist->curblk = slist->tail = NULL;
	return(slist);
}
