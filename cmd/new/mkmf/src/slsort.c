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
#ident	"$Header: slsort.c,v 1.1.1.2 90/05/09 18:04:09 wje Exp $"

/*
 * Author: Peter J. Nicklin
 */

/*
 * slsort() sorts list slist according to comparison function compar().
 * compar() is to be called with two arguments and must return an integer
 * greater than, equal to, or less than 0, depending on the lexicographic
 * relationship between the two arguments. Returns integer YES if
 * successful, otherwise NO if out of memory.
 */
#include <stdio.h>
#include "null.h"
#include "slist.h"
#include "yesno.h"

extern char *PGN;			/* program name */

static int (*sscmp)();			/* string compare function */

slsort(compar, slist)
	int (*compar)();		/* compare two strings */
	SLIST *slist;			/* pointer to list head block */
{
	char **kp;			/* pointer to key pointer array */
	char *malloc();			/* memory allocator */
	char **skp;			/* ptr to start of key ptr array */
	int comparb();			/* compare 2 list blocks */
	SLBLK *curblk;			/* current list block */

	if (slist->nk <= 0)
		return(YES);
	else if ((skp = (char **) malloc((unsigned)slist->nk*sizeof(char *))) == NULL)
		{
		if (*PGN != '\0')
			fprintf(stderr, "%s: ", PGN);
		fprintf(stderr, "out of memory\n");
		return(NO);
		}
	for (kp = skp, curblk = slist->head; curblk != NULL; kp++, curblk = curblk->next)
		*kp = curblk->key;

	sscmp = compar;
	qsort((char *) skp, slist->nk, sizeof(char *), comparb);

	for (kp = skp, curblk = slist->head; curblk != NULL; kp++, curblk = curblk->next)
		curblk->key = *kp;
	
	free((char *) skp);
	return(YES);
}



/*
 * comparb() compares key strings in 2 list blocks. Returns whatever
 * sscmp() returns. sscmp() is a string compare function.
 */
static int
comparb(s1, s2)
	char **s1;			/* string pointer */
	char **s2;			/* string pointer */
{
	return(sscmp(*s1, *s2));
}
