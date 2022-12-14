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
#ident	"$Header: ialloc.c,v 1.3.2.2 90/05/10 00:58:37 wje Exp $"

/*LINTLIBRARY*/

#include "stdio.h"

#ifndef alloc_t
#define alloc_t	unsigned
#endif /* !alloc_t */

#ifdef MAL
#define NULLMAL(x)	((x) == NULL || (x) == MAL)
#else /* !MAL */
#define NULLMAL(x)	((x) == NULL)
#endif /* !MAL */

extern char *	calloc();
extern char *	malloc();
extern char *	realloc();
extern char *	strcpy();

char *
imalloc(n)
{
#ifdef MAL
	register char *	result;

	if (n == 0)
		n = 1;
	result = malloc((alloc_t) n);
	return (result == MAL) ? NULL : result;
#else /* !MAL */
	if (n == 0)
		n = 1;
	return malloc((alloc_t) n);
#endif /* !MAL */
}

char *
icalloc(nelem, elsize)
{
	if (nelem == 0 || elsize == 0)
		nelem = elsize = 1;
	return calloc((alloc_t) nelem, (alloc_t) elsize);
}

char *
irealloc(pointer, size)
char *	pointer;
{
	if (NULLMAL(pointer))
		return imalloc(size);
	if (size == 0)
		size = 1;
	return realloc(pointer, (alloc_t) size);
}

char *
icatalloc(old, new)
char *	old;
char *	new;
{
	register char *	result;
	register	oldsize, newsize;

	oldsize = NULLMAL(old) ? 0 : strlen(old);
	newsize = NULLMAL(new) ? 0 : strlen(new);
	if ((result = irealloc(old, oldsize + newsize + 1)) != NULL)
		if (!NULLMAL(new))
			(void) strcpy(result + oldsize, new);
	return result;
}

char *
icpyalloc(string)
char *	string;
{
	return icatalloc((char *) NULL, string);
}

ifree(p)
char *	p;
{
	if (!NULLMAL(p))
		free(p);
}
