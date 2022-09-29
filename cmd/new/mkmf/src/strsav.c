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
#ident	"$Header: strsav.c,v 1.1.1.2 90/05/09 18:04:27 wje Exp $"

/*
 * Author: Peter J. Nicklin
 */

/*
 * strsav() saves a string somewhere and returns a pointer to the somewhere.
 * Returns NULL on error.
 */
#include "null.h"

char *
strsav(s)
	char *s;
{
	char *sptr;			/* somewhere string pointer */
	char *malloc();			/* memory allocator */
	char *strcpy();			/* string copy */
	int strlen();			/* string length */

	if ((sptr = malloc((unsigned)(strlen(s)+1))) == NULL)
		return(NULL);
	return(strcpy(sptr, s));
}
