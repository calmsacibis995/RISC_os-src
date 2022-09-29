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
#ident	"$Header: getproject.c,v 1.1.1.2 90/05/09 18:01:18 wje Exp $"

/*
 * Author: Peter J. Nicklin
 */

/*
 * getproject() saves the contents of the PROJECT environment variable.
 * If the PROJECT variable is undefined or a null string, null is returned.
 */
#include "null.h"

char *_PROJECT = NULL;			/* project root directory pathname */

void
getproject()
{
	register char *pv;		/* ptr to start of PROJECT variable */
	char *getenv();			/* get environment variable */
	char *strsav();			/* save a string somewhere */

	if ((pv = getenv("PROJECT")) != NULL && *pv != '\0')
		_PROJECT = strsav(pv);
}
