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
#ident	"$Header: pathhead.c,v 1.1.1.2 90/05/09 18:02:58 wje Exp $"

/*
 * Author: Peter J. Nicklin
 */

/*
 * pathhead() removes tail of pathname and returns pathname. The tail is
 * defined as that part of the pathname after the last separator.
 */
#include "null.h"
#include "path.h"

char *
pathhead(pathname)
	register char *pathname;
{
	register char *ls;		/* last separator character */
	register char *p;		/* pathname pointer */

	ls = NULL;
	for (p = pathname; *p != '\0'; p++)
		if (*p == _PSC)
			ls = p;
	if (ls != NULL) *ls = '\0';
	return(pathname);
}
