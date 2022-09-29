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
#ident	"$Header: getcwp.c,v 1.1.1.2 90/05/09 18:01:13 wje Exp $"

/*
 * Author: Peter J. Nicklin
 */

/*
 * getcwp() returns the pathname of the current working project. If the
 * PROJECT environment variable is undefined or a null string, null is
 * returned.
 */
#include "null.h"

char *
getcwp()
{
	extern char *_PROJECT;		/* project root directory pathname */
	void getproject();		/* get PROJECT environment variable */

	if (_PROJECT == NULL)
		getproject();
	return(_PROJECT);
}
