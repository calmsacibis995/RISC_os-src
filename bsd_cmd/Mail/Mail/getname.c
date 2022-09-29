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
#ident	"$Header: getname.c,v 1.1.1.2 90/05/07 17:57:06 wje Exp $"

/*
 * Copyright (c) 1980 Regents of the University of California.
 * All rights reserved.
 *
 * Redistribution and use in source and binary forms are permitted
 * provided that this notice is preserved and that due credit is given
 * to the University of California at Berkeley. The name of the University
 * may not be used to endorse or promote products derived from this
 * software without specific prior written permission. This software
 * is provided ``as is'' without express or implied warranty.
 */

#ifdef notdef
static char sccsid[] = "@(#)getname.c	5.4 (Berkeley) 2/18/88";
#endif /* notdef */

#include <pwd.h>

/*
 * Getname / getuserid for those with
 * hashed passwd data base).
 *
 */

#include "rcv.h"

/*
 * Search the passwd file for a uid.  Return name through ref parameter
 * if found, indicating success with 0 return.  Return -1 on error.
 */

getname(uid, namebuf)
	char namebuf[];
{
	struct passwd *pw;

	if ((pw = getpwuid(uid)) == NULL)
		return -1;
	strcpy(namebuf, pw->pw_name);
	return 0;
}

/*
 * Convert the passed name to a user id and return it.  Return -1
 * on error.
 */

getuserid(name)
	char name[];
{
	struct passwd *pw;

	if ((pw = getpwnam(name)) == NULL)
		return -1;
	return pw->pw_uid;
}
