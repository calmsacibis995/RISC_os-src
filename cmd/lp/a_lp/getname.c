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
#ident	"$Header: getname.c,v 1.4.2.2 90/05/09 16:26:44 wje Exp $"

/*	Copyright (c) 1984 AT&T	*/
/*	  All Rights Reserved  	*/

/*	THIS IS UNPUBLISHED PROPRIETARY SOURCE CODE OF AT&T	*/
/*	The copyright notice above does not evidence any   	*/
/*	actual or intended publication of such source code.	*/

/*
 *	getname(name)  --  get logname
 *
 *		getname tries to find the user's logname from:
 *			${LOGNAME}, if set and if it is telling the truth
 *			/etc/passwd, otherwise
 *
 *		The logname is returned as the value of the function.
 *
 *		Getname returns the user's user id converted to ASCII
 *		for unknown lognames.
 *
 */

#include	"lp.h"


char *
getname(name)
char *name;
{
	int uid;
	struct passwd *p, *getpwnam(), *getpwuid();
	static char logname[LOGMAX + 1];
	char *l, *getenv(), *strncpy();

	uid = getuid();
	setpwent();

	if((l = getenv("LOGNAME")) == NULL ||
	   (p = getpwnam(l)) == NULL || p -> pw_uid != uid)
		if((p = getpwuid(uid)) != NULL)
			l = p->pw_name;
		else
			l = NULL;

	if(l != NULL) {
		strncpy(logname, l, LOGMAX);
		logname[LOGMAX] = '\0';
	}
	else
		sprintf(logname, "%d", uid);
	endpwent();
	return(logname);
}
