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
#ident	"$Header: getpwinfo.c,v 1.4.2.2 90/05/10 00:35:43 wje Exp $"

/*	Copyright (c) 1984 AT&T	*/
/*	  All Rights Reserved  	*/

/*	THIS IS UNPUBLISHED PROPRIETARY SOURCE CODE OF AT&T	*/
/*	The copyright notice above does not evidence any   	*/
/*	actual or intended publication of such source code.	*/


#include "uucp.h"

#include <pwd.h>
extern struct passwd *getpwuid(), *getpwnam();
extern char	*getlogin();


/*
 * get passwd file info for logname or uid
 *	uid	-> uid #	
 *	name	-> address of buffer to return ascii user name
 *		This will be set to pw->pw_name.
 *
 * return:
 *	0	-> success
 *	FAIL	-> failure (logname and uid not found)
 */
guinfo(uid, name)
int uid;
char *name;
{
	register struct passwd *pwd;
	char	*login_name;

	/* look for this user as logged in utmp */
	if ((login_name = getlogin()) != NULL) {
		pwd = getpwnam(login_name);
		if (pwd != NULL && pwd->pw_uid == uid)
			goto uid_found;
	}

	/* no dice on utmp -- get first from passwd file */
	if ((pwd = getpwuid(uid)) == NULL) {
	    if ((pwd = getpwuid(UUCPUID)) == NULL)
		/* can not find uid in passwd file */
		return(FAIL);
	}

uid_found:
	(void) strcpy(name, pwd->pw_name);
	return(0);
}

/*
 * get passwd file info for name
 *	name	-> ascii user name
 *	uid	-> address of integer to return uid # in
 *	path	-> address of buffer to return working directory in
 * returns:
 *	0	-> success
 *	FAIL	-> failure
 */
gninfo(name, uid, path)
char *path, *name;
int *uid;
{
	register struct passwd *pwd;

	if ((pwd = getpwnam(name)) == NULL) {
		/* can not find name in passwd file */
		*path = '\0';
		return(FAIL);
	}

	(void) strcpy(path, pwd->pw_dir);
	*uid = pwd->pw_uid;
	return(0);
}


