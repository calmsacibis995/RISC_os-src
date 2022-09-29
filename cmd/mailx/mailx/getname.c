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
#ident	"$Header: getname.c,v 1.4.1.2.1.2 90/08/08 17:59:34 hawkes Exp $"

/*	Copyright (c) 1984 AT&T	*/
/*	  All Rights Reserved  	*/

/*	THIS IS UNPUBLISHED PROPRIETARY SOURCE CODE OF AT&T	*/
/*	The copyright notice above does not evidence any   	*/
/*	actual or intended publication of such source code.	*/

#


/*
 * mailx -- a modified version of a University of California at Berkeley
 *	mail program
 *
 * Getname / getuserid for those with no
 * hashed passwd data base).
 * Do not compile this module in if you DO have hashed
 * passwd's -- this is slower.
 *
 * Also provided here is a getpw routine which can share
 * the open file.  This is used for the Version 6 getenv
 * implementation.
 */


#include "rcv.h"

#ifdef VIS
/* Changes required for VIS to do its job */
#include <pwd.h>		/* password structure */
#else
static FILE *pwfile =	NULL;		/* Pw file held open */
static char *pwname = 	"/etc/passwd";	/* Name of passwd file */
#endif

/*
 * Search the passwd file for a uid.  Return name through ref parameter
 * if found, indicating success with 0 return.  Return -1 on error.
 * If -1 is passed as the user id, close the passwd file.
 */

getname(uid, namebuf)
	char namebuf[];
{
#ifdef VIS
	struct passwd *pwd, *getpwuid();

	if (uid == -1)
		return(0);
	if ((pwd = getpwuid(uid)) == NULL)
		return(-1);
	strcpy(namebuf, pwd->pw_name);
	return(0);
#else
	register char *cp, *cp2;
	char linebuf[BUFSIZ];

	if (uid == -1) {
		if (pwfile != NULL)
			fclose(pwfile);
		pwfile = NULL;
		return(0);
	}
	if (pwfile == NULL && (pwfile = fopen(pwname, "r")) == NULL)
		return(-1);
	rewind(pwfile);
	while (fgets(linebuf, BUFSIZ, pwfile) != NULL)
		if (pweval(linebuf) == uid) {
			for (cp = linebuf, cp2 = namebuf; *cp != ':';
			    *cp2++ = *cp++)
				;
			*cp2 = '\0';
			return(0);
		}
	return(-1);
#endif /* VIS */
}

#ifndef VIS	/* Not called, so won't port */
/*
 * Read the users password file line into the passed line
 * buffer.
 */

getpw(uid, linebuf)
	char linebuf[];
{
	register char *cp, *cp2;

	if (uid == -1) {
		if (pwfile != NULL)
			fclose(pwfile);
		pwfile = NULL;
		return(0);
	}
	if (pwfile == NULL && (pwfile = fopen(pwname, "r")) == NULL)
		return(-1);
	rewind(pwfile);
	while (fgets(linebuf, BUFSIZ, pwfile) != NULL)
		if (pweval(linebuf) == uid) {
			if (linebuf[0] != '\0')
				linebuf[strlen(linebuf)-1] = '\0';
			return(0);
		}
	return(-1);
}
#endif /* VIS */

/*
 * Convert the passed name to a user id and return it.  Return -1
 * on error.  Iff the name passed is -1 (yech) close the pwfile.
 */

getuserid(name)
	char name[];
{
#ifdef VIS
	struct passwd *pwd, *getpwnam();

	if (name == (char *) -1)
		return(0);
	if ((pwd = getpwnam(name)) == NULL)
		return(-1);
	return(pwd->pw_uid);
#else
	register char *cp, *cp2;
	char linebuf[BUFSIZ];

	if (name == (char *) -1) {
		if (pwfile != NULL)
			fclose(pwfile);
		pwfile = NULL;
		return(0);
	}
	if (pwfile == NULL && (pwfile = fopen(pwname, "r")) == NULL)
		return(-1);
	rewind(pwfile);
	while (fgets(linebuf, BUFSIZ, pwfile) != NULL) {
		for (cp = name, cp2 = linebuf; *cp++ == *cp2++;)
			;
		if (*--cp == '\0' && *--cp2 == ':')
			return(pweval(linebuf));
	}
	return(-1);
#endif /* VIS */
}

/*
 * Evaluate the user id of the passed passwd line and return it.
 */

static
pweval(line)
	char line[];
{
	register char *cp;
	register int i;
	register int uid;

	for (cp = line, i = 0; i < 2; i += (*cp++ == ':'))
		;
	uid = atoi(cp);

#ifdef UIDGID
	while (*cp && *cp != ':')
		cp++;
	cp++;
	uid |= atoi(cp) << 8;
#endif

	return(uid);
}
