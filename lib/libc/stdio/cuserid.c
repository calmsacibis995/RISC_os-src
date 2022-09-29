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
 * |         950 DeGuigne Drive                                |
 * |         Sunnyvale, CA 94086                               |
 * |-----------------------------------------------------------|
 */
#ident	"$Header: cuserid.c,v 1.7.1.2.1.2 90/07/23 10:19:10 hawkes Exp $"

/*	Copyright (c) 1984 AT&T	*/
/*	  All Rights Reserved  	*/

/*	THIS IS UNPUBLISHED PROPRIETARY SOURCE CODE OF AT&T	*/
/*	The copyright notice above does not evidence any   	*/
/*	actual or intended publication of such source code.	*/

/*LINTLIBRARY*/
#include <stdio.h>
#include <pwd.h>

#ifdef	SYSTYPE_POSIX
#define	endpwent	_endpwent
#endif

extern char 		*strcpy();
extern char 		*getlogin();
extern int 		getuid();
extern int 		geteuid();
extern struct passwd 	*getpwuid();
static char 		res[L_cuserid];

char *
cuserid(s)
char	*s;
{
	register struct passwd  *pw;
	register char           *p;
	int 			euid;
	int 			uid;

	if (s == NULL)
		s = res;
#ifdef	SYSTYPE_POSIX
/* 
 * POSIX P1003.3 Part II, Draft 11.0, Section 4.2.4.2 
 *	   A call to cuserid() returns the name associated with the effective
 *         user ID of the process.
 */

	euid = geteuid();
	if ((pw = getpwuid(euid)) == NULL) {
		*s = '\0';
		endpwent(); 
		return (NULL);
	}
	else {
		endpwent(); 
		return (strcpy(s, pw->pw_name));
	}
#else
	p = getlogin();
        if (p != NULL)
                return (strcpy(s, p));
        pw = getpwuid(getuid());
        endpwent();
        if (pw != NULL)
		return (strcpy(s, pw->pw_name));
        *s = '\0';
        return (NULL);
#endif  SYSTYPE_POSIX
}
