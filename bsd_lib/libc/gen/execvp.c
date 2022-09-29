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
#ident	"$Header: execvp.c,v 1.2.1.2.1.1.1.2 90/10/05 09:58:02 beacker Exp $"

#if defined(LIBC_SCCS) && !defined(lint)
static char sccsid[] = "@(#)execvp.c	5.3 (Berkeley) 4/24/88";
#endif LIBC_SCCS and not lint

/*
 *	execlp(name, arg,...,0)	(like execl, but does path search)
 *	execvp(name, argv)	(like execv, but does path search)
 */
#include <sys/param.h>
#include <varargs.h>
#include <errno.h>
#ifndef NULL
#define	NULL	0
#endif

static	char shell[] =	"/bin/sh";
char	*getenv();
extern	errno;

execlp(name, va_alist)
char *name;
va_dcl
{
	va_list ap;
	va_start(ap);
	return(execvp(name, ap));
}

execvp(name, argv)
char *name, **argv;
{
	char *pathstr;
	register char *cp;
	char fname[2*MAXPATHLEN];
	char *newargs[256], *execat();
	int i;
	register unsigned etxtbsy = 1;
	register eacces = 0;

	if ((pathstr = getenv("PATH")) == NULL)
		pathstr = ":/bin:/usr/bin";
	cp = index(name, '/')? "": pathstr;

	do {
		cp = execat(cp, name, fname);
	retry:
		execv(fname, argv);
		switch(errno) {
		case ENOEXEC:
			newargs[0] = "sh";
			newargs[1] = fname;
			for (i=1; newargs[i+1]=argv[i]; i++) {
				if (i>=254) {
					errno = E2BIG;
					return(-1);
				}
			}
			execv(shell, newargs);
			return(-1);
		case ETXTBSY:
			if (++etxtbsy > 5)
				return(-1);
			sleep(etxtbsy);
			goto retry;
		case EACCES:
			eacces++;
			break;
		case ENOMEM:
		case E2BIG:
			return(-1);
		}
	} while (cp);
	if (eacces)
		errno = EACCES;
	return(-1);
}

static char *
execat(s1, s2, si)
register char *s1, *s2;
char *si;
{
	register char *s;

	s = si;
	while (*s1 && *s1 != ':')
		*s++ = *s1++;
	if (si != s)
		*s++ = '/';
	while (*s2)
		*s++ = *s2++;
	*s = '\0';
	return(*s1? ++s1: 0);
}
