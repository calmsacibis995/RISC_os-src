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
#ident	"$Header: execvp.c,v 1.9.1.2.1.1.1.2 90/10/05 09:58:24 beacker Exp $"

/*	Copyright (c) 1984 AT&T	*/
/*	  All Rights Reserved  	*/

/*	THIS IS UNPUBLISHED PROPRIETARY SOURCE CODE OF AT&T	*/
/*	The copyright notice above does not evidence any   	*/
/*	actual or intended publication of such source code.	*/

/*LINTLIBRARY*/
/*
 *	execlp(name, arg,...,0)	(like execl, but does path search)
 *	execvp(name, argv)	(like execv, but does path search)
 */
#ifdef  SYSTYPE_POSIX
#include <limits.h>
#else
#include <sys/limits.h>
#endif
#include <sys/errno.h>
#include <varargs.h>
#define	NULL	0

static char *execat(), shell[] = "/bin/sh";
extern char *getenv(), *strchr();
extern unsigned sleep();
extern int errno, execv();

/*VARARGS1*/
int
execlp(name, va_alist)
char	*name;
va_dcl
{
	return(execvp(name, &va_alist));
}

int
execvp(name, argv)
char	*name, **argv;
{
	char	*pathstr;
	char	fname[2*PATH_MAX];
	char	*newargs[256];
	int	i;
	register char	*cp;
	register unsigned etxtbsy=1;
	register int eacces=0;

	if((pathstr = getenv("PATH")) == NULL)
#ifdef SVR3
		pathstr = "/usr/net:/bin:/usr/bin";
#else
		pathstr = ":/bin:/usr/bin";
#endif
	cp = strchr(name, '/')? "": pathstr;

	do {
		cp = execat(cp, name, fname);
	retry:
		(void) execv(fname, argv);
		switch(errno) {
		case ENOEXEC:
			newargs[0] = "sh";
			newargs[1] = fname;
			for(i=1; newargs[i+1]=argv[i]; ++i) {
				if(i >= 254) {
					errno = E2BIG;
					return(-1);
				}
			}
			(void) execv(shell, newargs);
			return(-1);
		case ETXTBSY:
			if(++etxtbsy > 5)
				return(-1);
			(void) sleep(etxtbsy);
			goto retry;
		case EACCES:
			++eacces;
			break;
		case ENOMEM:
		case E2BIG:
#ifdef	SYSTYPE_POSIX
		case ENAMETOOLONG:
#endif
			return(-1);
		}
	} while(cp);
	if(eacces)
		errno = EACCES;
	return(-1);
}

static char *
execat(s1, s2, si)
register char *s1, *s2;
char	*si;
{
	register char	*s;

	s = si;
	while(*s1 && *s1 != ':')
		*s++ = *s1++;
	if(si != s)
		*s++ = '/';
	while(*s2)
		*s++ = *s2++;
	*s = '\0';
	return(*s1? ++s1: 0);
}
