/*
 * |-----------------------------------------------------------|
 * | Copyright (c) 1989       MIPS Computer Systems, Inc.      |
 * | All Rights Reserved                                       |
 * |-----------------------------------------------------------|
 * | Use, duplication, or disclosure by the Government is      |
 * | subject to restrictions as set forth in                   |
 * | subparagraph (c)(1)(ii) of the Rights in Technical        |
 * | Data and Computer Software Clause of DFARS 52.227-7013.   |
 * |         MIPS Computer Systems, Inc.                       |
 * |         928 Arques Avenue                                 |
 * |         Sunnyvale, CA 94086                               |
 * |-----------------------------------------------------------|
 */
#ident "$Header: ht_getpwent.c,v 1.3.1.10.2.2.1.2 90/11/05 11:03:59 beacker Exp $"

/*	Copyright (c) 1984 AT&T	*/
/*	  All Rights Reserved  	*/

/*	THIS IS UNPUBLISHED PROPRIETARY SOURCE CODE OF AT&T	*/
/*	The copyright notice above does not evidence any   	*/
/*	actual or intended publication of such source code.	*/

/*LINTLIBRARY*/
#include <sys/param.h>
#include <ctype.h>
#include <stdio.h>
#include <pwd.h>
#include <netdb.h>

#ifdef   SYSTYPE_POSIX
#include <sysv/unistd.h>
#else    !SYSTYPE_POSIX
#ifdef   SYSTYPE_SYSV 
#include <unistd.h>
#else    !SYSTYPE_SYSV
#ifdef   LIBBSD
#include <unistd.h>
#else    !SYSTYPE_SYSV && !LIBBSD  (must be bsd43)
#include <sys/fcntl.h>
#endif
#endif
#endif

#ifndef MAXUID
#define MAXUID 65536
#endif MAXUID
#ifndef MAXGID
#define MAXGID 65536
#endif MAXGID

extern void rewind();
extern long atol();
extern FILE *fopen();
extern int fclose();
extern char *fgets();

#ifndef  SYSTYPE_SYSV 
static char *SHADOW = "/etc/shadow";
static FILE *spwf = NULL;
static char spline[BUFSIZ+1];
#else   
#ifdef   LIBBSD
static char *SHADOW = "/etc/shadow";
static FILE *spwf = NULL;
static char spline[BUFSIZ+1];
#endif   LIBBSD
#endif   SYSTYPE_SYSV

static char *PASSWD = "/etc/passwd"; 
static char EMPTY[] = "";
static FILE *pwf = NULL;
static char line[BUFSIZ+1];
static struct passwd passwd;

int _pw_stayopen;	/* not used, needed by some programs */

void
setpwfile(name)
	char *name;
{
	PASSWD = name;
}

void
ht_setpwent(a)
	int a;
{
	if(pwf == NULL)
		pwf = fopen(PASSWD, "r");
	else
		rewind(pwf);
}

void
ht_endpwent(a)
	int a;
{
	if(pwf != NULL) {
		(void) fclose(pwf);
		pwf = NULL;
	}
#ifndef SYSTYPE_SYSV
	if (spwf != NULL) {
		(void) fclose(spwf);
		spwf = NULL;
	}
#else /* in SYSTYPE_SYSV */
#ifdef LIBBSD
	if (spwf != NULL) {
		(void) fclose(spwf);
		spwf = NULL;
	}
#endif
#endif
}

static char *
pwskip(p)
register char *p;
{
	while(*p && *p != ':' && *p != '\n')
		++p;
	if(*p == '\n')
		*p = '\0';
	else if(*p)
		*p++ = '\0';
	return(p);
}

#ifndef SYSTYPE_SYSV
/*
 * If the /etc/shadow password file exists and is readable, search on 
 * the /etc/passwd name and get your passwd info here.
 */

char *
_shadow(name)
char *name;
{
	register char *p;
	char	      *trash;
	char 	      *sppw;

	if(spwf == NULL) {
		if((spwf = fopen(SHADOW, "r")) == NULL) 
			return(NULL);
	}
	else
		fseek(spwf, 0, 0);		/* rewind the file */
	p = fgets(spline, BUFSIZ, spwf);
	if (p == NULL) {
		return (NULL);
	}
	trash = p;
	p = pwskip(p);
	while (strcmp(trash, name) != 0) {
		p = fgets(spline, BUFSIZ, spwf);
		trash = p;
		if (p != NULL)
			p = pwskip(p);
		else
			return (NULL);
	}
	if (strcmp(trash, name) != 0) 
		return (NULL);
	if (p != NULL) {
		sppw = p;
		p = pwskip(p);
	}
	else
		return (NULL);
	return (sppw);
}

#else  SYSTYPE_SYSV
#ifdef LIBBSD
/*
 * If the /etc/shadow password file exists and is readable, search on 
 * the /etc/passwd name and get your passwd info here.
 */

char *
_shadow(name)
char *name;
{
	register char *p;
	char	      *trash;
	char 	      *sppw;

	if(spwf == NULL) {
		if((spwf = fopen(SHADOW, "r")) == NULL) 
			return(NULL);
	}
	else
		fseek(spwf, 0, 0);		/* rewind the file */
	p = fgets(spline, BUFSIZ, spwf);
	if (p == NULL) {
		return (NULL);
	}
	trash = p;
	p = pwskip(p);
	while (strcmp(trash, name) != 0) {
		p = fgets(spline, BUFSIZ, spwf);
		trash = p;
		if (p != NULL)
			p = pwskip(p);
		else
			return (NULL);
	}
	if (strcmp(trash, name) != 0) 
		return (NULL);
	if (p != NULL) {
		sppw = p;
		p = pwskip(p);
	}
	else
		return (NULL);
	return (sppw);
}
#endif LIBBSD
#endif SYSTYPE_SYSV

struct passwd *
ht_getpwent(a)
	int a;
{

	extern struct passwd *fgetpwent();

	if(pwf == NULL) {
		if((pwf = fopen(PASSWD, "r")) == NULL)
			return(NULL);
	}
	return (fgetpwent(pwf));
}

#ifdef SYSTYPE_POSIX
static 
#endif
struct passwd *
fgetpwent(f)
FILE *f;
{
	register char *p;
	char *endp;
	long	x, strtol();
	char *memchr();

	if (f == (FILE *)NULL)
		return(NULL);
	p = fgets(line, BUFSIZ, f);
	if(p == NULL)
		return(NULL);
	passwd.pw_name = p;
	if ((*p == '+') || (*p == '-'))
	/* + or - means a yp entry, punt and let yp_xx do it*/
		return(NULL);
	p = pwskip(p);
#ifndef SYSTYPE_SYSV
	if (access(SHADOW, R_OK) == 0) {        /* shadow password       */
                passwd.pw_passwd = _shadow(passwd.pw_name);
                if (passwd.pw_passwd == NULL)   /* no shadow passwd, use */
                        passwd.pw_passwd = p;   /* /etc/passwd's passwd  */
        }
        else
#else   SYSTYPE_SYSV
#ifdef  LIBBSD
	if (access(SHADOW, R_OK) == 0) {        /* shadow password       */
                passwd.pw_passwd = _shadow(passwd.pw_name);
                if (passwd.pw_passwd == NULL)   /* no shadow passwd, use */
                        passwd.pw_passwd = p;   /* /etc/passwd's passwd  */
        }
        else
#endif LIBBSD
#endif SYSTYPE_SYSV
		passwd.pw_passwd = p;
	p = pwskip(p);
	if (p == NULL || *p == ':')
		/* check for non-null uid */
		return (NULL);
	x = strtol(p, &endp, 10);	
	if (endp != memchr(p, ':', strlen(p)))
		/* check for numeric value */
		return (NULL);
	p = pwskip(p);
	passwd.pw_uid = (x < 0 || x > MAXUID)? (MAXUID+1): x;
	if (p == NULL || *p == ':')
		/* check for non-null uid */
		return (NULL);
	x = strtol(p, &endp, 10);	
	if (endp != memchr(p, ':', strlen(p)))
		/* check for numeric value */
		return (NULL);
	p = pwskip(p);
	passwd.pw_gid = (x < 0 || x > MAXGID)? (MAXGID+1): x;
	passwd.pw_comment = p;
	passwd.pw_gecos = p;
	p = pwskip(p);
	passwd.pw_dir = p;
	p = pwskip(p);
	passwd.pw_shell = p;
	(void) pwskip(p);

	p = passwd.pw_passwd;
	while(*p && *p != ',')
		p++;
	if(*p)
		*p++ = '\0';
#ifdef SYSTYPE_BSD43
	passwd.pw_quota = 0;
#else SYSTYPE_BSD43
	passwd.pw_age = p;
#endif SYSTYPE_BSD43
	return(&passwd);
}

struct passwd *
ht_getpwnam(name)
char	*name;
{
	register struct passwd *p;

	ht_setpwent();
	while ((p = ht_getpwent()) && strcmp(name, p->pw_name))
		;
	ht_endpwent();
	return (p);
}

struct passwd *
ht_getpwuid(uid)
register int uid;
{
	register struct passwd *p;

	ht_setpwent();
	while((p = ht_getpwent()) && p->pw_uid != uid)
		;
	ht_endpwent();
	return(p);
}

int
ht_getpw(req)
	Vis_getpw_req *req;
{
	int  uid  = req->r_uid;
	char *buf = req->r_buffer;
	static char etc_pass[] = "/etc/passwd";
	register n, c;
	register char *bp;

	if(pwf == 0)
		pwf = fopen(etc_pass, "r");
	if(pwf == NULL)
		return(1);
	rewind(pwf);

	while(1) {
		bp = buf;
		while((c=getc(pwf)) != '\n') {
			if(c == EOF)
				return(1);
			*bp++ = c;
		}
		*bp = '\0';
		bp = buf;
		n = 3;
		while(--n)
			while((c = *bp++) != ':')
				if(c == '\n')
					return(1);
		while((c = *bp++) != ':')
			if(isdigit(c))
				n = n*10+c-'0';
			else
				continue;
		if(n == uid)
			return(0);
	}
}
