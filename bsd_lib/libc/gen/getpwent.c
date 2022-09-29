/* --------------------------------------------------- */
/* | Copyright (c) 1986 MIPS Computer Systems, Inc.  | */
/* | All Rights Reserved.                            | */
/* --------------------------------------------------- */
/* $Header: getpwent.c,v 1.4.1.4.2.2 90/08/08 21:38:45 alexp Exp $ */

/*	Copyright (c) 1984 AT&T	*/
/*	  All Rights Reserved  	*/

/*	THIS IS UNPUBLISHED PROPRIETARY SOURCE CODE OF AT&T	*/
/*	The copyright notice above does not evidence any   	*/
/*	actual or intended publication of such source code.	*/

/*LINTLIBRARY*/
#include <sys/param.h>
#include <stdio.h>
#include <pwd.h>

#ifdef  SYSTYPE_POSIX
#define setpwent        _setpwent
#define getpwent        _getpwent
#define endpwent        _endpwent
#define fgetpwent       _fgetpwent
#include <sysv/netdb.h>
#else
#include <netdb.h>
#endif

extern void rewind();
extern long atol();
extern FILE *fopen();
extern int fclose();
extern char *fgets();

static char *PASSWD = "/etc/passwd";
static char EMPTY[] = "";
static FILE *pwf = NULL;
static char line[BUFSIZ+1];
static struct passwd passwd;

int _pw_stayopen;	/* not used, needed by some programs */

void
setpwent()
{
	int p;
        char *(*vis_func)() = VIS_FIRST_CALL;

        while ((vis_func = vis_nextserv(vis_func, VIS_SETPWENT))
                != VIS_DONE) {
                p = (int)((*vis_func)(VIS_SETPWENT, 0));
                if (p != -1)
                        return;
        }
        return;
}

void
endpwent()
{
	int p;
        char *(*vis_func)() = VIS_FIRST_CALL;

        while ((vis_func = vis_nextserv(vis_func, VIS_ENDPWENT))
                != VIS_DONE) {
                p = (int)((*vis_func)(VIS_ENDPWENT, 0));
                if (p != -1)
                        return;
        }
        return;
}

struct passwd *
getpwent()
{
	struct passwd *p;
        char *(*vis_func)() = VIS_FIRST_CALL;

        while ((vis_func = vis_nextserv(vis_func, VIS_GETPWENT))
                != VIS_DONE) {
                p = (struct passwd *)((*vis_func)(VIS_GETPWENT, 0));
                if ((p != (struct passwd *)-1) && (p != (struct passwd *)0))
                        return(p);
        }
        return((struct passwd *)0);

}

struct passwd *
getpwnam(name)
char	*name;
{
	struct passwd *p;
        char *(*vis_func)() = VIS_FIRST_CALL;

        while ((vis_func = vis_nextserv(vis_func, VIS_GETPWBYNAME))
                != VIS_DONE) {
                p = (struct passwd *)((*vis_func)(VIS_GETPWBYNAME, name));
                if ((p != (struct passwd *)-1) && (p != (struct passwd *)0))
                        return(p);
        }
        return((struct passwd *)0);
}

struct passwd *
getpwuid(uid)
register int uid;
{
	struct passwd *p;
        char *(*vis_func)() = VIS_FIRST_CALL;

        while ((vis_func = vis_nextserv(vis_func, VIS_GETPWBYUID))
                != VIS_DONE) {
                p = (struct passwd *)((*vis_func)(VIS_GETPWBYUID, uid));
                if ((p != (struct passwd *)-1) && (p != (struct passwd *)0))
                        return(p);
        }
        return((struct passwd *)0);
}
