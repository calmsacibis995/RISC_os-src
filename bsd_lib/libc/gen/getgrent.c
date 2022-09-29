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
#ident	"$Header: getgrent.c,v 1.4.1.5 90/05/07 20:36:43 wje Exp $"

#if defined(LIBC_SCCS) && !defined(lint)
static char sccsid[] = "@(#)getgrent.c	5.3 (Berkeley) 11/5/87";
#endif LIBC_SCCS and not lint

#include <stdio.h>
#include <grp.h>

#ifdef  SYSTYPE_POSIX
#define fgetgrent       _fgetgrent
#define getgrent        _getgrent
#define setgrent        _setgrent
#define endgrent        _endgrent
#include <sysv/netdb.h>
#else
#include <netdb.h>
#endif

setgrent()
{
	int p;
        char *(*vis_func)() = VIS_FIRST_CALL;

        while ((vis_func = vis_nextserv(vis_func, VIS_SETGRENT))
                != VIS_DONE) {
                p = (int)((*vis_func)(VIS_SETGRENT, 0));
                if (p != -1)
                        return(0);
        }
        return(0);
}

endgrent()
{
	int p;
        char *(*vis_func)() = VIS_FIRST_CALL;

        while ((vis_func = vis_nextserv(vis_func, VIS_ENDGRENT))
                != VIS_DONE) {
                p = (int)((*vis_func)(VIS_ENDGRENT, 0));
                if (p != -1)
                        return(0);
        }
        return(0);
}

struct group *
getgrent()
{
	struct group * p;
        char *(*vis_func)() = VIS_FIRST_CALL;

        while ((vis_func = vis_nextserv(vis_func, VIS_GETGRENT))
                != VIS_DONE) {
                p = (struct group *)((*vis_func)(VIS_GETGRENT, 0));
                if ((p != (struct group *)-1) && (p != (struct group *)0))
                        return(p);
        }
        return((struct group *)0);
}

struct group *
fgetgrent(a)
FILE 	*a;
{
	struct group * p;
        char *(*vis_func)() = VIS_FIRST_CALL;

        while ((vis_func = vis_nextserv(vis_func, VIS_FGETGRENT))
                != VIS_DONE) {
                p = (struct group *)((*vis_func)(VIS_FGETGRENT, a));
                if ((p != (struct group *)-1) && (p != (struct group *)0))
                        return(p);
        }
        return((struct group *)0);
}
