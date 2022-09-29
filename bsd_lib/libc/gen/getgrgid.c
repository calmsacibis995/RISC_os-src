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
#ident	"$Header: getgrgid.c,v 1.3.1.4.1.2 90/08/08 21:38:06 alexp Exp $"

#if defined(LIBC_SCCS) && !defined(lint)
static char sccsid[] = "@(#)getgrgid.c	5.2 (Berkeley) 3/9/86";
#endif LIBC_SCCS and not lint

#include <grp.h>

#ifdef  SYSTYPE_POSIX
#define getgrent _getgrent
#define setgrent _setgrent
#define endgrent _endgrent
#include <sysv/netdb.h>
#else
#include <netdb.h>
#endif

struct group *
getgrgid(gid)
register gid;
{
	register struct group *p;
        char *(*vis_func)() = VIS_FIRST_CALL;

        while ((vis_func = vis_nextserv(vis_func, VIS_GETGRBYGID))
                != VIS_DONE) {
                p = (struct group *)((*vis_func)(VIS_GETGRBYGID, gid));
                if ((p != (struct group *)-1) && (p != (struct group *)0))
                        return(p);
        }
        return((struct group *)0);
}
