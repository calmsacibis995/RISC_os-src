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
#ident	"$Header: getnetbyname.c,v 1.2.1.2 90/05/07 20:51:42 wje Exp $"

#include <netdb.h>

struct netent *
getnetbyname(name)
	register char *name;
{
        char *(*vis_func)() = VIS_FIRST_CALL;
	register struct netent *p;

        while ((vis_func = vis_nextserv(vis_func, VIS_GETNETBYNAME))
                != VIS_DONE) {
                p = (struct netent *)((*vis_func)(VIS_GETNETBYNAME, name));
		if ((p != (struct netent *)-1) && (p != (struct netent *)0))
                        return(p);
        }
        return((struct netent *)0);
}
