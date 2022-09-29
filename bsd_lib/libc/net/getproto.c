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
#ident	"$Header: getproto.c,v 1.3.1.2 90/05/07 20:52:00 wje Exp $"

#include <netdb.h>

struct protoent *
getprotobynumber(proto)
	register int proto;
{
	register struct protoent *p;
        char *(*vis_func)() = VIS_FIRST_CALL;

        while ((vis_func = vis_nextserv(vis_func, VIS_GETPROTOBYNUM))
                != VIS_DONE) {
                p = (struct protoent *)((*vis_func)(VIS_GETPROTOBYNUM, proto));
		if ((p != (struct protoent *)-1) && (p != (struct protoent *)0))
                        return(p);
        }
        return((struct protoent *)0);
}
