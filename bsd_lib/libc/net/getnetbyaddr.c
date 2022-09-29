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
#ident	"$Header: getnetbyaddr.c,v 1.3.1.2 90/05/07 20:51:36 wje Exp $"

/*
 * Copyright (c) 1983 Regents of the University of California.
 * All rights reserved.  The Berkeley software License Agreement
 * specifies the terms and conditions for redistribution.
 */

#include <netdb.h>

struct netent *
getnetbyaddr(net, type)
	register int net, type;
{
        char *(*vis_func)() = VIS_FIRST_CALL;
	Vis_nbyaddr_req req;
	register struct netent *p;

	req.r_net = net;
	req.r_type = type;

        while ((vis_func = vis_nextserv(vis_func, VIS_GETNETBYADDR))
                != VIS_DONE) {
                p = (struct netent *)((*vis_func)(VIS_GETNETBYADDR, &req));
		if ((p != (struct netent *)-1) && (p != (struct netent *)0))
                        return(p);
        }
        return((struct netent *)0);
}
