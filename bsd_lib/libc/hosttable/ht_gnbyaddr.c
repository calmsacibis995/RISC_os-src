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
#ident	"$Header: ht_gnbyaddr.c,v 1.2.1.2 90/05/07 20:46:50 wje Exp $"

/*
 * Copyright (c) 1983 Regents of the University of California.
 * All rights reserved.  The Berkeley software License Agreement
 * specifies the terms and conditions for redistribution.
 */

#include <netdb.h>

extern int _net_stayopen;

struct netent *
ht_getnetbyaddr(req)
	Vis_nbyaddr_req *req;
{
	register struct netent *p;
	register int net = req->r_net, type = req->r_type;
	struct netent *ht_getnetent();

	ht_setnetent(_net_stayopen);
	while (p = ht_getnetent())
		if (p->n_addrtype == type && p->n_net == net)
			break;
	if (!_net_stayopen)
		ht_endnetent();
	return (p);
}
