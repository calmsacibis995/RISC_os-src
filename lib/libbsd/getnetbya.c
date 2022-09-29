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
#ident	"$Header: getnetbya.c,v 1.5.2.2 90/05/10 01:15:23 wje Exp $"

/*
 * Copyright (c) 1983 Regents of the University of California.
 * All rights reserved.  The Berkeley software License Agreement
 * specifies the terms and conditions for redistribution.
 */

#include <bsd/netdb.h>

extern int _net_stayopen;

struct netent *
getnetbyaddr(net, type)
	register int net, type;
{
	register struct netent *p;

	setnetent(_net_stayopen);
	while (p = getnetent())
		if (p->n_addrtype == type && p->n_net == net)
			break;
	if (!_net_stayopen)
		endnetent();
	return (p);
}
