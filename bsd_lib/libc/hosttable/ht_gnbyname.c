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
#ident	"$Header: ht_gnbyname.c,v 1.2.1.2 90/05/07 20:47:19 wje Exp $"

/*
 * Copyright (c) 1983 Regents of the University of California.
 * All rights reserved.  The Berkeley software License Agreement
 * specifies the terms and conditions for redistribution.
 */

#include <netdb.h>

extern int _net_stayopen;
struct netent *ht_getnetent();

struct netent *
ht_getnetbyname(name)
	register char *name;
{
	register struct netent *p;
	register char **cp;

	ht_setnetent(_net_stayopen);
	while (p = ht_getnetent(0)) {
		if (strcmp(p->n_name, name) == 0)
			break;
		for (cp = p->n_aliases; *cp != 0; cp++)
			if (strcmp(*cp, name) == 0)
				goto found;
	}
found:
	if (!_net_stayopen)
		ht_endnetent(0);
	return (p);
}
