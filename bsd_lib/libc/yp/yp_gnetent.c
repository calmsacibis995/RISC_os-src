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
#ident	"$Header: yp_gnetent.c,v 1.3.1.2 90/05/07 21:30:24 wje Exp $"
/*
 * Copyright (c) 1983 Regents of the University of California.
 * All rights reserved.  The Berkeley software License Agreement
 * specifies the terms and conditions for redistribution.
 */

/* 
 * Copyright (c) 1984 by Sun Microsystems, Inc.
 */

#if defined(LIBC_SCCS) && !defined(lint)
static char sccsid[] = "@(#)getnetent.c	1.3 88/03/31 4.0NFSSRC; from	5.3 (Berkeley) 5/19/86"; /* and from 1.21 88/02/08 SMI Copyr 1984 Sun Micro */
#endif LIBC_SCCS and not lint

#include <stdio.h>
#include <sys/types.h>
#include <sys/socket.h>
#include <netdb.h>
#include <rpcsvc/ypclnt.h>
#ifdef SYSTYPE_BSD43
#include <netinet/in.h>
#include <arpa/inet.h>
#else
#ifdef SYSTYPE_SYSV
#include <bsd/netinet/in.h>
#include <bsd/arpa/inet.h>
#endif
#endif

/*
 * Internet version.
 */
static struct netdata {
	FILE	*netf;
	char	*current;
	int	currentlen;
	int	stayopen;
#define	MAXALIASES	35
	char	*net_aliases[MAXALIASES];
	struct	netent net;
	char	line[BUFSIZ+1];
	char	*map;
	char	*domain;
	int	usingyellow;
	char	ntoabuf[16];
} *netdata, *_netdata();

static	struct netent *interpret();
struct	netent *getnetent();
char	*inet_ntoa();
char	*any();

static	char NETWORKS[] = "/etc/networks";
static	char *nettoa();

static struct netdata *
_netdata()
{
	register struct netdata *d = netdata;

	if (d == 0) {
		d = (struct netdata *)calloc(1, sizeof (struct netdata));
		netdata = d;
	}
	return (d);
}

struct netent *
yp_getnetbyaddr(req)
	Vis_nbyaddr_req *req;
{
	int anet = req->r_net, type = req->r_type;
	register struct netdata *d = _netdata();
	register struct netent *p;
	char *adrstr, *val = NULL;
	int vallen;

	if (d == 0)
		return (0);
	d->map = "networks.byaddr";
	yp_setnetent(0);
	adrstr = nettoa(anet);
	if (yp_match(d->domain, d->map,
	    adrstr, strlen(adrstr), &val, &vallen))
		p = NULL;
	else {
		p = interpret(val, vallen);
		free(val);
	}
	yp_endnetent(0);
	return (p);
}

struct netent *
yp_getnetbyname(name)
	register char *name;
{
	register struct netdata *d = _netdata();
	register struct netent *p;
	register char **cp;
	char *val = NULL;
	int vallen;

	if (d == 0)
		return (0);
	d->map = "networks.byname";
	yp_setnetent(0);
	if (yp_match(d->domain, d->map,
	    name, strlen(name), &val, &vallen))
		p = NULL;
	else {
		p = interpret(val, vallen);
		free(val);
	}
found:
	yp_endnetent();
	return (p);
}

yp_setnetent(f)
	int f;
{
	register struct netdata *d = _netdata();

	if (d == 0)
		return;
	if (d->netf == NULL)
		d->netf = fopen(NETWORKS, "r");
	else
		rewind(d->netf);
	if (d->current)
		free(d->current);
	d->current = NULL;
	d->stayopen |= f;
	if (d->map)
		yellowup();	/* recompute whether yellow pages are up */
}

yp_endnetent(a)
	int a;
{
	register struct netdata *d = _netdata();

	if (d == 0)
		return;
	if (d->current && !d->stayopen) {
		free(d->current);
		d->current = NULL;
	}
	if (d->netf && !d->stayopen) {
		fclose(d->netf);
		d->netf = NULL;
	}
}

char	BYADDR[] = "networks.byaddr";

struct netent *
yp_getnetent(a)
	int a;
{
	register struct netdata *d = _netdata();
	char *key = NULL, *val = NULL;
	int keylen, vallen;
	static char *line1 = NULL;
	struct netent *np;

	if (d == 0)
		return (0);
	if (line1 == NULL)
		line1 = (char *)calloc(1, BUFSIZ+1);
	d->map = BYADDR;
	yellowup();
	if (d->current == NULL) {
		if (yp_first(d->domain, d->map,
		    &key, &keylen, &val, &vallen))
			return (NULL);
	} else {
		if (yp_next(d->domain, d->map,
		    d->current, d->currentlen, &key, &keylen, &val, &vallen))
			return (NULL);
	}
	if (d->current)
		free(d->current);
	d->current = key;
	d->currentlen = keylen;
	np = interpret(val, vallen);
	free(val);
	return (np);
}

static struct netent *
interpret(val, len)
{
	register struct netdata *d = _netdata();
	char *p;
	register char *cp, **q;

	if (d == 0)
		return (0);
	strncpy(d->line, val, len);
	p = d->line;
	d->line[len] = '\n';
	if (*p == '#')
		return (getnetent());
	cp = any(p, "#\n");
	if (cp == NULL)
		return (getnetent());
	*cp = '\0';
	d->net.n_name = p;
	cp = any(p, " \t");
	if (cp == NULL)
		return (getnetent());
	*cp++ = '\0';
	while (*cp == ' ' || *cp == '\t')
		cp++;
	p = any(cp, " \t");
	if (p != NULL)
		*p++ = '\0';
	d->net.n_net = inet_network(cp);
	d->net.n_addrtype = AF_INET;
	q = d->net.n_aliases = d->net_aliases;
	if (p != NULL) 
		cp = p;
	while (cp && *cp) {
		if (*cp == ' ' || *cp == '\t') {
			cp++;
			continue;
		}
		if (q < &d->net_aliases[MAXALIASES - 1])
			*q++ = cp;
		cp = any(cp, " \t");
		if (cp != NULL)
			*cp++ = '\0';
	}
	*q = NULL;
	return (&d->net);
}

static char *
any(cp, match)
	register char *cp;
	char *match;
{
	register char *mp, c;

	while (c = *cp) {
		for (mp = match; *mp; mp++)
			if (*mp == c)
				return (cp);
		cp++;
	}
	return ((char *)0);
}

/* 
 * check to see if yellow pages are up, and store that fact in usingyellow.
 */
static
yellowup()
{
	register struct netdata *d = _netdata();

	if (d == 0)
		return;
        if (d->domain == NULL)
                d->usingyellow = 1;
}

static
char *
nettoa(anet)
	unsigned anet;
{
	register struct netdata *d = _netdata();
	char *p, *index(), *rindex();
	struct in_addr in;
	int addr;

	if (d == 0)
		return (NULL);
	in = inet_makeaddr(anet, INADDR_ANY);
	addr = in.s_addr;
	strcpy(d->ntoabuf, inet_ntoa(in));
	if (IN_CLASSA(htonl(addr))) {
		p = index(d->ntoabuf, '.');
		if (p == NULL)
			return (NULL);
		*p = 0;
	} else if (IN_CLASSB(htonl(addr))) {
		p = index(d->ntoabuf, '.');
		if (p == NULL)
			return (NULL);
		p = index(p+1, '.');
		if (p == NULL)
			return (NULL);
		*p = 0;
	} else if (IN_CLASSC(htonl(addr))) {
		p = rindex(d->ntoabuf, '.');
		if (p == NULL)
			return (NULL);
		*p = 0;
	}
	return (d->ntoabuf);
}
