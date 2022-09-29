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
#ident	"$Header: yp_getservent.c,v 1.3.1.2 90/05/07 21:30:19 wje Exp $"
/*
 * Copyright (c) 1983 Regents of the University of California.
 * All rights reserved.  The Berkeley software License Agreement
 * specifies the terms and conditions for redistribution.
/* 
 * Copyright (c) 1984 by Sun Microsystems, Inc.
 */

#if defined(LIBC_SCCS) && !defined(lint)
static char sccsid[] = "@(#)getservent.c	1.3 88/03/31 4.0NFSSRC; from	5.3 (Berkeley) 5/19/86"; /* and from 1.19 88/02/08 SMI Copyr 1984 Sun Micro */
#endif LIBC_SCCS and not lint

#include <stdio.h>
#include <sys/types.h>
#include <sys/socket.h>
#include <netdb.h>
#include <rpcsvc/ypclnt.h>
#ifdef SYSTYPE_BSD43
#include <netinet/in.h>
#else
#ifdef SYSTYPE_BSD43
#include <bsd/netinet/in.h>
#endif
#endif

/*
 * Internet version.
 */
static struct servdata {
	FILE	*servf;
	char	*current;
	int	currentlen;
	int	stayopen;
#define MAXALIASES	35
	char	*serv_aliases[MAXALIASES];
	struct	servent serv;
	char	line[BUFSIZ+1];
	char	*domain;
	int	usingyellow;
} *servdata, *_servdata();

static struct servent *interpret();
struct servent *yp_getservent();
char *inet_ntoa();
char *malloc();
static char *any();

static char SERVDB[] = "/etc/services";
static char YPMAP[] = "services.byname";

static struct servdata *
_servdata()
{
	register struct servdata *d = servdata;

	if (d == 0) {
		d = (struct servdata *)calloc(1, sizeof (struct servdata));
		servdata = d;
	}
	return (d);
}

struct servent *
yp_getservbyport(req)
	Vis_sbyport_req *req;
{
	int svc_port = req->r_port;
	char *proto = req->r_proto;
	register struct servdata *d = _servdata();
	register struct servent *p = NULL;
	register u_short port = svc_port;

	if (d == 0)
		return (0);
	/*
	 * first try to do very fast yp lookup.
	 */
	yellowup();
	if (d->usingyellow && (proto != 0)) {
		char portstr[12];
		char *key, *val;
		int keylen, vallen;
 
		sprintf(portstr, "%d", ntohs(port));
		keylen = strlen(portstr) + 1 + strlen(proto);
		key = malloc(keylen + 1);
		sprintf(key, "%s/%s", portstr, proto);
		if (!yp_match(d->domain, YPMAP, key, keylen,
		    &val, &vallen)) {
			p = interpret(val, vallen);
			free(val);
		}
		free(key);
		return(p);
	}
	yp_setservent(0);
	while (p = yp_getservent()) {
		if (p->s_port != port)
			continue;
		if (proto == 0 || strcmp(p->s_proto, proto) == 0)
			break;
	}
	yp_endservent();
	return (p);
}

struct servent *
yp_getservbyname(req)
	Vis_sbyname_req *req;
{
	register char *name = req->r_name, *proto = req->r_proto;
	register struct servdata *d = _servdata();
	register struct servent *p;
	register char **cp;

	if (d == 0)
		return (0);
	yp_setservent(0);
	while (p = yp_getservent()) {
		if (strcmp(name, p->s_name) == 0)
			goto gotname;
		for (cp = p->s_aliases; *cp; cp++)
			if (strcmp(name, *cp) == 0)
				goto gotname;
		continue;
gotname:
		if (proto == 0 || strcmp(p->s_proto, proto) == 0)
			break;
	}
	yp_endservent();
	return (p);
}

yp_setservent(f)
	int f;
{
	register struct servdata *d = _servdata();

	if (d == 0)
		return;
	if (d->servf == NULL)
		d->servf = fopen(SERVDB, "r");
	else
		rewind(d->servf);
	if (d->current)
		free(d->current);
	d->current = NULL;
	d->stayopen |= f;
	yellowup();	/* recompute whether yellow pages are up */
}

yp_endservent(a)
	int a;
{
	register struct servdata *d = _servdata();

	if (d == 0)
		return;
	if (d->current && !d->stayopen) {
		free(d->current);
		d->current = NULL;
	}
	if (d->servf && !d->stayopen) {
		fclose(d->servf);
		d->servf = NULL;
	}
}

struct servent *
yp_getservent(a)
	int a;
{
	register struct servdata *d = _servdata();
	int reason;
	char *key = NULL, *val = NULL;
	int keylen, vallen;
	struct servent *sp;

	if (d == 0)
		return NULL;
	yellowup();
	if (d->current == NULL) {
		if (reason =  yp_first(d->domain, YPMAP,
		    &key, &keylen, &val, &vallen)) {
#ifdef DEBUG
			fprintf(stderr, "reason yp_first failed is %d\n",
			    reason);
#endif
			return NULL;
		    }
	}
	else {
		if (reason = yp_next(d->domain, YPMAP,
		    d->current, d->currentlen, &key, &keylen, &val, &vallen)) {
#ifdef DEBUG
			fprintf(stderr, "reason yp_next failed is %d\n",
			    reason);
#endif
			return NULL;
		}
	}
	if (d->current)
		free(d->current);
	d->current = key;
	d->currentlen = keylen;
	sp = interpret(val, vallen);
	free(val);
	return (sp);
}

static struct servent *
interpret(val, len)
{
	register struct servdata *d = _servdata();
	char *p;
	register char *cp, **q;

	if (d == 0)
		return (0);
	strncpy(d->line, val, len);
	p = d->line;
	d->line[len] = '\n';
	if (*p == '#')
		return (getservent());
	cp = any(p, "#\n");
	if (cp == NULL)
		return (getservent());
	*cp = '\0';
	d->serv.s_name = p;
	p = any(p, " \t");
	if (p == NULL)
		return (getservent());
	*p++ = '\0';
	while (*p == ' ' || *p == '\t')
		p++;
	cp = any(p, ",/");
	if (cp == NULL)
		return (getservent());
	*cp++ = '\0';
	d->serv.s_port = htons((u_short)atoi(p));
	d->serv.s_proto = cp;
	q = d->serv.s_aliases = d->serv_aliases;
	cp = any(cp, " \t");
	if (cp != NULL)
		*cp++ = '\0';
	while (cp && *cp) {
		if (*cp == ' ' || *cp == '\t') {
			cp++;
			continue;
		}
		if (q < &(d->serv_aliases[MAXALIASES - 1]))
			*q++ = cp;
		cp = any(cp, " \t");
		if (cp != NULL)
			*cp++ = '\0';
	}
	*q = NULL;
	return (&d->serv);
}

/*
 * check to see if yellow pages are up, and store that fact in d->usingyellow.
 */
static
yellowup()
{
	register struct servdata *d = _servdata();

	if (d->domain == NULL) {
		d->usingyellow = usingypmap(&d->domain, YPMAP);
	}
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
