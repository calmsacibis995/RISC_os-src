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
#ident	"$Header: yp_getrpcent.c,v 1.2.1.2 90/05/07 21:30:13 wje Exp $"
#if !defined(lint) && defined(SCCSIDS)
static char sccsid[] = 	"@(#)getrpcent.c	1.1 88/05/10 4.0NFSSRC SMI"; /* @(#) from SUN 1.12   */
#endif

/* 
 * Copyright (c) 1985 by Sun Microsystems, Inc.
 */

#include <stdio.h>
#include <netdb.h>
#include <sys/types.h>
#include <sys/socket.h>
#include <rpcsvc/ypclnt.h>

/*
 * Internet version.
 */
struct rpcdata {
	FILE	*rpcf;
	char	*current;
	int	currentlen;
	int	stayopen;
#define	MAXALIASES	35
	char	*rpc_aliases[MAXALIASES];
	struct	rpcent rpc;
	char	line[BUFSIZ+1];
	char	*domain;
	int	usingyellow;
} *rpcdata, *_rpcdata();

static	struct rpcent *interpret();
struct	hostent *gethostent();
struct rpcent * yp_getrpcent();
char	*inet_ntoa();
char *strpbrk();

static char RPCDB[] = "/etc/rpc";

static char *YPMAP = "rpc.bynumber";

static struct rpcdata *
_rpcdata()
{
	register struct rpcdata *d = rpcdata;

	if (d == 0) {
		d = (struct rpcdata *)calloc(1, sizeof (struct rpcdata));
		rpcdata = d;
	}
	return (d);
}

struct rpcent *
yp_getrpcbynumber(number)
	register int number;
{
	register struct rpcdata *d = _rpcdata();
	register struct rpcent *p;
	int reason;
	char adrstr[16], *val = NULL;
	int vallen;

	if (d == 0)
		return (0);
	yp_setrpcent(0);
	sprintf(adrstr, "%d", number);
	if (reason = yp_match(d->domain, YPMAP,
	    adrstr, strlen(adrstr), &val, &vallen)) {
#ifdef DEBUG
		fprintf(stderr, "reason yp_match failed is %d\n",
		    reason);
#endif
		p = NULL;
	    }
	else {
		p = interpret(val, vallen);
		free(val);
	}
	yp_endrpcent();
	return (p);
}

struct rpcent *
yp_getrpcbyname(name)
	char *name;
{
	struct rpcent *rpc;
	char **rp;

	yp_setrpcent(0);
	while(rpc = yp_getrpcent()) {
		if (strcmp(rpc->r_name, name) == 0)
			return (rpc);
		for (rp = rpc->r_aliases; *rp != NULL; rp++) {
			if (strcmp(*rp, name) == 0)
				return (rpc);
		}
	}
	yp_endrpcent();
	return (NULL);
}

yp_setrpcent(f)
	int f;
{
	register struct rpcdata *d = _rpcdata();

	if (d == 0)
		return;
	if (d->rpcf == NULL)
		d->rpcf = fopen(RPCDB, "r");
	else
		rewind(d->rpcf);
	if (d->current)
		free(d->current);
	d->current = NULL;
	d->stayopen |= f;
	yellowup();	/* recompute whether yellow pages are up */
}

yp_endrpcent(a)
	int a;
{
	register struct rpcdata *d = _rpcdata();

	if (d == 0)
		return;
	if (d->current && !d->stayopen) {
		free(d->current);
		d->current = NULL;
	}
	if (d->rpcf && !d->stayopen) {
		fclose(d->rpcf);
		d->rpcf = NULL;
	}
}

struct rpcent *
yp_getrpcent(a)
	int a;
{
	struct rpcent *hp;
	int reason;
	char *key = NULL, *val = NULL;
	int keylen, vallen;
	register struct rpcdata *d = _rpcdata();

	if (d == 0)
		return (0);
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
	hp = interpret(val, vallen);
	free(val);
	return (hp);
}

static struct rpcent *
interpret(val, len)
{
	register struct rpcdata *d = _rpcdata();
	char *p;
	register char *cp, **q;

	if (d == 0)
		return (0);
	strncpy(d->line, val, len);
	p = d->line;
	d->line[len] = '\n';
	if (*p == '#')
		return (yp_getrpcent());
	cp = strpbrk(p, "#\n");
	if (cp == NULL)
		return (yp_getrpcent());
	*cp = '\0';
	cp = strpbrk(p, " \t");
	if (cp == NULL)
		return (yp_getrpcent());
	*cp++ = '\0';
	/* THIS STUFF IS INTERNET SPECIFIC */
	d->rpc.r_name = d->line;
	while (*cp == ' ' || *cp == '\t')
		cp++;
	d->rpc.r_number = atoi(cp);
	q = d->rpc.r_aliases = d->rpc_aliases;
	cp = strpbrk(cp, " \t");
	if (cp != NULL) 
		*cp++ = '\0';
	while (cp && *cp) {
		if (*cp == ' ' || *cp == '\t') {
			cp++;
			continue;
		}
		if (q < &(d->rpc_aliases[MAXALIASES - 1]))
			*q++ = cp;
		cp = strpbrk(cp, " \t");
		if (cp != NULL)
			*cp++ = '\0';
	}
	*q = NULL;
	return (&d->rpc);
}

/* 
 * check to see if yellow pages are up, and store that fact in d->usingyellow.
 */
static
yellowup()
{
	register struct rpcdata *d = _rpcdata();

	if (d == 0)
		return;
	if (d->domain == NULL) {
		d->usingyellow = usingypmap(&d->domain, YPMAP);
	}
}
