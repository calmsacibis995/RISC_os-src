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
#ident	"$Header: yp_gethostent.c,v 1.3.1.2.1.2 90/07/23 10:18:40 hawkes Exp $"
/*
 * Copyright (c) 1983 Regents of the University of California.
 * All rights reserved.  The Berkeley software License Agreement
 * specifies the terms and conditions for redistribution.
 */

/* 
 * Copyright (c) 1985 by Sun Microsystems, Inc.
 */

#if defined(LIBC_SCCS) && !defined(lint)
static char sccsid[] = "@(#)gethostent.c	1.6 88/07/27 4.0NFSSRC; from	5.3 (Berkeley) 3/9/86";
#endif LIBC_SCCS and not lint

#include <stdio.h>
#include <sys/types.h>
#include <sys/file.h>
#include <sys/socket.h>
#include <ctype.h>
#include <rpcsvc/ypclnt.h>
#include <netdb.h>
#ifdef SYSTYPE_BSD43
#include <netinet/in.h>
#include <ndbm.h>
#else
#ifdef SYSTYPE_SYSV
#include <bsd/netinet/in.h>
#include <bsd/ndbm.h>
#endif
#endif

/*
 * Internet version.
 */
static struct hostdata {
	FILE	*hostf;
	DBM	*host_db;	/* set by gethostbyname(), gethostbyaddr() */
	char	*current;
	int	currentlen;
	int	stayopen;
#define	MAXALIASES	35
	char	*host_aliases[MAXALIASES];
	char	*host_addrs[2];
#define	MAXADDRSIZE	14
	char	hostaddr[MAXADDRSIZE];
	char	*addr_list[2];
	char	line[BUFSIZ+1];
	struct	hostent host;
	char	*domain;
	char	*map;
	int	usingyellow;
} *hostdata, *_hostdata();
#define	MAXADDRSIZE	14

static	struct hostent *interpret();
struct	hostent *gethostent();
char	*inet_ntoa();
static char *any();

static	char *HOSTDB = "/etc/hosts";

static struct hostdata *
_hostdata()
{
	register struct hostdata *d = hostdata;

	if (d == 0) {
		d = (struct hostdata *)calloc(1, sizeof (struct hostdata));
		hostdata = d;
		d->host_addrs[0] = d->hostaddr;
		d->host_addrs[1] = NULL;
	}
	return (d);
}


int h_errno;

static struct hostent *
fetchhost(key)
	datum key;
{
	register struct hostdata *d = _hostdata();
	register char *cp, *tp, **ap;
	int naliases;

	if (d == 0) 
		return ((struct hostent*)NULL);
	if (key.dptr == 0)
		return ((struct hostent *)NULL);
	key = dbm_fetch(d->host_db, key);
	if (key.dptr == 0)
		return ((struct hostent *)NULL);
	cp = key.dptr;
	tp = d->line;
	d->host.h_name = tp;
	while (*tp++ = *cp++)
		;
	bcopy(cp, (char *)&naliases, sizeof(int)); cp += sizeof (int);
	for (ap = d->host_aliases; naliases > 0; naliases--) {
		*ap++ = tp;
		while (*tp++ = *cp++)
			;
	}
	*ap = (char *)NULL;
	d->host.h_aliases = d->host_aliases;
	bcopy(cp, (char *)&d->host.h_addrtype, sizeof (int));
	cp += sizeof (int);
	bcopy(cp, (char *)&d->host.h_length, sizeof (int));
	cp += sizeof (int);
	d->host.h_addr_list = d->host_addrs;
	d->host.h_addr = tp;
	bcopy(cp, tp, d->host.h_length);
        return (&d->host);
}

struct hostent *
yp_gethostbyname(name)
	register char *name;
{
	register struct hostdata *d = _hostdata();
	register struct hostent *p;
	register char **cp;
	char *val = NULL;
	char *lowname;          /* local copy of name in lower case */
	datum key;
	int vallen;

	if (d == 0) {
		h_errno = NO_RECOVERY;
		return ((struct hostent*)NULL);
	}

	lowname = (char *)malloc(strlen(name)+1);
	for (val = lowname; *name; name++)
		*val++ = isupper(*name) ? tolower(*name) : *name;
	*val = '\0';

	d->map = "hosts.byname";
	yp_sethostent(0);
	if (yp_match(d->domain, d->map,
	    lowname, val - lowname, &val, &vallen))
		p = NULL;
	else {
		p = interpret(val, vallen);
		free(val);
		}
found:
	if (p == NULL)
		h_errno = HOST_NOT_FOUND;
	yp_endhostent(0);
	free(lowname);        
	return (p);
}

struct hostent *
yp_gethostbyaddr(req)
	Vis_hbyaddr_req *req;
{
	char *addr = req->r_addr;
	register int len = req->r_len, type = req->r_type;
	register struct hostdata *d = _hostdata();
	register struct hostent *p;
	datum key;
	char *adrstr, *val = NULL;
	int vallen;

	if (d == 0) {
		h_errno = NO_RECOVERY;
		return ((struct hostent*)NULL);
	}
	d->map = "hosts.byaddr";
	yp_sethostent(0);
	adrstr = inet_ntoa(*(struct in_addr *)addr);
	if (yp_match(d->domain, d->map,
	    adrstr, strlen(adrstr), &val, &vallen))
		p = NULL;
	else {
		p = interpret(val, vallen);
		free(val);
	}
	if (p == NULL)
	    h_errno = HOST_NOT_FOUND;
	yp_endhostent();
	return (p);
}

yp_sethostent(f)
	int f;
{
	register struct hostdata *d = _hostdata();

	if (d == 0)
		return;
	if (d->hostf == NULL)
		d->hostf = fopen(HOSTDB, "r");
	else
		rewind(d->hostf);
	if (d->current)
		free(d->current);
	d->current = NULL;
	d->stayopen |= f;
	if (d->map)
		yellowup();	/* recompute whether yellow pages are up */
}

yp_endhostent(a)
	int a;
{
	register struct hostdata *d = _hostdata();

	if (d == 0)
		return;
	if (d->current && !d->stayopen) {
		free(d->current);
		d->current = NULL;
	}
	if (d->hostf && !d->stayopen) {
		fclose(d->hostf);
		d->hostf = NULL;
	}
	if (!d->usingyellow && !d->stayopen)
		if (d->host_db) {
			dbm_close(d->host_db);
			d->host_db = (DBM *)NULL;
		}
}

struct hostent *
yp_gethostent(a)
	int a;
{
	register struct hostdata *d = _hostdata();
	struct hostent *hp;
	char *key = NULL, *val = NULL;
	int keylen, vallen;

	if (d == 0)
		return ((struct hostent*)NULL);
	d->map = "hosts.byaddr";
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
	hp = interpret(val, vallen);
	free(val);
	return (hp);
}

static struct hostent *
interpret(val, len)
char	*val;
int	len;
{
	register struct hostdata *d = _hostdata();
	char *p;
	register char *cp, **q;

	if (d == 0)
		return (0);
	strncpy(d->line, val, len);
	p = d->line;
	d->line[len] = '\n';
	if (*p == '#')
		return (gethostent());
	cp = any(p, "#\n");
	if (cp == NULL)
		return (gethostent());
	*cp = '\0';
	cp = any(p, " \t");
	if (cp == NULL)
		return (gethostent());
	*cp++ = '\0';
	/* THIS STUFF IS INTERNET SPECIFIC */
	d->addr_list[0] = d->hostaddr;
	d->addr_list[1] = NULL;
	d->host.h_addr_list = d->addr_list;
	*((u_long *)d->host.h_addr) = inet_addr(p);
	d->host.h_length = sizeof (u_long);
	d->host.h_addrtype = AF_INET;
	while (*cp == ' ' || *cp == '\t')
		cp++;
	d->host.h_name = cp;
	q = d->host.h_aliases = d->host_aliases;
	cp = any(cp, " \t");
	if (cp != NULL) 
		*cp++ = '\0';
	while (cp && *cp) {
		if (*cp == ' ' || *cp == '\t') {
			cp++;
			continue;
		}
		if (q < &(d->host_aliases[MAXALIASES - 1]))
			*q++ = cp;
		cp = any(cp, " \t");
		if (cp != NULL)
			*cp++ = '\0';
	}
	*q = NULL;
	return (&d->host);
}

yp_sethostfile(file)
	char *file;
{
	HOSTDB = file;
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
	register struct hostdata *d = _hostdata();

        if (d->domain == NULL)
                d->usingyellow = usingypmap(&d->domain, d->map);
}
