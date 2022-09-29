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
#ident	"$Header: ht_grpcent.c,v 1.2.1.2 90/05/07 20:47:36 wje Exp $"

/* 
 * Copyright (c) 1985 by Sun Microsystems, Inc.
 */

#include <stdio.h>
#ifdef SYSTYPE_BSD43
#include <netdb.h>
#include <sys/types.h>
#include <sys/socket.h>
#endif
#ifdef SYSTYPE_SYSV
#include <bsd/netdb.h>
#include <bsd/sys/types.h>
#include <bsd/sys/socket.h>
static struct rpcent *interpret();
#endif

/*
 * Internet version.
 */
#define	MAXALIASES	35
#define	MAXADDRSIZE	14

struct rpcent *ht_getrpcent();
static char domain[256];
static int stayopen;
static char *current = NULL;	/* current entry, analogous to hostf */
static int currentlen;
static struct rpcent *interpret();
struct hostent *gethostent();
char *inet_ntoa();
static char *any();
static char RPCDB[] = "/etc/rpc";
static FILE *rpcf = NULL;

struct rpcent *
ht_getrpcbynumber(number)
	register int number;
{
	register struct rpcent *p;
	int reason;
	char adrstr[10], *val;
	int vallen;

	ht_setrpcent(0);
	while (p = ht_getrpcent()) {
		if (p->r_number == number)
			break;
	}
	ht_endrpcent();
	return (p);
}

struct rpcent *
ht_getrpcbyname(name)
	char *name;
{
	struct rpcent *rpc;
	char **rp;

	ht_setrpcent(0);
	while(rpc = ht_getrpcent()) {
		if (strcmp(rpc->r_name, name) == 0)
			return (rpc);
		for (rp = rpc->r_aliases; *rp != NULL; rp++) {
			if (strcmp(*rp, name) == 0)
				return (rpc);
		}
	}
	ht_endrpcent();
	return (NULL);
}

ht_setrpcent(f)
	int f;
{
	if (getdomainname(domain, sizeof(domain)) < 0) {
		fprintf(stderr, 
		    "setrpcent: getdomainname system call missing\n");
		exit(1);
	}
	if (rpcf == NULL)
		rpcf = fopen(RPCDB, "r");
	else
		rewind(rpcf);
	if (current)
		free(current);
	current = NULL;
	stayopen |= f;
}

ht_endrpcent(a)
	int a;
{
	if (current && !stayopen) {
		free(current);
		current = NULL;
	}
	if (rpcf && !stayopen) {
		fclose(rpcf);
		rpcf = NULL;
	}
}

struct rpcent *
ht_getrpcent(a)
	int a;
{
	struct rpcent *hp;
	int reason;
	char *key, *val;
	int keylen, vallen;
	static char line1[BUFSIZ+1];

	if (rpcf == NULL && (rpcf = fopen(RPCDB, "r")) == NULL)
		return (NULL);
	if (fgets(line1, BUFSIZ, rpcf) == NULL)
		return (NULL);
	return interpret(line1, strlen(line1));
}

static struct rpcent *
interpret(val, len)
{
	static char *rpc_aliases[MAXALIASES];
	static struct rpcent rpc;
	static char line[BUFSIZ+1];
	char *p;
	register char *cp, **q;

	strncpy(line, val, len);
	p = line;
	line[len] = '\n';
	if (*p == '#')
		return (ht_getrpcent());
	cp = any(p, "#\n");
	if (cp == NULL)
		return (ht_getrpcent());
	*cp = '\0';
	cp = any(p, " \t");
	if (cp == NULL)
		return (ht_getrpcent());
	*cp++ = '\0';
	/* THIS STUFF IS INTERNET SPECIFIC */
	rpc.r_name = line;
	while (*cp == ' ' || *cp == '\t')
		cp++;
	rpc.r_number = atoi(cp);
	q = rpc.r_aliases = rpc_aliases;
	cp = any(cp, " \t");
	if (cp != NULL) 
		*cp++ = '\0';
	while (cp && *cp) {
		if (*cp == ' ' || *cp == '\t') {
			cp++;
			continue;
		}
		if (q < &rpc_aliases[MAXALIASES - 1])
			*q++ = cp;
		cp = any(cp, " \t");
		if (cp != NULL)
			*cp++ = '\0';
	}
	*q = NULL;
	return (&rpc);
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
