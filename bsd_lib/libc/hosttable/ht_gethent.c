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
#ident	"$Header: ht_gethent.c,v 1.3.1.2 90/05/07 20:46:14 wje Exp $"

/*
 * This file contains the routines sethostent(), gethostent(), endhostent(),
 * and sethostfile() that work with both the NFS yellow pages code and with
 * the standard 4.3BSD hashed host table code.
 */

#include <stdio.h>
#include <sys/types.h>
#include <sys/socket.h>
#include <netdb.h>
#include <ctype.h>
#include <rpcsvc/ypclnt.h>
#ifdef SYSTYPE_BSD43
#include <ndbm.h>
#else
#ifdef SYSTYPE_SYSV
#include <bsd/ndbm.h>
#endif
#endif

/*
 * Internet version.
 */
#define	MAXALIASES	35
#define	MAXADDRSIZE	14

static FILE *hostf = NULL;
static char line[BUFSIZ+1];
static char hostaddr[MAXADDRSIZE + sizeof(u_long)];
static struct hostent host;
static char *host_aliases[MAXALIASES];
static char *host_addrs[] = {
	NULL,
	NULL
};
static char *current = NULL;	/* current entry, analogous to hostf */
static int currentlen;
static int standard_file = 1;	/* unset by sethostfile() */
struct hostent *gethostent();
char *inet_ntoa();
static char *any();

/*
 * The following is shared with gethostnamadr.c
 */
char	*_host_file = "/etc/hosts";
extern int	_host_stayopen;
extern DBM	*_host_db;	/* set by gethostbyname(), gethostbyaddr() */
extern int	_using_yellow;
struct hostent *_host_interpret();
char	_host_domain[256];

static char *any();

struct hostent *
ht_gethostent()
{
	static char line1[BUFSIZ+1];

	if (hostf == NULL && (hostf = fopen(_host_file, "r")) == NULL)
		return (NULL);
	if (fgets(line1, BUFSIZ, hostf) == NULL)
		return (NULL);
	return _host_interpret(line1, strlen(line1));
}

struct hostent *
_host_interpret(val, len)
{
	static char *host_aliases[MAXALIASES];
	static char hostaddr[MAXADDRSIZE];
	static struct hostent host;
	static char line[BUFSIZ+1];
	char *p;
	register char *cp, **q;

	strncpy(line, val, len);
	p = line;
	line[len] = '\n';
	if (*p == '#')
		return (ht_gethostent());
	cp = any(p, "#\n");
	if (cp == NULL)
		return (ht_gethostent());
	*cp = '\0';
	cp = any(p, " \t");
	if (cp == NULL)
		return (ht_gethostent());
	*cp++ = '\0';
	/* THIS STUFF IS INTERNET SPECIFIC */
	host_addrs[0] = (char *)hostaddr;
	host.h_addr_list = host_addrs;
	*((u_long *)host.h_addr) = inet_addr(p);
	host.h_length = sizeof (u_long);
	host.h_addrtype = AF_INET;
	while (*cp == ' ' || *cp == '\t')
		cp++;
	host.h_name = cp;
	q = host.h_aliases = host_aliases;
	cp = any(cp, " \t");
	if (cp != NULL) 
		*cp++ = '\0';
	while (cp && *cp) {
		if (*cp == ' ' || *cp == '\t') {
			cp++;
			continue;
		}
		if (q < &host_aliases[MAXALIASES - 1])
			*q++ = cp;
		cp = any(cp, " \t");
		if (cp != NULL)
			*cp++ = '\0';
	}
	*q = NULL;
	return (&host);
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
