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
#ident	"$Header: yp_innetgr.c,v 1.2.1.3 90/05/07 21:30:36 wje Exp $"

/* 
 * Copyright (c) 1985 by Sun Microsystems, Inc.
 */

#include <stdio.h>
#include <ctype.h>
#include <rpcsvc/ypclnt.h>
#include <netdb.h>
#ifdef RISCOS
#ifdef SYSTYPE_BSD43
#include <syslog.h>
#else SYSTYPE_BSD43
#include <bsd/syslog.h>
#endif SYSTYPE_BSD43
#endif

/* 
 * innetgr: test whether I'm in /etc/netgroup
 * 
 */

char	*malloc();
char	*strpbrk(), *index(), *strcpy();

static struct innetgrdata {
	char	*name;
	char	*machine;
	char	*domain;
	char	**list;
#define	LISTSIZE 200			/* recursion limit */
	char	**listp;		/* pointer into list */
	char	*thisdomain;
};

yp_innetgr(req)
	Vis_innetgr_req *req;
{
	char *group = req->r_grp, *machine = req->r_mach, *name = req->r_nm, *domain = req->r_dom;
	int res;
	register struct innetgrdata *d;
	char *thisdomain;

	(void) usingypmap(&thisdomain, NULL);
	if (domain) {
		if (name && !machine) {
			if (lookup(thisdomain,
			    "netgroup.byuser",group,name,domain,&res)) {
				return(res);
			}
		} else if (machine && !name) {
			if (lookup(thisdomain,
			    "netgroup.byhost",group,machine,domain,&res)) {
				return(res);
			}
		}
	}
	d = (struct innetgrdata *)malloc(sizeof (struct innetgrdata));
	if (d == 0)
		return (0);
	d->machine = machine;
	d->name = name;
	d->domain = domain;
	d->thisdomain = thisdomain;
	d->list = (char **)calloc(LISTSIZE, sizeof (char *));
	d->listp = d->list;
	if (d->list == 0) {
		free(d);
		return (0);
	}
	res = doit(d, group);
	free(d->list);
	free(d);
	return (res);
}
	
/* 
 * calls itself recursively
 */
static
doit(d, group)
	register struct innetgrdata *d;
	char *group;
{
	char *key, *val;
	int vallen,keylen;
	char *r;
	int match;
	register char *p, *q;
	register char **lp;
	int err;
	
	*d->listp++ = group;
	if (d->listp > d->list + LISTSIZE) {
#ifdef RISCOS
		syslog(LOG_ERR, "innetgr: recursive overflow\r\n");
#else
		(void) fprintf(stderr, "innetgr: recursive overflow\r\n");
#endif
		d->listp--;
		return (0);
	}
	key = group;
	keylen = strlen(group);
	err = yp_match(d->thisdomain, "netgroup", key, keylen, &val, &vallen);
	if (err) {
#ifdef DEBUG
		if (err == YPERR_KEY)
			(void) fprintf(stderr,
			    "innetgr: no such netgroup as %s\n", group);
		else
			(void) fprintf(stderr, "innetgr: yp_match, %s\n",yperr_string(err));
#endif
		d->listp--;
		return(0);
	}
	/* 
	 * check for recursive loops
	 */
	for (lp = d->list; lp < d->listp-1; lp++)
		if (strcmp(*lp, group) == 0) {
#ifdef RISCOS
			syslog(LOG_ERR,
			    "innetgr: netgroup %s called recursively\r\n",
			    group);
#else
			(void) fprintf(stderr,
			    "innetgr: netgroup %s called recursively\r\n",
			    group);
#endif
			d->listp--;
			free(val);
			return(0);
		}
	
	p = val;
	p[vallen] = 0;
	while (p != NULL) {
		match = 0;
		while (*p == ' ' || *p == '\t')
			p++;
		if (*p == 0 || *p == '#')
			break;
		if (*p == '(') {
			p++;
			while (*p == ' ' || *p == '\t')
				p++;
			r = q = index(p, ',');
			if (q == NULL) {
#ifdef RISCOS
				syslog(LOG_ERR,
				    "innetgr: syntax error in /etc/netgroup\r\n");
#else
				(void) fprintf(stderr,
				    "innetgr: syntax error in /etc/netgroup\r\n");
#endif
				d->listp--;
				free(val);
				return(0);
			}
			if (p == q || d->machine == NULL)
				match++;
			else {
				while (*(q-1) == ' ' || *(q-1) == '\t')
					q--;
				if (strncmp(d->machine, p, q-p) == 0)
					match++;
			}
			p = r+1;

			while (*p == ' ' || *p == '\t')
				p++;
			r = q = index(p, ',');
			if (q == NULL) {
#ifdef RISCOS
				syslog(LOG_ERR,
				    "innetgr: syntax error in /etc/netgroup\r\n");
#else
				(void) fprintf(stderr,
				    "innetgr: syntax error in /etc/netgroup\r\n");
#endif
				d->listp--;
				free(val);
				return(0);
			}
			if (p == q || d->name == NULL)
				match++;
			else {
				while (*(q-1) == ' ' || *(q-1) == '\t')
					q--;
				if (strncmp(d->name, p, q-p) == 0)
					match++;
			}
			p = r+1;

			while (*p == ' ' || *p == '\t')
				p++;
			r = q = index(p, ')');
			if (q == NULL) {
#ifdef RISCOS
				syslog(LOG_ERR,
				    "innetgr: syntax error in /etc/netgroup\r\n");
#else
				(void) fprintf(stderr,
				    "innetgr: syntax error in /etc/netgroup\r\n");
#endif
				d->listp--;
				free(val);
				return(0);
			}
			if (p == q || d->domain == NULL)
				match++;
			else {
				while (*(q-1) == ' ' || *(q-1) == '\t')
					q--;
				if (strncmp(d->domain, p, q-p) == 0)
					match++;
			}
			p = r+1;
			if (match == 3) {
				free(val);
				d->listp--;
				return 1;
			}
		}
		else {
			q = strpbrk(p, " \t\n#");
			if (q && *q == '#')
				break;
			if (q)
				*q = 0;
			if (doit(d, p)) {
				free(val);
				d->listp--;
				return 1;
			}
			if (q)
				*q = ' ';
		}
		p = strpbrk(p, " \t");
	}
	free(val);
	d->listp--;
	return 0;
}

/*
 * return 1 if "what" is in the comma-separated, newline-terminated "d->list"
 */
static
inlist(what,list)
	char *what;
	char *list;
{
#	define TERMINATOR(c)    (c == ',' || c == '\n')

	register char *p;
	int len;
         
	len = strlen(what);     
	p = list;
	do {             
		if (strncmp(what,p,len) == 0 && TERMINATOR(p[len])) {
			return(1);
		}
		while (!TERMINATOR(*p)) {
			p++;
		}
		p++;
	} while (*p);
	return(0);
}




/*
 * Lookup a host or user name in a yp map.  Set result to 1 if group in the 
 * lookup list of groups. Return 1 if the map was found.
 */
static
lookup(thisdomain,map,group,name,domain,res)
	char *thisdomain;
	char *map;
	char *group;
	char *name;
	char *domain;
	int *res;
{
	int err;
	char *val;
	int vallen;
	char key[256];
	char *wild = "*";
	int i;

	for (i = 0; i < 4; i++) {
		switch (i) {
		case 0: makekey(key,name,domain); break;
		case 1: makekey(key,wild,domain); break;	
		case 2: makekey(key,name,wild); break;
		case 3: makekey(key,wild,wild); break;	
		}
		err  = yp_match(thisdomain,map,key,strlen(key),&val,&vallen); 
		if (!err) {
			*res = inlist(group,val);
			free(val);
			if (*res) {
				return(1);
			}
		} else {
#ifdef DEBUG
			(void) fprintf(stderr,
				"yp_match(%s,%s) failed: %s.\n",map,key,yperr_string(err));
#endif
			if (err != YPERR_KEY)  {
				return(0);
			}
		}
	}
	*res = 0;
	return(1);
}



/*
 * Generate a key for a netgroup.byXXXX yp map
 */
static
makekey(key,name,domain)
	register char *key;
	register char *name;
	register char *domain;
{
	while (*key++ = *name++)
		;
	*(key-1) = '.';
	while (*key++ = *domain++)
		;
}	
