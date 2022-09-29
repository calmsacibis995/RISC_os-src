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
#ident	"$Header: ht_innetgr.c,v 1.2.1.3 90/05/07 20:47:48 wje Exp $"

/* 
 * Copyright (c) 1985 by Sun Microsystems, Inc.
 */

#include <stdio.h>
#include <ctype.h>
#include <netdb.h>
#define RISCOS
#ifndef RISCOS
#include <rpcsvc/ypclnt.h>
#endif RISCOS

/* 
 * innetgr: test whether I'm in /etc/netgroup
 * 
 */

extern char *malloc();

static char *any();
extern char *index(), *strcpy();
static char *name, *machine, *domain;
static char thisdomain[256];
static char *list[200];		/* can nest recursively this deep */
static char **listp;		/* pointer into list */

ht_innetgr(req)
	Vis_innetgr_req *req;
{
	char *grp = req->r_grp;
	char *mach = req->r_mach;
	char *nm = req->r_nm;
	char *dom = req->r_dom;
	int res;

	if (getdomainname(thisdomain, sizeof(thisdomain)) < 0) {
		(void) fprintf(stderr, 
		    "innetgr: getdomainname system call missing\r\n");
	    exit(1);
	}
	listp = list;
	machine = mach;
	name = nm;
	domain = dom;
#ifndef RISCOS
	if (domain) {
		if (name && !machine) {
			if (lookup("netgroup.byuser",grp,name,domain,&res)) {
				return(res);
			}
		} else if (machine && !name) {
			if (lookup("netgroup.byhost",grp,machine,domain,&res)) {
				return(res);
			}
		}
	}
	return doit(grp);
#else RISCOS
	ht_setnetgrent(grp);
	{
		Vis_gnetgrent_req req;
		char *name, *machine, *domain;
		req.r_namep = &name;
		req.r_machinep = &machine;
		req.r_domainp = &domain;

		while (ht_getnetgrent(&req)) {
			if ((nm == NULL || *(req.r_namep) == NULL ||
			     !strcmp(nm,*(req.r_namep))) &&
		    	    (mach == NULL || *(req.r_machinep) == NULL ||
			     !strcmp(mach,*(req.r_machinep))) &&
			    (dom == NULL || *(req.r_domainp) == NULL ||
			     !strcmp(dom,*(req.r_domainp)))) {
				ht_endnetgrent();
				return(1);
			};
		}
	}
	ht_endnetgrent(grp);
	return(0);
#endif RISCOS
}
	

	

#ifndef RISCOS
/* 
 * calls itself recursively
 */
static
doit(group)
	char *group;
{
	char *key, *val;
	int vallen,keylen;
	char *r;
	int match;
	register char *p, *q;
	register char **lp;
	int err;
	
	*listp++ = group;
	if (listp > list + sizeof(list)) {
		(void) fprintf(stderr, "innetgr: recursive overflow\r\n");
		listp--;
		return (0);
	}
	key = group;
	keylen = strlen(group);
	err = yp_match(thisdomain, "netgroup", key, keylen, &val, &vallen);
	if (err) {
#ifdef DEBUG
		if (err == YPERR_KEY)
			(void) fprintf(stderr,
			    "innetgr: no such netgroup as %s\n", group);
		else
			(void) fprintf(stderr, "innetgr: yp_match, %s\n",yperr_string(err));
#endif
		listp--;
		return(0);
	}

	/* 
	 * check for recursive loops
	 */
	for (lp = list; lp < listp-1; lp++)
		if (strcmp(*lp, group) == 0) {
			(void) fprintf(stderr,
			    "innetgr: netgroup %s called recursively\r\n",
			    group);
			listp--;
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
				(void) fprintf(stderr,
				    "innetgr: syntax error in /etc/netgroup\r\n");
				listp--;
				return(0);
			}
			if (p == q || machine == NULL)
				match++;
			else {
				while (*(q-1) == ' ' || *(q-1) == '\t')
					q--;
				if (strncmp(machine, p, q-p) == 0)
					match++;
			}
			p = r+1;

			while (*p == ' ' || *p == '\t')
				p++;
			r = q = index(p, ',');
			if (q == NULL) {
				(void) fprintf(stderr,
				    "innetgr: syntax error in /etc/netgroup\r\n");
				listp--;
				return(0);
			}
			if (p == q || name == NULL)
				match++;
			else {
				while (*(q-1) == ' ' || *(q-1) == '\t')
					q--;
				if (strncmp(name, p, q-p) == 0)
					match++;
			}
			p = r+1;

			while (*p == ' ' || *p == '\t')
				p++;
			r = q = index(p, ')');
			if (q == NULL) {
				(void) fprintf(stderr,
				    "innetgr: syntax error in /etc/netgroup\r\n");
				listp--;
				return(0);
			}
			if (p == q || domain == NULL)
				match++;
			else {
				while (*(q-1) == ' ' || *(q-1) == '\t')
					q--;
				if (strncmp(domain, p, q-p) == 0)
					match++;
			}
			p = r+1;
			if (match == 3) {
				free(val);
				listp--;
				return 1;
			}
		}
		else {
			q = any(p, " \t\n#");
			if (q && *q == '#')
				break;
			if (q)
				*q = 0;
			if (doit(p)) {
				free(val);
				listp--;
				return 1;
			}
			if (q)
				*q = ' ';
		}
		p = any(p, " \t");
	}
	free(val);
	listp--;
	return 0;
}

/* 
 * scans cp, looking for a match with any character
 * in match.  Returns pointer to place in cp that matched
 * (or NULL if no match)
 */
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
#endif RISCOS

/*
 * return 1 if "what" is in the comma-separated, newline-terminated "list"
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




#ifndef RISCOS
/*
 * Lookup a host or user name in a yp map.  Set result to 1 if group in the 
 * lookup-up list of groups. Return 1 if the map was found.
 */
static
lookup(map,group,name,domain,res)
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
#endif RISCOS
