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
#ident	"$Header: yp_gnetgr.c,v 1.2.1.3 90/05/07 21:30:30 wje Exp $"

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

#define MAXGROUPLEN 1024

/* 
 * access members of a netgroup
 */

static struct grouplist {		/* also used by pwlib */
	char	*gl_machine;
	char	*gl_name;
	char	*gl_domain;
	struct	grouplist *gl_nxt;
} *grouplist, *grlist;

extern char *malloc();
extern char *strcpy(),*strncpy();

char *strpbrk(), *index();

static struct list {			/* list of names to check for loops */
	char *name;
	struct list *nxt;
};

static	void doit();
static	char *fill();
static	char *match();

static	char *domain;
static	char *oldgrp;

char	NETGROUP[] = "netgroup";

yp_setnetgrent(grp)
	char *grp;
{
	
	if (oldgrp == NULL)
		oldgrp = (char *)calloc(1,256);
	if (strcmp(oldgrp, grp) == 0)
		grlist = grouplist;
	else {
		if (grouplist != NULL)
			yp_endnetgrent();
		doit(grp, (struct list *) NULL);
		grlist = grouplist;
		(void) strcpy(oldgrp, grp);
	}
}

yp_endnetgrent(a)
	int a;
{
	register struct grouplist *gl;
	
	for (gl = grouplist; gl != NULL; gl = gl->gl_nxt) {
		if (gl->gl_name)
			free(gl->gl_name);
		if (gl->gl_domain)
			free(gl->gl_domain);
		if (gl->gl_machine)
			free(gl->gl_machine);
		free((char *) gl);
	}
	grouplist = NULL;
	grlist = NULL;
	if (oldgrp) {
		free(oldgrp);
		oldgrp = 0;
	}
}

yp_getnetgrent(req)
	Vis_gnetgrent_req *req;
{
	char **machinep = req->r_machinep, **namep = req->r_namep, **domainp = req->r_domainp;

	if (grlist == 0)
		return (0);
	*machinep = grlist->gl_machine;
	*namep = grlist->gl_name;
	*domainp = grlist->gl_domain;
	grlist = grlist->gl_nxt;
	return (1);
}

/*
 * recursive function to find the members of netgroup "group". "list" is
 * the path followed through the netgroups so far, to check for cycles.
 */
static void
doit(group,list)
	char *group;
	struct list *list;
{
	register char *p, *q;
	register struct list *ls;
	struct list this_group;
	char *val;
	struct grouplist *gpls;
 
	/*
	 * check for non-existing groups
	 */
	if ((val = match(group)) == NULL)
		return;
 
	/*
	 * check for cycles
	 */
	for (ls = list; ls != NULL; ls = ls->nxt)
		if (strcmp(ls->name, group) == 0) {
#ifdef RISCOS
			syslog(LOG_ERR,
			    "yp_gnetgr: Cycle detected in /etc/netgroup: %s.\n", group);
#else
			(void) fprintf(stderr,
			    "Cycle detected in /etc/netgroup: %s.\n", group);
#endif
			return;
		}
 
	ls = &this_group;
	ls->name = group;
	ls->nxt = list;
	list = ls;
    
	p = val;
	while (p != NULL) {
		while (*p == ' ' || *p == '\t')
			p++;
		if (*p == 0 || *p =='#')
			break;
		if (*p == '(') {
			gpls = (struct grouplist *)
			    malloc(sizeof(struct grouplist));
			p++;
			if (!(p = fill(p,&gpls->gl_machine,',')))
				goto syntax_error;
			if (!(p = fill(p,&gpls->gl_name,',')))
				goto syntax_error;
			if (!(p = fill(p,&gpls->gl_domain,')')))
				goto syntax_error;
			gpls->gl_nxt = grouplist;
			grouplist = gpls;
		} else {
			q = strpbrk(p, " \t\n#");
			if (q && *q == '#')
				break;
			*q = 0;
			doit(p,list);
			*q = ' ';
		}
		p = strpbrk(p, " \t");
	}
	return;
 
syntax_error:
#ifdef RISCOS
	(void) syslog(LOG_ERR,"yp_gnetgr: syntax error in /etc/netgroup\n");
	(void) syslog(LOG_ERR,"yp_gnetgr: --- %s\n",val);
#else
	(void) fprintf(stderr,"syntax error in /etc/netgroup\n");
	(void) fprintf(stderr,"--- %s\n",val);
#endif
	return;
}

/*
 * Fill a buffer "target" selectively from buffer "start".
 * "termchar" terminates the information in start, and preceding
 * or trailing white space is ignored. The location just after the
 * terminating character is returned.  
 */
static char *
fill(start,target,termchar)
	char *start, **target, termchar;
{
	register char *p, *q; 
	char *r;
	unsigned size;
 
	for (p = start; *p == ' ' || *p == '\t'; p++)
		;
	r = index(p, termchar);
	if (r == NULL)
		return (NULL);
	if (p == r)
		*target = NULL;	
	else {
		for (q = r-1; *q == ' ' || *q == '\t'; q--)
			;
		size = q - p + 1;
		*target = malloc(size+1);
		(void) strncpy(*target,p,(int) size);
		(*target)[size] = 0;
	}
	return (r+1);
}

static char *
match(group)
	char *group;
{
	char *val;
	int vallen;

	if (domain == NULL)
		(void) usingypmap(&domain, NULL);
	if (yp_match(domain, NETGROUP, group, strlen(group), &val, &vallen))
		return (NULL);
	return (val);
}
