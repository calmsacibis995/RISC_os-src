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
#ident	"$Header: yp_getgrent.c,v 1.3.1.3.1.2 90/08/13 17:14:12 hawkes Exp $"

#if !defined(lint) && defined(SCCSIDS)
static char sccsid[] = 	"@(#)getgrent.c	1.7 88/05/11 4.0NFSSRC SMI"; /* from UCB 5.2 3/9/86 SMI 1.23 */
#endif

#include <stdio.h>
#include <grp.h>
#include <rpcsvc/ypclnt.h>
#include <netdb.h>

extern void rewind();
extern long strtol();
extern int strlen();
extern int strcmp();
extern int fclose();
extern char *strcpy();
extern char *calloc();
extern char *malloc();

void yp_setgrent(), yp_endgrent();
struct group *yp_getgrent();

#define MAXINT 0x7fffffff;
#define	MAXGRP	200

static struct _grjunk {
	char	*_domain;
	struct	group _NULLGP;
	FILE	*_grf;
	char	*_yp;
	int	_yplen;
	char	*_oldyp;
	int	_oldyplen;
	char	*_agr_mem[MAXGRP];
	struct list {
		char *name;
		struct list *nxt;
	} *_minuslist;
	struct	group _interpgp;
	char	_interpline[BUFSIZ+1];
} *_grjunk;

#define	domain	(_gr->_domain)
#define	NULLGP	(_gr->_NULLGP)
#define	grf	(_gr->_grf)
#define	yp	(_gr->_yp)
#define	yplen	(_gr->_yplen)
#define	oldyp	(_gr->_oldyp)
#define	oldyplen (_gr->_oldyplen)
#define	agr_mem	(_gr->_agr_mem)
#define	minuslist (_gr->_minuslist)
#define	interpgp (_gr->_interpgp)
#define	interpline (_gr->_interpline)

static char GROUP[] = "/etc/group";
static int usingyellow;               /* are yellow pages up? */
static int firsttime = 0;
static struct group *interpret();
static struct group *interpretwithsave();
static struct group *save();
static struct group *getnamefromyellow();
static struct group *getgidfromyellow();

struct _grjunk *
setgrjunk()
{
	if (_grjunk == 0) 
		_grjunk = (struct _grjunk *)calloc(1, sizeof (struct _grjunk));
	return(_grjunk);
};

struct group *
yp_getgrgid(gid)
	register gid;
{
	register struct _grjunk *_gr = setgrjunk();
	struct group *p;
	char line[BUFSIZ+1];

	yp_setgrent();
	if (_gr == 0)
		return (0);
	/* even though we KNOW we should be usingyellow, maybe YP is down*/
	/* it is a fielders choice on whether to call ht_getgrent here
	 * I was worried about indefinite recursion if we had yp "+" entries
	 * and yp was down, so just call the yp_get routine
	 */
	if (!usingyellow)
	{
		while( (p = yp_getgrent()) && p->gr_gid != gid);
		yp_endgrent();
		return (p);
	}
	if ( !grf)
		return NULL;
	while (fgets(line, BUFSIZ, grf) != NULL) {
		if ((p = interpret(line, strlen(line))) == NULL)
			continue;
		if (matchgid(line, &p, gid)) {
			yp_endgrent();
			return p;
		}
	}
	yp_endgrent();
	return NULL;
}

struct group *
yp_getgrnam(name)
	register char *name;
{
	register struct _grjunk *_gr = setgrjunk();
	struct group *p;
	char line[BUFSIZ+1];

	if (_gr == 0)
		return NULL;
	yp_setgrent();
	/* see comments above on yp_getgrgid */
	if (!usingyellow)
	{
		while( (p = yp_getgrent()) && strcmp(p->gr_name,name) );
		yp_endgrent();
		return(p);
	}
	if (!grf)
		return NULL;
	while (fgets(line, BUFSIZ, grf) != NULL) {
		if ((p = interpret(line, strlen(line))) == NULL)
			continue;
		if (matchname(line, &p, name)) {
			yp_endgrent();
			return p;
		}
	}
	yp_endgrent();
	return NULL;
}

void
yp_setgrent(a)
	int a;
{
	register struct _grjunk *_gr = setgrjunk();

	if (_gr == 0)
		return;
	if (domain == NULL) {
		(void) usingypmap(&domain, NULL);
	}
	if (domain == NULL) 
		usingyellow = 0;
	else {
		firsttime = 1;
		usingyellow = !yp_bind(domain);
        }
	if(!grf)
		grf = fopen(GROUP, "r");
	else
		rewind(grf);
	if (yp)
		free(yp);
	yp = NULL;
	freeminuslist();
}

void
yp_endgrent(a)
	int a;
{
	register struct _grjunk *_gr = setgrjunk();

	if (_gr == 0)
		return;
	if(grf) {
		(void) fclose(grf);
		grf = NULL;
	}
	if (yp)
		free(yp);
	yp = NULL;
	freeminuslist();
}

struct group *
yp_fgetgrent(f)
	FILE *f;
{
	char line1[BUFSIZ+1];

	if(fgets(line1, BUFSIZ, f) == NULL)
		return(NULL);
	return (interpret(line1, strlen(line1)));
}

static char *
grskip(p,c)
	register char *p;
	register c;
{
	while(*p && *p != c && *p != '\n') ++p;
	if (*p == '\n')
		*p = '\0';
	else if (*p != '\0')
		*p++ = '\0';
	return(p);
}

struct group *
yp_getgrent(a)
	int a;
{
	register struct _grjunk *_gr = setgrjunk();
	char line1[BUFSIZ+1];
	static struct group *savegp;
	struct group *gp;

	if (_gr == 0)
		return(0);
	if (domain == NULL) {
		(void) usingypmap(&domain, NULL);
	}
	if (domain == NULL) 
		usingyellow = 0;
        else if (firsttime == 0) {
		firsttime = 1;
		usingyellow = !yp_bind(domain);
	}
	if(!grf && !(grf = fopen(GROUP, "r")))
		return(NULL);
  again:
	if (yp) {
		gp = interpretwithsave(yp, yplen, savegp);
		free(yp);
		if (gp == NULL)
			return(NULL);
		getnextfromyellow();
		if (onminuslist(gp))
			goto again;
		else
			return gp;
	}
	else if (fgets(line1, BUFSIZ, grf) == NULL)
		return(NULL);
	if ((gp = interpret(line1, strlen(line1))) == NULL)
		return(NULL);
	if (!usingyellow)
	{
	    if ((gp->gr_name[0] == '+') || (gp->gr_name[0] == '-'))
	    {
		return NULL;  /* with yp down, can't handle + or -.  Return
			       * NULL so as not to crud up ls -l output
			       * saying the group name is +
			       */
	    }
	    else
	    {
		return gp; /* this is an OK line, didn't need yp */
	    }
	}
	else
	    switch(line1[0]) {
		case '+':
			if (strcmp(gp->gr_name, "+") == 0) {
				getfirstfromyellow();
				savegp = save(gp);
				goto again;
			}
			/* 
			 * else look up this entry in yellow pages
			 */
			savegp = save(gp);
			gp = getnamefromyellow(gp->gr_name+1, savegp);
			if (gp == NULL)
				goto again;
			else if (onminuslist(gp))
				goto again;
			else
				return gp;
			break;
		case '-':
			addtominuslist(gp->gr_name+1);
			goto again;
			break;
		default:
			if (onminuslist(gp))
				goto again;
			return gp;
			break;
	}
	return (NULL);
}

static struct group *
interpret(val, len)
	char *val;
{
	register struct _grjunk *_gr = setgrjunk();
	register char *p, **q;
	char *end;
	long x;
	register int ypentry;

	if (_gr == 0)
		return (0);
	strncpy(interpline, val, len);
	p = interpline;
	interpline[len] = '\n';
	interpline[len+1] = 0;

	/*
 	 * Set "ypentry" if this entry references the Yellow Pages;
	 * if so, null GIDs are allowed (because they will be filled in
	 * from the matching Yellow Pages entry).
	 */
	ypentry = (*p == '+');

	interpgp.gr_name = p;
	p = grskip(p,':');
	interpgp.gr_passwd = p;
	p = grskip(p,':');
	if (*p == ':' && !ypentry)
		/* check for non-null gid */
		return (NULL);
	x = strtol(p, &end, 10);	
	p = end;
	if (*p++ != ':' && !ypentry)
		/* check for numeric value - must have stopped on the colon */
		return (NULL);
	interpgp.gr_gid = x;
	interpgp.gr_mem = agr_mem;
	(void) grskip(p,'\n');
	q = agr_mem;
	while(*p){
		if (q < &agr_mem[MAXGRP-1])
			*q++ = p;
		p = grskip(p,',');
	}
	*q = NULL;
	return(&interpgp);
}

static
freeminuslist() {
	register struct _grjunk *_gr = _grjunk;
	struct list *ls;

	if (_gr == 0)
		return;
	for (ls = minuslist; ls != NULL; ls = ls->nxt) {
		free(ls->name);
		free(ls);
	}
	minuslist = NULL;
}

static struct group *
interpretwithsave(val, len, savegp)
	char *val;
	struct group *savegp;
{
	register struct _grjunk *_gr = _grjunk;
	struct group *gp;

	if (_gr == 0)
		return NULL;
	if ((gp = interpret(val, len)) == NULL)
		return NULL;
	if (savegp->gr_passwd && *savegp->gr_passwd)
		gp->gr_passwd =  savegp->gr_passwd;
	if (savegp->gr_mem && *savegp->gr_mem)
		gp->gr_mem = savegp->gr_mem;
	return gp;
}

static
onminuslist(gp)
	struct group *gp;
{
	register struct _grjunk *_gr = _grjunk;
	struct list *ls;
	register char *nm;

	if (_gr == 0)
		return 0;
	nm = gp->gr_name;
	for (ls = minuslist; ls != NULL; ls = ls->nxt)
		if (strcmp(ls->name, nm) == 0)
			return 1;
	return 0;
}

static
getnextfromyellow()
{
	register struct _grjunk *_gr = _grjunk;
	int reason;
	char *key = NULL;
	int keylen;

	if (_gr == 0)
		return;
	if (reason = yp_next(domain, "group.byname",
	    oldyp, oldyplen, &key, &keylen,
	    &yp, &yplen)) {
#ifdef DEBUG
fprintf(stderr, "reason yp_next failed is %d\n", reason);
#endif
		yp = NULL;
	}
	if (oldyp)
		free(oldyp);
	oldyp = key;
	oldyplen = keylen;
}

static
getfirstfromyellow()
{
	register struct _grjunk *_gr = _grjunk;
	int reason;
	char *key = NULL;
	int keylen;

	if (_gr == 0)
		return;
	if (reason =  yp_first(domain, "group.byname",
	    &key, &keylen, &yp, &yplen)) {
#ifdef DEBUG
fprintf(stderr, "reason yp_first failed is %d\n", reason);
#endif
		yp = NULL;
	}
	if (oldyp)
		free(oldyp);
	oldyp = key;
	oldyplen = keylen;
}

static struct group *
getnamefromyellow(name, savegp)
	char *name;
	struct group *savegp;
{
	register struct _grjunk *_gr = _grjunk;
	struct group *gp;
	int reason;
	char *val;
	int vallen;

	if (_gr == 0)
		return NULL;
	if (reason = yp_match(domain, "group.byname",
	    name, strlen(name), &val, &vallen)) {
#ifdef DEBUG
fprintf(stderr, "reason yp_next failed is %d\n", reason);
#endif
		return NULL;
	}
	else {
		gp = interpret(val, vallen);
		free(val);
		if (gp == NULL)
			return NULL;
		if (savegp->gr_passwd && *savegp->gr_passwd)
			gp->gr_passwd =  savegp->gr_passwd;
		if (savegp->gr_mem && *savegp->gr_mem)
			gp->gr_mem = savegp->gr_mem;
		return gp;
	}
}

static
addtominuslist(name)
	char *name;
{
	register struct _grjunk *_gr = _grjunk;
	struct list *ls;
	char *buf;
	
	if (_gr == 0)
		return;
	ls = (struct list *)malloc(sizeof(struct list));
	buf = (char *)malloc(strlen(name) + 1);
	(void) strcpy(buf, name);
	ls->name = buf;
	ls->nxt = minuslist;
	minuslist = ls;
}

/* 
 * save away psswd, gr_mem fields, which are the only
 * ones which can be specified in a local + entry to override the
 * value in the yellow pages
 */
static struct group *
save(gp)
	struct group *gp;
{
	register struct _grjunk *_gr = _grjunk;
	static struct group *sv = 0;
	char *gr_mem[MAXGRP];	
	char **av, **q = 0;
	int lnth;
	
	if (_gr == 0)
		return 0;
	/* 
	 * free up stuff from last time around
	 */
	if (sv) {
		for (av = sv->gr_mem; *av != NULL; av++) {
#ifdef BROKEN
			if (q >= &gr_mem[MAXGRP-1] || *q == 0)
				break;
			free(*q);
			q++;
#endif
			/* all that stuff with q and gr_mem is hokey
			 * gr_mem and q are local variables which
			 * munched by other calls
			 * gr_mem is copied to sv->gr_mem after malloc-ing
			 * so use it instead.
			 */
			 free(*av);
		}
		free(sv->gr_passwd);
		free(sv->gr_mem);
		free(sv);
	}
	sv = (struct group *)malloc(sizeof(struct group));
	sv->gr_passwd = (char *)malloc(strlen(gp->gr_passwd) + 1);
	(void) strcpy(sv->gr_passwd, gp->gr_passwd);

	q = gr_mem;
	for (av = gp->gr_mem; *av != NULL; av++) {
		if (q >= &gr_mem[MAXGRP-1])
			break;
		*q = (char *)malloc(strlen(*av) + 1);
		(void) strcpy(*q, *av);
		q++;
	}
	*q = 0;
	lnth = (sizeof (char *)) * (q - gr_mem + 1);
	sv->gr_mem = (char **)malloc(lnth);
	bcopy((char *)gr_mem, (char *)sv->gr_mem, lnth);
	return sv;
}

static
matchname(line1, gpp, name)
	char line1[];
	struct group **gpp;
	char *name;
{
	register struct _grjunk *_gr = _grjunk;
	struct group *savegp;
	struct group *gp = *gpp;

	if (_gr == 0)
		return 0;
	switch(line1[0]) {
		case '+':
			if (strcmp(gp->gr_name, "+") == 0) {
				savegp = save(gp);
				gp = getnamefromyellow(name, savegp);
				if (gp) {
					*gpp = gp;
					return 1;
				}
				else
					return 0;
			}
			if (strcmp(gp->gr_name+1, name) == 0) {
				savegp = save(gp);
				gp = getnamefromyellow(gp->gr_name+1, savegp);
				if (gp) {
					*gpp = gp;
					return 1;
				}
				else
					return 0;
			}
			break;
		case '-':
			if (strcmp(gp->gr_name+1, name) == 0) {
				*gpp = NULL;
				return 1;
			}
			break;
		default:
			if (strcmp(gp->gr_name, name) == 0)
				return 1;
	}
	return 0;
}

static
matchgid(line1, gpp, gid)
        char line1[];
        struct group **gpp;
{
        register struct _grjunk *_gr = _grjunk;
        struct group *savegp;
        struct group *gp = *gpp;
 
        if (_gr == 0)
                return 0;
        switch(line1[0]) {
                case '+':
                        if (strcmp(gp->gr_name, "+") == 0) {
                                savegp = save(gp);
                                gp = getgidfromyellow(gid, savegp);
                                if (gp) {
                                        *gpp = gp;
                                        return 1;
                                }
                                else
                                        return 0;
                        }
                        savegp = save(gp);
                        gp = getnamefromyellow(gp->gr_name+1, savegp);                        if (gp && gp->gr_gid == gid) {
                                *gpp = gp;
                                return 1;
                        }
                        else
                                return 0;
                        break;
                case '-':
                        if (gid == gidof(gp->gr_name+1)) {
                                *gpp = NULL;
                                return 1;
                        }
                        break;
                default: 
                        if (gp->gr_gid == gid)
                                return 1;
        }
        return 0;
}        
 
static
gidof(name)
        char *name;
{
        register struct _grjunk *_gr = _grjunk;
        struct group *gp;

        if (_gr == 0)     
                return 0;
        gp = getnamefromyellow(name, &NULLGP);
        if (gp)
                return gp->gr_gid;
        else
                return MAXINT;
}
 
static struct group *
getgidfromyellow(gid, savegp)
        int gid;
        struct group *savegp;
{
        register struct _grjunk *_gr = _grjunk;
        struct group *gp;
        int reason;
        char *val;
        int vallen;      
        char gidstr[20];

        if (_gr == 0)    
                return 0;
        sprintf(gidstr, "%d", gid);
        if (reason = yp_match(domain, "group.bygid",
            gidstr, strlen(gidstr), &val, &vallen)) {
#ifdef DEBUG
fprintf(stderr, "reason yp_next failed is %d\n", reason);
#endif
                return NULL;
        }
        else {
                gp = interpret(val, vallen);
                free(val);
                if (gp == NULL)
                        return NULL;
                if (savegp->gr_passwd && *savegp->gr_passwd)
                        gp->gr_passwd =  savegp->gr_passwd;
                if (savegp->gr_mem && *savegp->gr_mem)
                        gp->gr_mem = savegp->gr_mem;
                return gp;
        }
}
