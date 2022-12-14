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
#ident	"$Header: name.c,v 1.7.2.2 90/05/09 18:59:10 wje Exp $"

/*	Copyright (c) 1984 AT&T	*/
/*	  All Rights Reserved  	*/

/*	THIS IS UNPUBLISHED PROPRIETARY SOURCE CODE OF AT&T	*/
/*	The copyright notice above does not evidence any   	*/
/*	actual or intended publication of such source code.	*/

/*
 * UNIX shell
 */

#include	"defs.h"

extern BOOL	chkid();
extern char	*simple();
extern int	mailchk;

#if BRL
struct namnod timenod =	/* for TIMEOUT */
{
	(struct namnod *)NIL,
#ifdef pyr
	&univnod;	/* very lopsided tree, but who cares */
#else
	(struct namnod *)NIL,
#endif
	timename
};
#endif
#ifdef pyr
struct namnod univnod =	/* for UNIVERSE */
{
	(struct namnod *)NIL,
	(struct namnod *)NIL,
	univname
};
#endif
struct namnod ps2nod =
{
	(struct namnod *)NIL,
	&acctnod,
	ps2name
};
struct namnod cdpnod = 
{
	(struct namnod *)NIL,
	(struct namnod *)NIL,
	cdpname
};
struct namnod pathnod =
{
	&mailpnod,
	(struct namnod *)NIL,
	pathname
};
struct namnod ifsnod =
{
	&homenod,
	&mailnod,
	ifsname
};
struct namnod ps1nod =
{
	&pathnod,
	&ps2nod,
	ps1name
};
struct namnod homenod =
{
	&cdpnod,
	(struct namnod *)NIL,
	homename
};
struct namnod mailnod =
{
	(struct namnod *)NIL,
	(struct namnod *)NIL,
	mailname
};
struct namnod mchknod =
{
	&ifsnod,
	&ps1nod,
	mchkname
};
struct namnod acctnod =
{
	(struct namnod *)NIL,
#if BRL
	&timenod,	/* lopsided tree, but who cares */
#else
#ifdef pyr
	&univnod,	/* lopsided tree, but who cares */
#else
	(struct namnod *)NIL,
#endif
#endif
	acctname
};
struct namnod mailpnod =
{
	(struct namnod *)NIL,
	(struct namnod *)NIL,
	mailpname
};


struct namnod *namep = &mchknod;

/* ========	variable and string handling	======== */

syslook(w, syswds, n)
	register char *w;
	register struct sysnod syswds[];
	int n;
{
	int	low;
	int	high;
	int	mid;
#if gould
	int	cond;	/* DAG -- avoid optimizer bug */
#else
	register int cond;
#endif

	if (w == 0 || *w == 0)
		return(0);

	low = 0;
	high = n - 1;

	while (low <= high)
	{
		mid = (low + high) / 2;

		if ((cond = cf(w, syswds[mid].sysnam)) < 0)
			high = mid - 1;
		else if (cond > 0)
			low = mid + 1;
		else
			return(syswds[mid].sysval);
	}
	return(0);
}

setlist(arg, xp)
register struct argnod *arg;
int	xp;
{
	if (flags & exportflg)
		xp |= N_EXPORT;

	while (arg)
	{
		register char *s = mactrim(arg->argval);
		setname(s, xp);
		arg = arg->argnxt;
		if (flags & execpr)
		{
			prs(s);
			if (arg)
				blank();
			else
				newline();
		}
	}
}


setname(argi, xp)	/* does parameter assignments */
char	*argi;
int	xp;
{
	register char *argscan = argi;
	register struct namnod *n;
#if BRL
	int timo;	/* TIMEOUT flag and value */
#endif

	if (letter(*argscan))
	{
		while (alphanum(*argscan))
			argscan++;

		if (*argscan == '=')
		{
			*argscan = 0;	/* make name a cohesive string */

			n = lookup(argi);
#if BRL
			timo = n == &timenod;
			if (timo && (flags&rshflg))
				failed(argi, restricted);
#endif
			*argscan++ = '=';
			attrib(n, xp);
#if BRL
			if (timo)
				while (!stimeout(argscan))
					argscan = userid ? deftimout : dsutimout;
#endif
			if (xp & N_ENVNAM)
				n->namenv = n->namval = argscan;
			else
				assign(n, argscan);
			return;
		}
	}
	failed(argi, notid);
}

#if BRL
int
stimeout(str)
	char	*str;
{
	register char	*cp = str;
	register int	val;

	for (val = 0; *cp; cp++)
	{
		if (!digit( *cp ))
			goto error;
		val = val*10 + (*cp-'0');
		if (val < 0 || val > 077777/60)
			goto error;
	}
	if (cp != str)
	{
		timeout = val * 60;
		return 1;
	}
	else
	{
    error:
		prs( badtimeout );
		return 0;
	}
}
#endif

replace(a, v)
register char	**a;
char	*v;
{
	free(*a);
	*a = make(v);
}

dfault(n, v)
struct namnod *n;
char	*v;
{
	if (n->namval == 0)
		assign(n, v);
}

assign(n, v)
struct namnod *n;
char	*v;
{
	if (n->namflg & N_RDONLY)
		failed(n->namid, wtfailed);

#if defined(RISCOS) || !defined(RES)
	
	else if (flags & rshflg)
	{
		if (n == &pathnod || eq(n->namid,"SHELL"))
			failed(n->namid, restricted);
	}

#endif

	else if (n->namflg & N_FUNCTN)
	{
		func_unhash(n->namid);
		freefunc(n);

		n->namenv = 0;
		n->namflg = N_DEFAULT;
	}

	if (n == &mchknod)
	{
		mailchk = stoi(v);
	}
		
	replace(&n->namval, v);
	attrib(n, N_ENVCHG);

	if (n == &pathnod)
	{
		zaphash();
		set_dotpath();
		return;
	}
	
	if (flags & prompt)
	{
		if ((n == &mailpnod) || (n == &mailnod && mailpnod.namflg == N_DEFAULT))
			setmail(n->namval);
	}
}

readvar(names)
char	**names;
{
	extern long	lseek();	/* DAG -- bug fix (was missing) */
	struct fileblk	fb;
	register struct fileblk *f = &fb;
	register char	c;
	register int	rc = 0;
	struct namnod *n = lookup(*names++);	/* done now to avoid storage mess */
	char	*rel = (char *)relstak();

	push(f);
	initf(dup(0));

	if (lseek(0, 0L, 1) == -1L)	/* DAG */
		f->fsiz = 1;

	/*
	 * strip leading IFS characters
	 */
	while ((any((c = nextc(0)), ifsnod.namval)) && !(eolchar(c)))
			;
	for (;;)
	{
		if ((*names && any(c, ifsnod.namval)) || eolchar(c))
		{
			zerostak();
			assign(n, absstak(rel));
			setstak(rel);
			if (*names)
				n = lookup(*names++);
			else
				n = 0;
			if (eolchar(c))
			{
				break;
			}
			else		/* strip imbedded IFS characters */
			{
				while ((any((c = nextc(0)), ifsnod.namval)) &&
					!(eolchar(c)))
					;
			}
		}
		else
		{
			if (c == '\\')
				c = readc();
			pushstak(c);
			c = nextc(0);

			if (eolchar(c))
			{
				char *top = staktop;
			
				while (any(*(--top), ifsnod.namval))
					;
				staktop = top + 1;
			}


		}
	}
	while (n)
	{
		assign(n, nullstr);
		if (*names)
			n = lookup(*names++);
		else
			n = 0;
	}

	if (eof)
		rc = 1;
	lseek(0, (long)(f->fnxt - f->fend), 1);
	pop();
	return(rc);
}

assnum(p, i)
char	**p;
int	i;
{
	itos(i);
	replace(p, numbuf);
}

char *
make(v)
char	*v;
{
	register char	*p;

	if (v)
	{
		movstr(v, p = alloc(length(v)));
		return(p);
	}
	else
		return(0);
}


struct namnod *
lookup(nam)
	register char	*nam;
{
	register struct namnod *nscan = namep;
	register struct namnod **prev;
	int		LR;

	if (!chkid(nam))
		failed(nam, notid);
	
	while (nscan)
	{
		if ((LR = cf(nam, nscan->namid)) == 0)
			return(nscan);

		else if (LR < 0)
			prev = &(nscan->namlft);
		else
			prev = &(nscan->namrgt);
		nscan = *prev;
	}
	/*
	 * add name node
	 */
	nscan = (struct namnod *)alloc(sizeof *nscan);
	nscan->namlft = nscan->namrgt = (struct namnod *)NIL;
	nscan->namid = make(nam);
	nscan->namval = 0;
	nscan->namflg = N_DEFAULT;
	nscan->namenv = 0;

	return(*prev = nscan);
}

BOOL
chkid(nam)
char	*nam;
{
	register char *cp = nam;

	if (!letter(*cp))
		return(FALSE);
	else
	{
		while (*++cp)
		{
			if (!alphanum(*cp))
				return(FALSE);
		}
	}
	return(TRUE);
}

static int (*namfn)();
namscan(fn)
	int	(*fn)();
{
	namfn = fn;
	namwalk(namep);
}

static int
namwalk(np)
register struct namnod *np;
{
	if (np)
	{
		namwalk(np->namlft);
		(*namfn)(np);
		namwalk(np->namrgt);
	}
}

printnam(n)
struct namnod *n;
{
	register char	*s;

	sigchk();

	if (n->namflg & N_FUNCTN)
	{
		prs_buff(n->namid);
		prs_buff("(){\n");
		prf(n->namenv);
		prs_buff("\n}\n");
	}
	else if (s = n->namval)
	{
		prs_buff(n->namid);
		prc_buff('=');
		prs_buff(s);
		prc_buff(NL);
	}
}

static char *
staknam(n)
register struct namnod *n;
{
	register char	*p;

	p = movstr(n->namid, staktop);
	p = movstr("=", p);
	p = movstr(n->namval, p);
	return(getstak(p + 1 - (char *)(stakbot)));
}

static int namec;

exname(n)
	register struct namnod *n;
{
	register int 	flg = n->namflg;

	if (flg & N_ENVCHG)
	{

		if (flg & N_EXPORT)
		{
			free(n->namenv);
			n->namenv = make(n->namval);
		}
		else
		{
			free(n->namval);
			n->namval = make(n->namenv);
		}
	}

	
	if (!(flg & N_FUNCTN))
		n->namflg = N_DEFAULT;

	if (n->namval)
		namec++;

}

printro(n)
register struct namnod *n;
{
	if (n->namflg & N_RDONLY)
	{
		prs_buff(readonly);
		prc_buff(SP);
		prs_buff(n->namid);
		prc_buff(NL);
	}
}

printexp(n)
register struct namnod *n;
{
	if (n->namflg & N_EXPORT)
	{
		prs_buff(export);
		prc_buff(SP);
		prs_buff(n->namid);
		prc_buff(NL);
	}
}

setup_env()
{
	register char **e = environ;

	while (*e)
		setname(*e++, N_ENVNAM);
}


static char **argnam;

pushnam(n)
struct namnod *n;
{
	if (n->namval)
		*argnam++ = staknam(n);
}

char **
setenv()
{
	register char	**er;

	namec = 0;
	namscan(exname);

	argnam = er = (char **)getstak(namec * BYTESPERWORD + BYTESPERWORD);
	namscan(pushnam);
	*argnam++ = 0;
	return(er);
}

struct namnod *
findnam(nam)
	register char	*nam;
{
	register struct namnod *nscan = namep;
	int		LR;

	if (!chkid(nam))
		return(0);
	while (nscan)
	{
		if ((LR = cf(nam, nscan->namid)) == 0)
			return(nscan);
		else if (LR < 0)
			nscan = nscan->namlft;
		else
			nscan = nscan->namrgt;
	}
	return(0); 
}


unset_name(name)
	register char 	*name;
{
	register struct namnod	*n;

	if (n = findnam(name))
	{
		if (n->namflg & N_RDONLY)
			failed(name, wtfailed);

		if (n == &pathnod ||
		    n == &ifsnod ||
		    n == &ps1nod ||
		    n == &ps2nod ||
		    n == &mchknod)
		{
			failed(name, badunset);
		}

#if BRL
		if ( n == &timenod )
			timeout = 0;	/* disable */
#endif

#if defined(RISCOS) || ! defined(RES)

		if ((flags & rshflg) && eq(name, "SHELL"))
			failed(name, restricted);

#endif

		if (n->namflg & N_FUNCTN)
		{
			func_unhash(name);
			freefunc(n);
		}
		else
		{
			free(n->namval);
			free(n->namenv);
		}

		n->namval = n->namenv = 0;
		n->namflg = N_DEFAULT;

		if (flags & prompt)
		{
			if (n == &mailpnod)
				setmail(mailnod.namval);
			else if (n == &mailnod && mailpnod.namflg == N_DEFAULT)
				setmail(0);
		}
	}
}
