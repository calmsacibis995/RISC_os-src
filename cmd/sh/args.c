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
#ident	"$Header: args.c,v 1.7.2.3 90/05/09 18:55:16 wje Exp $"

/*	Copyright (c) 1984 AT&T	*/
/*	  All Rights Reserved  	*/

/*	THIS IS UNPUBLISHED PROPRIETARY SOURCE CODE OF AT&T	*/
/*	The copyright notice above does not evidence any   	*/
/*	actual or intended publication of such source code.	*/


/*
 *	UNIX shell
 */

#include	"defs.h"

static struct dolnod *copyargs();
static void freedolh();	/* DAG -- bug fix (no value returned) */
extern struct dolnod *freeargs();
static struct dolnod *dolh;

/* Used to save outermost positional parameters */
static struct dolnod *globdolh;
static char **globdolv;
static int globdolc;

#ifdef RISCOS
char	flagadr[19];
#else RISCOS
#if BRL
#if JOBS
char	flagadr[17];
#else JOBS
char	flagadr[16];
#endif JOBS
#else BRL	/* !BRL */
#if JOBS
char	flagadr[15];
#else JOBS
char	flagadr[14];
#endif JOBS
#endif BRL
#endif RISCOS

char	flagchar[] =
{
	'x',
	'n', 
	'v', 
	't', 
	STDFLG, 
	'i', 
	'e', 
	'r', 
	'k', 
	'u', 
	'h',
	'f',
	'a',
#if BRL
	'I',
	'E',
#endif
#if JOBS
	'J',
#endif
#ifdef RISCOS
	'T',			/* tilde substitution */
	'B',			/* echo -n and other BSD-isms available */
#endif RISCOS
	 0
};

long	flagval[]  =
{
	execpr,	
	noexec,	
	readpr,	
	oneflg,	
	stdflg,	
	intflg,	
	errflg,	
	rshflg,	
	keyflg,	
	setflg,	
	hashflg,
	nofngflg,
	exportflg,
#if BRL
	infoflg,
	noeotflg,
#endif
#if JOBS
	jobflg,
#endif
#ifdef RISCOS
	tilde_sub_flg,
	echo_dash_n_flg,
#endif RISCOS
	  0
};

/* ========	option handling	======== */


options(argc,argv)
	char	**argv;
	int	argc;
{
	register char *cp;
	register char **argp = argv;
	register char *flagc;
	char	*flagp;

	if (argc > 1 && *argp[1] == '-')
	{
		/*
		 * if first argument is "--" then options are not
		 * to be changed. Fix for problems getting 
		 * $1 starting with a "-"
		 */

		cp = argp[1];
		if (cp[1] == '-')
		{
			argp[1] = argp[0];
			argc--;
			return(argc);
		}
		if (cp[1] == '\0')
			flags &= ~(execpr|readpr);

		/*
		 * Step along 'flagchar[]' looking for matches.
		 * 'sicr' are not legal with 'set' command.
		 */

		while (*++cp)
		{
			flagc = flagchar;
			while (*flagc && *flagc != *cp)
				flagc++;
			if (*cp == *flagc)
			{
				if (eq(argv[0], "set") && any(*cp, "sicr"))
					failed(argv[1], badopt);
				else
				{
#if JOBS
					if (*cp == 'J')
						j_init();
#endif
					flags |= flagval[flagc-flagchar];
					if (flags & errflg)
						eflag = errflg;
				}
			}
			else if (*cp == 'c' && argc > 2 && comdiv == 0)
			{
				comdiv = argp[2];
				argp[1] = argp[0];
				argp++;
				argc--;
			}
			else
				failed(argv[1],badopt);
		}
		argp[1] = argp[0];
		argc--;
	}
	else if (argc > 1 && *argp[1] == '+')	/* unset flags x, k, t, n, v, e, u */
						/* or any added BRL/JOBS flags */
	{
		cp = argp[1];
		while (*++cp)
		{
			flagc = flagchar;
			while (*flagc && *flagc != *cp)
				flagc++;
			/*
			 * step through flags
			 */
			if (!any(*cp, "sicr") && *cp == *flagc)
			{
				/*
				 * only turn off if already on
				 */
				if ((flags & flagval[flagc-flagchar]))
				{
#if JOBS
					if (*cp != 'J' || !j_finish(FALSE))
#endif
					flags &= ~(flagval[flagc-flagchar]);
					if (*cp == 'e')
						eflag = 0;
				}
			}
		}
		argp[1] = argp[0];
		argc--;
	}
	/*
	 * set up $-
	 */
	flagp = flagadr;
	if (flags)
	{
		flagc = flagchar;
		while (*flagc)
		{
			if (flags & flagval[flagc-flagchar])
				*flagp++ = *flagc;
			flagc++;
		}
	}
	*flagp = 0;
	return(argc);
}

/*
 * sets up positional parameters
 */
setargs(argi)
	char	*argi[];
{
	register char **argp = argi;	/* count args */
	register int argn = 0;

	while (Rcheat(*argp++) != ENDARGS)
		argn++;
	/*
	 * free old ones unless on for loop chain
	 */
	freedolh();
	dolh = copyargs(argi, argn);
	dolc = argn - 1;
}


static void	/* DAG -- bug fix (no value returned) */
freedolh()
{
	register char **argp;
	register struct dolnod *argblk;

	if (argblk = dolh)
	{
		if ((--argblk->doluse) == 0)
		{
			for (argp = argblk->dolarg; Rcheat(*argp) != ENDARGS; argp++)
				free(*argp);
			free(argblk);
		}
	}
}

struct dolnod *
freeargs(blk)
	struct dolnod *blk;
{
	register char **argp;
	register struct dolnod *argr = 0;
	register struct dolnod *argblk;
	int cnt;

	if (argblk = blk)
	{
		argr = argblk->dolnxt;
		cnt  = --argblk->doluse;

		if (argblk == dolh)
		{
			if (cnt == 1)
				return(argr);
			else
				return(argblk);
		}
		else
		{			
			if (cnt == 0)
			{
				for (argp = argblk->dolarg; Rcheat(*argp) != ENDARGS; argp++)
					free(*argp);
				free(argblk);
			}
		}
	}
	return(argr);
}

static struct dolnod *
copyargs(from, n)
	char	*from[];
{
	register struct dolnod *np = (struct dolnod *)alloc(sizeof(char**) * n + 3 * BYTESPERWORD);
	register char **fp = from;
	register char **pp;

	np -> dolnxt = 0;
	np->doluse = 1;	/* use count */
	pp = np->dolarg;
	dolv = pp;
	
	while (n--)
		*pp++ = make(*fp++);
	*pp++ = ENDARGS;
	return(np);
}


struct dolnod *
clean_args(blk)
	struct dolnod *blk;
{
	register char **argp;
	register struct dolnod *argr = 0;
	register struct dolnod *argblk;

	if (argblk = blk)
	{
		argr = argblk->dolnxt;

		if (argblk == dolh)
			argblk->doluse = 1;
		else
		{
			for (argp = argblk->dolarg; Rcheat(*argp) != ENDARGS; argp++)
				free(*argp);
			free(argblk);
		}
	}
	return(argr);
}

clearup()
{
	/*
	 * force `for' $* lists to go away
	 */
	if(globdolv) {
		dolv = globdolv;
		dolc = globdolc;
		dolh = globdolh;
		globdolv = 0;
		globdolc = 0;
		globdolh = 0;
	}
	while (argfor = clean_args(argfor))
		;
	/*
	 * clean up io files
	 */
	while (pop())
		;

	/* 
	 * Clean up pipe file descriptor
	 * from command substitution
	 */
	
	if(savpipe != -1) {
		close(savpipe);
		savpipe = -1;
	}

	/*
	 * clean up tmp files
	*/
	while (poptemp())
		;
}

/* Save positiional parameters before outermost function invocation 
 * in case we are interrupted. 
 * Increment use count for current positional parameters so that they aren't thrown
 * away. 
 */

struct dolnod *savargs(funcnt)
int funcnt;
{
	if (!funcnt) {
		globdolh = dolh;
		globdolv = dolv;
		globdolc = dolc;
	}
	useargs();
	return(dolh);
}

/* After function invocation, free positional parameters, 
 * restore old positional parameters, and restore
 * use count.
 */

void restorargs(olddolh, funcnt)
struct dolnod *olddolh;
{
	if(argfor != olddolh)
		while ((argfor = clean_args(argfor)) != olddolh);
	freedolh();
	dolh = olddolh;
	if(dolh)
		dolh -> doluse++; /* increment use count so arguments aren't freed */
	argfor = freeargs(dolh);
	if(funcnt == 1) { 
		globdolh = 0;
		globdolv = 0;
		globdolc = 0;
	}
}

struct dolnod *
useargs()
{
	if (dolh)
	{
		if (dolh->doluse++ == 1)
		{
			dolh->dolnxt = argfor;
			argfor = dolh;
		}
	}
	return(dolh);
}
