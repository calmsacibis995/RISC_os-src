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
#ident	"$Header: main.c,v 1.8.2.2 90/05/09 18:58:52 wje Exp $"

/*	Copyright (c) 1984 AT&T	*/
/*	  All Rights Reserved  	*/

/*	THIS IS UNPUBLISHED PROPRIETARY SOURCE CODE OF AT&T	*/
/*	The copyright notice above does not evidence any   	*/
/*	actual or intended publication of such source code.	*/

/*
 * UNIX shell
 */

#include	"defs.h"
#include	"sym.h"
#include	"timeout.h"
#include	<sys/types.h>
#include	<sys/stat.h>
#ifdef RISCOS
#include	<sys/termio.h>
#include	<sys/fcntl.h>
#else RISCOS
#ifdef RES	/* DAG -- conditionalize */
#include        "dup.h"

#include	<sgtty.h>
#else RES
#include	<fcntl.h>		/* DAG -- for defines */
#endif RES
#endif RISCOS

#if BRL || JOBS
BOOL		catcheof = FALSE;	/* not yet */
#endif
#if BRL && pdp11
char		*quomsg = bye;		/* normal EOT exit for quota */
#endif

static BOOL	beenhere = FALSE;
char		tmpout[20] = "/tmp/sh-";
struct fileblk	stdfile;
struct fileblk *standin = &stdfile;
int mailchk = 0;

static char	*mailp;
static long	*mod_time = 0;


#if vax
char **execargs = (char**)(0x7ffffffc);
#endif vax

#if defined(pdp11) && !defined(BRL)
#include	<execargs.h>
#endif

#if pdp11
char **execargs = (char**)(-2);
#endif


extern int	exfile();
extern char 	*simple();

#ifdef RISCOS
int	cur_getdtablesize; 	/* getdtablesize() value	    */

extern char *calloc();
int	cur_getdtablesize;
extern	int	getdtablesize();

#endif RISCOS


main(c, v, e)
int	c;
char	*v[];
char	*e[];
{
	register int	rflag = ttyflg;
	int		rsflag = 1;	/* local restricted flag */
	struct namnod	*n;
#if BRL
	char		*sim;

	argv0 = v[0];
	userid = geteuid();
#if pdp11
	{
		short	lwflags;

		if (_ltimes( &lwflags, sizeof lwflags ) == 0)
			loginsh = lwflags & 01;	/* child-of-init bit */
		else
			loginsh = getppid() == 1;
		if (argv0[0] != '-' || argv0[1] != 'S')
			loginsh = 0;
	}
#endif pdp11
#endif BRL
#if !defined(BRL) || !defined(pdp11)
	loginsh = argv0[0] == '-';
#endif


	stdsigs();

	/*
	 * initialise storage allocation
	 */

	stakbot = 0;
	addblok((unsigned)0);

#ifdef RISCOS
	cur_getdtablesize = getdtablesize();
	fdmap = (struct fdsave *) calloc(sizeof(struct fdsave),
					 cur_getdtablesize);
#endif RISCOS

	/*
	 * set names from userenv
	 */

	setup_env();

#ifndef BSD_SYS
	/*
	 * 'rsflag' is zero if SHELL variable is
	 *  set in environment and 
	 *  the simple file part of the value.
	 *  is rsh
	 */
	if (n = findnam("SHELL"))
	{
		if (!strcmp("rsh", simple(n->namval)))
			rsflag = 0;
	}
	/*
	 * a shell is also restricted if the simple name of argv(0) is
	 * rsh or -rsh in its simple name
	 */

#if defined(RISCOS) ||  !defined(RES)

	if (c > 0 && (!strcmp("rsh", simple(*v)) || !strcmp("-rsh", simple(*v))))
		rflag = 0;

#endif
#else BSD_SYS
	/*
	 * 'rsflag' is non-zero if SHELL variable is
	 *  set in environment and contains an'r' in
	 *  the simple file part of the value.
	 */
	if (n = findnam("SHELL"))
	{
		if (any('r', simple(n->namval)))
			rsflag = 0;
	}

	/*
	 * a shell is also restricted if argv(0) has
	 * an 'r' in its simple name
	 */

#ifndef RES

#if BRL
	if (c > 0 && (eq(sim = simple(argv0), rshell) || eq(sim, drshell)))
#else
	if (c > 0 && any('r', simple(*v)))
#endif
		rflag = 0;

#endif RES
#endif BSD_SYS

	hcreate();
	set_dotpath();

#if JOBS
#if defined(RISCOS) || defined(BSD_SYS) || defined(BRL) && !defined(pdp11)
	j_default_pg = getpid();	/* "Berkeley has a better idea" */
#endif
	j_original_pg = getpgrp();	/* j_default_pg also, sometimes */
#endif JOBS

	/*
	 * look for options
	 * dolc is $#
	 */
	dolc = options(c, v);

	if (dolc < 2)
	{
		flags |= stdflg;
		{
			register char *flagc = flagadr;

			while (*flagc)
				flagc++;
			*flagc++ = STDFLG;
			*flagc = 0;
		}
	}
	if ((flags & stdflg) == 0)
		dolc--;
	dolv = v + c - dolc;
	dolc--;

#if JOBS

	/* enable job control if simple part of argv[0] is "jsh" */

	if ((flags & jobflg) == 0 && c > 0 && 
	    (eq(jshstr, simple(argv0)) || 
	     eq("-jsh",argv0)) &&
	    comdiv == 0 /* set by options() */ &&
	    (flags & stdflg))
	{
		j_init();
		flags |= jobflg;
		{			/* append 'J' to $- string */
		register char *flagc = flagadr;

		while (*flagc)
			flagc++;
		*flagc++ = 'J';
		*flagc = 0;
		}
	}
#endif

	/*
	 * return here for shell file execution
	 * but not for parenthesis subshells
	 */
	setjmp(subshell);

	/*
	 * number of positional parameters
	 */
	replace(&cmdadr, dolv[0]);	/* cmdadr is $0 */

	/*
	 * set pidname '$$'
	 */
	assnum(&pidadr, getpid());

	/*
	 * set up temp file names
	 */
	settmp();

	/*
	 * default internal field separators - $IFS
	 */
	dfault(&ifsnod, sptbnl);

#if BRL
	dfault( &timenod, userid ? deftimout : dsutimout );
	while (!stimeout( timenod.namval ))
		replace( &timenod.namval, userid ? deftimout : dsutimout );
#endif

	dfault(&mchknod, MAILCHECK);
	mailchk = stoi(mchknod.namval);

#ifdef pyr
	/*
	 * find out current universe, initialize UNIVERSE
	 */
	if ( (cur_univ = setuniverse( U_UCB )) == -1 )
		cur_univ = U_UCB;		/* unknown, use default */
	else
		set_universe( cur_univ );	/* restore to original */

	/*
	 * force value, ignore whatever was in environment
	 */
	assign( &univnod, univ_name[cur_univ - 1] );
#endif
	
	/*	initialize OPTIND for getopt */
	n = lookup("OPTIND");
	assign(n, "1");

	if ((beenhere) == FALSE)	/* ? profile */	/* DAG -- only increment once */
	{
		++beenhere;		/* DAG */
		if (*(simple(cmdadr)) == '-')
		{			/* system profile */
#if BRL
			struct stat	statb;	/* needed for test */
#endif

#ifdef SECURITY
			if ( getegid() != getgid() )
				setgid( getgid() );
			if ( geteuid() != getuid() )
				setuid( getuid() );
#endif

#if defined(RISCOS) || !defined(RES)

			if ((input = pathopen(nullstr, sysprofile)) >= 0)
				exfile(rflag);		/* file exists */

#endif

			if ((input = pathopen(nullstr, profile)) >= 0)
#if BRL
			if (userid == 0
			 && (fstat(input, &statb) != 0 || statb.st_uid != 0))
				close(input);	/* protect super-user */
			else
#endif
			{
				exfile(rflag);
				flags &= ~ttyflg;
			}
		}
		if (rsflag == 0 || rflag == 0)
			flags |= rshflg;
		/*
		 * open input file if specified
		 */
		if (comdiv)
		{
			estabf(comdiv);
			input = -1;
		}
		else
		{
			input = ((flags & stdflg) ? 0 : chkopen(cmdadr));

#ifdef ACCT
			if (input != 0)
				preacct(cmdadr);
#endif
			comdiv--;
		}
	}
#if defined(pdp11) && !defined(BRL)
	else
		*execargs = (char *)dolv;	/* for `ps' cmd */
#endif
		
	exfile(0);
	done();
}

static int
exfile(prof)
BOOL	prof;
{
	long	mailtime = 0;	/* Must not be a register variable */
	long 	curtime = 0;
#ifndef BRL
	register int	userid;
#endif

	/*
	 * move input
	 */
	if (input > 0)
	{
		Ldup(input, INIO);
		input = INIO;
	}

#ifndef BRL	/* BRL is paranoid */
	userid = geteuid();
#endif

	/*
	 * decide whether interactive
	 */
	if ((flags & intflg) ||
	    ((flags&oneflg) == 0 &&
	    isatty(output) &&
	    isatty(input)) )
	    
	{
		dfault(&ps1nod, (userid ? stdprompt : supprompt));
		dfault(&ps2nod, readmsg);
		flags |= ttyflg | prompt;
		ignsig(SIGTERM);
		if (mailpnod.namflg != N_DEFAULT)
			setmail(mailpnod.namval);
		else
			setmail(mailnod.namval);
	}
	else
	{
		flags |= prof;
		flags &= ~prompt;
	}

	if (setjmp(errshell) && prof)
	{
		close(input);
		return;
	}
	/*
	 * error return here
	 */

	loopcnt = peekc = peekn = 0;
	fndef = 0;
	nohash = 0;
	iopend = 0;

	if (input >= 0)
		initf(input);
	/*
	 * command loop
	 */
	for (;;)
	{
		tdystak(0);
		stakchk();	/* may reduce sbrk */
		exitset();

		if ((flags & prompt) && standin->fstak == 0 && !eof)
		{

			if (mailp)
#if BRL
			if (*mailp)	/* don't call time() unnecessarily */
#endif
			{
				time(&curtime);

				if ((curtime - mailtime) >= mailchk)
				{
					chkmail();
				        mailtime = curtime;
				}
			}

			prs(ps1nod.namval);

#if BRL && ! defined(RISCOS)
			if (userid == 0 && !eq(ps1nod.namval, supprompt))
				prs(supprompt);	/* append "# " */
			alarm(timeout);
#else
#ifdef TIME_OUT
			alarm(TIMEOUT);
#endif
#endif

			flags |= waiting;
		}

#if BRL || JOBS
		if (flags & noeotflg)
			catcheof = TRUE;
#endif
		trapnote = 0;
		peekc = readc();
#if BRL || JOBS
		catcheof = FALSE;
#endif
		if (eof)
			return;

#if defined(TIME_OUT) || (BRL && ! defined(RISCOS))
		alarm(0);
#endif

		flags &= ~waiting;

		execute(cmd(NL, MTFLG), 0, eflag);
		eof |= (flags & oneflg);
	}
}

chkpr()
{
	if ((flags & prompt) && standin->fstak == 0)
		prs(ps2nod.namval);
}

settmp()
{
	itos(getpid());
	serial = 0;
	tmpnam = movstr(numbuf, &tmpout[TMPNAM]);
}

Ldup(fa, fb)
register int	fa, fb;
{
#ifdef RISCOS
	if (fa >= 0) {
		dup2(fa,fb);
		close(fa);
		fcntl(fb, F_SETFD, 1);
	}
#else RISCOS
#ifdef RES

	dup(fa | DUPFLG, fb);
	close(fa);
	ioctl(fb, FIOCLEX, 0);

#else RES

	if (fa >= 0)
		{ close(fb);
		  fcntl(fa,F_DUPFD,fb);		/* normal dup */	/* DAG -- use defines */
		  close(fa);
		  fcntl(fb, F_SETFD, 1);	/* autoclose for fb */	/* DAG */
		}

#endif RES
#endif RISCOS
}


chkmail()
{
	register char 	*s = mailp;
	register char	*save;

	long	*ptr = mod_time;
	char	*start;
	BOOL	flg; 
	struct stat	statb;

	while (*s)
	{
		start = s;
		save = 0;
		flg = 0;

		while (*s)
		{
			if (*s != COLON)	
			{
				if (*s == '%' && save == 0)
					save = s;
			
				s++;
			}
			else
			{
				flg = 1;
				*s = 0;
			}
		}

		if (save)
			*save = 0;

		if (*start && stat(start, &statb) >= 0)
		{
			if(statb.st_size && *ptr
				&& statb.st_mtime != *ptr)
			{
				if (save)
				{
					prs(save+1);
					newline();
				}
				else
					prs(mailmsg);
			}
			*ptr = statb.st_mtime;
		}
		else if (*ptr == 0)
			*ptr = 1;

		if (save)
			*save = '%';

		if (flg)
			*s++ = COLON;

		ptr++;
	}
}


setmail(mailpath)
	char *mailpath;
{
	register char	*s = mailpath;
	register int 	cnt = 1;

	long	*ptr;

	free(mod_time);
	if (mailp = mailpath)
	{
		while (*s)
		{
			if (*s == COLON)
				cnt += 1;

			s++;
		}

		ptr = mod_time = (long *)alloc(sizeof(long) * cnt);

		while (cnt)
		{
			*ptr = 0;
			ptr++;
			cnt--;
		}
	}
}
