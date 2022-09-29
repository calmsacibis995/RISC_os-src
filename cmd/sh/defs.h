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
/* $Header: defs.h,v 1.7.2.3 90/05/09 18:56:31 wje Exp $ */

/*	Copyright (c) 1984 AT&T	*/
/*	  All Rights Reserved  	*/

/*	THIS IS UNPUBLISHED PROPRIETARY SOURCE CODE OF AT&T	*/
/*	The copyright notice above does not evidence any   	*/
/*	actual or intended publication of such source code.	*/

/*
 *	UNIX shell
 */

#include "defaults.h"

#if defined(BSD_SYS) || defined(gould) || defined(pyr)
#define	void	int	/* avoid compiler bug */
#endif


/* error exits from various parts of shell */
#define 	ERROR		1
#define 	SYNBAD		2
#define 	SIGFAIL 	2000
#define	 	SIGFLG		0200

/* command tree */
#define 	FPRS		0x0100
#define 	FINT		0x0200
#define 	FAMP		0x0400
#define 	FPIN		0x0800
#define 	FPOU		0x1000
#define 	FPCL		0x2000
#define 	FCMD		0x4000
#define 	COMMSK		0x00F0
#define		CNTMSK		0x000F

#define 	TCOM		0x0000
#define 	TPAR		0x0010
#define 	TFIL		0x0020
#define 	TLST		0x0030
#define 	TIF			0x0040
#define 	TWH			0x0050
#define 	TUN			0x0060
#define 	TSW			0x0070
#define 	TAND		0x0080
#define 	TORF		0x0090
#define 	TFORK		0x00A0
#define 	TFOR		0x00B0
#define		TFND		0x00C0

/* execute table */
#define 	SYSSET		1
#define 	SYSCD		2
#define 	SYSEXEC		3

#if defined(RISCOS) || defined(RES) || (defined(BSD_SYS) && !defined(BRL))
					/*	include login code	*/
#define 	SYSLOGIN	4
#else
#if !defined(BSD_SYS)
#define 	SYSNEWGRP 	4
#endif
#endif

#define 	SYSTRAP		5
#define 	SYSEXIT		6
#define 	SYSSHFT 	7
#define 	SYSWAIT		8
#define 	SYSCONT 	9
#define 	SYSBREAK	10
#define 	SYSEVAL 	11
#define 	SYSDOT		12
#define 	SYSRDONLY 	13
#define 	SYSTIMES 	14
#define 	SYSXPORT	15
#define 	SYSNULL 	16
#define 	SYSREAD 	17
#define		SYSTST		18

#if BRL
#define		SYSLOGOUT	19
#endif

#if defined(RISCOS) || !defined(RES) /*	exclude umask code	*/
#define 	SYSUMASK 	20
#define 	SYSULIMIT 	21
#endif

#define 	SYSECHO		22
#define		SYSHASH		23
#define		SYSPWD		24
#define 	SYSRETURN	25
#define		SYSUNS		26
#define		SYSMEM		27
#define		SYSTYPE  	28
#ifndef RISCOS
#define		SYSGETOPT	29
#endif RISCOS

#if JOBS
#define		SYSJOBS		29
#define		SYSFG		30
#define		SYSBG		31
#endif

#ifdef pyr
#define		SYSATT		32
#define		SYSUCB		33
#define		SYSUNIVERSE	34
#define		U_ATT		1	/* ATT is Universe # 1 */
#define		U_UCB		2	/* UCB is Universe # 2 */
#endif

#ifdef RISCOS
#define 	SYSNEWGRP 	35
#define		SYSGETOPT	36
#endif RISCOS

/* used for input and output of shell */
#ifdef RISCOS
#define 	INIO		(cur_getdtablesize - 1)
#else RISCOS
#define 	INIO 		19
#endif RISCOS

/*io nodes*/
#define 	USERIO		10
#define 	IOUFD		15
#define 	IODOC		16
#define 	IOPUT		32
#define 	IOAPP		64
#define 	IOMOV		128
#define 	IORDW		256
#define		IOSTRIP		512
#define 	INPIPE		0
#define 	OTPIPE		1

/* arg list terminator */
#define 	ENDARGS		0

#include	"mac.h"
#include	"mode.h"
#include	"name.h"
#include	<signal.h>


/*	error catching */
extern int 		errno;

/* getopt */

extern int		optind;
extern char 		*optarg;

/* result type declarations */

#define 	alloc 		malloc

extern char				*alloc();
extern char				*make();
extern char				*movstr();
extern char				*movstrn();
extern struct trenod	*cmd();
extern struct trenod	*makefork();
extern struct namnod	*lookup();
extern struct namnod	*findnam();
extern struct dolnod	*useargs();
extern float			expr();
extern char				*catpath();
extern char				*getpath();
extern char				*nextpath();
extern char				**scan();
extern char				*mactrim();
extern char				*macro();
extern char				*execs();
#ifdef TILDE_SUB
extern char				*homedir();
extern char				*retcwd();
#endif
extern int				exname();
extern char				*staknam();
extern int				printnam();
extern int				printro();
extern int				printexp();
extern unsigned char				readc();
extern unsigned char				nextc();
extern unsigned char				skipc();
extern char				**setenv();
extern long				time();
#if !defined(RISCOS) && (defined(BSD_SYS) || defined(BRL) && !defined(pdp11))
extern int				cwdir();	/* chdir() interface */
#endif
#if BRL
extern int				stimeout();	/* sets `timeout' */
extern int				stoo();		/* octal scanner */
#endif
#if JOBS
extern BOOL				unpost();
extern BOOL				j_finish();
extern char				*j_macro();
#endif

#define 	attrib(n,f)		(n->namflg |= f)
#define 	round(a,b)		(((int)(((char *)(a)+b)-1))&~((b)-1))
#define 	closepipe(x)	(close(x[INPIPE]), close(x[OTPIPE]))
#define 	eq(a,b)			(cf(a,b)==0)
#define 	max(a,b)		((a)>(b)?(a):(b))
#define 	assert(x)		;

/* temp files and io */
extern int				output;
extern int				ioset;
extern struct ionod		*iotemp;	/* files to be deleted sometime */
extern struct ionod		*fiotemp;	/* function files to be deleted sometime */
extern struct ionod		*iopend;	/* documents waiting to be read at NL */
#ifdef RISCOS
extern struct fdsave *fdmap;
#else RISCOS
extern struct fdsave	fdmap[];
#endif RISCOS
extern int savpipe;

#ifdef pyr
/* keep track of the current universe */
extern int				cur_univ;
extern char				*univ_name[];	/* from <universe.h> */
extern char				*univ_longname[];
#endif


/* substitution */
extern int				dolc;
extern char				**dolv;
extern struct dolnod	*argfor;
extern struct argnod	*gchain;

/* stak stuff */
#include		"stak.h"

/* string constants */
extern char				atline[];
extern char				readmsg[];
extern char				colon[];
extern char				minus[];
extern char				nullstr[];
extern char				sptbnl[];
extern char				unexpected[];
extern char				endoffile[];
extern char				synmsg[];
extern char				amperstr[];	/* DAG */
extern char				lparstr[];	/* DAG */
extern char				rparstr[];	/* DAG */
extern char				pipestr[];	/* DAG */
extern char				andstr[];	/* DAG */
extern char				orstr[];	/* DAG */
extern char				forstr[];	/* DAG */
extern char				instr[];	/* DAG */
extern char				dostr[];	/* DAG */
extern char				donestr[];	/* DAG */
extern char				whilestr[];	/* DAG */
extern char				untilstr[];	/* DAG */
extern char				ifstr[];	/* DAG */
extern char				thenstr[];	/* DAG */
extern char				elsestr[];	/* DAG */
extern char				fistr[];	/* DAG */
extern char				casestr[];	/* DAG */
extern char				dsemistr[];	/* DAG */
extern char				esacstr[];	/* DAG */
extern char				fromstr[];	/* DAG */
extern char				toastr[];	/* DAG */
extern char				fromastr[];	/* DAG */
extern char				ontostr[];	/* DAG */
extern char				hashhdr[];	/* DAG */
extern char				isbuiltin[];	/* DAG */
extern char				isfunct[];	/* DAG */
extern char				efnlstr[];	/* DAG */
extern char				nfstr[];	/* DAG */
extern char				ishashed[];	/* DAG */
extern char				rpnlstr[];	/* DAG */
extern char				isstr[];	/* DAG */
#if JOBS
extern char				jshstr[];
extern char				rsqbrk[];
extern char				spspstr[];
extern char				fgdstr[];
extern char				stpdstr[];
extern char				lotspstr[];
extern char				psgpstr[];
extern char				ptinstr[];
extern char				ptoustr[];
extern char				bgdstr[];
extern char				spcstr[];
extern char				rdinstr[];
extern char				appdstr[];
extern char				inlnstr[];
extern char				sfnstr[];
extern char				efnstr[];
extern char				semspstr[];
extern char				lpnstr[];
extern char				rpnstr[];
extern char				insstr[];
extern char				sdostr[];
extern char				sdonstr[];
extern char				sthnstr[];
extern char				selsstr[];
extern char				sfistr[];
extern char				iesacstr[];
#endif JOBS
#if SYMLINK
extern char				nolstat[];
#endif
#if BRL
extern char				drshell[];
extern char				rshell[];
extern char				hangup[];
extern char				logout[];
extern char				terminate[];
extern char				timout[];
extern char				dsupath[];
extern char				deftimout[];
extern char				dsutimout[];
#if pdp11
extern char				quota[];
extern char				bye[];
#endif
#endif

/* name tree and words */
extern struct sysnod	reserved[];
extern int				no_reserved;
extern struct sysnod	commands[];
extern int				no_commands;

extern int				wdval;
extern int				wdnum;
extern int				fndef;
extern int				nohash;
extern struct argnod	*wdarg;
extern int				wdset;
extern BOOL				reserv;

/* prompting */
extern char				stdprompt[];
extern char				supprompt[];
extern char				profile[];
extern char				sysprofile[];

/* built in names */
extern struct namnod	fngnod;
extern struct namnod	cdpnod;
extern struct namnod	ifsnod;
extern struct namnod	homenod;
extern struct namnod	mailnod;
extern struct namnod	pathnod;
extern struct namnod	ps1nod;
extern struct namnod	ps2nod;
extern struct namnod	mchknod;
extern struct namnod	acctnod;
extern struct namnod	mailpnod;
#if BRL
extern struct namnod	timenod;
#endif
#ifdef pyr
extern struct namnod	univnod;
#endif

/* special names */
extern char				flagadr[];
extern char				*pcsadr;
extern char				*pidadr;
extern char				*cmdadr;

extern char				defpath[];

/* names always present */
extern char				mailname[];
extern char				homename[];
extern char				pathname[];
extern char				cdpname[];
extern char				ifsname[];
extern char				ps1name[];
extern char				ps2name[];
extern char				mchkname[];
extern char				acctname[];
extern char				mailpname[];
#if BRL
extern char				timename[];	/* TIMEOUT */
#endif
#ifdef pyr
extern char				univname[];	/* UNIVERSE */
#endif

/* transput */
extern char				tmpout[];
extern char				*tmpnam;
extern int				serial;

#define		TMPNAM 		7

extern struct fileblk	*standin;

#define 	input		(standin->fdes)
#define 	eof			(standin->feof)

extern int				peekc;
extern int				peekn;
extern char				*comdiv;
extern char				devnull[];
#if BRL || JOBS
extern BOOL		catcheof;	/* set to catch EOF in readc() */
#endif
#if JOBS
#if defined(RISCOS) || defined(BSD_SYS) || defined(BRL) && !defined(pdp11)
extern int		j_original_pg;
#else
#define	j_original_pg	j_default_pg
#endif
extern int		j_default_pg;
extern BOOL		j_son_of_jobs;
#endif

/* flags */
#define		noexec		01
#define		sysflg		01
#define		intflg		02
#define		prompt		04
#define		setflg		010
#define		errflg		020
#define		ttyflg		040
#define		forked		0100
#define		oneflg		0200
#define		rshflg		0400
#define		waiting		01000
#define		stdflg		02000
#define		STDFLG		's'
#define		execpr		04000
#define		readpr		010000
#define		keyflg		020000
#define		hashflg		040000
#if JOBS
#define		jobflg		0100000
#endif
#define		nofngflg	0200000
#define		exportflg	0400000
#if BRL
#define 	infoflg		01000000
#define		noeotflg	02000000
#define		dotflg		04000000
#endif
#ifdef TILDE_SUB
#define		tilde_sub_flg	010000000
#endif TILDE_SUB
#define		echo_dash_n_flg	020000000

extern long				flags;
extern int				rwait;	/* flags read waiting */

/* error exits from various parts of shell */
#include	<setjmp.h>
extern jmp_buf			subshell;
extern jmp_buf			errshell;
#if defined(BSD_SYS) && !defined(BSD41C) && !defined(pyr)
#define	setjmp( env )		_setjmp( env )
#define	longjmp( env, val )	_longjmp( env, val )
#endif

/* fault handling */
#include	"brkincr.h"

extern unsigned			brkincr;
#define 	MINTRAP		0
#if defined(RISCOS) || defined(BSD_SYS) || defined(BRL) && !defined(pdp11)
#define		MAXTRAP		32
#else
#define 	MAXTRAP		20
#endif

#define 	TRAPSET		2
#define 	SIGSET		4
#define 	SIGMOD		8
#define 	SIGCAUGHT	16

extern void				done();		/* DAG */
extern void				fault();	/* DAG */
extern BOOL				trapnote;
extern char				*trapcom[];
extern BOOL				trapflg[];

/* name tree and words */
extern char				**environ;
extern char				numbuf[];
extern char				export[];
extern char				duperr[];
extern char				readonly[];

/* execflgs */
extern int				exitval;
extern int				retval;
#if gould
extern int				execbrk;
#else
extern BOOL				execbrk;
#endif
extern int				loopcnt;
extern int				breakcnt;
extern int				funcnt;

/* messages */
extern char				mailmsg[];
extern char				coredump[];
extern char				badopt[];
extern char				badparam[];
extern char				unset[];
extern char				badsub[];
extern char				nospace[];
extern char				nostack[];
extern char				notfound[];
extern char				badtrap[];
extern char				baddir[];
extern char				badshift[];
extern char				restricted[];
extern char				execpmsg[];
extern char				notid[];
extern char 				badulimit[];
extern char				wtfailed[];
extern char				badcreate[];
extern char				nofork[];
extern char				noswap[];
extern char				piperr[];
extern char				badopen[];
extern char				badnum[];
extern char				arglist[];
extern char				txtbsy[];
extern char				toobig[];
extern char				badexec[];
extern char				badfile[];
extern char				badreturn[];
extern char				badexport[];
extern char				badunset[];
extern char				nohome[];
extern char				badperm[];
extern char				mssgargn[];
extern char				libacc[];
extern char				libbad[];
extern char				libscn[];
extern char				libmax[];
extern char                             emultihop[];
extern char                             nulldir[];
extern char                             enotdir[];
extern char                             enoent[];
extern char                             eacces[];
extern char                             enolink[];
#if JOBS
extern char				cjpostr[];
extern char				jcoffstr[];
extern char				jpanstr[];
extern char				jinvstr[];
extern char				ncjstr[];
extern char				nstpstr[];
extern char				tasjstr[];
#endif
#if BRL
extern char				badtimeout[];
extern char				noeot[];
#if pdp11
extern char				quofail[];
#endif
#endif

/*	'builtin' error messages	*/

extern char				btest[];
extern char				badop[];

/*	fork constant	*/

#define 	FORKLIM 	32

extern address			end[];

#include	"ctype.h"

extern int				wasintr;	/* used to tell if break or delete is hit
				   					 *  while executing a wait
									 */
extern int				eflag;

#if BRL
extern char		*argv0;		/* shell's argv[0] */
extern BOOL		loginsh;	/* TRUE iff login shell */
extern int		timeout;	/* TIMEOUT value in seconds */
extern int		userid;		/* effective UID */
#if pdp11
extern char		*quomsg;	/* message for "quota" on exit */
#endif
#endif


/*
 * Find out if it is time to go away.
 * `trapnote' is set to SIGSET when fault is seen and
 * no trap has been set.
 */

#define		sigchk()	if (trapnote & SIGSET)	\
							exitsh(exitval ? exitval : SIGFAIL)

#define 	exitset()	retval = exitval

#ifdef RISCOS
extern	int	cur_getdtablesize;	/* getdtablesize() value	    */
#define Setpgrp setpgid

#include	<bsd43/sys/syscall.h>
#define dup2(a,b) syscall(BSD43_SYS_dup2,a,b)
#define getpgrp() syscall(BSD43_SYS_getpgrp,0)

#include	<sys.s>
#define wait3(a,b,c) syscall(SYS_wait3,a,b,c)

#define signal(a,b) sigset(a,b)

#define killpg(p,s) kill(-(p), (s))

#else RISCOS
#define Setpgrp setpgrp
#endif RISCOS

#ifndef NULL
#define NULL (0)
#endif NULL
