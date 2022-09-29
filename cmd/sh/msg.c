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
#ident	"$Header: msg.c,v 1.8.2.3 90/05/09 18:59:03 wje Exp $"

/*	Copyright (c) 1984 AT&T	*/
/*	  All Rights Reserved  	*/

/*	THIS IS UNPUBLISHED PROPRIETARY SOURCE CODE OF AT&T	*/
/*	The copyright notice above does not evidence any   	*/
/*	actual or intended publication of such source code.	*/

/*
 *	UNIX shell
 */


#include	"defs.h"
#include	"sym.h"

/*
 * error messages
 */
char	badopt[]	= "bad option(s)";
char	mailmsg[]	= "you have mail\n";
char	nospace[]	= "no space";
char	nostack[]	= "no stack space";
char	synmsg[]	= "syntax error";

char	badnum[]	= "bad number";
char	badparam[]	= "parameter null or not set";
char	unset[]		= "parameter not set";
char	badsub[]	= "bad substitution";
char	badcreate[]	= "cannot create";
char	nofork[]	= "fork failed - too many processes";
char	noswap[]	= "cannot fork: no swap space";
char	restricted[]	= "restricted";
char	piperr[]	= "cannot make pipe";
char	badopen[]	= "cannot open";
char	coredump[]	= " - core dumped";
char	arglist[]	= "arg list too long";
char	txtbsy[]	= "text busy";
char	toobig[]	= "too big";
char	badexec[]	= "cannot execute";
char	notfound[]	= "not found";
char	badfile[]	= "bad file number";
char	badshift[]	= "cannot shift";
char	baddir[]	= "bad directory";
char	badtrap[]	= "bad trap";
char	wtfailed[]	= "is read only";
char	notid[]		= "is not an identifier";
char 	badulimit[]	= "bad ulimit";	/* DAG -- lower case */
char	badreturn[] 	= "cannot return when not in function";
char	badexport[] 	= "cannot export functions";
char	badunset[] 	= "cannot unset";
char	nohome[]	= "no home directory";
char 	badperm[]	= "execute permission denied";
char	longpwd[]	= "sh error: pwd too long";
char	mssgargn[]	= "missing arguments";
char	libacc[] 	= "can't access a needed shared library";
char	libbad[]	= "accessing a corrupted shared library";
char	libscn[]	= ".lib section in a.out corrupted";
char	libmax[]	= "attempting to link in too many libs";
char    emultihop[]     = "Multihop attempted";
char    nulldir[]       = "null directory";
char    enotdir[]       = "not a directory";
char    enoent[]        = "does not exist";
char    eacces[]        = "permission denied";
char    enolink[]       = "remote link inactive";

char	no11trap[]	= "cannot trap 11";		/* DAG -- made sharable */
char	argcount[]	= "argument count";		/* DAG */
char	neworsemi[]	= "newline or ;";		/* DAG */
char	dotstat[]	= "pwd: cannot stat .";		/* DAG */
char	paropen[]	= "pwd: cannot open ..";	/* DAG */
char	parstat[]	= "pwd: cannot stat ..";	/* DAG */
char	parread[]	= "pwd: read error in ..";	/* DAG */
#if JOBS
char	cjpostr[]	= ": couldn't jpost\n";
char	jcoffstr[]	= "job control not enabled\n";
char	jpanstr[]	= "sh bug: j_print_ent--no number ";
char	jinvstr[]	= "invalid job number\n";
char	ncjstr[]	= "no current job\n";
char	nstpstr[]	= ": not stopped\n";
char	tasjstr[]	= "There are stopped jobs.\n";
#endif JOBS
#if SYMLINK
char	nolstat[]	= ": can't lstat component";
#endif SYMLINK
#if BRL
char	badtimeout[]	= "bad timeout value\n";
char	noeot[]		= "use \"exit\" or \"logout\"\n";
#if pdp11
char	quofail[]	= "quota system failed\n";
#endif pdp11
#endif BRL
/*
 * messages for 'builtin' functions
 */
char	btest[]		= "test";
char	badop[]		= "unknown operator ";
char	argexp[]	= "argument expected";	/* DAG -- made sharable */
char	parexp[]	= ") expected";		/* DAG */
char	rbmiss[]	= "] missing";		/* DAG */
/*
 * built in names
 */
char	pathname[]	= "PATH";
char	cdpname[]	= "CDPATH";
char	homename[]	= "HOME";
char	mailname[]	= "MAIL";
char	ifsname[]	= "IFS";
char	ps1name[]	= "PS1";
char	ps2name[]	= "PS2";
char	mchkname[]	= "MAILCHECK";
char	acctname[]  	= "SHACCT";
char	mailpname[]	= "MAILPATH";
#if BRL
char	timename[]	= "TIMEOUT";
#endif
#ifdef pyr
char	univname[]	= "UNIVERSE";
#endif

/*
 * string constants
 */
char	nullstr[]	= "";
char	sptbnl[]	= " \t\n";
#ifdef RISCOS
char	defpath[]	= "/usr/net:/bin:/usr/bin:/usr/ucb";
#else RISCOS
char	defpath[]	= ":/bin:/usr/bin";
#endif RISCOS
char	colon[]		= ": ";
char	minus[]		= "-";
char	endoffile[]	= "end of file";
char	unexpected[] 	= " unexpected";
char	atline[]	= " at line ";
char	devnull[]	= "/dev/null";
char	execpmsg[]	= "+ ";
char	readmsg[]	= "> ";
char	stdprompt[]	= "$ ";
char	supprompt[]	= "# ";
char	profile[]	= ".profile";
#if defined(RISCOS) || !defined(BRL) || defined(pdp11)
char	sysprofile[]	= "/etc/profile";
#else
char	sysprofile[]	= "/usr/5lib/profile";
#endif
#ifdef notdef
char	setstr[]	= "set";			/* DAG -- made sharable */
char	sicrstr[]	= "sicr";			/* DAG */
char	bang[]		= "!";				/* DAG */
char	pdstr[]		= ".";				/* DAG */
char	dotdot[]	= "..";				/* DAG */
char	slstr[]		= "/";				/* DAG */
char	eqlstr[]	= "=";				/* DAG */
char	neqstr[]	= "!=";				/* DAG */
char	lbstr[]		= "[";				/* DAG */
char	lpstr[]		= "(";				/* DAG */
char	rbstr[]		= "]";				/* DAG */
char	rpstr[]		= ")";				/* DAG */
char	dasheq[]	= "-eq";			/* DAG */
char	dashne[]	= "-ne";			/* DAG */
char	dashgt[]	= "-gt";			/* DAG */
char	dashlt[]	= "-lt";			/* DAG */
char	dashge[]	= "-ge";			/* DAG */
char	dashle[]	= "-le";			/* DAG */
char	dashnt[]	= "-nt";
char	dashot[]	= "-ot";
char	dasha[]		= "-a";				/* DAG */
char	dashb[]		= "-b";				/* DAG */
char	dashc[]		= "-c";				/* DAG */
char	dashd[]		= "-d";				/* DAG */
char	dashf[]		= "-f";				/* DAG */
char	dashg[]		= "-g";				/* DAG */
char	dashk[]		= "-k";				/* DAG */
char	dashn[]		= "-n";				/* DAG */
char	dasho[]		= "-o";				/* DAG */
char	dashp[]		= "-p";				/* DAG */
char	dashr[]		= "-r";				/* DAG */
char	dashs[]		= "-s";				/* DAG */
char	dasht[]		= "-t";				/* DAG */
char	dashu[]		= "-u";				/* DAG */
char	dashw[]		= "-w";				/* DAG */
char	dashx[]		= "-x";				/* DAG */
char	dashz[]		= "-z";				/* DAG */
char	ptrcolon[]	= "ptrace: ";			/* DAG */
char	shell[]		= "SHELL";			/* DAG */
char	sfuncstr[]	= "(){\n";			/* DAG */
char	efuncstr[]	= "\n}";			/* DAG */
#endif notdef
char	amperstr[]	= " &";				/* DAG */
char	lparstr[]	= "( ";				/* DAG */
char	rparstr[]	= " )";				/* DAG */
char	pipestr[]	= " | ";			/* DAG */
char	andstr[]	= " && ";			/* DAG */
char	orstr[]		= " || ";			/* DAG */
char	forstr[]	= "for ";			/* DAG */
char	instr[]		= " in";			/* DAG */
char	dostr[]		= "\ndo\n";			/* DAG */
char	donestr[]	= "\ndone";			/* DAG */
char	whilestr[]	= "while ";			/* DAG */
char	untilstr[]	= "until ";			/* DAG */
char	ifstr[]		= "if ";			/* DAG */
char	thenstr[]	= "\nthen\n";			/* DAG */
char	elsestr[]	= "\nelse\n";			/* DAG */
char	fistr[]		= "\nfi";			/* DAG */
char	casestr[]	= "case ";			/* DAG */
char	dsemistr[]	= ";;";				/* DAG */
char	esacstr[]	= "\nesac";			/* DAG */
char	fromstr[]	= "<<";				/* DAG */
char	toastr[]	= ">&";				/* DAG */
char	fromastr[]	= "<&";				/* DAG */
char	ontostr[]	= ">>";				/* DAG */
char	hashhdr[]	= "hits\tcost\tcommand\n";	/* DAG */
char	isbuiltin[]	= " is a shell builtin\n";	/* DAG */
char	isfunct[]	= " is a function\n";		/* DAG */
char	efnlstr[]	= "\n}\n";			/* DAG */
char	nfstr[]		= " not found\n";		/* DAG */
char	ishashed[]	= " is hashed (";		/* DAG */
char	rpnlstr[]	= ")\n";			/* DAG */
char	isstr[]		= " is ";			/* DAG */
#if JOBS
char	jshstr[]	= "jsh";
char	rsqbrk[]	= "] ";
char	spspstr[]	= " \ ";
char	fgdstr[]	= "foreground \ \ \ \ \ \ ";
char	stpdstr[]	= "stopped";
char	lotspstr[]	= " \ \ \ \ \ \ \ \ \ ";
char	psgpstr[]	= " (signal) ";
char	ptinstr[]	= " (tty in) ";
char	ptoustr[]	= " (tty out)";
char	bgdstr[]	= "background \ \ \ \ \ \ ";
char	spcstr[]	= " ";
char	rdinstr[]	= "< ";
char	appdstr[]	= ">> ";
char	inlnstr[]	= "<< ";
char	sfnstr[]	= "(){ ";
char	efnstr[]	= " }";
char	semspstr[]	= "; ";
char	lpnstr[]	= "(";
char	rpnstr[]	= ")";
char	insstr[]	= " in ";
char	sdostr[]	= "; do ";
char	sdonstr[]	= "; done";
char	sthnstr[]	= "; then ";
char	selsstr[]	= "; else ";
char	sfistr[]	= "; fi";
char	iesacstr[]	= " in ... esac";
#endif JOBS
#if BRL
char	drshell[]	= "-rsh";
char	rshell[]	= "rsh";
#if pdp11
char	quota[]		= "/bin/quota";
char	bye[]		= "$";		/* for quota system */
#endif
char	hangup[]	= "hangup";
char	logout[]	= "logout";
char	terminate[]	= "terminate";
char	timout[]	= "timeout";
#ifdef RISCOS
char	dsupath[]	= "/bin:/usr/bin:/usr/ucb:/etc:/bsd43/bin:.";
#else RISCOS
#if pdp11
char	dsupath[]	= "/bin:/usr/bin:/usr/sbin:.";	/* su default PATH */
#else pdp11
char	dsupath[]	= "/bin:/usr/bin:/usr/ucb:/etc:/usr/brl/bin:.";
#endif pdp11
#endif RISCOS
char	deftimout[]	= "30";		/* normal timeout (minutes) */
char	dsutimout[]	= "0";		/* superuser timeout */
#endif BRL

/*
 * tables
 */

struct sysnod reserved[] =
{
	{ "case",	CASYM	},
	{ "do",		DOSYM	},
	{ "done",	ODSYM	},
	{ "elif",	EFSYM	},
	{ "else",	ELSYM	},
	{ "esac",	ESSYM	},
	{ "fi",		FISYM	},
	{ "for",	FORSYM	},
	{ "if",		IFSYM	},
	{ "in",		INSYM	},
	{ "then",	THSYM	},
	{ "until",	UNSYM	},
	{ "while",	WHSYM	},
	{ "{",		BRSYM	},
	{ "}",		KTSYM	}
};

int no_reserved = 15;

char	*sysmsg[] =
{
	0,			/* 0 */
	"Hangup",		/* 1 = SIGHUP */
	0,	/* Interrupt */	/* 2 = SIGINT */
	"Quit",			/* 3 = SIGQUIT */
	"Illegal instruction",	/* 4 = SIGILL */
	"Trace/BPT trap",	/* 5 = SIGTRAP */
	"abort",		/* 6 = SIGABRT */
#ifdef RISCOS
	"CPU time limit", /* 7 = SIGXCPU */
#else RISCOS
	"EMT trap",		/* 7 = SIGEMT */
#endif RISCOS
	"Floating exception",	/* 8 = SIGFPE */
	"Killed",		/* 9 = SIGKILL */
	"Bus error",		/* 10 = SIGBUS */
	"Memory fault",		/* 11 = SIGSEGV */
	"Bad system call",	/* 12 = SIGSYS */
	0,	/* Broken pipe */ /* 13 = SIGPIPE */
	"Alarm call",		/* 14 = SIGALRM */
	"Terminated",		/* 15 = SIGTERM */
#ifdef RISCOS
	"User defined signal 1", /* 16 = SIGUSR1 */
	"User defined signal 2", /* 17 = SIGUSR2 */
	"Child status change",	/* 18 = SIGCLD */
	"File size limit", /* 19 = SIGXFSZ */
	"Stop",			/* 20 = SIGSTOP */
	"Stop from keyboard",	/* 21 = SIGTSTP */
	"Pollable event occurred", /* 22 = SIGPOLL */
	"I/O possible", 	/* 23 = SIGIO */
	"Urgent condition on IO channel", /* 24 = SIGURG */
	"Window size change",	/* 25 = SIGWINCH */
	"Virtual time alarm",	/* 26 = SIGVTALRM */
	"Profiling timer alarm", /* 27 = SIGPROF */
	"Continue", 		/* 28 = SIGCONT */
	"Background read", 	/* 29 = SIGTTIN */
	"Background write", 	/* 30 = SIGTTOU */
	"Resource lost", 	/* 31 = SIGLOST */
	"Signal 32"
#else RISCOS
	"Signal 16",
#if defined(BSD_SYS) || defined(BRL) && !defined(pdp11)
	"Stop",
	"Stop from keyboard",
	"Continue",
	"Child status change",
	"Background read",
	"Background write",
	"I/O possible",
	"CPU time limit",
	"File size limit",
	"Virtual time alarm",
	"Profiling timer alarm",
#if gould
	"Stack overflow",
#else gould
	"Signal 28",
#endif gould
	"Signal 29",
	"Signal 30",
	"Signal 31",
	"Signal 32",
#else
	"Signal 17",
#if BRL
	"Signal 18",
	"Signal 19",
#else BRL
	"Child death",
	"Power Fail"
#endif BRL
#endif
#endif RISCOS
};

char	export[] = "export";
char	duperr[] = "cannot dup";
char	readonly[] = "readonly";


struct sysnod commands[] =
{
	{ ".",		SYSDOT	},
	{ ":",		SYSNULL	},

#if defined(RISCOS) || ! defined(RES)
	{ "[",		SYSTST },
#endif

#ifdef pyr
	{ "att",	SYSATT },
#endif

#if JOBS
	{ "bg",		SYSBG },
#endif

	{ "break",	SYSBREAK },
	{ "cd",		SYSCD	},
	{ "continue",	SYSCONT	},
	{ "echo",	SYSECHO },
	{ "eval",	SYSEVAL	},
	{ "exec",	SYSEXEC	},
	{ "exit",	SYSEXIT	},
	{ "export",	SYSXPORT },

#if JOBS
	{ "fg",		SYSFG },
#endif

	{ "getopts",	SYSGETOPT },
	{ "hash",	SYSHASH	},

#if JOBS
	{ "jobs",	SYSJOBS },
#endif

#if defined(RISCOS) || RES || BSD_SYS
#if defined(RISCOS) || ! defined(BRL)
	{ "login",	SYSLOGIN },
#endif
#endif

#if BRL
	{ logout,	SYSLOGOUT },
#endif

#if defined(RISCOS) || ! defined(BSD_SYS)
#if defined(RES) && ! defined(RISCOS)
	{ "newgrp",	SYSLOGIN },
#else
	{ "newgrp",	SYSNEWGRP },
#endif
#endif

	{ "pwd",	SYSPWD },
	{ "read",	SYSREAD	},
	{ "readonly",	SYSRDONLY },
	{ "return",	SYSRETURN },
	{ "set",	SYSSET	},
	{ "shift",	SYSSHFT	},
	{ "test",	SYSTST },
	{ "times",	SYSTIMES },
	{ "trap",	SYSTRAP	},
	{ "type",	SYSTYPE },

#ifdef pyr
	{ "ucb",	SYSUCB },
#endif

#if defined(RISCOS) || ! defined(RES)
	{ "ulimit",	SYSULIMIT },
	{ "umask",	SYSUMASK },
#endif

#ifdef pyr
	{ "universe",	SYSUNIVERSE },
#endif

	{ "unset", 	SYSUNS },
	{ "wait",	SYSWAIT	}
};

int	no_commands = sizeof commands / sizeof(struct sysnod);	/* DAG -- improved */

#ifdef pyr
#include	<sys/types.h>
#include	<sys/inode.h>
#include	<universe.h>	/* for univ_name[] and univ_longname[] */
#endif
