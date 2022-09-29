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
#ident	"$Header: sh.init.c,v 1.4.1.2 90/05/07 18:16:28 wje Exp $"

/*
 * Copyright (c) 1980 Regents of the University of California.
 * All rights reserved.  The Berkeley Software License Agreement
 * specifies the terms and conditions for redistribution.
 */

#ifndef lint
static char *sccsid = "@(#)sh.init.c	5.3 (Berkeley) 9/16/87";
#endif

#include "sh.local.h"

/*
 * C shell
 */

extern	int doalias();
extern	int dobg();
extern	int dobreak();
extern	int dochngd();
extern	int docontin();
extern	int dodirs();
extern	int doecho();
extern	int doelse();
extern	int doend();
extern	int doendif();
extern	int doendsw();
extern	int doeval();
extern	int doexit();
extern	int dofg();
extern	int doforeach();
extern	int doglob();
extern	int dogoto();
extern	int dohash();
extern	int dohist();
extern	int doif();
extern	int dojobs();
extern	int dokill();
extern	int dolet();
extern	int dolimit();
extern	int dologin();
extern	int dologout();
#ifdef NEWGRP
extern	int donewgrp();
#endif
extern	int donice();
extern	int donotify();
extern	int donohup();
extern	int doonintr();
extern	int dopopd();
extern	int dopushd();
extern	int dorepeat();
extern	int doset();
extern	int dosetenv();
extern	int dosource();
extern	int dostop();
extern	int dosuspend();
extern	int doswbrk();
extern	int doswitch();
extern	int dotime();
extern	int dounlimit();
extern	int doumask();
extern	int dowait();
extern	int dowhile();
extern	int dozip();
extern	int execash();
extern	int goodbye();
#ifdef VFORK
extern	int hashstat();
#endif
extern	int shift();
extern	int showall();
extern	int unalias();
extern	int dounhash();
extern	int unset();
extern	int dounsetenv();
extern	int dowhich();

#define	INF	1000

struct	biltins {
	char	*bname;
	int	(*bfunct)();
	short	minargs, maxargs;
} bfunc[] = {
	"@",		dolet,		0,	INF,
	"alias",	doalias,	0,	INF,
	"alloc",	showall,	0,	1,
	"bg",		dobg,		0,	INF,
	"break",	dobreak,	0,	0,
	"breaksw",	doswbrk,	0,	0,
#ifdef IIASA
	"bye",		goodbye,	0,	0,
#endif
	"case",		dozip,		0,	1,
	"cd",		dochngd,	0,	1,
	"chdir",	dochngd,	0,	1,
	"continue",	docontin,	0,	0,
	"default",	dozip,		0,	0,
	"dirs",		dodirs,		0,	1,
	"echo",		doecho,		0,	INF,
	"else",		doelse,		0,	INF,
	"end",		doend,		0,	0,
	"endif",	dozip,		0,	0,
	"endsw",	dozip,		0,	0,
	"eval",		doeval,		0,	INF,
	"exec",		execash,	1,	INF,
	"exit",		doexit,		0,	INF,
	"fg",		dofg,		0,	INF,
	"foreach",	doforeach,	3,	INF,
#ifdef IIASA
	"gd",		dopushd,	0,	1,
#endif
	"glob",		doglob,		0,	INF,
	"goto",		dogoto,		1,	1,
#ifdef VFORK
	"hashstat",	hashstat,	0,	0,
#endif
	"history",	dohist,		0,	3,
	"if",		doif,		1,	INF,
	"jobs",		dojobs,		0,	1,
	"kill",		dokill,		1,	INF,
	"limit",	dolimit,	0,	3,
	"login",	dologin,	0,	1,
	"logout",	dologout,	0,	0,
#ifdef NEWGRP
	"newgrp",	donewgrp,	1,	1,
#endif
	"nice",		donice,		0,	INF,
	"nohup",	donohup,	0,	INF,
	"notify",	donotify,	0,	INF,
	"onintr",	doonintr,	0,	2,
	"popd",		dopopd,		0,	1,
	"pushd",	dopushd,	0,	1,
#ifdef IIASA
	"rd",		dopopd,		0,	1,
#endif
	"rehash",	dohash,		0,	0,
	"repeat",	dorepeat,	2,	INF,
	"set",		doset,		0,	INF,
	"setenv",	dosetenv,	0,	2,
	"shift",	shift,		0,	1,
	"source",	dosource,	1,	2,
	"stop",		dostop,		1,	INF,
	"suspend",	dosuspend,	0,	0,
	"switch",	doswitch,	1,	INF,
	"time",		dotime,		0,	INF,
	"umask",	doumask,	0,	1,
	"unalias",	unalias,	1,	INF,
	"unhash",	dounhash,	0,	0,
	"unlimit",	dounlimit,	0,	INF,
	"unset",	unset,		1,	INF,
	"unsetenv",	dounsetenv,	1,	INF,
	"wait",		dowait,		0,	0,
	"which",	dowhich,	1,	INF,
	"while",	dowhile,	1,	INF,
};
int nbfunc = sizeof bfunc / sizeof *bfunc;

#define	ZBREAK		0
#define	ZBRKSW		1
#define	ZCASE		2
#define	ZDEFAULT 	3
#define	ZELSE		4
#define	ZEND		5
#define	ZENDIF		6
#define	ZENDSW		7
#define	ZEXIT		8
#define	ZFOREACH	9
#define	ZGOTO		10
#define	ZIF		11
#define	ZLABEL		12
#define	ZLET		13
#define	ZSET		14
#define	ZSWITCH		15
#define	ZTEST		16
#define	ZTHEN		17
#define	ZWHILE		18

struct srch {
	char	*s_name;
	short	s_value;
} srchn[] = {
	"@",		ZLET,
	"break",	ZBREAK,
	"breaksw",	ZBRKSW,
	"case",		ZCASE,
	"default", 	ZDEFAULT,
	"else",		ZELSE,
	"end",		ZEND,
	"endif",	ZENDIF,
	"endsw",	ZENDSW,
	"exit",		ZEXIT,
	"foreach", 	ZFOREACH,
	"goto",		ZGOTO,
	"if",		ZIF,
	"label",	ZLABEL,
	"set",		ZSET,
	"switch",	ZSWITCH,
	"while",	ZWHILE,
};
int nsrchn = sizeof srchn / sizeof *srchn;

struct	mesg {
	char	*iname;
	char	*pname;
} mesg[] = {
	0,	0,
	"HUP",	"Hangup",			/* 1 */
	"INT",	"Interrupt",			/* 2 */
	"QUIT",	"Quit",				/* 3 */
	"ILL",	"Illegal instruction",		/* 4 */
	"TRAP",	"Trace/BPT trap",		/* 5 */
#ifdef RISCOS
	"ABRT", "Process abort",		/* 6 */
	"XCPU",	"Cputime limit exceeded",	/* 7 */
#else RISCOS
	"IOT",	"IOT trap",			/* 6 */
	"EMT",	"EMT trap",			/* 7 */
#endif RISCOS
	"FPE",	"Floating exception",		/* 8 */
	"KILL",	"Killed",			/* 9 */
	"BUS",	"Bus error",			/* 10 */
	"SEGV",	"Segmentation fault",		/* 11 */
	"SYS",	"Bad system call",		/* 12 */
	"PIPE",	"Broken pipe",			/* 13 */
	"ALRM",	"Alarm clock",			/* 14 */
	"TERM",	"Terminated",			/* 15 */
#ifdef RISCOS
	"USR1",	"User defined signal 1",	/* 16 */
	"USR2",	"User defined signal 2",	/* 17 */
	"CHLD",	"Child exited",			/* 18 */
	"XFSZ", "Filesize limit exceeded",	/* 19 */
	"STOP",	"Suspended (signal)",		/* 20 */
	"TSTP",	"Suspended",			/* 21 */
	"POLL",	"Poll",				/* 22 */
#else RISCOS
	"URG",	"Urgent I/O condition",		/* 16 */
	"STOP",	"Suspended (signal)",		/* 17 */
	"TSTP",	"Suspended",			/* 18 */
	"CONT",	"Continued",			/* 19 */
	"CHLD",	"Child exited",			/* 20 */
	"TTIN", "Suspended (tty input)",	/* 21 */
	"TTOU", "Suspended (tty output)",	/* 22 */
#endif
	"IO",	"I/O possible",			/* 23 */
#ifdef RISCOS
	"URG",	"Urgent I/O condition",		/* 24 */
	"WINCH","Window size changed",		/* 25 */
#else RISCOS
	"XCPU",	"Cputime limit exceeded",	/* 24 */
	"XFSZ", "Filesize limit exceeded",	/* 25 */
#endif RISCOS
	"VTALRM","Virtual timer expired",	/* 26 */
	"PROF",	"Profiling timer expired",	/* 27 */
#ifdef RISCOS
	"CONT",	"Continued",			/* 28 */
	"TTIN", "Suspended (tty input)",	/* 29 */
	"TTOU", "Suspended (tty output)",	/* 30 */
	"LOST", "Resource lost",		/* 31 */
#else RISCOS
	"WINCH","Window size changed",		/* 28 */
	0,	"Signal 29",			/* 29 */
	"USR1",	"User defined signal 1",	/* 30 */
	"USR2",	"User defined signal 2",	/* 31 */
#endif RISCOS
	0,	"Signal 32"			/* 32 */
};
