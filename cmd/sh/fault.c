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
#ident	"$Header: fault.c,v 1.7.2.4 90/05/09 18:57:26 wje Exp $"

/*	Copyright (c) 1984 AT&T	*/
/*	  All Rights Reserved  	*/

/*	THIS IS UNPUBLISHED PROPRIETARY SOURCE CODE OF AT&T	*/
/*	The copyright notice above does not evidence any   	*/
/*	actual or intended publication of such source code.	*/

/*
 * UNIX shell
 */

#include	"defs.h"

extern void	done();	/* DAG */

char	*trapcom[MAXTRAP + 1] = { 0 };
BOOL	trapflg[MAXTRAP + 1] =
{
	0,	/* 0 */
	0,	/* 1 = hangup */
	0,	/* 2 = interrupt */
	0,	/* 3 = quit */
	0,	/* 4 = illegal instr */
	0,	/* 5 = trace trap */
	0,	/* 6 = IOT */
#ifndef RISCOS
	0,	/* 7 = EMT */
#else RISCOS
	0,	/* 7 = XCPU */
#endif RISCOS
	0,	/* 8 = float pt. exp */
	0,	/* 9 = kill */
	0,	/* 10 = bus error */
	0,	/* 11 = segmentation violation */
	0,	/* 12 = bad argument to system call */
	0,	/* 13 = write on a pipe with no one to read it */
	0,	/* 14 = alarm clock */
	0,	/* 15 = software termination signal from kill */
	0,	/* 16 = user defined signal 1 */
	0,	/* 17 = user defined signal 2 */
	0,	/* 18 = death of a child */
#ifndef RISCOS
	0,	/* 19 = power-fail restart */
#else RISCOS
	0,	/* 19 = exceeded file size limit */
#endif RISCOS
	0,	/* 20 = sendable stop signal not from tty */
	0,	/* 21 = stop signal from tty */
	0,	 /* 22 = pollable event occured */
	0,	/* 23 = input/output possible signal */
	0,	/* 24 = urgent condition on IO channel */
	0,	 /* 25 = window size changes */
	0,	 /* 26 = virtual time alarm */
	0,	/* 27 = profiling alarm */
	0,	/* 28 = continue a stopped process */
	0,	/* 29 = to readers pgrp upon background tty read */
	0,	/* 30 = like TTIN for output if (tp->t_local&LTOSTOP) */
	0,	/* 31 = resource lost (eg, record-lock) */
	0,	/* 32 = unassigned */
};

void 	(*(sigval[MAXTRAP + 1]))() =
{
	SIG_DFL, /* 0 */
	fault,  /* 1 = hangup */
	fault,	/* 2 = interrupt */
	fault,	/* 3 = quit */
	done,	/* 4 = illegal instr */
	done,	/* 5 = trace trap */
	done,	/* 6 = IOT */
#ifndef RISCOS
	done,	/* 7 = EMT */
#else RISCOS
	done,	/* 7 = XCPU */
#endif RISCOS	
	done,	/* 8 = float point exception */
	SIG_DFL, /* 9 = kill */
	done,	/* 10 = bus error */
	done,	/* 11 = memory faults */
	done,	/* 12 = bad sys call */
	done,	/* 13 = bad pipe call */
	fault,	/* 14 = alarm */
	fault,	/* 15 = software termination */
	done,	/* 16 = user defined signal 1 */
	done,	/* 17 = user defined signal 2 */
	SIG_DFL, /* 18 = death of child (if not BSD_SYS) */
#ifndef RISCOS
	done,	/* 19 = power fail */
#else RISCOS
	done,	/* 19 = exceed file size limit */
#endif RISCOS
	SIG_DFL, /* 20 = sendable stop signal not from tty */
	SIG_DFL, /* 21 = stop signal from tty */
	done,	 /* 22 = pollable event occured */
	done,	/* 23 = input/output possible signal */
	done,	/* 24 = urgent condition on IO channel */
	SIG_DFL, /* 25 = window size changes */
	fault,	 /* 26 = virtual time alarm */
	fault,	/* 27 = profiling alarm */
	SIG_DFL, /* 28 = continue a stopped process */
	SIG_DFL, /* 29 = to readers pgrp upon background tty read */
	SIG_DFL, /* 30 = like TTIN for output if (tp->t_local&LTOSTOP) */
	SIG_DFL, /* 31 = resource lost (eg, record-lock) */
	done	/* 32 = unassigned */
};

/* ========	fault handling routines	   ======== */


void	/* DAG */
fault(sig,code,scp)
register int	sig;
int code;
struct sigcontext *scp;
{
	register int	flag;

	if (sig == SIGSEGV)
	{
		if (setbrk(brkincr) == -1) {
			error(nospace);
		}
	}
	else if (sig == SIGALRM)
	{
		if (flags & waiting)
		{
#if BRL
#if pdp11
			prs(quomsg = timout);	/* for user and quota */
#else
			prs(timout);	/* for user */
#endif
			newline();
#endif
			done();
		}
	}
#if BRL
	else if (loginsh && sig == SIGHUP)
	{
#if pdp11
		quomsg = hangup;	/* for quota */
#endif
		done();
	}
	else if (loginsh && sig == SIGTERM)
	{
#if pdp11
		prs(quomsg = terminate);/* for user and quota */
#else
		prs(terminate);	/* for user */
#endif
		newline();
		done();
	}
#endif
	else
	{
		flag = (trapcom[sig] ? TRAPSET : SIGSET);
		trapnote |= flag;
		trapflg[sig] |= flag;
		if (sig == SIGINT)
			wasintr++;
	}
}

stdsigs()
{
	int	i;

	for (i = 1; i < MAXTRAP; i++)
		switch (i) {
		case SIGQUIT:
			ignsig(i);
			break;
		case SIGSEGV:
			sigset(i,fault);
			break;
		case SIGKILL:
			break;
		case SIGTSTP:
			break;
		default:
			setsig(i);
			break;
		};
}

ignsig(n)
{
	register int	s, i;

	i = n;
	if ((s = (sigset(i, SIG_IGN) == SIG_IGN)) == 0)
	{
		trapflg[i] |= SIGMOD;
	}
	return(s);
}

getsig(n)
{
	register int	i;

	if (trapflg[i = n] & SIGMOD || ignsig(i) == 0)
		sigset(i, fault);
}


setsig(n)
{
	register int	i;

	if (ignsig(i = n) == 0)
		sigset(i, sigval[i]);
}

oldsigs()
{
	register int	i;
	register char	*t;

	i = MAXTRAP;
	while (i--)
	{
		t = trapcom[i];
		if (t == 0 || *t)
			clrsig(i);
		trapflg[i] = 0;
	}
	trapnote = 0;
}

clrsig(i)
int	i;
{
	free(trapcom[i]);
	trapcom[i] = 0;
	if (trapflg[i] & SIGMOD)
	{
		trapflg[i] &= ~SIGMOD;
		sigset(i, sigval[i]);
	}
}

/*
 * check for traps
 */
chktrap()
{
	register int	i = MAXTRAP;
	register char	*t;

	trapnote &= ~TRAPSET;
	while (--i)
	{
		if (trapflg[i] & TRAPSET)
		{
			trapflg[i] &= ~TRAPSET;
			if (t = trapcom[i])
			{
				int	savxit = exitval;

				execexp(t, 0);
				exitval = savxit;
				exitset();
			}
		}
	}
}
