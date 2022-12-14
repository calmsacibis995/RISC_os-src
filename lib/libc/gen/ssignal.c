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
#ident	"$Header: ssignal.c,v 1.7.2.2 90/05/10 01:37:30 wje Exp $"

/*	Copyright (c) 1984 AT&T	*/
/*	  All Rights Reserved  	*/

/*	THIS IS UNPUBLISHED PROPRIETARY SOURCE CODE OF AT&T	*/
/*	The copyright notice above does not evidence any   	*/
/*	actual or intended publication of such source code.	*/

/*LINTLIBRARY*/
/*
 *	ssignal, gsignal: software signals
 */
#include <signal.h>

/* Highest allowable user signal number */
#ifndef MAXSIG
#define MAXSIG 16
#endif

/* Lowest allowable signal number (lowest user number is always 1) */
#define MINSIG (-4)

/* Table of signal values */
static int (*sigs[MAXSIG-MINSIG+1])();

int
(*ssignal(sig, fn))()
register int sig;
register int (*fn)();
{
	register int (*savefn)();

	if(sig >= (int)MINSIG && sig <= (int)MAXSIG) {
		savefn = sigs[sig-MINSIG];
		sigs[sig-MINSIG] = fn;
	} else
		savefn = (int (*)())SIG_DFL;

	return(savefn);
}

int
gsignal(sig)
register int sig;
{
	register int (*sigfn)();

	if(sig < MINSIG || sig > MAXSIG ||
	   (sigfn = sigs[sig-MINSIG]) == (int (*)())SIG_DFL)
		return(0);
	else if(sigfn == (int (*)())SIG_IGN)
		return(1);
	else {
		sigs[sig-MINSIG] = (int (*)())SIG_DFL;
		return((*sigfn)(sig));
	}
}
