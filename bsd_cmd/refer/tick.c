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
#ident	"$Header: tick.c,v 1.1.1.2 90/05/07 19:13:50 wje Exp $"
#ifndef lint
static char sccsid[] = "@(#)tick.c	4.2 (Berkeley) 8/11/83";
#endif

/* time programs */
# include "stdio.h"
# include "sys/types.h"
# include "sys/timeb.h"
struct tbuffer {
	long	proc_user_time;
	long	proc_system_time;
	long	child_user_time;
	long	child_system_time;
};
static long start, user, system;
tick()
{
	struct tbuffer tx;
	struct timeb tp;
	times (&tx);
	ftime (&tp);
	user =  tx.proc_user_time;
	system= tx.proc_system_time;
	start = tp.time*1000+tp.millitm;
}
tock()
{
	struct tbuffer tx;
	struct timeb tp;
	float lap, use, sys;
	if (start==0) return;
	times (&tx);
	ftime (&tp);
	lap = (tp.time*1000+tp.millitm-start)/1000.;
	use = (tx.proc_user_time - user)/60.;
	sys = (tx.proc_system_time - system)/60.;
	printf("Elapsed %.2f CPU %.2f (user %.2f, sys %.2f)\n",
		lap, use+sys, use, sys);
}
