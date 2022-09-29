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
#ident	"$Header: kern_proc.c,v 1.3.1.2 90/05/10 04:40:11 wje Exp $"

/*
 * Routines from BSD kern_proc.c
 *
 * Copyright (c) 1982, 1986 Regents of the University of California.
 * All rights reserved.  The Berkeley software License Agreement
 * specifies the terms and conditions for redistribution.
 *
 *	@(#)kern_proc.c	7.1 (Berkeley) 6/5/86
 */

/*
 * Headers: r !includes proc.h user.h var.h
 */
#include "sys/types.h"
#include "sys/signal.h"		/* needed by user.h */
#include "sys/pcb.h"		/* needed by user.h */
#include "sys/user.h"
#include "sys/immu.h"		/* needed by proc.h */
#include "sys/sbd.h"		/* needed by proc.h */
#include "sys/region.h"		/* needed by proc.h */
#include "sys/proc.h"
#include "sys/var.h"
#include "sys/errno.h"

/*
 * Clear any pending stops for top and all descendents.
 */
spgrp(top)
	struct proc *top;
{
	register struct proc *p;
	int f = 0;

	p = top;
	for (;;) {
		p->p_sig &=
			  ~(bsd43_sigmask(BSD43_SIGTSTP)|
			    bsd43_sigmask(BSD43_SIGTTIN)|
			    bsd43_sigmask(BSD43_SIGTTOU));
		f++;
		/*
		 * If this process has children, descend to them next,
		 * otherwise do any siblings, and if done with this level,
		 * follow back up the tree (but not past top).
		 */
		if (p->p_child)
			p = p->p_child;
		else if (p == top)
			return (f);
		else if (p->p_sibling)
			p = p->p_sibling;
		else for (;;) {
			p = p->p_parent;
			if (p == top)
				return (f);
			if (p->p_sibling) {
				p = p->p_sibling;
				break;
			}
		}
	}
}

/*
 * Is p an inferior of the current process?
 */
bsd_inferior(p)
	register struct proc *p;
{
	return(inferior(p));
}

inferior(p)
	register struct proc *p;
{
	for (; p != u.u_procp; p = p->p_parent)
		if (p->p_ppid == 0)
			return (0);
	return (1);
}



struct proc *bsd43_pfind();


struct proc *
bsd_pfind(pid)
	int pid;
{
	return(bsd43_pfind(pid));
}


struct proc *
bsd43_pfind(pid)
{
	register struct proc *p;

	if (pid < 0)
		return ((struct proc *)0);

	for (p = proc; p < (struct proc *)v.ve_proc; p++) {
		if (p->p_pid == pid && p->p_stat != NULL && p->p_stat != SZOMB)
			return (p);
	}
	return ((struct proc *)0);
}

/*
 * init the process queues
 */
#ifdef NOTDEF
pqinit()
{
	register struct proc *p;

	/*
	 * most procs are initially on freequeue
	 *	nb: we place them there in their "natural" order.
	 */

	freeproc = NULL;
	for (p = v.ve_proc; --p > proc; freeproc = p)
		p->p_flink = freeproc;

	/*
	 * but proc[0] is special ...
	 */

	p = &proc[0];
	allproc = p;
	p->p_flink = NULL;
	p->p_blink = &allproc;

	zombproc = NULL;
}
#endif NOTDEF
