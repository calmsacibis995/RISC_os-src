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
#ident	"$Header: posix_prot.c,v 1.3.1.5 90/05/10 20:48:59 wje Exp $"

/*
 * Routines from BSD kern_prot.c
 *
 * System calls related to processes and protection
 *
 * Copyright (c) 1982, 1986 Regents of the University of California.
 * All rights reserved.  The Berkeley software License Agreement
 * specifies the terms and conditions for redistribution.
 *
 *	@(#)kern_prot.c	7.1 (Berkeley) 6/5/86
 */

/*
 * :r !includes proc.h user.h errno.h
 */

#include "sys/types.h"
#include "sys/immu.h"
#include "sys/region.h"
#include "sys/sbd.h"
#include "sys/proc.h"
#include "sys/param.h"
#include "sys/errno.h"
#include "sys/signal.h"
#include "sys/pcb.h"
#include "sys/dir.h"
#include "sys/user.h"
#include "sys/sema.h"
#include "sys/comm.h"
#include "sys/ipc.h"
#include "sys/msg.h"
#include "sys/vnode.h"
#include "sys/stream.h"
#include "sys/termio.h"
#include "sys/stty_ld.h"
#include "sys/strstat.h"
#include "sys/file.h"
#include "sys/conf.h"
#include "sys/mount.h"
#include "sys/message.h"
#include "sys/errno.h"
#include "sys/var.h"
#include "sys/debug.h"
#include "sys/numips.h"

posix_getpgrp()
{
	u.u_rval1 = u.u_procp->p_jcpgrp;
}

setsid()
{
	register struct proc *p = u.u_procp;
	struct  proc *q;
	struct	vnode *vp;

	if (p->p_pgrp == p->p_pid) {
		u.u_error = EPERM;
		return;
	}

	for (q = &proc[2]; q < (struct proc *)v.ve_proc; q++) {
		if ((q != p) && (q->p_jcpgrp == p->p_pid) &&
		    (q->p_stat != SZOMB)) {
			u.u_error = EPERM;
			return;
		}
	}
		
	u.u_ttyd = 0;		  	
	u.u_ttyp = NULL;
	u.u_ttyvp = NULL;
	clr_controlling_tty();
	p->p_pgrp = p->p_pid;
	p->p_jcpgrp = p->p_pgrp;
	u.u_rval1 = p->p_pgrp;
}
