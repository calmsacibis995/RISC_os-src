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
#ident	"$Header: klog.c,v 1.4.1.2 90/05/10 05:22:32 wje Exp $"
/*
 * Copyright (c) 1982, 1986 Regents of the University of California.
 * All rights reserved.  The Berkeley software License Agreement
 * specifies the terms and conditions for redistribution.
 *
 *	@(#)subr_log.c	7.1 (Berkeley) 6/5/86
 */

/*
 * Error log buffer for kernel printf's.
 */

#include "sys/sbd.h"
#include "sys/types.h"
#include "sys/sysmacros.h"
#include "sys/param.h"
#include "sys/buf.h"
#include "sys/signal.h"
#include "sys/var.h"
#include "sys/immu.h"
#include "sys/region.h"
#include "sys/pcb.h"
#include "bsd/sys/time.h"
#include "sys/user.h"
#include "sys/proc.h"
#include "sys/ioctl.h"
#include "bsd43/sys/msgbuf.h"
#include "sys/file.h"
#include "sys/errno.h"
#include "sys/termio.h"

#ifdef mips
#define msgbuf (*bsd43_pmsgbuf)
#endif mips

#ifndef MIN
#define MIN(a,b)	((a) <= (b) ? (a) : (b))
#endif MIN

#define BSD43_LOG_RDPRI	(PZERO + 1)

#define BSD43_LOG_NBIO	0x02
#define BSD43_LOG_ASYNC	0x04
#define BSD43_LOG_RDWAIT	0x08

struct logsoftc {
	int	sc_state;		/* see above for possibilities */
	struct	proc *sc_selp;		/* process waiting on select call */
	int	sc_pgrp;		/* process group for async I/O */
} logsoftc;

int	log_open;			/* also used in log() */

/*ARGSUSED*/
logopen(dev)
	dev_t	dev;
{

	if (log_open)
		return (EBUSY);
	log_open = 1;
	logsoftc.sc_selp = 0;
	logsoftc.sc_pgrp = u.u_procp->p_jcpgrp;
	/*
	 * Potential race here with putchar() but since putchar should be
	 * called by autoconf, msg_magic should be initialized by the time
	 * we get here.
	 */
	if (msgbuf.msg_magic != BSD43_MSG_MAGIC) {
		register int i;

		msgbuf.msg_magic = BSD43_MSG_MAGIC;
		msgbuf.msg_bufx = msgbuf.msg_bufr = 0;
		for (i=0; i < BSD43_MSG_BSIZE; i++)
			msgbuf.msg_bufc[i] = 0;
	}
	return (0);
}

/*ARGSUSED*/
logclose(dev, flag)
	dev_t dev;
{
	log_open = 0;
	logsoftc.sc_state = 0;
	logsoftc.sc_selp = 0;
	logsoftc.sc_pgrp = 0;
}

/*ARGSUSED*/
logread(dev)
	dev_t dev;
{
	register long l;
	register int s;
	int error = 0;

	s = splhigh();
	while (msgbuf.msg_bufr == msgbuf.msg_bufx) {
		if (logsoftc.sc_state & BSD43_LOG_NBIO) {
			splx(s);
			return (EWOULDBLOCK);
		}
		logsoftc.sc_state |= BSD43_LOG_RDWAIT;
		sleep((caddr_t)&msgbuf, BSD43_LOG_RDPRI);
	}
	splx(s);
	logsoftc.sc_state &= ~BSD43_LOG_RDWAIT;

	while (u.u_count > 0) {
		l = msgbuf.msg_bufx - msgbuf.msg_bufr;
		if (l < 0)
			l = BSD43_MSG_BSIZE - msgbuf.msg_bufr;
		l = MIN(l, (unsigned)u.u_count);
		if (l == 0)
			break;
		error = iomove((caddr_t)&msgbuf.msg_bufc[msgbuf.msg_bufr],
			(int)l, B_READ);
		if (error)
			break;
		msgbuf.msg_bufr += l;
		if (msgbuf.msg_bufr < 0 || msgbuf.msg_bufr >= BSD43_MSG_BSIZE)
			msgbuf.msg_bufr = 0;
	}
	return (error);
}

/*ARGSUSED*/
logselect(dev, rw)
	dev_t dev;
	int rw;
{
	int s = splhigh();

	switch (rw) {

	case FREAD:
		if (msgbuf.msg_bufr != msgbuf.msg_bufx) {
			splx(s);
			return (1);
		}
		logsoftc.sc_selp = u.u_procp;
		break;
	}
	splx(s);
	return (0);
}

logwakeup()
{

	if (!log_open)
		return;
	if (logsoftc.sc_selp) {
		selwakeup(logsoftc.sc_selp, 0);
		logsoftc.sc_selp = 0;
	}
	if (logsoftc.sc_state & BSD43_LOG_ASYNC)
		signal(logsoftc.sc_pgrp, SIGIO); 
	if (logsoftc.sc_state & BSD43_LOG_RDWAIT) {
		wakeup((caddr_t)&msgbuf);
		logsoftc.sc_state &= ~BSD43_LOG_RDWAIT;
	}
}

/*ARGSUSED*/
logioctl(dev, com, data, flag)
	dev_t dev;
	unsigned int com;
	caddr_t data;
	int flag;
{
	long l;
	int s;

	switch (com) {

	/* return number of characters immediately available */
	case FIONREAD:
		s = splhigh();
		l = msgbuf.msg_bufx - msgbuf.msg_bufr;
		splx(s);
		if (l < 0)
			l += BSD43_MSG_BSIZE;
		*(off_t *)data = l;
		break;

	case FIONBIO:
		if (*(int *)data)
			logsoftc.sc_state |= BSD43_LOG_NBIO;
		else
			logsoftc.sc_state &= ~BSD43_LOG_NBIO;
		break;

	case FIOASYNC:
		if (*(int *)data)
			logsoftc.sc_state |= BSD43_LOG_ASYNC;
		else
			logsoftc.sc_state &= ~BSD43_LOG_ASYNC;
		break;

	case TIOCSPGRP:
		logsoftc.sc_pgrp = *(int *)data;
		break;

	case TIOCGPGRP:
		*(int *)data = logsoftc.sc_pgrp;
		break;

	default:
		u.u_error = EINVAL;
		return(-1);
	}
	return (0);
}
