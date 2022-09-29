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
#ident	"$Header: gentty.c,v 1.12.1.7 90/05/22 18:31:52 wje Exp $"

#ifdef mips
#include "sys/sbd.h"
#include "sys/types.h"
#include "sys/sysmacros.h"
#include "sys/conf.h"
#include "sys/pcb.h"
#include "sys/signal.h"
#include "sys/immu.h"
#include "sys/region.h"
#include "sys/user.h"
#include "sys/proc.h"
#include "sys/vnode.h"
#include "sys/errno.h"
#include "sys/var.h"
#include "sys/file.h"
#include "sys/termio.h"
#else
#include "../h/param.h"
#include "../h/inode.h"
#include "../h/user.h"
#include "../h/conf.h"
#include "../h/proc.h"
#endif
#include "sys/poll.h"
#include "sys/cmn_err.h"

/*
 * indirect driver for controlling tty.
 */

#define STR_TTY() (u.u_procp->p_ttyvp != NULL \
		   	? cdevsw[major(u.u_procp->p_ttyvp->v_rdev)].d_str \
			: 0)
#define USE_CDEV(op) (u.u_procp->p_ttyvp != NULL \
			? *cdevsw[major(u.u_procp->p_ttyvp->v_rdev)].op \
			: 0)

/* ARGSUSED */
syopen(dev, flag)
dev_t dev;
int flag;
{
	if (sycheck()) {
		int 	cur_pgrp;
		int	cur_mask;
		struct file  *fp;
		struct vnode *vp;

		/* Since p_ttyvp is non-NULL, controlling terminal is
		 * already in open state.
		 */
		vp = u.u_procp->p_ttyvp;
		fp = u.u_procp->p_ttyfp;
		if (!fp || !vp || (struct vnode *)fp->f_data != vp) {
		    cmn_err(CE_WARN,
			"syopen: Inconsistent data fp:%x vp:%x", fp, vp);
		    u.u_error = EIO; /* inode not open */
		    return;
		}

		/*
	 	 * Require that terminal's current process group be 
		 * in same session as current process.
		 */
		cur_mask = u.u_procp->p_hold;
		u.u_procp->p_hold |= (sigmask(SIGTTIN) | sigmask(SIGTTOU));
		if (iioctl(u.u_procp->p_ttyvp,TIOCGPGRP,(char *) &cur_pgrp,
			      FWRITE | FREAD)) {
			u.u_error = 0;
			cur_pgrp = 0;
		};
		u.u_procp->p_hold = cur_mask;

		if (cur_pgrp != 0 &&
		    cur_pgrp != u.u_procp->p_jcpgrp) {
		  	struct proc *p;
	
		  	for (p = &proc[1]; p < (struct proc *)v.ve_proc; p++)
				if (p->p_jcpgrp == cur_pgrp) {
					if (p->p_pgrp == u.u_procp->p_pgrp)
						break;
					/* allow members of same session */
					/* to open /dev/tty, even if job */
					/* control process group does not */
					/* match 			 */
					break;
				};
			if (p >= (struct proc *)v.ve_proc) {
			    u.u_error = EIO;
			    return;
			}
		}
	}
}

/* ARGSUSED */
syclose(dev,flag,otyp)
dev_t dev;
int	flag;
int	otyp;
{
}

/* ARGSUSED */
syread(dev)
dev_t dev;
{
	int	resid;

	if (sycheck()) {
		struct file *ttyfp = u.u_procp->p_ttyfp;

		FP_HOLD(ttyfp);
		if (STR_TTY()) {
			if (u.u_procp->p_ttyvp->v_stream == NULL) {
				u.u_error = EIO;
				return;
			};
			strread(u.u_procp->p_ttyvp);
		} else
			USE_CDEV(d_read)(u.u_procp->p_ttyvp->v_rdev);
		FP_RELE(ttyfp);
	}
}

/* ARGSUSED */
sywrite(dev)
dev_t dev;
{
	if (sycheck()) {
		struct file *ttyfp = u.u_procp->p_ttyfp;

		FP_HOLD(ttyfp);
		if (STR_TTY()) {
			if (u.u_procp->p_ttyvp->v_stream == NULL) {
				u.u_error = EIO;
				return;
			};
			strwrite(u.u_procp->p_ttyvp);
		} else
			USE_CDEV(d_write)(u.u_procp->p_ttyvp->v_rdev);
		FP_RELE(ttyfp);
	}
}

/* ARGSUSED */
syioctl(dev, cmd, arg, mode)
dev_t dev;
int cmd, arg;
int mode;
{
	if (sycheck()) {
		struct file *ttyfp = u.u_procp->p_ttyfp;

		FP_HOLD(ttyfp);
		if (STR_TTY()) {
			if (u.u_procp->p_ttyvp->v_stream == NULL) {
				u.u_error = EIO;
				return;
			};
			strioctl(u.u_procp->p_ttyvp, cmd, arg, mode);
		} else
			USE_CDEV(d_ioctl)(u.u_procp->p_ttyvp->v_rdev, cmd, arg, mode);
		FP_RELE(ttyfp);
	}
}

/* ARGSUSED */
syselect(dev, selopt)
dev_t dev;
int selopt;
{
	register int	stat;
	if (sycheck()) {
		struct file *ttyfp = u.u_procp->p_ttyfp;

		FP_HOLD(ttyfp);
		if (STR_TTY()) {
			register int events;

			if (u.u_procp->p_ttyvp->v_stream == NULL) {
				u.u_error = EIO;
				FP_RELE(ttyfp);
				return (0);
			};

			if (selopt == SEL_READ)
				events = POLLIN; 
			else if (selopt == SEL_WRITE)
				events = POLLOUT;
			else if (selopt == SEL_EXC)
				events = POLLPRI;
			else {
				FP_RELE(ttyfp);
				return(0);
			}

			events = strpoll(u.u_procp->p_ttyvp->v_stream,events,0);
			stat = 0;
			if (selopt==SEL_READ && (events & POLLIN) ||
			    selopt==SEL_WRITE && (events&(POLLOUT|POLLPRI)) ||
			    selopt == SEL_EXC && (events & POLLHUP))
				stat = 1;
			else
				stat = 0;
		} else {
			stat = USE_CDEV(d_select)(u.u_procp->p_ttyvp->v_rdev,selopt);
		}
		FP_RELE(ttyfp);
		return(stat);
	}
	return(0);
}

sycheck()
{

	if (u.u_procp->p_ttyvp == NULL) {
		u.u_error = ENXIO;
		return(0);
	}

	return(1);			/* all is well */
}
