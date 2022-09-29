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
#ident	"$Header: utssys.c,v 1.11.1.2 90/05/10 05:58:04 wje Exp $"

/*	Copyright (c) 1984 AT&T	*/
/*	  All Rights Reserved  	*/

/*	THIS IS UNPUBLISHED PROPRIETARY SOURCE CODE OF AT&T	*/
/*	The copyright notice above does not evidence any   	*/
/*	actual or intended publication of such source code.	*/

#include "sys/types.h"
#include "sys/uadmin.h"
#include "sys/errno.h"
#include "sys/signal.h"
#include "sys/pcb.h"
#include "sys/user.h"
#include "sys/var.h"
#include "sys/immu.h"
#include "sys/region.h"
#include "sys/sbd.h"
#include "sys/proc.h"
#include "sys/systm.h"
#include "sys/bootconf.h"
#include "sys/utsname.h"
#include "sys/debug.h"
#include "sys/vfs.h"
#include "sys/fs/ufs_mount.h"

utssys()
{
	register i;
	register struct a {
		char	*cbuf;
		int	mv;
		int	type;
	} *uap;

	uap = (struct a *)u.u_ap;
	switch (uap->type) {
	case 0:		/* uname */
		if (copyout(&utsname, uap->cbuf,
		  sizeof(struct utsname)))
			u.u_error = EFAULT;
		return;

	/* case 1 was umask */

	case 2:		/* ustat */
		ustat(uap->mv, uap->cbuf);
		return;

	default:
		u.u_error = EFAULT;
	}
}

/*
 * administrivia system call
 */

uadmin()
{
	register struct a {
		int	cmd;
		int	fcn;
		int	mdep;
	} *uap;
	static ualock;

	if (ualock || !suser())
		return;
	ualock = 1;
	uap = (struct a *)u.u_ap;

	if ((uap->cmd == A_SHUTDOWN || uap->cmd == A_REBOOT))
	  graphics_shutdown();

	switch (uap->cmd) {

	case A_SHUTDOWN:
		{
			register struct proc *p = &proc[2];

			for (; p < (struct proc *)v.ve_proc; p++) {
				if (p->p_stat == NULL)
					continue;
				if (p != u.u_procp)
					psignal(p, SIGKILL);
			}
		}
		delay(HZ);	/* allow other procs to exit */
		xumount(rootvfs);
		update();
		VFS_MOUNTROOT(rootvfs, &rootvp, rootfs.bo_name, ROOT_UNMOUNT);
	/* FALL THROUGH */
	case A_REBOOT:
		mdboot(uap->fcn, uap->mdep);
		/* no return expected */
		break;

	case A_REMOUNT:
		/* remount root file system */
		VFS_MOUNTROOT(rootvfs, &rootvp, rootfs.bo_name, ROOT_REMOUNT);
		break;

	default:
		u.u_error = EINVAL;
	}
	ualock = 0;
}

