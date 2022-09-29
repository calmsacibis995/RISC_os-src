/* ------------------------------------------------------------------ */
/* | Copyright Unpublished, MIPS Computer Systems, Inc.  All Rights | */
/* | Reserved.  This software contains proprietary and confidential | */
/* | information of MIPS and its suppliers.  Use, disclosure or     | */
/* | reproduction is prohibited without the prior express written   | */
/* | consent of MIPS.                                               | */
/* ------------------------------------------------------------------ */
/* $Header: misc.c,v 1.3.1.5 90/05/09 15:06:26 wje Exp $ */

#include "sys/param.h"
#include "sys/types.h"
#include "sys/numips.h"
#include "sys/sysmacros.h"
#include "bsd/sys/time.h"
#include "sys/systm.h"
#include "sys/tuneable.h"
#include "sys/signal.h"
#include "sys/sbd.h"
#include "sys/immu.h"
#include "sys/psw.h"
#include "sys/pcb.h"
#include "sys/user.h"
#include "sys/var.h"
#include "sys/errno.h"
#include "sys/file.h"
#include "sys/vnode.h"
#include "sys/fs/fifo.h"
#include "posix/sys/sysmips.h"
#include "bsd43/sys/dirent.h"
#include "sys/termio.h"

/*
 *	Return number of signals the kernel know about.
 */
getmaxsig()
{
	u.u_rval1 = NSIG;
}

extern	int _posix_chown_restricted;
extern 	int _posix_vdisable;

posix_sysmips()
{
	register struct  a {
		int	cmd;
		int	arg1, arg2, arg3;
	} *uap = (struct a *) u.u_ap;

	switch (uap->cmd) {
	case GET_POSIX_CHOWN_RESTRICTED:
		u.u_rval1 = (_posix_chown_restricted == 0 ? 0 : -1);
		return;

	case GET_POSIX_VDISABLE:
		u.u_rval1 = (_posix_vdisable == 0 ? (char) CDEL : (int) -1);
		return;

	case GET_PIPE_BUF:
		u.u_rval1 = FIFO_BUF;
		return;

	case GET_OPEN_MAX:
		u.u_rval1 = v.v_nofiles;
		return;

	case GET_CHILD_MAX:
		u.u_rval1 = v.v_maxup;
		return;
	
	case GET_ARG_MAX:
		u.u_rval1 = NCARGS;
		return;
	
	case GET_NGROUPS_MAX:
		u.u_rval1 = NGROUPS;
		return;
	
	case GET_CLK_TCK:
		u.u_rval1 = HZ;
		return;
	
	case GET_NAME_MAX:
		u.u_rval1 = MAXNAMLEN;
		return;

	case GET_PATH_MAX:
		u.u_rval1 = MAXPATHLEN - 1;
		return;

	case GET_LINK_MAX:
		u.u_rval1 = MAXLINK;
		return;

	case GET_MAX_CANON:
		u.u_rval1 = CANBSIZ;
		return;

	case GET_MAX_INPUT:
		u.u_rval1 = -1; 	/* indeterminant: depends on #  */
		return;			/* of available streams buffers */

	default:
		u.u_rval1 = -1;
		u.u_error = EINVAL;
		return;
	}
}
