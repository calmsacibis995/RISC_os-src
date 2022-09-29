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
#ident	"$Header: pipe.c,v 1.10.1.2 90/05/10 05:51:31 wje Exp $"

/*	Copyright (c) 1984 AT&T	*/
/*	  All Rights Reserved  	*/

/*	THIS IS UNPUBLISHED PROPRIETARY SOURCE CODE OF AT&T	*/
/*	The copyright notice above does not evidence any   	*/
/*	actual or intended publication of such source code.	*/

#include "sys/types.h"
#include "sys/signal.h"		/* needed by user.h */
#include "sys/pcb.h"		/* needed by user.h */
#include "sys/user.h"
#include "sys/vnode.h"
#include "sys/file.h"
#include "sys/debug.h"

/*
 * Generic pipe sys call. 
 */

sysv_pipe()
{
	struct vnode *vp;
	register struct file *rf, *wf;
	register int r;
	extern	struct vnode *makefifovp();
	extern	struct fileops vnodefops;

	/* get a vnode from the fifo/spec filesystem */
	vp = makefifovp();
	if ((rf = falloc()) == NULL) {
		VN_RELE(vp);
		return(u.u_error);
	}
	r = u.u_rval1;
	if ((wf = falloc()) == NULL) {
		VN_RELE(vp);
		u.u_ofile[r] = NULL;
		unfalloc(rf);
		return(u.u_error);
	}
	u.u_rval2 = u.u_rval1;
	u.u_rval1 = r;
	VN_HOLD(vp);	/* Bump vp->v_count:  we have 2 file descriptors */
	rf->f_flag = FREAD;
	wf->f_flag = FWRITE;
	rf->f_type = wf->f_type = DTYPE_VNODE;
	rf->f_ops  = wf->f_ops  = &vnodefops;
	rf->f_data = wf->f_data = (caddr_t)vp;
	(void) VOP_OPEN(&vp, FREAD|FWRITE, u.u_cred);
}


