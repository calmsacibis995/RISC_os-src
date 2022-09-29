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
#ident	"$Header: clone.c,v 1.7.1.2 90/05/10 05:12:02 wje Exp $"

/*	Copyright (c) 1984 AT&T	*/
/*	  All Rights Reserved  	*/

/*	THIS IS UNPUBLISHED PROPRIETARY SOURCE CODE OF AT&T	*/
/*	The copyright notice above does not evidence any   	*/
/*	actual or intended publication of such source code.	*/

/*
 * Clone Driver.
 */

#include "sys/sbd.h"
#include "sys/types.h"
#include "sys/sysmacros.h"
#include "sys/param.h"
#include "sys/errno.h"
#include "sys/pcb.h"
#include "sys/signal.h"
#include "sys/user.h"
#include "sys/vnode.h"
#include "sys/conf.h"
#include "sys/debug.h"
#include "sys/stream.h"

int clnopen();
static struct module_info clnm_info = { 0, "CLONE", 0, 0, 0, 0 };
static struct qinit clnrinit = { NULL, NULL, clnopen, NULL, NULL, &clnm_info, NULL };
static struct qinit clnwinit = { NULL, NULL, NULL, NULL, NULL, &clnm_info, NULL };
struct streamtab clninfo = { &clnrinit, &clnwinit };

/*
 * Clone open.  Dev is the major device of the streams device to
 * open.  Look up the device in the cdevsw[].   Attach its qinit
 * structures to the read and write queues and call its open
 * with the q and the dev CLONEOPEN.  Swap in a new inode with
 * the real device number constructed from the major and the
 * minor returned by the device open.  
 */

int
clnopen(q, dev, flag, sflag)
queue_t *q;
int dev, flag, sflag;
{
	register short int extdev, mindev;
	register short int intdev;
	register int i;
	dev_t rdev;
	register struct streamtab *st;
	struct vnode *vp, *nvp;
	extern struct vnode *makespecvp();

	if (sflag) return(OPENFAIL);

	/*
	 * Get the device to open.
	 */
	extdev = minor(dev);
	intdev = major(makedev(extdev,0));
	if ((intdev >= cdevcnt) || !(st = cdevsw[intdev].d_str)) {
		u.u_error = ENXIO;
		return(OPENFAIL);
	}

	/*
	 * Substitute the real qinit values for the current ones
	 */
	setq(q, st->st_rdinit, st->st_wrinit);

	/*
	 * Call the device open with the stream flag  CLONEOPEN.  The device
	 * will either fail this or return a minor device number.
	 */
	rdev = makedev(extdev, 0);
	mindev = (*q->q_qinfo->qi_qopen)(q, rdev, flag, CLONEOPEN);
	if (mindev == OPENFAIL)
		return(OPENFAIL);

	/*
	 * Get the vnode at top of this stream, allocate a new vnode,
	 * and exchange the new vnode for the old one.
	 */
#if DMP
	rdev = makedev(extdev+emajor(mindev), minor(mindev));
#else
	rdev = makedev(extdev, mindev);
#endif
	vp = ((struct stdata *)(q->q_next->q_ptr))->sd_vnode;

	ASSERT(vp->v_stream);

	/* set up dummy vnode */

	/*
	 * Allocate a vnode from the special file system.
	 */
	nvp = makespecvp(rdev, VCHR);
	plock(vp);

	nvp->v_stream = vp->v_stream;	/* splice stream onto new vnode */

	/*
	 * link dummy vnode to file table and to the stream
	 */

	/*
	 * A comment is called for...
	 *
	 * In the old world (RISC/os 4.0 or earlier), the file structure
	 * was made to point to the (i,v)node as soon as it was allocated.
	 * Any clone device had to look through the file table, and break
	 * that link if a new (i,v)node was allocated and should be used.
	 *
	 * With NFS4.0, the f_data field does not get set until after
	 * the VOP_OPEN routine is called, and it used a return value
	 * from VOP_OPEN as the "true" value of the vnode.  As such, what
	 * we would like to do was change the return value of the vnode
	 * from a clone device.  Unfortunately, the interface between
	 * stropen and all devices would have to be changed to support
	 * that, which is not easy.  Instead, we use a kludgy method
	 * of communication.  All streams devices are opened via spec_open,
	 * since they live in the spec filesystem.  spec_open is able to
	 * affect the value of the returned vnode (which we want to stick
	 * in fp->f_data).  So we set up the strdata structure to point at
	 * the new vnode (which we have to do anyway) and then spec_open
	 * magically looks to see if the vnode has changed.
	 */
	nvp->v_stream->sd_vnode = nvp;
	nvp->v_stream->sd_strtab = st;
	prele(nvp);

	return(mindev);
}	
