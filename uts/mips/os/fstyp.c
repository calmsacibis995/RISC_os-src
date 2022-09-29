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
#ident	"$Header: fstyp.c,v 1.9.1.2 90/05/10 05:48:05 wje Exp $"

/*	Copyright (c) 1984 AT&T	*/
/*	  All Rights Reserved  	*/

/*	THIS IS UNPUBLISHED PROPRIETARY SOURCE CODE OF AT&T	*/
/*	The copyright notice above does not evidence any   	*/
/*	actual or intended publication of such source code.	*/

#include "sys/types.h"
#include "sys/signal.h"		/* needed by user.h */
#include "sys/pcb.h"		/* needed by user.h */
#include "sys/user.h"
#include "sys/uio.h"
#include "sys/pathname.h"
#include "sys/vfs.h"
#include "sys/fstyp.h"
#include "sys/errno.h"
#include "sys/cmn_err.h"

/*
 * NOTE: Developers making use of the File System Switch mechanism to
 * build file system types in SVR3 should be aware that the architecture
 * is not frozen and may change in future releases of System V.  File
 * system types developed for SVR3 may require changes in order to work
 * with future releases of the system.
 */

int fsstray();
int fsnull();
int fsstray();
int fs2stray();
struct inode *fsinstray();
int *fsistray();



fsinit()
{
}

fsnull()
{
	return(1);
}

#ifdef DEBUG
_fsstray(arg0, ra)
{
	printf ("stray fs switch: arg0 0x%x, ra 0x%x\n", arg0, ra);
}
#else
fsstray()
{
	/* oops */
	cmn_err(CE_WARN, "stray fs switch call");
}
#endif

int *
fsistray()
{
	printf("stray fs (int *) call\n");
	u.u_error = EINVAL;
	return(NULL);
}

struct inode *
fsinstray()
{
	printf("stray fs (inode *) call\n");
	u.u_error = EINVAL;
	return(NULL);
}

sysfs()
{

	register struct uap {
		int	opcode;
	} *uap;

	uap = (struct uap *) u.u_ap;
	switch (uap->opcode) {
	case GETFSIND:   
	{

	/*
	 *	Translate fs identifier to an index
	 *	into the fsinfo structure.
	 */
		register struct	a {
			int	opcode;
			char	*fsname;
		} *uap;

		struct pathname pn;
		struct vfssw *vs;

		uap = (struct a *) u.u_ap;
		u.u_error = pn_get(uap->fsname, UIO_USERSPACE, &pn);
		if (pn.pn_pathlen > FSTYPSZ || pn.pn_pathlen <= 0) {
			pn_free(&pn);
			u.u_error = EINVAL;
		};
		if (u.u_error)
			return;
		for (vs = vfssw; vs < vfsNVFS; vs++) {
			if (vs->vsw_name != 0 &&
			    strcmp(pn.pn_path, vs->vsw_name) == 0) {
				u.u_rval1 = (int)(vs - vfssw);
				pn_free(&pn);
				return;
			}
		}

		pn_free(&pn);
		u.u_error = EINVAL;
		break;
	}

	case GETFSTYP:
	{

	/*
	 *	Translate fstype index into an fs identifier
	 */
		register struct a {
			int	opcode;
			int	fs_index;
			char	*cbuf;
		} *uap;
		register int	index;
		register char	*src;
		char	*osrc;

		struct vfssw *vs;

		uap = (struct a *) u.u_ap;
		index = uap->fs_index;
		if (index <= 0 || index >= (int)(vfsNVFS - vfssw)) {
			u.u_error = EINVAL;
			return;
		}
		vs = &vfssw[index];
		src = vs->vsw_name ? vs->vsw_name : "";
		for (osrc = src; *src++;)
			;
	
		if (copyout(osrc, uap->cbuf, src - osrc))
			u.u_error = EFAULT;

		break;
	}

	case GETNFSTYP:

	/*
	 *	Return number of fstypes configured in the
	 *	system.
	 */

		u.u_rval1 = ((int)(vfsNVFS - vfssw)) - 1;
		break;

	default:
		u.u_error = EINVAL;
	}
}
