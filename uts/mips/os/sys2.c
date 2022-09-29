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
 * |         950 DeGuigne Drive                                |
 * |         Sunnyvale, CA 94086                               |
 * |-----------------------------------------------------------|
 */
#ident	"$Header: sys2.c,v 1.33.1.4.1.1.1.2 90/11/15 13:42:06 beacker Exp $"

/*	Copyright (c) 1984 AT&T	*/
/*	  All Rights Reserved  	*/

/*	THIS IS UNPUBLISHED PROPRIETARY SOURCE CODE OF AT&T	*/
/*	The copyright notice above does not evidence any   	*/
/*	actual or intended publication of such source code.	*/

#include "sys/types.h"
#include "sys/signal.h"		/* needed by user.h */
#include "sys/pcb.h"		/* needed by user.h */
#include "sys/user.h"
#include "sys/immu.h"		/* needed by proc.h */
#include "sys/sbd.h"		/* needed by proc.h */
#include "sys/region.h"		/* needed by proc.h */
#include "sys/proc.h"
#include "sys/uio.h"
#include "sys/sysinfo.h"
#include "sys/var.h"
#include "sys/errno.h"
#include "sys/cmn_err.h"
#include "sys/debug.h"
#include "sys/numips.h"
#include "sys/file.h"
#include "bsd43/sys/file.h"
#include "sys/vnode.h"

/*
 * read system call
 */
/* Essentially stolen from NFS4.0:sys_generic.c */
read()
{
	register struct a {
		int	fdes;
		char	*cbuf;
		unsigned count;
	} *uap = (struct a *)u.u_ap;
	struct uio auio;
	struct iovec aiov;

	sysinfo.sysread++;
	aiov.iov_base = (caddr_t)uap->cbuf;
	aiov.iov_len = uap->count;
	auio.uio_iov = &aiov;
	auio.uio_iovcnt = 1;
	rwuio(&auio, UIO_READ);
}


/* Essentially stolen from NFS4.0:sys_generic.c */
readv()
{
	register struct a {
		int	fdes;
		struct	iovec *iovp;
		unsigned iovcnt;
	} *uap = (struct a *)u.u_ap;
	struct uio auio;
	struct iovec aiov[16];		/* XXX */

	sysinfo.sysread++;
	if (uap->iovcnt > sizeof(aiov)/sizeof(aiov[0])) {
		u.u_error = EINVAL;
		return;
	}
	auio.uio_iov = aiov;
	auio.uio_iovcnt = uap->iovcnt;
	u.u_error = bsd_copyin((caddr_t)uap->iovp, (caddr_t)aiov,
	    uap->iovcnt * sizeof (struct iovec));
	if (u.u_error)
		return;
	rwuio(&auio, UIO_READ);
}


/*
 * write system call
 */
/* Essentially stolen from NFS4.0:sys_generic.c */
write()
{
	register struct a {
		int	fdes;
		char	*cbuf;
		unsigned count;
	} *uap = (struct a *)u.u_ap;
	struct uio auio;
	struct iovec aiov;

	sysinfo.syswrite++;
	auio.uio_iov = &aiov;
	auio.uio_iovcnt = 1;
	aiov.iov_base = uap->cbuf;
	aiov.iov_len = uap->count;
	rwuio(&auio, UIO_WRITE);
}


/* Essentially stolen from NFS4.0:sys_generic.c */
writev()
{
	register struct a {
		int	fdes;
		struct	iovec *iovp;
		unsigned iovcnt;
	} *uap = (struct a *)u.u_ap;
	struct uio auio;
	struct iovec aiov[16];		/* XXX */

	sysinfo.syswrite++;
	if (uap->iovcnt > sizeof(aiov)/sizeof(aiov[0])) {
		u.u_error = EINVAL;
		return;
	}
	auio.uio_iov = aiov;
	auio.uio_iovcnt = uap->iovcnt;
	u.u_error = bsd_copyin((caddr_t)uap->iovp, (caddr_t)aiov,
	    uap->iovcnt * sizeof (struct iovec));
	if (u.u_error)
		return;
	rwuio(&auio, UIO_WRITE);
}


/* Essentially stolen from NFS4.0:sys_generic.c */
rwuio(uio, rw)
	register struct uio *uio;
	enum uio_rw rw;
{
	struct a {
		int	fdes;
	};
	register struct file *fp;
	register struct iovec *iov;
	int i; 
	unsigned count;

	GETF(fp, ((struct a *)u.u_ap)->fdes);
	if ((fp->f_flag&(rw==UIO_READ ? FREAD : FWRITE)) == 0) {
		u.u_error = EBADF;
		return;
	}
	uio->uio_resid = 0;
	uio->uio_segflg = UIO_USERSPACE;
	uio->uio_fmode = fp->f_flag;
	iov = uio->uio_iov;
	for (i = 0; i < uio->uio_iovcnt; i++) {
		uio->uio_resid += iov->iov_len;
		iov++;
	}
	/* POSIX requires that a 0 length read or a 
	 * 0 length write on a regular file, just return 0.
	 */
	if (POSIX_SYSCALL && uio->uio_resid == 0 && 
	    (rw == UIO_READ || 
	     (rw == UIO_WRITE && ((struct vnode *)fp->f_data)->v_type == VREG))) {
		u.u_rval1 = 0;
		return;
	}
	count = uio->uio_resid;
	uio->uio_offset = fp->f_offset;
	if (setjmp(u.u_qsav)) {
#if defined(RISCOS)
		/* In RISC/os, we must modify the code because on a system
		 * call we are not guaranteed to always get a long jump back.
		 * Therefore, we restart system calls a bit different for
		 * the BSD world.  Note that we have placed similar code after
		 * the guts of the call.  SystemV has no notion of a
		 * system call restart, so we don't.
		 */
		if (uio->uio_resid != count) {	/* if partial write */
			u.u_error = 0;
		} else if (BSD_SYSCALL &&
			 (u.u_sigintr & bsd43_sigmask(u.u_procp->p_cursig)) == 0) {
			u.u_eosys = RESTARTSYS;
			u.u_error = 0;
		} else
			u.u_error = EINTR;
#else
		if (uio->uio_resid == count) {
			if ((u.u_sigintr & sigmask(u.u_procp->p_cursig)) != 0)
				u.u_error = EINTR;
			else
				u.u_eosys = RESTARTSYS;
		}
#endif /* RISCOS */
	} else
		u.u_error = (*fp->f_ops->fo_rw)(fp, rw, uio);

#if defined(RISCOS)
	if (u.u_error == EINTR) {		/* restart system call */
		if (uio->uio_resid != count) {  /* if partial write */
			u.u_error = 0;
		} else if (BSD_SYSCALL && 
			   (u.u_sigintr & bsd43_sigmask(u.u_procp->p_cursig)) == 0) {
			u.u_eosys = RESTARTSYS;
			u.u_error = 0;
		}
	};
	/* POSIX: writes as many bytes as there is room for, and return
	 * number of bytes written.
	 */
	if (u.u_error == ENOSPC && POSIX_SYSCALL && rw == UIO_WRITE) {
	  	if (uio->uio_resid != count)	/* if partial write */
		  	u.u_error = 0;
	}

	if (u.u_error == 0 && u.u_rval1 == -1)	/* For pipes: see the SVID */
		if (BSD_SYSCALL)
			u.u_error = EWOULDBLOCK;
		else
			u.u_error = EAGAIN;

	if (u.u_error)				/* sanity checking */
		u.u_eosys = NORMALRETURN;

	if (u.u_error ||			/* don't modify below vars */
	    u.u_eosys == RESTARTSYS)
		return;
#endif /* RISCOS */

	u.u_rval1 = count - uio->uio_resid;
	u.u_ioch += (unsigned)u.u_rval1;
	fp->f_offset += u.u_rval1;
	if (rw == UIO_READ) {
		sysinfo.readch += (unsigned)u.u_rval1;
	} else {
		sysinfo.writech += (unsigned)u.u_rval1;
	}
}

/*
 * open system call
 */
bsd43_open(uap)
	register struct a {
		char *fnamep;
		int fmode;
		int cmode;
	} *uap;
{
	register int flags;

	flags = open_bsd_to_sysv(uap->fmode - FOPEN);
	u.u_error = copen(uap->fnamep, flags, uap->cmode);
}

/* SAMEFLAGS is the file flags which are unchanged between SysV (internal
 * and used by SysV progs) and BSD (used only by BSD progs).
 */
#define SAMEFLAGS (FREAD|FWRITE|FNDELAY|FAPPEND|FASYNC)

/* This expects internal file flags, and returns internal file flags. */
open_bsd_to_sysv(bsdflags)
	int	bsdflags;
{
	int	sysvflags = 0;

	sysvflags = bsdflags & SAMEFLAGS;
	if (bsdflags & BSD43_FCREAT)
		sysvflags |= FCREAT;
	if (bsdflags & BSD43_FTRUNC)
		sysvflags |= FTRUNC;
	if (bsdflags & BSD43_FEXCL)
		sysvflags |= FEXCL;
	if (bsdflags & BSD43_FDEFER)
		sysvflags |= FDEFER;
	if (bsdflags & BSD43_FMARK)
		sysvflags |= FMARK;
	if (bsdflags & BSD43_FSHLOCK)
		sysvflags |= FSHLOCK;
	if (bsdflags & BSD43_FEXLOCK)
		sysvflags |= FEXLOCK;
	if (bsdflags & BSD43_FSYNC)
		sysvflags |= FSYNC;
	return(sysvflags);
}
#undef SAMEFLAGS

/*
 * close system call
 */
close()
{
	register struct file *fp;
	register struct a {
		int	fdes;
	} *uap;

	uap = (struct a *)u.u_ap;
	GETF(fp, uap->fdes);

	/* Release all System-V style record locks, if any */
	(void) vno_lockrelease(fp);	/* WHAT IF error returned? */

	if (u.u_pofile[uap->fdes] & UF_MAPPED)
	  	munmapfd(uap->fdes);
	u.u_ofile[uap->fdes] = NULL;
	u.u_pofile[uap->fdes] = 0;
	closef(fp);
}

