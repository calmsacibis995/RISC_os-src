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
#ident	"$Header: vfs_xxx.c,v 1.2.1.4 90/05/10 06:00:40 wje Exp $"

/*	@(#)vfs_xxx.c	2.1 88/05/18 4.0NFSSRC SMI;  from SMI 2.9 86/11/11	*/

/* Originally included param.h systm.h user.h vnode.h file.h vfs.h */

#include "sys/types.h"
#include "sys/signal.h"		/* needed by user.h */
#include "sys/pcb.h"		/* needed by user.h */
#include "sys/user.h"
#include "sys/uio.h"
#include "sys/errno.h"
#include "sys/vnode.h"
#include "sys/file.h"
#include "sys/vfs.h"

#if RISCOS
#define COMPAT	1
#endif

#ifdef COMPAT
/*
 * Oh, how backwards compatibility is ugly!!!
 */
struct	ostat {
	dev_t	ost_dev;
	u_short	ost_ino;
	u_short ost_mode;
	short  	ost_nlink;
	short  	ost_uid;
	short  	ost_gid;
	dev_t	ost_rdev;
	int	ost_size;
	int	ost_atime;
	int	ost_mtime;
	int	ost_ctime;
};

/*
 * The old fstat system call.
 */
ofstat()
{
	register struct a {
		int	fd;
		struct ostat *sb;
	} *uap = (struct a *)u.u_ap;
	struct file *fp;
	extern struct file *getinode();

	u.u_error = getvnodefp(uap->fd, &fp);
	if (u.u_error)
		return;
	u.u_error = ostat1((struct vnode *)fp->f_data, uap->sb);
}

/*
 * Old stat system call.  This version follows links.
 */
ostat()
{
	struct vnode *vp;
	register struct a {
		char	*fname;
		struct ostat *sb;
	} *uap;

	uap = (struct a *)u.u_ap;
	u.u_error =
#if 0
	    lookupname(uap->fname, UIOSEG_USER, FOLLOW_LINK,
		(struct vnode **)0, &vp);
#endif
	    lookupname(uap->fname, UIO_USERSPACE, FOLLOW_LINK,
		(struct vnode **)0, &vp);
	if (u.u_error)
		return;
	u.u_error = ostat1(vp, uap->sb);
#if 0
	VRELE(vp);
#endif
	VN_RELE(vp);
}

int
ostat1(vp, ub)
	register struct vnode *vp;
	struct ostat *ub;
{
	struct ostat ds;
	struct vattr vattr;
	register int error;

	error = VOP_GETATTR(vp, &vattr, u.u_cred);
	if (error)
		return(error);
	/*
	 * Copy from inode table
	 */
	ds.ost_dev = vattr.va_fsid;
	ds.ost_ino = (short)vattr.va_nodeid;
	ds.ost_mode = (u_short)vattr.va_mode;
	ds.ost_nlink = vattr.va_nlink;
	ds.ost_uid = (short)vattr.va_uid;
	ds.ost_gid = (short)vattr.va_gid;
	ds.ost_rdev = (dev_t)vattr.va_rdev;
	ds.ost_size = (int)vattr.va_size;
	ds.ost_atime = (int)vattr.va_atime.tv_sec;
	ds.ost_mtime = (int)vattr.va_mtime.tv_sec;
	ds.ost_ctime = (int)vattr.va_atime.tv_sec;
#if RISCOS
	return (copyout((caddr_t)&ds, (caddr_t)ub, sizeof(ds)) ? EFAULT : 0 );
#else
	return (copyout((caddr_t)&ds, (caddr_t)ub, sizeof(ds)));
#endif
}

/*
 * Set IUPD and IACC times on file.
 * Can't set ICHG.
 */
outime()
{
	register struct a {
		char	*fname;
		time_t	*tptr;
	} *uap = (struct a *)u.u_ap;
	struct vattr vattr;
	time_t tv[2];

	u.u_error = copyin((caddr_t)uap->tptr, (caddr_t)tv, sizeof (tv));
#if RISCOS
	if (u.u_error)
		u.u_error = EFAULT;
#endif
	if (u.u_error)
		return;
	vattr_null(&vattr);
	vattr.va_atime.tv_sec = tv[0];
	vattr.va_atime.tv_usec = 0;
	vattr.va_mtime.tv_sec = tv[1];
	vattr.va_mtime.tv_usec = 0;
	u.u_error = namesetattr(uap->fname, FOLLOW_LINK, &vattr);
}
#endif

#if RISCOS
#include "sys/ustat.h"
#else
/*
 * System V Interface Definition-compatible "ustat" call.
 */
struct ustat {
	daddr_t	f_tfree;	/* Total free blocks */
	ino_t	f_tinode;	/* Number of free inodes */
	char	f_fname[6];	/* null */
	char	f_fpack[6];	/* null */
};
#endif /* RISCOS */

#if RISCOS
ustat(dev, buf)
	int	dev;
	struct ustat *buf;
{ /* } */
#else
ustat()
{
	register struct a {
		int	dev;
		struct ustat *buf;
	} *uap = (struct a *)u.u_ap;
#endif
	struct vfs *vfs;
	struct bsd43_statfs sb;
	struct ustat usb;

	/*
	 * The most likely source of the "dev" here is a "stat" structure.
	 * It's a "short" there.  Unfortunately, it's a "long" before
	 * it gets there, and has its sign bit set when it's an NFS
	 * file system.  Turning it into a "short" and back to a "int"/"long"
	 * smears the sign bit through the upper 16 bits; we have to get
	 * rid of the sign bit.  Yuk.
	 */
#if RISCOS
	u.u_error = vafsidtovfs((long)(dev & 0xffff), &vfs);
#else
	u.u_error = vafsidtovfs((long)(uap->dev & 0xffff), &vfs);
#endif
	if (u.u_error)
		return;
#ifdef RISCOS
	bzero(&sb, sizeof(sb));
#endif
	u.u_error = VFS_STATFS(vfs, &sb);
	if (u.u_error)
		return;
	bzero((caddr_t)&usb, sizeof(usb));
	/*
	 * We define a "block" as being the unit defined by DEV_BSIZE.
	 * System V doesn't define it at all, except operationally, and
	 * even there it's self-contradictory; sometimes it's a hardcoded
	 * 512 bytes, sometimes it's whatever the "physical block size"
	 * of your filesystem is.
	 */
	usb.f_tfree = howmany(sb.f_bavail * sb.f_bsize, DEV_BSIZE);
	usb.f_tinode = sb.f_ffree;
#if RISCOS
	bcopy(sb.f_fname, usb.f_fname, sizeof(usb.f_fname));
	bcopy(sb.f_fpack, usb.f_fpack, sizeof(usb.f_fpack));
	u.u_error = copyout((caddr_t)&usb, (caddr_t)buf, sizeof(usb));
	if (u.u_error)
		u.u_error = EFAULT;
#else
	u.u_error = copyout((caddr_t)&usb, (caddr_t)uap->buf, sizeof(usb));
#endif
}
