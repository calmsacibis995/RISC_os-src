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
/* $Header: fstyp.h,v 1.12.3.2 90/05/10 06:20:36 wje Exp $ */

#ifndef	_SYS_FSTYP_
#define	_SYS_FSTYP_	1


/*	Copyright (c) 1984 AT&T	*/
/*	  All Rights Reserved  	*/

/*	THIS IS UNPUBLISHED PROPRIETARY SOURCE CODE OF AT&T	*/
/*	The copyright notice above does not evidence any   	*/
/*	actual or intended publication of such source code.	*/

#define NULL_FS	0		/* Null FS type - Invalid */

#define FSTYPSZ		16	/* max size of fs identifier */

/*
 * Opcodes for the sysfs() system call.
 */
#define GETFSIND	1	/* translate fs identifier to fstype index */
#define GETFSTYP	2	/* translate fstype index to fs identifier */
#define GETNFSTYP	3	/* return the number of fstypes */

#ifdef INKERNEL
/* Bit settings for fs_flags */
#define FS_NOICACHE	0x1	/* Retain old inodes in in-core cache. */
				/* Should an iput be done on last */
				/* reference?  If this flag is set, an */
				/* iput() is done.  System V fs should have */
				/* this clear */

#define	FSBSIZE(ip)	((ip)->i_mntdev)->m_bsize

/* Passed to fs_getinode to indicate intended use of inode */
#define FSG_PIPE	1	/* Pipe */
#define FSG_CLONE	2	/* Use for clone device */

extern	short	pipefstyp;

#define	FSPTR	1
#ifdef FSPTR	/* XXX some structs are defined only #ifdef FSPTR */

#define FS_IPUT(ip) (*(ip)->i_fstypp->fs_iput)(ip)
#define FS_IREAD(ip) (*(ip)->i_fstypp->fs_iread)(ip)
#define FS_IUPDAT(ip, tm1, tm2) (*(ip)->i_fstypp->fs_iupdat)(ip, tm1, tm2)
#define FS_READI(ip) (*(ip)->i_fstypp->fs_readi)(ip)
#define FS_WRITEI(ip) (*(ip)->i_fstypp->fs_writei)(ip)
#define FS_ITRUNC(ip) (*(ip)->i_fstypp->fs_itrunc)(ip)
#define FS_STATF(ip, arg) (*(ip)->i_fstypp->fs_statf)(ip, arg)
#define FS_NAMEI(ip, p, arg) (*(ip)->i_fstypp->fs_namei)(p, arg)
#define FS_OPENI(ip, mode) (*(ip)->i_fstypp->fs_openi)(ip, mode)
#define FS_CLOSEI(ip, f, c, o) (*(ip)->i_fstypp->fs_closei)(ip, f, c, o)
#define FS_ACCESS(ip, mode) (*(ip)->i_fstypp->fs_access)(ip, mode)
#define FS_GETDENTS(ip, bp, bsz) (*(ip)->i_fstypp->fs_getdents)(ip, bp, bsz)
#define FS_ALLOCMAP(ip) (*(ip)->i_fstypp->fs_allocmap)(ip)
#define FS_FREEMAP(ip) (*(ip)->i_fstypp->fs_freemap)(ip)
#define FS_READMAP(ip, off, sz, va, sf) (*(ip)->i_fstypp->fs_readmap)(ip, off, sz, va, sf)
#define FS_SETATTR(ip, mode) (*(ip)->i_fstypp->fs_setattr)(ip, mode)
#define FS_NOTIFY(ip, arg) (*(ip)->i_fstypp->fs_notify)(ip, arg)
#define FS_FCNTL(ip, cmd, arg, flag, offset) (*(ip)->i_fstypp->fs_fcntl)(ip, cmd, arg, flag, offset)
#define FS_IOCTL(ip, cmd, arg, flag) (*(ip)->i_fstypp->fs_ioctl)(ip, cmd, arg, flag)

/*
 * Extensions for uniformity's sake.
 */
#define	FS_INIT(fstyp) \
	(*fstypsw[fstyp].fs_init)()
#define	FS_MOUNT(fstyp, devip, mp, flags) \
	(*fstypsw[fstyp].fs_mount)(devip, mp, flags)
#define	FS_UMOUNT(mp) \
	(*fstypsw[(mp)->m_fstyp].fs_umount)(mp)
#define	FS_STATFS(ip, sfsp, fstyp) \
	(*fstypsw[(fstyp)? (fstyp): (ip)->i_fstyp].fs_statfs)(ip, sfsp, fstyp)
#define	FS_UPDATE(mp) \
	(*fstypsw[(mp)->m_fstyp].fs_update)(mp)

/*
 * Extensions for inode and root manipulation.
 */
struct bmapval {		/* structure used by fs-switch bmap routine */
	daddr_t	bn;		/* physical block # */
	long	length;		/* length of this extent in bytes */
};

#define	FS_BMAP(ip, how, direct, readahead) \
	(*(ip)->i_fstypp->fs_bmap)(ip, how, direct, readahead)
#define	FS_IALLOC(ip, mode, nlinks, cbdev) \
	(*(ip)->i_fstypp->fs_ialloc)(ip, mode, nlinks, cbdev)
#define	FS_IDESTROY(ip) \
	(*(ip)->i_fstypp->fs_idestroy)(ip)
#define	FS_IFREE(ip) \
	(*(ip)->i_fstypp->fs_ifree)(ip)
#define	FS_SETSIZE(ip, size) \
	(*(ip)->i_fstypp->fs_setsize)(ip, size)
#define	FS_ROOTMOUNT(i, iflg) \
	(*fstypsw[i].fs_rmount)(iflg)
#define	FS_ROOTUNMOUNT(i) \
	(*fstypsw[i].fs_rumount)()
#define FS_FSYNC(ip,cred) \
  	(*(ip)->i_fstypp->fs_fsync)(ip,cred)

/*
 * Directory operation extensions.
 * NB: DIR_LOOKUP should account blocks read via sysinfo.dirblk++
 */
struct dirlookupres {
	ino_t	dlr_inum;	/* entry inumber or zero if not-found */
	off_t	dlr_offset;	/* offset or free space cookie if not-found */
};

typedef	unsigned char dflag_t;	/* flag for fs_dirisempty */
#define	DIR_HASDOT	0x1
#define	DIR_HASDOTDOT	0x2

#define	DIR_LOOKUP(dp, name, len, startoff, dlresp) \
	(*(dp)->i_fstypp->fs_dirlookup)(dp, name, len, startoff, dlresp)
#define	DIR_ENTER(dp, name, len, dlresp) \
	(*(dp)->i_fstypp->fs_direnter)(dp, name, len, dlresp)
#define	DIR_REMOVE(dp, name, len, dlresp) \
	(*(dp)->i_fstypp->fs_dirremove)(dp, name, len, dlresp)
#define	DIR_INIT(dp, pinum) \
	(*(dp)->i_fstypp->fs_dirinit)(dp, pinum)
#define	DIR_ISEMPTY(dp, dflagp) \
	(*(dp)->i_fstypp->fs_dirisempty)(dp, dflagp)

#else	/* FSPTR */

#define FS_IPUT(ip) (*fstypsw[(ip)->i_fstyp].fs_iput)(ip)
#define FS_IREAD(ip) (*fstypsw[(ip)->i_fstyp].fs_iread)(ip)
#define FS_IUPDAT(ip, tm1, tm2) (*fstypsw[(ip)->i_fstyp].fs_iupdat)(ip, tm1, tm2)
#define FS_READI(ip) (*fstypsw[(ip)->i_fstyp].fs_readi)(ip)
#define FS_WRITEI(ip) (*fstypsw[(ip)->i_fstyp].fs_writei)(ip)
#define FS_ITRUNC(ip) (*fstypsw[(ip)->i_fstyp].fs_itrunc)(ip)
#define FS_STATF(ip, arg) (*fstypsw[(ip)->i_fstyp].fs_statf)(ip, arg)
#define FS_NAMEI(ip, p, arg) (*fstypsw[(ip)->i_fstyp].fs_namei)(p, arg)
#define FS_OPENI(ip, mode) (*fstypsw[(ip)->i_fstyp].fs_openi)(ip, mode)
#define FS_CLOSEI(ip, f, c, o) (*fstypsw[(ip)->i_fstyp].fs_closei)(ip, f, c, o)
#define FS_ACCESS(ip, mode) (*fstypsw[(ip)->i_fstyp].fs_access)(ip, mode)
#define FS_GETDENTS(ip, bp, bsz) (*fstypsw[(ip)->i_fstyp].fs_getdents)(ip, bp, bsz)
#define FS_ALLOCMAP(ip) (*fstypsw[(ip)->i_fstyp].fs_allocmap)(ip)
#define FS_FREEMAP(ip) (*fstypsw[(ip)->i_fstyp].fs_freemap)(ip)
#define FS_READMAP(ip, off, sz, va, sf) (*fstypsw[(ip)->i_fstyp].fs_readmap)(ip, off, sz, va, sf)
#define FS_SETATTR(ip, mode) (*fstypsw[(ip)->i_fstyp].fs_setattr)(ip, mode)
#define FS_NOTIFY(ip, arg) (*fstypsw[(ip)->i_fstyp].fs_notify)(ip, arg)
#define FS_FCNTL(ip, cmd, arg, flag, offset) (*fstypsw[(ip)->i_fstyp].fs_fcntl)(ip, cmd, arg, flag, offset)
#define FS_IOCTL(ip, cmd, arg, flag) (*fstypsw[(ip)->i_fstyp].fs_ioctl)(ip, cmd, arg, flag)

#endif	/* FSPTR */
#endif	/* INKERNEL */

#endif	_SYS_FSTYP_
