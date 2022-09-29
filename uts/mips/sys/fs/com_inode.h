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
/* $Header: com_inode.h,v 1.8.4.2 90/05/10 06:15:49 wje Exp $ */

#ifndef	_SYS_FS_COM_INODE_
#define	_SYS_FS_COM_INODE_

/*
 * Common inode support.  Filesystems which share a similar set of
 * semantics and information can also share procedures if they
 * use this structure.
 */

/*
 * Common inode information.  For filesystems using this, this structure
 * must be the first element in the private inode date (pointed at by
 * i_fsptr).
 */
struct com_inode {
	long	ci_magic;	/* magic number */
	time_t	ci_atime;	/* time last accessed */
	time_t	ci_mtime;	/* time last modified */
	time_t	ci_ctime;	/* time created or changed */
	ushort	ci_mode;	/* protection mode */
	short	ci_lastlen;	/* last length read */
	daddr_t	ci_lastbn;	/* last logical block read */
	daddr_t	*ci_map;	/* block list map for paged files */
};

/* convert inode fs pointer to structure pointer */
#define	com_fsptr(ip)	((struct com_inode *) (ip)->i_fsptr)

#define	COM_MAGIC	0x12345678	/* identifies "com" filesystem */
#define	COM_ROOTINO	((ino_t) 2)	/* XXX should be in fsinfo[] */

#ifdef	INKERNEL
extern	short	com_fstyp;
#endif

#endif	_SYS_FS_COM_INODE_
