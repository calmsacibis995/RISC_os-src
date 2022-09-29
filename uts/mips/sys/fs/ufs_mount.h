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
/* $Header: ufs_mount.h,v 1.2.1.2 90/05/10 06:19:38 wje Exp $ */

/*
 * Copyright (c) 1982, 1986 Regents of the University of California.
 * All rights reserved.  The Berkeley software License Agreement
 * specifies the terms and conditions for redistribution.
 *
 */
/*	@(#)mount.h	2.2 88/06/09 4.0NFSSRC SMI;	from UCB 7.1 6/4/86	*/
/*				@(#) from SUN 2.10	*/

#ifndef _SYS_FS_UFS_MOUNT_
#define _SYS_FS_UFS_MOUNT_	1

/*
 * Mount structure.
 * One allocated on every ufs mount.
 * Used to find the super block.
 */
struct	mount {
	struct vfs	*m_vfsp;	/* vfs structure for this filesystem */
	dev_t		m_dev;		/* device mounted */
	struct vnode	*m_devvp;	/* vnode for block device mounted */
	struct buf	*m_bufp;	/* pointer to superblock */
	struct inode	*m_qinod;	/* QUOTA: pointer to quota file */
	u_short		m_qflags;	/* QUOTA: filesystem flags */
#if defined(RISCOS) && defined(TAHOE_QUOTA)
	u_char		m_bwarn;	/* QUOTA: block warning count */
	u_char		m_fwarn;	/* QUOTA: file warning count */
#endif
	u_long		m_btimelimit;	/* QUOTA: block time limit */
	u_long		m_ftimelimit;	/* QUOTA: file time limit */
	struct mount	*m_nxt;		/* linked list of all mounts */
};
#ifdef KERNEL
extern struct	mount *mounttab;
/*
 * Convert vfs ptr to mount ptr.
 */
#define VFSTOM(VFSP)	((struct mount *)((VFSP)->vfs_data))

/*
 * Operations
 */
struct mount *getmp();
#endif

#endif _SYS_FS_UFS_MOUNT_
