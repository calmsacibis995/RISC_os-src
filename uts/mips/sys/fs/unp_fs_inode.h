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
/* $Header: unp_fs_inode.h,v 1.3.1.2 90/05/10 06:19:51 wje Exp $ */

#ifndef	_SYS_FS_UNP_FS_INODE_
#define	_SYS_FS_UNP_FS_INODE_	1


/*
 * Definitions for "unp_fs" filesystem.
 */
#include "sys/fs/com_inode.h"

  /*
   * Common inode information for UNIX domain sockets.
   */
struct unp_fs_inode {
	struct	com_inode unp_fs_com;
};

#define	unp_fs_fsptr(ip)	((struct unp_fs_inode *) (ip)->i_fsptr)
#define unp_fs_socket(ip)	((struct socket *) ((ip)->i_sptr))

#define	UNP_FS_MAGIC	0x43880910

#endif	_SYS_FS_UNP_FS_INODE_
