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
/* $Header: vfs_stat.h,v 1.2.1.2 90/05/10 06:44:19 wje Exp $ */
#ifndef _SYS_VFS_STAT_
#define _SYS_VFS_STAT_

/*
 * The stat structure is a two dimentional array. The major index is
 * the op number defined below, the minor index is VS_HIT or VS_MISS.
 */

#define	VS_CALL		0	/* Op called */
#define	VS_MISS		1	/* Cache miss */

/*
 * VFS OPS
 */
#define	VS_ROOT		0
#define	VS_STATFS	1
#define	VS_SYNC		2
#define	VS_VGET		3

/*
 * Vnode ops
 */
#define VS_OPEN		4
#define VS_CLOSE	5
#define VS_READ		6
#define VS_WRITE	7
#define VS_IOCTL	8
#define VS_SELECT	9
#define VS_GETATTR	10
#define VS_SETATTR	11
#define VS_ACCESS	12
#define VS_LOOKUP	13
#define VS_CREATE	14
#define VS_REMOVE	15
#define VS_LINK		16
#define VS_RENAME	17
#define VS_MKDIR	18
#define VS_RMDIR	19
#define VS_READDIR	20
#define VS_SYMLINK	21
#define VS_READLINK	22
#define VS_FSYNC	23
#define VS_INACTIVE	24
#define VS_BMAP		25
#define VS_STRATEGY	26
#define	VS_BREAD	27
#define	VS_BRELSE	28
#define VS_LOCKCTL	29
#define VS_FID		30
#define VS_DUMP		31
#define VS_CMP		32
#define VS_REALVP	33

#define	VS_NOPS		34

#ifndef KERNEL
char *vs_opnames[VS_NOPS] = {
	"root", "statfs", "sync", "vget", "open", "close", "read", "write",
	"ioctl", "select", "getattr", "setattr", "access", "lookup", "create",
	"remove", "link", "rename", "mkdir", "rmdir", "readdir", "symlink",
	"readlink", "fsync", "inactive", "bmap", "strategy", "bread", "brelse",
	"lockctl", "fid", "dump","cmp", "realvp"
};
#endif

struct vfsstats {
	time_t	vs_time;
	int	vs_counts[VS_NOPS][2];
};

#ifdef VFSSTATS
#define	VFS_RECORD(vfs, op, hitmiss) \
	((vfs)->vfs_stats \
	    ? ((struct vfsstats *)(vfs)->vfs_stats)->vs_counts[op][hitmiss]++ \
	    : 0 )
#else
#define	VFS_RECORD(vfs, op, hitmiss)
#endif

#endif !_SYS_VFS_STAT_
