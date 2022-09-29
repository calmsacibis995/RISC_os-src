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
/* $Header: vfs_stat.h,v 1.2.1.2 90/05/10 04:59:56 wje Exp $ */
/*
 * The stat structure is a two dimentional array. The major index is
 * the op number defined below, the minor index is VS_HIT or BSD43_VS_MISS.
 */

#define	BSD43_VS_CALL		0	/* Op called */
#define	BSD43_VS_MISS		1	/* Cache miss */

/*
 * VFS OPS
 */
#define	BSD43_VS_ROOT		0
#define	BSD43_VS_STATFS		1
#define	BSD43_VS_SYNC		2
#define	BSD43_VS_VGET		3

/*
 * Vnode ops
 */
#define BSD43_VS_OPEN		4
#define BSD43_VS_CLOSE		5
#define BSD43_VS_READ		6
#define BSD43_VS_WRITE		7
#define BSD43_VS_IOCTL		8
#define BSD43_VS_SELECT		9
#define BSD43_VS_GETATTR	10
#define BSD43_VS_SETATTR	11
#define BSD43_VS_ACCESS		12
#define BSD43_VS_LOOKUP		13
#define BSD43_VS_CREATE		14
#define BSD43_VS_REMOVE		15
#define BSD43_VS_LINK		16
#define BSD43_VS_RENAME		17
#define BSD43_VS_MKDIR		18
#define BSD43_VS_RMDIR		19
#define BSD43_VS_READDIR	20
#define BSD43_VS_SYMLINK	21
#define BSD43_VS_READLINK	22
#define BSD43_VS_FSYNC		23
#define BSD43_VS_INACTIVE	24
#define BSD43_VS_BMAP		25
#define BSD43_VS_STRATEGY	26
#define	BSD43_VS_BREAD		27
#define	BSD43_VS_BRELSE		28
#define BSD43_VS_LOCKCTL	29
#define BSD43_VS_FID		30
#define BSD43_VS_DUMP		31
#define BSD43_VS_CMP		32
#define BSD43_VS_REALVP		33

#define	BSD43_VS_NOPS		34

#ifndef KERNEL
char *vs_opnames[BSD43_VS_NOPS] = {
	"root", "statfs", "sync", "vget", "open", "close", "read", "write",
	"ioctl", "select", "getattr", "setattr", "access", "lookup", "create",
	"remove", "link", "rename", "mkdir", "rmdir", "readdir", "symlink",
	"readlink", "fsync", "inactive", "bmap", "strategy", "bread", "brelse",
	"lockctl", "fid", "dump","cmp", "realvp"
};
#endif !KERNEL

struct bsd43_(vfsstats) {
	time_t vs_time;
	int	vs_counts[BSD43_VS_NOPS][2];
};

#ifdef VFSSTATS
#define	BSD43_VFS_RECORD(vfs, op, hitmiss) \
	((vfs)->vfs_stats \
	    ? ((struct bsd43_(vfsstats) *)(vfs)->vfs_stats)->vs_counts[op][hitmiss]++ \
	    : 0 )
#else
#define	BSD43_VFS_RECORD(vfs, op, hitmiss)
#endif

/*----- NUMIPS: STRIP MACRO PREFIXES (BEGIN) ---------------------------------*/
/* 
 * These defines strip the "BSD43_" and "bsd43_" prefixes from the names
 * above so that these files may be included in standard BSD code.
 */
#ifdef SYSTYPE_BSD43
#   define VS_CALL	BSD43_VS_CALL
#   define VS_MISS	BSD43_VS_MISS
#   define VS_ROOT	BSD43_VS_ROOT
#   define VS_STATFS	BSD43_VS_STATFS
#   define VS_SYNC	BSD43_VS_SYNC
#   define VS_VGET	BSD43_VS_VGET
#   define VS_OPEN	BSD43_VS_OPEN
#   define VS_CLOSE	BSD43_VS_CLOSE
#   define VS_READ	BSD43_VS_READ
#   define VS_WRITE	BSD43_VS_WRITE
#   define VS_IOCTL	BSD43_VS_IOCTL
#   define VS_SELECT	BSD43_VS_SELECT
#   define VS_GETATTR	BSD43_VS_GETATTR
#   define VS_SETATTR	BSD43_VS_SETATTR
#   define VS_ACCESS	BSD43_VS_ACCESS
#   define VS_LOOKUP	BSD43_VS_LOOKUP
#   define VS_CREATE	BSD43_VS_CREATE
#   define VS_REMOVE	BSD43_VS_REMOVE
#   define VS_LINK	BSD43_VS_LINK
#   define VS_RENAME	BSD43_VS_RENAME
#   define VS_MKDIR	BSD43_VS_MKDIR
#   define VS_RMDIR	BSD43_VS_RMDIR
#   define VS_READDIR	BSD43_VS_READDIR
#   define VS_SYMLINK	BSD43_VS_SYMLINK
#   define VS_READLINK	BSD43_VS_READLINK
#   define VS_FSYNC	BSD43_VS_FSYNC
#   define VS_INACTIVE	BSD43_VS_INACTIVE
#   define VS_BMAP	BSD43_VS_BMAP
#   define VS_STRATEGY	BSD43_VS_STRATEGY
#   define VS_BREAD	BSD43_VS_BREAD
#   define VS_BRELSE	BSD43_VS_BRELSE
#   define VS_LOCKCTL	BSD43_VS_LOCKCTL
#   define VS_FID	BSD43_VS_FID
#   define VS_DUMP	BSD43_VS_DUMP
#   define VS_CMP	BSD43_VS_CMP
#   define VS_REALVP	BSD43_VS_REALVP
#   define VS_NOPS	BSD43_VS_NOPS
#   define VFS_RECORD	BSD43_VFS_RECORD
#   define VFS_RECORD	BSD43_VFS_RECORD
#endif SYSTYPE_BSD43
/*----- NUMIPS: STRIP MACRO PREFIXES (END) -----------------------------------*/

