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
/* $Header: vfs.h,v 1.7.1.2 90/05/10 04:59:49 wje Exp $ */
#ifndef BSD43_
#    include <bsd43/bsd43_.h>
#endif BSD43_

/*
 * File system identifier. Should be unique (at least per machine).
 */
typedef struct {
	long val[2];			/* file system id type */
} bsd43_(fsid_t);

/*
 * File identifier. Should be unique per filesystem on a single machine.
 */
#define	BSD43_MAXFIDSZ	16
#define	bsd43_freefid(fidp) \
    kmem_free((caddr_t)(fidp), sizeof (struct bsd43_(fid)) - BSD43_MAXFIDSZ + (fidp)->fid_len)

struct bsd43_(fid) {
	u_short	fid_len;		/* length of data in bytes */
	char		fid_data[BSD43_MAXFIDSZ]; /* data (variable length) */
};

/*
 * Structure per mounted file system.
 * Each mounted file system has an array of
 * operations and an instance record.
 * The file systems are put on a singly linked list.
 */
struct bsd43_(vfs) {
	struct bsd43_(vfs) *vfs_next;		/* next vfs in vfs list */
	struct bsd43_(vfsops) *vfs_op;		/* operations on vfs */
	struct vnode *vfs_vnodecovered;	/* vnode we mounted on */
	int		vfs_flag;		/* flags */
	int		vfs_bsize;		/* native block size */
	bsd43_(fsid_t)	vfs_fsid;		/* file system id */
	caddr_t 	vfs_stats;		/* filesystem statistics */
	caddr_t		vfs_data;		/* private data */
};

/*
* vfs flags.
* BSD43_VFS_MLOCK lock the vfs so that name lookup cannot proceed past the vfs.
* This keeps the subtree stable during mounts and unmounts.
*/
#define BSD43_VFS_RDONLY	0x01	/* read only vfs */
#define BSD43_VFS_MLOCK		0x02	/* lock vfs so that subtree is stable*/
#define BSD43_VFS_MWAIT		0x04	/* someone is waiting for lock */
#define BSD43_VFS_NOSUID	0x08	/* turn off set-uid on exec */
#define BSD43_VFS_GRPID		0x10	/* Old BSD group-id on create */
#define BSD43_VFS_NOSUB		0x20	/* No mounts allowed beneath this fs */
#define BSD43_VFS_REMOUNT	0x40	/* modify mount otions only */
#define BSD43_VFS_MULTI		0x80	/* Do multi-component lookup on files*/

/*
 * Operations supported on virtual file system.
 */
struct bsd43_(vfsops) {
	int	(*vfs_mount)();		/* mount file system */
	int	(*vfs_unmount)();	/* unmount file system */
	int	(*vfs_root)();		/* get root vnode */
	int	(*vfs_statfs)();	/* get fs statistics */
	int	(*vfs_sync)();		/* flush fs buffers */
	int	(*vfs_vget)();		/* get vnode from fid */
	int	(*vfs_mountroot)();	/* mount the root filesystem */
	int	(*vfs_swapvp)();	/* return vnode for swap */
};

#define BSD43_VFS_MOUNT(VFSP, PATH, DATA) \
				 (*(VFSP)->vfs_op->vfs_mount)(VFSP, PATH, DATA)
#define BSD43_VFS_UNMOUNT(VFSP)	 (*(VFSP)->vfs_op->vfs_unmount)(VFSP)
#define BSD43_VFS_ROOT(VFSP, VPP)	 (*(VFSP)->vfs_op->vfs_root)(VFSP,VPP)
#define BSD43_VFS_STATFS(VFSP, SBP)	 (*(VFSP)->vfs_op->vfs_statfs)(VFSP,SBP)
#define BSD43_VFS_SYNC(VFSP)		 (*(VFSP)->vfs_op->vfs_sync)(VFSP)
#define BSD43_VFS_VGET(VFSP, VPP, FIDP) (*(VFSP)->vfs_op->vfs_vget)(VFSP, VPP, FIDP)
#define BSD43_VFS_MOUNTROOT(VFSP, VPP, NM, OP) \
			 (*(VFSP)->vfs_op->vfs_mountroot)(VFSP, VPP, NM, OP)
#define BSD43_VFS_SWAPVP(VFSP, VPP, NM) (*(VFSP)->vfs_op->vfs_swapvp)(VFSP, VPP, NM)

/*
 * Specific operation to perform when calling vfs_mountroot()
 */
enum bsd43_(mountrootop) { ROOT_MOUNT, ROOT_REMOUNT, ROOT_UNMOUNT };
typedef enum bsd43_(mountrootop) bsd43_(mountrootop_t);

/*
 * file system statistics
 */
struct bsd43_(statfs) {
	long f_type;			/* type of info, zero for now */
	long f_bsize;			/* fundamental file system block size*/
	long f_blocks;			/* total blocks in file system */
	long f_bfree;			/* free block in fs */
	long f_bavail;			/* free blocks avail to non-superuser*/
	long f_files;			/* total file nodes in file system */
	long f_ffree;			/* free file nodes in fs */
	bsd43_(fsid_t) f_fsid;			/* file system id */
	long f_spare[7];		/* spare for later */
};

#ifdef KERNEL
/*
 * Filesystem type switch table
 */
struct bsd43_(vfssw) {
	char		*vsw_name;	/* type name string */
	struct bsd43_(vfsops) *vsw_ops;	/* filesystem operations vector */
};

/*
 * public operations
 */
extern void	vfs_mountroot();	/* mount the root */
extern int	vfs_add();		/* add a new vfs to mounted vfs list */
extern void	vfs_remove();		/* remove a vfs from mounted vfs list */
extern int	vfs_lock();		/* lock a vfs */
extern void	vfs_unlock();		/* unlock a vfs */
extern struct bsd43_(vfs) *getvfs();	/* return vfs given fsid */
extern struct bsd43_(vfssw) *getfstype(); /* find default filesystem type */
extern int vfs_getmajor();		/* get major device # for an fs type */
extern void vfs_putmajor();		/* free major device # for an fs type */
extern int vfs_getnum();		/* get device # for an fs type */
extern void vfs_putnum();		/* release device # for an fs type */

#define BSD43_VFS_INIT(VFSP, OP, DATA)	{ \
	(VFSP)->vfs_next = (struct bsd43_(vfs) *)0; \
	(VFSP)->vfs_op = (OP); \
	(VFSP)->vfs_flag = 0; \
	(VFSP)->vfs_stats = NULL; \
	(VFSP)->vfs_data = (DATA); \
}

/*
 * globals
 */
extern struct bsd43_(vfs) *rootvfs;	/* ptr to root vfs structure */
extern struct bsd43_(vfssw) vfssw[];	/* table of filesystem types */
extern struct bsd43_(vfssw) *vfsNVFS;	/* vfs switch table end marker */
#endif KERNEL

/*----- NUMIPS: STRIP MACRO PREFIXES (BEGIN) ---------------------------------*/
/* 
 * These defines strip the "BSD43_" and "bsd43_" prefixes from the names
 * above so that these files may be included in standard BSD code.
 */
#ifdef SYSTYPE_BSD43
#   define MAXFIDSZ	BSD43_MAXFIDSZ
#   define freefid	bsd43_freefid
#   define VFS_RDONLY	BSD43_VFS_RDONLY
#   define VFS_MLOCK	BSD43_VFS_MLOCK
#   define VFS_MWAIT	BSD43_VFS_MWAIT
#   define VFS_NOSUID	BSD43_VFS_NOSUID
#   define VFS_GRPID	BSD43_VFS_GRPID
#   define VFS_NOSUB	BSD43_VFS_NOSUB
#   define VFS_REMOUNT	BSD43_VFS_REMOUNT
#   define VFS_MULTI	BSD43_VFS_MULTI
#   define VFS_MOUNT	BSD43_VFS_MOUNT
#   define VFS_UNMOUNT	BSD43_VFS_UNMOUNT
#   define VFS_ROOT	BSD43_VFS_ROOT
#   define VFS_STATFS	BSD43_VFS_STATFS
#   define VFS_SYNC	BSD43_VFS_SYNC
#   define VFS_VGET	BSD43_VFS_VGET
#   define VFS_MOUNTROOT	BSD43_VFS_MOUNTROOT
#   define VFS_SWAPVP	BSD43_VFS_SWAPVP
#   define VFS_INIT	BSD43_VFS_INIT
#endif SYSTYPE_BSD43
/*----- NUMIPS: STRIP MACRO PREFIXES (END) ----------------------------------*/

