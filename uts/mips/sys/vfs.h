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
/* $Header: vfs.h,v 1.3.1.5 90/05/10 06:44:11 wje Exp $ */
#ifndef _SYS_VFS_
#define _SYS_VFS_

/*
 * File system identifier. Should be unique (at least per machine).
 */
typedef struct {
	long val[2];			/* file system id type */
} fsid_t;

/*
 * File identifier. Should be unique per filesystem on a single machine.
 */
#define	MAXFIDSZ	16
#define	freefid(fidp) \
    kmem_free((caddr_t)(fidp), sizeof (struct fid) - MAXFIDSZ + (fidp)->fid_len)

struct fid {
	u_short		fid_len;		/* length of data in bytes */
	char		fid_data[MAXFIDSZ];	/* data (variable length) */
};

/*
 * Structure per mounted file system.
 * Each mounted file system has an array of
 * operations and an instance record.
 * The file systems are put on a singly linked list.
 */
struct vfs {
	struct vfs	*vfs_next;		/* next vfs in vfs list */
	struct vfsops	*vfs_op;		/* operations on vfs */
	struct vnode	*vfs_vnodecovered;	/* vnode we mounted on */
	int		vfs_flag;		/* flags */
	int		vfs_bsize;		/* native block size */
	fsid_t		vfs_fsid;		/* file system id */
	caddr_t 	vfs_stats;		/* filesystem statistics */
	caddr_t		vfs_data;		/* private data */
};

/*
 * vfs flags.
 * VFS_MLOCK: lock the vfs so that name lookup cannot proceed past the vfs.
 *	This keeps the subtree stable during mounts and unmounts.
 * VFS_SYNC: RISC/os will perform synchronous NFS writes rather than
 *	asynchronous ones.  The asynchronous case is much faster.
 */
#define VFS_RDONLY	0x01		/* read only vfs */
#define VFS_MLOCK	0x02		/* lock vfs so that subtree is stable */
#define VFS_MWAIT	0x04		/* someone is waiting for lock */
#define VFS_NOSUID	0x08		/* turn off set-uid on exec */
#define VFS_GRPID	0x10		/* Old BSD group-id on create */
#define VFS_NOSUB	0x20		/* No mounts allowed beneath this fs */
#define VFS_REMOUNT	0x40		/* modify mount otions only */
#define VFS_MULTI	0x80		/* Do multi-component lookup on files*/
#define VFS_NFSSYNC	0x40000000	/* NFS option: synchronous writes */
#define VFS_NFSASYNC	0x20000000	/* NFS option: asynchronous writes */

/*
 * Operations supported on virtual file system.
 */
struct vfsops {
	int	(*vfs_mount)();		/* mount file system */
	int	(*vfs_unmount)();	/* unmount file system */
	int	(*vfs_root)();		/* get root vnode */
	int	(*vfs_statfs)();	/* get fs statistics */
	int	(*vfs_sync)();		/* flush fs buffers */
	int	(*vfs_vget)();		/* get vnode from fid */
	int	(*vfs_mountroot)();	/* mount the root filesystem */
	int	(*vfs_swapvp)();	/* return vnode for swap */
};

#define VFS_MOUNT(VFSP, PATH, DATA) \
				 (*(VFSP)->vfs_op->vfs_mount)(VFSP, PATH, DATA)
#define VFS_UNMOUNT(VFSP)	 (*(VFSP)->vfs_op->vfs_unmount)(VFSP)
#define VFS_ROOT(VFSP, VPP)	 (*(VFSP)->vfs_op->vfs_root)(VFSP,VPP)
#define VFS_STATFS(VFSP, SBP)	 (*(VFSP)->vfs_op->vfs_statfs)(VFSP,SBP,0)
#define VFS_SYNC(VFSP)		 (*(VFSP)->vfs_op->vfs_sync)(VFSP)
#define VFS_VGET(VFSP, VPP, FIDP) (*(VFSP)->vfs_op->vfs_vget)(VFSP, VPP, FIDP)
#define VFS_MOUNTROOT(VFSP, VPP, NM, OP) \
			 (*(VFSP)->vfs_op->vfs_mountroot)(VFSP, VPP, NM, OP)
#define VFS_SWAPVP(VFSP, VPP, NM) (*(VFSP)->vfs_op->vfs_swapvp)(VFSP, VPP, NM)

/*
 * Specific operation to perform when calling vfs_mountroot()
 */
enum mountrootop { ROOT_MOUNT, ROOT_REMOUNT, ROOT_UNMOUNT };
typedef enum mountrootop mountrootop_t;

/*
 * file system statistics
 */
/*
 * This struct conflicts with the one in statfs.h.  We need both
 * structs; if this were in a separate file, we could include the
 * bsd43/sys/?.h file with proper prefix macros.  As it is, we'll
 * just have to prefix this by hand for now.
 *
 * NOTE: The f_frsize field is defined because this structure is used
 *	with the VFS_STATFS() switch and sysv_statfs() needs this field
 *	defined.  It is zeroed before returning from the BSD system call.
 */
struct bsd43_statfs {
	long f_type;			/* type of info, zero for now */
	long f_bsize;			/* fundamental filesystem block size */
	long f_blocks;			/* total blocks in file system */
	long f_bfree;			/* free block in fs */
	long f_bavail;			/* free blcks avail to non-superuser */
	long f_files;			/* total file nodes in file system */
	long f_ffree;			/* free file nodes in fs */
	fsid_t f_fsid;			/* file system id */
	long f_spare[3];		/* spare for later */
	long f_frsize;			/* fragment size if applicable */
	char f_fname[6];		/* Volume name, for SVID compat */
	char f_fpack[6];		/* Pack name, for SVID compat */
};

#ifdef KERNEL
/*
 * Filesystem type switch table
 */
struct vfssw {
	char		*vsw_name;	/* type name string */
	struct vfsops	*vsw_ops;	/* filesystem operations vector */
};

/*
 * public operations
 */
extern void	vfs_mountroot();	/* mount the root */
extern int	vfs_add();		/* add a new vfs to mounted vfs list */
extern void	vfs_remove();		/* remove a vfs from mounted vfs list */
extern int	vfs_lock();		/* lock a vfs */
extern void	vfs_unlock();		/* unlock a vfs */
extern struct vfs *getvfs();		/* return vfs given fsid */
extern struct vfssw *getfstype();	/* find default filesystem type */
extern int vfs_getmajor();		/* get major device # for an fs type */
extern void vfs_putmajor();		/* free major device # for an fs type */
extern int vfs_getnum();		/* get device # for an fs type */
extern void vfs_putnum();		/* release device # for an fs type */

#define VFS_INIT(VFSP, OP, DATA)	{ \
	(VFSP)->vfs_next = (struct vfs *)0; \
	(VFSP)->vfs_op = (OP); \
	(VFSP)->vfs_flag = 0; \
	(VFSP)->vfs_stats = NULL; \
	(VFSP)->vfs_data = (DATA); \
}

/*
 * globals
 */
extern struct vfs *rootvfs;		/* ptr to root vfs structure */
extern struct vfssw vfssw[];		/* table of filesystem types */
extern struct vfssw *vfsNVFS;		/* vfs switch table end marker */
#endif KERNEL

#endif !_SYS_VFS_
