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
/* $Header: vnode.h,v 1.8.1.2 90/05/10 05:00:43 wje Exp $ */
#ifndef _BSD43_SYS_VNODE_
#define _BSD43_SYS_VNODE_ 1

#ifndef BSD43_
#    include <bsd43/bsd43_.h>
#endif BSD43_

/*
 * The vnode is the focus of all file activity in UNIX.
 * There is a unique vnode allocated for each active file, each current
 * directory, each mounted-on file, each mapped file, and the root.
 */

/*
 * vnode types. VNON means no type.
 */
enum vtype 	{ VNON, VREG, VDIR, VBLK, VCHR, VLNK, VSOCK, VBAD, VFIFO };

struct vnode {
	u_short	v_flag;			/* vnode flags (see below) */
	u_short	v_count;		/* reference count */
	u_short	v_shlockc;		/* count of shared locks */
	u_short	v_exlockc;		/* count of exclusive locks */
	struct bsd43_(vfs) *v_vfsmountedhere; 	/* ptr to vfs mounted here */
	struct bsd43_(vnodeops)	*v_op;			/* vnode operations */
	union {
		struct bsd43_(socket) *v_Socket;	/* unix ipc */
		struct bsd43_(stdata) *v_Stream;	/* stream */
	} v_s;
	struct bsd43_(vfs) *v_vfsp;		/* ptr to vfs we are in */
	enum vtype	v_type;			/* vnode type */
	dev_t		v_rdev;			/* device (VCHR, VBLK) */
	caddr_t		v_data;			/* private data for fs */
	struct bsd43_(text) *v_text;		/* text entry pointer */
};
#define	bsd43_v_stream	v_s.v_Stream
#define	bsd43_v_socket	v_s.v_Socket

/*
 * vnode flags.
 */
#define	BSD43_VROOT		0x01	/* root of its file system */
#define BSD43_VTEXT		0x02	/* vnode is a pure text prototype */
#define	BSD43_VEXLOCK		0x04	/* exclusive lock */
#define	BSD43_VSHLOCK		0x08	/* shared lock */
#define	BSD43_VLWAIT		0x10	/* proc is waiting on shared or excl. lock */
#define	BSD43_BSD43_VTEXTMOD	0x20	/* text has been modified (e.g. ptrace) */
#define	BSD43_VNOCACHE	0x40	/* don't keep cache pages on vnode */
#define	BSD43_VISSWAP		0x80	/* vnode is part of virtual swap device */

/*
 * Operations on vnodes.
 */
struct bsd43_(vnodeops) {
	int	(*vn_open)();
	int	(*vn_close)();
	int	(*vn_rdwr)();
	int	(*vn_ioctl)();
	int	(*vn_select)();
	int	(*vn_getattr)();
	int	(*vn_setattr)();
	int	(*vn_access)();
	int	(*vn_lookup)();
	int	(*vn_create)();
	int	(*vn_remove)();
	int	(*vn_link)();
	int	(*vn_rename)();
	int	(*vn_mkdir)();
	int	(*vn_rmdir)();
	int	(*vn_readdir)();
	int	(*vn_symlink)();
	int	(*vn_readlink)();
	int	(*vn_fsync)();
	int	(*vn_inactive)();
	int	(*vn_bmap)();
	int	(*vn_strategy)();
	int	(*vn_bread)();
	int	(*vn_brelse)();
	int	(*vn_lockctl)();
	int	(*vn_fid)();
	int	(*vn_dump)();
	int	(*vn_cmp)();
	int	(*vn_realvp)();
};

#ifdef KERNEL

#define BSD43_VOP_OPEN(VPP,F,C) (*(*(VPP))->v_op->vn_open)(VPP, F, C)
#define BSD43_VOP_CLOSE(VP,F,C,CR) (*(VP)->v_op->vn_close)(VP,F,C,CR)
#define BSD43_VOP_RDWR(VP,UIOP,RW,F,C) (*(VP)->v_op->vn_rdwr)(VP,UIOP,RW,F,C)
#define BSD43_VOP_IOCTL(VP,C,D,F,CR) (*(VP)->v_op->vn_ioctl)(VP,C,D,F,CR)
#define BSD43_VOP_SELECT(VP,W,C) (*(VP)->v_op->vn_select)(VP,W,C)
#define BSD43_VOP_GETATTR(VP,VA,C) (*(VP)->v_op->vn_getattr)(VP,VA,C)
#define BSD43_VOP_SETATTR(VP,VA,C) (*(VP)->v_op->vn_setattr)(VP,VA,C)
#define BSD43_VOP_ACCESS(VP,M,C) (*(VP)->v_op->vn_access)(VP,M,C)
#define BSD43_VOP_LOOKUP(VP,NM,VPP,C,PN,F) (*(VP)->v_op->vn_lookup)(VP,NM,VPP,C,PN,F)
#define BSD43_VOP_CREATE(VP,NM,VA,E,M,VPP,C)	(*(VP)->v_op->vn_create) \
 (VP,NM,VA,E,M,VPP,C)
#define BSD43_VOP_REMOVE(VP,NM,C) (*(VP)->v_op->vn_remove)(VP,NM,C)
#define BSD43_VOP_LINK(VP,TDVP,TNM,C) (*(TDVP)->v_op->vn_link)(VP,TDVP,TNM,C)
#define BSD43_VOP_RENAME(VP,NM,TDVP,TNM,C)	(*(VP)->v_op->vn_rename) \
						(VP,NM,TDVP,TNM,C)
#define BSD43_VOP_MKDIR(VP,NM,VA,VPP,C)	(*(VP)->v_op->vn_mkdir)(VP,NM,VA,VPP,C)
#define BSD43_VOP_RMDIR(VP,NM,C) (*(VP)->v_op->vn_rmdir)(VP,NM,C)
#define BSD43_VOP_READDIR(VP,UIOP,C) (*(VP)->v_op->vn_readdir)(VP,UIOP,C)
#define BSD43_VOP_SYMLINK(VP,LNM,VA,TNM,C)	(*(VP)->v_op->vn_symlink) \
						(VP,LNM,VA,TNM,C)
#define BSD43_VOP_READLINK(VP,UIOP,C)		(*(VP)->v_op->vn_readlink)(VP,UIOP,C)
#define BSD43_VOP_FSYNC(VP,C)			(*(VP)->v_op->vn_fsync)(VP,C)
#define BSD43_VOP_INACTIVE(VP,C)	(*(VP)->v_op->vn_inactive)(VP,C)
#define BSD43_VOP_LOCKCTL(VP,LD,CMD,C,ID)	(*(VP)->v_op->vn_lockctl) \
						(VP,LD,CMD,C,ID)
#define BSD43_VOP_BMAP(VP,BN,VPP,BNP) (*(VP)->v_op->vn_bmap)(VP,BN,VPP,BNP)
#define BSD43_VOP_BREAD(VP,BN,BPP)	(*(VP)->v_op->vn_bread)(VP,BN,BPP)
#define BSD43_VOP_BRELSE(VP,BP)		(*(VP)->v_op->vn_brelse)(VP,BP)
#define BSD43_VOP_FID(VP, FIDPP)	(*(VP)->v_op->vn_fid)(VP, FIDPP)
#define BSD43_VOP_DUMP(VP, ADDR, BN, CT)	(*(VP)->v_op->vn_dump)(VP, ADDR, BN, CT)
#define BSD43_VOP_CMP(VP1, VP2)		(*(VP1)->v_op->vn_cmp)(VP1, VP2)
#define BSD43_VOP_REALVP(VP, VPP)	(*(VP)->v_op->vn_realvp)(VP, VPP)
#define BSD43_VOP_STRATEGY(BP)		(*(BP)->b_vp->v_op->vn_strategy)(BP)

/*
 * Flags for above
 */
#define BSD43_IO_UNIT		0x01 /* do io atomically for BSD43_VOP_RDWR */
#define BSD43_IO_APPEND	0x02		/* append write for BSD43_VOP_RDWR */
#define BSD43_IO_SYNC		0x04		/* sync io for BSD43_VOP_RDWR */
#define BSD43_LOOKUP_DIR	0x08 /* want parent dir. vnode, BSD43_VOP_LOOKUP */

#endif KERNEL

/*
 * Vnode attributes.  A field value of -1
 * represents a field whose value is unavailable
 * (getattr) or which is not to be changed (setattr).
 */
struct bsd43_(vattr) {
	enum vtype	va_type;	/* vnode type (for create) */
	u_short	va_mode;	/* files access mode and type */
	uid_t		va_uid;		/* owner user id */
	gid_t		va_gid;		/* owner group id */
	long		va_fsid;	/* file system id (dev for now) */
	long		va_nodeid;	/* node id */
	short		va_nlink;	/* number of references to file */
	u_long	va_size;	/* file size in bytes (quad?) */
	long		va_blocksize;	/* blocksize preferred for i/o */
	struct bsd43_(timeval) va_atime; /* time of last access */
	struct bsd43_(timeval) va_mtime; /* time of last modification */
	struct bsd43_(timeval) va_ctime; /* time file ``created */
	dev_t		va_rdev;	/* device the file represents */
	long		va_blocks;	/* kbytes of disk space held by file */
};

/*
 *  Modes. Some values same as Ixxx entries from inode.h for now
 */
#define	BSD43_VSUID	04000		/* set user id on execution */
#define	BSD43_VSGID	02000		/* set group id on execution */
#define BSD43_VSVTX	01000		/* save swapped text even after use */
#define	BSD43_VREAD	0400		/* read, write, execute permissions */
#define	BSD43_VWRITE	0200
#define	BSD43_VEXEC	0100

#ifdef KERNEL
/*
 * public vnode manipulation functions
 */
extern int vn_open();			/* open vnode */
extern int vn_create();			/* creat/mkdir vnode */
extern int vn_rdwr();			/* read or write vnode */
extern int vn_close();			/* close vnode */
extern void vn_rele();			/* release vnode */
extern int vn_link();			/* make hard link */
extern int vn_rename();			/* rename (move) */
extern int vn_remove();			/* remove/rmdir */
extern void vattr_null();		/* set attributes to null */
extern int getvnodefp();		/* get fp from vnode fd */

#define BSD43_VN_HOLD(VP)	{(VP)->v_count++;}

#define BSD43_VN_RELE(VP)	{vn_rele(VP);}

#define BSD43_VN_INIT(VP, VFSP, TYPE, DEV)	{ \
	(VP)->v_flag = 0; \
	(VP)->v_count = 1; \
	(VP)->v_shlockc = (VP)->v_exlockc = 0; \
	(VP)->v_vfsp = (VFSP); \
	(VP)->v_type = (TYPE); \
	(VP)->v_rdev = (DEV); \
	(VP)->bsd43_v_socket = 0; \
}

/*
 * Compare two vnodes for equality.
 * This macro should always be used,
 * rather than calling BSD43_VOP_CMP directly.
 */
#define BSD43_VN_CMP(VP1,VP2)		((VP1) == (VP2) ? 1 : \
	((VP1) && (VP2) && ((VP1)->v_op == (VP2)->v_op) ? BSD43_VOP_CMP(VP1,VP2) : 0))

/*
 * Flags for above
 */
enum rm		{ FILE, DIRECTORY };		/* rmdir or rm (remove) */
enum symfollow	{ NO_FOLLOW, FOLLOW_LINK };	/* follow symlinks (lookuppn) */
enum vcexcl	{ NONEXCL, EXCL};		/* (non)excl create (create) */

/*
 * Global vnode data.
 */
extern struct vnode *rootdir;		/* root (i.e. "/") vnode */
extern enum vtype	mftovt_tab[];
#define	BSD43_MFMT		0170000			/* type of file */
#define BSD43_MFTOVT(M)	(mftovt_tab[((M) & BSD43_MFMT) >> 13])

#endif KERNEL

/*----- NUMIPS: STRIP MACRO PREFIXES (BEGIN) ---------------------------------*/
/* 
 * These defines strip the "BSD43_" and "bsd43_" prefixes from the names
 * above so that these files may be included in standard BSD code.
 */
#ifdef SYSTYPE_BSD43
#   define v_stream	bsd43_v_stream
#   define v_socket	bsd43_v_socket
#   define VROOT	BSD43_VROOT
#   define VTEXT	BSD43_VTEXT
#   define VEXLOCK	BSD43_VEXLOCK
#   define VSHLOCK	BSD43_VSHLOCK
#   define VLWAIT	BSD43_VLWAIT
#   define VTEXTMOD	BSD43_VTEXTMOD
#   define VNOCACHE	BSD43_VNOCACHE
#   define VISSWAP	BSD43_VISSWAP
#   define VOP_OPEN	BSD43_VOP_OPEN
#   define VOP_CLOSE	BSD43_VOP_CLOSE
#   define VOP_RDWR	BSD43_VOP_RDWR
#   define VOP_IOCTL	BSD43_VOP_IOCTL
#   define VOP_SELECT	BSD43_VOP_SELECT
#   define VOP_GETATTR	BSD43_VOP_GETATTR
#   define VOP_SETATTR	BSD43_VOP_SETATTR
#   define VOP_ACCESS	BSD43_VOP_ACCESS
#   define VOP_LOOKUP	BSD43_VOP_LOOKUP
#   define VOP_CREATE	BSD43_VOP_CREATE
#   define VOP_REMOVE	BSD43_VOP_REMOVE
#   define VOP_LINK	BSD43_VOP_LINK
#   define VOP_RENAME	BSD43_VOP_RENAME
#   define VOP_MKDIR	BSD43_VOP_MKDIR
#   define VOP_RMDIR	BSD43_VOP_RMDIR
#   define VOP_READDIR	BSD43_VOP_READDIR
#   define VOP_SYMLINK	BSD43_VOP_SYMLINK
#   define VOP_READLINK	BSD43_VOP_READLINK
#   define VOP_FSYNC	BSD43_VOP_FSYNC
#   define VOP_INACTIVE	BSD43_VOP_INACTIVE
#   define VOP_LOCKCTL	BSD43_VOP_LOCKCTL
#   define VOP_BMAP	BSD43_VOP_BMAP
#   define VOP_BREAD	BSD43_VOP_BREAD
#   define VOP_BRELSE	BSD43_VOP_BRELSE
#   define VOP_FID	BSD43_VOP_FID
#   define VOP_DUMP	BSD43_VOP_DUMP
#   define VOP_CMP	BSD43_VOP_CMP
#   define VOP_REALVP	BSD43_VOP_REALVP
#   define VOP_STRATEGY	BSD43_VOP_STRATEGY
#   define IO_UNIT	BSD43_IO_UNIT
#   define IO_APPEND	BSD43_IO_APPEND
#   define IO_SYNC	BSD43_IO_SYNC
#   define LOOKUP_DIR	BSD43_LOOKUP_DIR
#   define VSUID	BSD43_VSUID
#   define VSGID	BSD43_VSGID
#   define VSVTX	BSD43_VSVTX
#   define VREAD	BSD43_VREAD
#   define VWRITE	BSD43_VWRITE
#   define VEXEC	BSD43_VEXEC
#   define VN_HOLD	BSD43_VN_HOLD
#   define VN_RELE	BSD43_VN_RELE
#   define VN_INIT	BSD43_VN_INIT
#   define VN_CMP	BSD43_VN_CMP
#   define MFMT	BSD43_MFMT
#   define MFTOVT	BSD43_MFTOVT
#endif SYSTYPE_BSD43
/*----- NUMIPS: STRIP MACRO PREFIXES (END) ----------------------------------*/

#endif _BSD43_SYS_VNODE_
