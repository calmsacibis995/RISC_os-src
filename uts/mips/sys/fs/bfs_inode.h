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
/* $Header: bfs_inode.h,v 1.8.3.2 90/05/10 06:14:53 wje Exp $ */

#ifndef	_SYS_FS_BFS_INODE_
#define	_SYS_FS_BFS_INODE_	1


/*
 * Copyright (c) 1982, 1986 Regents of the University of California.
 * All rights reserved.  The Berkeley software License Agreement
 * specifies the terms and conditions for redistribution.
 *
 *	@(#)inode.h	7.1 (Berkeley) 6/4/86
 */

/*
 * The I node is the focus of all file activity in UNIX.
 * There is a unique inode allocated for each active file,
 * each current directory, each mounted-on file, text file, and the root.
 * An inode is 'named' by its dev/inumber pair. (iget/iget.c)
 * Data in bfs_icommon is read in from permanent inode on volume.
 */

/*
 * Under the sysV file system switch, there is both a system inode 
 * structure, and a file system dependant inode which the system
 * This file defines the BFS dependant inode.
 *
 * In addition, since BFS relies on the common file system code, there
 * is also a common inode structure which must appear as the first
 * member of of the BFS dependant inode.
 *
 * All this seems confusing, but the #define's below map the old
 * simple names into the correct location, so access is transparent.
 *
 * The bfs_fsptr returns the bfs private data pointer for an inode.
 */
#define	bfs_fsptr(ip)	((struct bfs_inode *) (ip)->i_fsptr)


#define BFS_NDADDR	12		/* direct addresses in inode */
#define BFS_NIADDR	3		/* indirect addresses in inode */

#include "sys/fs/com_inode.h"

struct bfs_inode {
	struct	com_inode bi_com;
	struct	fs *bi_fs;	/* file sys associated with this inode */
	struct	text *bi_text;	/* ~~~text entry. (Used?? Should be region???)*/
	daddr_t	bi_lastr;	/* last read (read-ahead) */
	struct 	bfs_icommon
	{
		/* 
		 * In BSD, the values stored here are left here and
		 * the file system code accesses them from here.
		 *
		 * With sysV and the common code, many of these values
		 * must be copied to new locations.  But since the
		 * structure below is a direct copy of what the file
		 * system store on the disk, it cannot be changed.
		 * To help keep things straight, I've flagged each
		 * entry:
		 *
		 * 	COM	Marks elements that are also in com_inode
		 *		they must be copied into the com structure
		 *		after disk reads, and copied back before
		 *		writes.
		 *		
		 *	SYS	Marks entries which must be kept in the 
		 *		system inode itself.  These value must
		 *		also be copied before and after writes.
		 */
		u_short	ic_mode;	/*  0: (COM) mode and type of file */
		short	ic_nlink;	/*  2: (SYS) number of links to file */
		uid_t	ic_uid;		/*  4: (SYS) owner's user id */
		gid_t	ic_gid;		/*  6: (SYS) owner's group id */
		quad	ic_size;	/*  8: (SYS) number of bytes in file */
		time_t	ic_atime;	/* 16: (COM) time last accessed */
		long	ic_atspare;
		time_t	ic_mtime;	/* 24: (COM) time last modified */
		long	ic_mtspare;
		time_t	ic_ctime;	/* 32: (COM) last time inode changed */
		long	ic_ctspare;
		daddr_t	ic_db[BFS_NDADDR];	/* 40: disk block addresses */
		daddr_t	ic_ib[BFS_NIADDR];	/* 88: indirect blocks */
		long	ic_flags;	/* 100: status, currently unused */
		long	ic_blocks;	/* 104: blocks actually held */
		long	ic_gen;		/* 108: generation number */
		long	ic_spare[4];	/* 112: reserved, currently unused */
	} bi_ic
};

struct bfs_dinode {
	union {
		struct	bfs_icommon di_icom;
		char	di_size[128];
	} di_un;
};

/*
 * Easy access to bfs_inode.bi_ic fields.
 *
 * Note: Some fields which are stored in the icommon structure in BSD
 * are stored in the inode itself or in the com_inode in SV.  Thus, some 
 * of the BSD defines have been changed or removed so that the value 
 * from the inode will be used.
 *
 * The #undefs block cpp error messages from values that are over-ridden.
 */
#undef i_mode
#undef i_atime
#undef i_mtime
#undef i_ctime

#define	bi_rdev		bi_ic.ic_db[0]		/* In ip->i_rdev while incore */
#define i_fs		i_bfsptr->bi_fs
#define	i_mode		i_bfsptr->bi_com.ci_mode
#define	i_db		i_bfsptr->bi_ic.ic_db
#define	i_ib		i_bfsptr->bi_ic.ic_ib
#define	i_atime		i_bfsptr->bi_com.ci_atime
#define	i_mtime		i_bfsptr->bi_com.ci_mtime
#define	i_ctime		i_bfsptr->bi_com.ci_ctime
#define i_blocks	i_bfsptr->bi_ic.ic_blocks
#define	i_lastr		i_bfsptr->bi_lastr
#define	i_socket	DONT_USE	
#define	i_forw		DONT_USE
#define	i_back		DONT_USE
#define	i_freef		DONT_USE
#define	i_freeb		DONT_USE

/*
 * Easy access to bfs_dinode.di_icom fields.
 */
#define di_ic		di_un.di_icom
#define	di_mode		di_ic.ic_mode
#define	di_nlink	di_ic.ic_nlink
#define	di_uid		di_ic.ic_uid
#define	di_gid		di_ic.ic_gid
#define	di_db		di_ic.ic_db
#define	di_ib		di_ic.ic_ib
#define	di_atime	di_ic.ic_atime
#define	di_mtime	di_ic.ic_mtime
#define	di_ctime	di_ic.ic_ctime
#define	di_rdev		di_ic.ic_db[0]
#define	di_blocks	di_ic.ic_blocks
#define	di_gen		di_ic.ic_gen

/*
 * Disk inodes have an 8 byte size field.  But current implementations
 * use only 4, so I just need to get the right ones.
 * (In SV inodes, i_size is just a long, not a quad.)
 */
#ifdef MIPSEB
#  define	di_size		di_ic.ic_size.val[1]
#endif MIPSEB
#ifdef MIPSEL
#  define	di_size		di_ic.ic_size.val[0]
#endif MIPSEL

#ifdef MIPSEB
#  define	ic_int_size		ic_size.val[1]
#endif MIPSEB
#ifdef MIPSEL
#  define	ic_int_size		ic_size.val[0]
#endif MIPSEL

#ifdef KERNEL

struct	inode *ialloc();
struct	inode *iget();
struct	inode *owner();
struct	inode *maknode();
struct	inode *namei();

ino_t	dirpref();
#endif KERNEL

/* 
 * Flags.  Most of these match the SV values.  
 *
 * Ones that don't match that I need, I map here.
 * Ones that don't match that I don't need, I map to DONT_USE
 * so that the compiler will complain if they get used.
 */
#define		ILOCKED	0x1		/* inode is locked */
#define		ISHLOCK	DONT_USE	/* file has shared lock */
#define		IEXLOCK	DONT_USE	/* file has exclusive lock */
#define		ILWAIT	DONT_USE	/* someone waiting on file lock */
#define		IMOD	DONT_USE	/* inode has been modified */
#define		IRENAME	DONT_USE	/* inode is being renamed */

/* 
 * Modes.  As with flags...
 */
#ifndef IFSOCK
#define		IFSOCK	0140000		/* socket */
#endif IFSOCK

#endif	_SYS_FS_BFS_INODE_
