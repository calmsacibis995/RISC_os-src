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
/* $Header: bfs_dir.h,v 1.8.4.2 90/05/10 06:14:38 wje Exp $ */

#ifndef	_SYS_FS_BFS_DIR_
#define	_SYS_FS_BFS_DIR_	1


/*
 * Copyright (c) 1982, 1986 Regents of the University of California.
 * All rights reserved.  The Berkeley software License Agreement
 * specifies the terms and conditions for redistribution.
 *
 *	@(#)dir.h	7.1 (Berkeley) 6/4/86
 */

/*
 * A directory consists of some number of blocks each of which is
 * less than or equal to the filesystem block size number of
 * bytes.
 *
 * Each block contains some number of directory entry structures,
 * which are of variable length.  Each directory entry has
 * a struct direct at the front of it, containing its file number,
 * the length of the entry, and the length of the name contained in
 * the entry.  These are followed by the name padded to a 4 byte boundary
 * with null bytes.  All names are guaranteed null terminated.
 * The maximum length of a name in a directory is MAXNAMLEN, plus
 * a null byte.
 *
 * The macro DIRSIZ(dp) gives the amount of space required to represent
 * a directory entry.  Free space in a directory is represented by
 * entries which have dp->d_reclen > DIRSIZ(dp).  All DIRBLKSIZ bytes
 * in a directory block are claimed by the directory entries.  This
 * usually results in the last entry in a directory having a large
 * dp->d_reclen.  When entries are deleted from a directory, the
 * space is returned to the previous entry in the same directory
 * block by increasing its dp->d_reclen.  If the first entry of
 * a directory block is free, then its dp->d_ino is set to 0.
 * Entries other than the first in a directory do not normally have
 * dp->d_ino set to 0.
 */
#if !defined(KERNEL) && !defined(BFS_DEV_BSIZE)
#   define	BFS_DEV_BSIZE	512
#endif
#define BFS_DIRBLKSIZ	BFS_DEV_BSIZE
#define BFS_DIRBLKMSK	(BFS_DEV_BSIZE-1)
#define	BFS_MAXNAMLEN	255

struct	bfs_direct {
	u_long	d_fileno;		/* file number of entry */
	u_short	d_reclen;		/* length of this record */
	u_short	d_namlen;		/* length of string in d_name */
	char	d_name[BFS_MAXNAMLEN + 1];	/* name must be no longer than this */
};

/*
 * The DIRSIZ macro gives the minimum record length which will hold
 * the directory entry.  This requires the amount of space in struct direct
 * without the d_name field, plus enough space for the name with a terminating
 * null byte (dp->d_namlen+1), rounded up to a 4 byte boundary.
 */
#define BFS_DIRSIZ(dp) \
    ((sizeof (struct bfs_direct) - (BFS_MAXNAMLEN+1)) + (((dp)->d_namlen+1 + 3) &~ 3))

/*
 * Bfs_lookup() returns a magic cookie which identifies the dent that
 * it found to other dir functions.  The word is arranged as follows:
 *
 *	| 31-32	| 29-0	|
 *	 state	 offset
 *
 * The 30 bit offset limits directory sizes to one gig instead of 4 gigs. 
 * I forsee no trouble.
 * To avoid this limit, or to add more state info, the offset could be
 * right shifted since it is always word alligned. 
 *
 * States:
 *
 *	USED 	 - Offset refers to contents of occupied dent.
 *	FREE	 - Offset refers to free space in dent.  (The dent my
 *		   be unoccupied, or it may have free space at the end.)
 *	COMPRESS - Offset refers to a dir block with compressible free space.
 *		   (Offset must point to the beginning of the dirblock.)
 */
#define BFS_DIR_MASK     0x3FFFFFFF	/* Leave only the offset. */
#define BFS_DIR_SHIFT	 0
#define BFS_MAKE_COOK(state, offset)	((state)|(offset))
#define BFS_COOK_TO_OFF(cookie)		((cookie)&BFS_DIR_MASK)
#define BFS_COOK_TO_STATE(cookie)	((cookie)&~BFS_DIR_MASK)
#define BFS_DIR_USED     0x00000000    /* states */
#define BFS_DIR_FREE     0x40000000
/* #define BFS_DIR_EMPTY    0x80000000 /* Not used. */
#define BFS_DIR_COMPRESS 0xc0000000

#endif	_SYS_FS_BFS_DIR_
