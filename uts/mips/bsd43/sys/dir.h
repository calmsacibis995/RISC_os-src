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
/* $Header: dir.h,v 1.9.1.2 90/05/10 04:50:05 wje Exp $ */
/*
 * Copyright (c) 1982, 1986 Regents of the University of California.
 * All rights reserved.  The Berkeley software License Agreement
 * specifies the terms and conditions for redistribution.
 *
 *	@(#)dir.h	7.1 (Berkeley) 6/4/86
 */
/*----- COPYRIGHT (END) ------------------------------------------------------*/

#ifndef BSD43_
#    include <bsd43/bsd43_.h>
#endif BSD43_


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
/* so user programs can just include dir.h */
#if !defined(KERNEL) && !defined(BSD43_DEV_BSIZE)
#define	BSD43_DEV_BSIZE	512
#endif
#define BSD43_DIRBLKSIZ	BSD43_DEV_BSIZE
#define	BSD43_MAXNAMLEN	255

struct	bsd43_(direct) {
	u_long	d_fileno;		/* file number of entry */
	u_short	d_reclen;		/* length of this record */
	u_short	d_namlen;		/* length of string in d_name */
	char	d_name[BSD43_MAXNAMLEN + 1];	/* name must be no longer than this */
};

/*
 * The DIRSIZ macro gives the minimum record length which will hold
 * the directory entry.  This requires the amount of space in struct direct
 * without the d_name field, plus enough space for the name with a terminating
 * null byte (dp->d_namlen+1), rounded up to a 4 byte boundary.
 */
#undef BSD43_DIRSIZ
#define BSD43_DIRSIZ(dp) \
    ((sizeof (struct bsd43_(direct)) - (BSD43_MAXNAMLEN+1)) + (((dp)->d_namlen+1 + 3) &~ 3))

#ifndef KERNEL
#define	bsd43_d_ino	d_fileno		/* compatibility */

/*
 * Definitions for library routines operating on directories.
 */
typedef struct bsd43_(_dirdesc) {
	int	dd_fd;
	long	dd_loc;
	long	dd_size;
	long	dd_bbase;
	long	dd_entno;
	long	dd_bsize;
	char	*dd_buf;
} BSD43_(DIR);

#ifndef BSD43_NULL
#define BSD43_NULL 0
#endif
extern	BSD43_(DIR) *bsd43_(opendir)();
extern	struct bsd43_(direct) *bsd43_(readdir)();
extern	long bsd43_(telldir)();
extern	void bsd43_(seekdir)();
#define bsd43_rewinddir(dirp)	bsd43_(seekdir)((dirp), (long)0)
extern	void bsd43_(closedir)();
#endif

#ifdef KERNEL
/*
 * Template for manipulating directories.
 * Should use struct direct's, but the name field
 * is MAXNAMLEN - 1, and this just won't do.
 */
struct bsd43_(dirtemplate) {
	u_long	dot_ino;
	short	dot_reclen;
	short	dot_namlen;
	char	dot_name[4];		/* must be multiple of 4 */
	u_long	dotdot_ino;
	short	dotdot_reclen;
	short	dotdot_namlen;
	char	dotdot_name[4];		/* ditto */
};
#endif

/*----- NUMIPS: STRIP MACRO PREFIXES (BEGIN) ---------------------------------*/
/* 
 * Strip off "BSD43_" and "bsd43_" for use with standard BSD code.
 * (GENERATED AUTOMATICALLY -- DON'T EDIT BY HAND)
 */
#ifdef SYSTYPE_BSD43
#   define DEV_BSIZE BSD43_DEV_BSIZE
#   define DIRBLKSIZ BSD43_DIRBLKSIZ
#   define DIRBUF BSD43_DIRBUF
#   define DIRSIZ BSD43_DIRSIZ
#   define MAXNAMLEN BSD43_MAXNAMLEN
#   define NULL BSD43_NULL
#   define d_ino bsd43_d_ino
#   define rewinddir bsd43_rewinddir
#endif SYSTYPE_BSD43
/*----- NUMIPS: STRIP MACRO PREFIXES (END) -----------------------------------*/


