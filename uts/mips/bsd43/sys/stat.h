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
/* $Header: stat.h,v 1.11.1.2 90/05/10 04:55:40 wje Exp $ */
/*
 * Copyright (c) 1982, 1986 Regents of the University of California.
 * All rights reserved.  The Berkeley software License Agreement
 * specifies the terms and conditions for redistribution.
 *
 *	@(#)stat.h	7.1 (Berkeley) 6/4/86
 */
/*----- COPYRIGHT (END) ------------------------------------------------------*/

#ifndef BSD43_
#    include <bsd43/bsd43_.h>
#endif BSD43_


struct	bsd43_(stat)
{
	dev_t	st_dev;
	ino_t	st_ino;
	unsigned 	short st_mode;
	short		st_nlink;
	uid_t	st_uid;
	gid_t	st_gid;
	dev_t	st_rdev;
	off_t	st_size;
	time_t	st_atime;
	int		st_spare1;
	time_t	st_mtime;
	int		st_spare2;
	time_t	st_ctime;
	int		st_spare3;
	long		st_blksize;
	long		st_blocks;
	long		st_spare4[2];
};

#define	BSD43_S_IFMT	0170000		/* type of file */
#define	     BSD43_S_IFDIR	0040000	/* directory */
#define	     BSD43_S_IFCHR	0020000	/* character special */
#define	     BSD43_S_IFBLK	0060000	/* block special */
#define	     BSD43_S_IFIFO	0010000 /* SYSV fifo -- for mixed sources */
#define	     BSD43_S_IFREG	0100000	/* regular */
#define	     BSD43_S_IFLNK	0120000	/* symbolic link */
#define	     BSD43_S_IFSOCK	0140000 /* socket */
#define	BSD43_S_ISUID	04000		/* set user id on execution */
#define	BSD43_S_ISGID	02000		/* set group id on execution */
#define	BSD43_S_ISVTX	01000		/* save swapped text even after use */
#define	BSD43_S_IREAD	00400		/* read permission, owner */
#define	BSD43_S_IWRITE	00200		/* write permission, owner */
#define	BSD43_S_IEXEC	00100		/* execute/search permission, owner */

/*----- NUMIPS: STRIP MACRO PREFIXES (BEGIN) ---------------------------------*/
/* 
 * Strip off "BSD43_" and "bsd43_" for use with standard BSD code.
 * (GENERATED AUTOMATICALLY -- DON'T EDIT BY HAND)
 */
#ifdef SYSTYPE_BSD43
#   define S_IEXEC BSD43_S_IEXEC
#   define S_IFBLK BSD43_S_IFBLK
#   define S_IFIFO BSD43_S_IFIFO
#   define S_IFCHR BSD43_S_IFCHR
#   define S_IFDIR BSD43_S_IFDIR
#   define S_IFLNK BSD43_S_IFLNK
#   define S_IFMT BSD43_S_IFMT
#   define S_IFREG BSD43_S_IFREG
#   define S_IFSOCK BSD43_S_IFSOCK
#   define S_IREAD BSD43_S_IREAD
#   define S_ISGID BSD43_S_ISGID
#   define S_ISUID BSD43_S_ISUID
#   define S_ISVTX BSD43_S_ISVTX
#   define S_IWRITE BSD43_S_IWRITE
#endif SYSTYPE_BSD43
/*----- NUMIPS: STRIP MACRO PREFIXES (END) -----------------------------------*/


