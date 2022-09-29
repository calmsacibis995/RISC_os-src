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
/* $Header: stat.h,v 1.3.1.3 90/05/10 06:02:49 wje Exp $ */

#ifndef	_POSIX_SYS_STAT_
#define	_POSIX_SYS_STAT_	1

/*	Copyright (c) 1984 AT&T	*/
/*	  All Rights Reserved  	*/

/*	THIS IS UNPUBLISHED PROPRIETARY SOURCE CODE OF AT&T	*/
/*	The copyright notice above does not evidence any   	*/
/*	actual or intended publication of such source code.	*/


/*
 * stat structure, used by stat(2) and fstat(2)
 */
struct	stat {
	dev_t	st_dev;
	ino_t	st_ino;
	mode_t 	st_mode;
	nlink_t	st_nlink;
	uid_t 	st_uid;
	gid_t 	st_gid;
	dev_t	st_rdev;
	off_t	st_size;
	time_t	st_atime;
	time_t	st_mtime;
	time_t	st_ctime;
};

#define	_S_IFMT		0170000 /* mask for file type */
#define	_S_IFREG	0100000	/* regular */
#define	_S_IFBLK	0060000	/* block special */
#define	_S_IFDIR	0040000	/* directory */
#define	_S_IFCHR	0020000	/* character special */
#define	_S_IFIFO	0010000	/* fifo */

#define	S_ISUID	04000		/* set user id on execution */
#define	S_ISGID	02000		/* set group id on execution */
#define	S_IRWXU	00700		/* read, write, execute: owner */
#define		S_IRUSR	00400	/* read permission: owner */
#define		S_IWUSR	00200	/* write permission: owner */
#define		S_IXUSR	00100	/* execute permission: owner */
#define	S_IRWXG	00070		/* read, write, execute: group */
#define		S_IRGRP	00040	/* read permission: group */
#define		S_IWGRP	00020	/* write permission: group */
#define		S_IXGRP	00010	/* execute permission: group */
#define	S_IRWXO	00007		/* read, write, execute: other */
#define		S_IROTH	00004	/* read permission: other */
#define		S_IWOTH	00002	/* write permission: other */
#define		S_IXOTH	00001	/* execute permission: other */

#define S_ISDIR(m)      (((m)&_S_IFMT) == _S_IFDIR)
#define S_ISCHR(m)      (((m)&_S_IFMT) == _S_IFCHR)
#define S_ISBLK(m)      (((m)&_S_IFMT) == _S_IFBLK)
#define S_ISREG(m)      (((m)&_S_IFMT) == _S_IFREG)
#define S_ISFIFO(m)     (((m)&_S_IFIFO) == _S_IFIFO)

extern	mode_t	umask();
extern	int	mkdir();
extern	int	mkfifo();
extern	int	stat();
extern	int	fstat();
extern	int	chmod();

#ifndef	_POSIX_SOURCE
#define	S_IFMT	0170000 /* mask for file type */
#define	S_IFREG	_S_IFREG	/* regular */
#define	S_IFBLK	_S_IFBLK	/* block special */
#define	S_IFDIR	_S_IFDIR	/* directory */
#define	S_IFCHR	_S_IFCHR	/* character special */
#define	S_IFIFO	_S_IFIFO	/* fifo */
#endif	/* !_POSIX_SOURCE */

#endif	_POSIX_SYS_STAT_
