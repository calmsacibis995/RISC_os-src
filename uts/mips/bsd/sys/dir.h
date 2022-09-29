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
/* $Header: dir.h,v 1.7.4.2 90/05/10 04:36:21 wje Exp $ */

#ifndef	_BSD_SYS_DIR_
#define	_BSD_SYS_DIR_	1


/* Copyright (c) 1982 Regents of the University of California */

/*
 * These definitions force the libbsd versions of these routines
 * to not clash with the libc versions.
 */

#define opendir		BSDopendir
#define closedir	BSDclosedir
#define readdir		BSDreaddir
#define scandir		BSDscandir
#define telldir		BSDtelldir
#define seekdir		BSDseekdir

/*
 * This sets the "page size" for directories.
 * Requirements are DEV_BSIZE <= DIRBLKSIZ <= MINBSIZE with
 * DIRBLKSIZ a power of two.
 * Dennis Ritchie feels that directory pages should be atomic
 * operations to the disk, so we use DEV_BSIZE.
 */
#ifndef DEV_BSIZE
#define	DEV_BSIZE	512
#endif
#define DIRBLKSIZ DEV_BSIZE

/*
 * This limits the directory name length. Its main constraint
 * is that it appears twice in the user structure. (u. area)
 */
#define MAXNAMLEN 255

struct	direct {
	u_long	d_ino;
	short	d_reclen;
	short	d_namlen;
	char	d_name[MAXNAMLEN + 1];
	/* typically shorter */
};

struct _dirdesc {
	int	dd_fd;
	long	dd_loc;
	long	dd_size;
	char	dd_buf[DIRBLKSIZ];
	struct	direct dd_direct;
};

/*
 * useful macros.
 */
#undef DIRSIZ
#define DIRSIZ(dp) \
    ((sizeof(struct direct) - MAXNAMLEN + (dp)->d_namlen + sizeof(ino_t) - 1) &\
    ~(sizeof(ino_t) - 1))
typedef	struct _dirdesc DIR;
#ifndef	NULL
#define	NULL	0
#endif

/*
 * functions defined on directories
 */
extern DIR *opendir();
extern struct direct *readdir();
extern long telldir();
extern void seekdir();
#define rewinddir(dirp)	seekdir((dirp), 0)
extern void closedir();

#include <sys/dirent.h>

#endif	_BSD_SYS_DIR_
