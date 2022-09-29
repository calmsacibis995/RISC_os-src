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
#ident	"$Header: readdir.c,v 1.6.2.2 90/05/10 01:18:19 wje Exp $"

/* Copyright (c) 1982 Regents of the University of California */

#include <bsd/sys/types.h>
#include <bsd/sys/param.h>
#ifdef mips
#include <bsd/sys/dir.h>
#else
#include <ndir.h>
#endif

/*
 * Use 'getdents' system call to read a directory and present it
 * to the caller as if it were a 4.2 style directory.
 */

/*
 * Return next entry in a directory.
 */
struct direct *
readdir(dirp)
	register DIR *dirp;
{
	register struct dirent *dp;	/* struct returned by getdents */
	register struct direct *retp;	/* BSD4.2 directory struct */

	for (;;) {
		/*
		|| If no data in current buffer, call kernel to get more
		*/
		if (dirp->dd_size == 0) {
			dirp->dd_size = getdents(dirp->dd_fd, dirp->dd_buf, 
			    DIRBLKSIZ);
			if (dirp->dd_size <= 0)
				return NULL;
			dirp->dd_loc = 0;
		}
		/*
		|| If current buffer is exhausted, go read more
		*/
		if (dirp->dd_loc >= dirp->dd_size) {
			dirp->dd_size = 0;
			continue;
		}
		dp = (struct dirent *)(dirp->dd_buf + dirp->dd_loc);
		dirp->dd_loc += dp->d_reclen;
		/*
		|| Don't return empty entries
		*/
		if (dp->d_ino == 0)
			continue;
		/*
		|| Copy the data from the getdents buffer to the
		|| BSD struct direct
		*/
		retp = &dirp->dd_direct;
		retp->d_ino = dp->d_ino;
		/*
		|| Names returned by getdents are guaranteed null terminated
		*/
		retp->d_namlen = strlen(dp->d_name);
		strcpy(retp->d_name, dp->d_name);
		retp->d_reclen = DIRSIZ(retp);
		return (retp);
	}
}
