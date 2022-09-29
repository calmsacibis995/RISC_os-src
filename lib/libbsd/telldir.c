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
#ident	"$Header: telldir.c,v 1.6.2.2 90/05/10 01:22:24 wje Exp $"

#include <bsd/sys/param.h>
#ifdef mips
#include <bsd/sys/dir.h>
#else
#include <ndir.h>
#endif

/*
 * return a pointer into a directory
 */
long
telldir(dirp)
	register DIR *dirp;
{
	struct dirent *dp;
	extern long lseek();

	if (dirp->dd_size == 0 || dirp->dd_loc >= dirp->dd_size)
		return (lseek(dirp->dd_fd, 0L, 1));

	dp = (struct dirent *) &dirp->dd_buf[dirp->dd_loc];
	return (dp->d_off);
}
