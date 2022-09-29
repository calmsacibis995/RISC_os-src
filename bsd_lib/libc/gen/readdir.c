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
#ident	"$Header: readdir.c,v 1.1.2.2 90/05/07 20:40:42 wje Exp $"

#include <sys/param.h>
#include <sys/dir.h>

/*
 * get next entry in a directory.
 */
struct direct *
readdir(dirp)
	register DIR *dirp;
{
	register struct direct *dp;
	extern int getdirentries();

	for (;;) {
		if (dirp->dd_loc == 0) {
			dirp->dd_size = getdirentries(dirp->dd_fd,
			    dirp->dd_buf, dirp->dd_bsize, &dirp->dd_bbase);
			if (dirp->dd_size <= 0)
				return (NULL);
			dirp->dd_entno = 0;
		}
		if (dirp->dd_loc >= dirp->dd_size) {
			dirp->dd_loc = 0;
			continue;
		}
		dp = (struct direct *)(dirp->dd_buf + dirp->dd_loc);
		if (dp->d_reclen <= 0)
			return (NULL);
		dirp->dd_loc += dp->d_reclen;
		dirp->dd_entno++;
		if (dp->d_fileno == 0)
			continue;
		return (dp);
	}
}
