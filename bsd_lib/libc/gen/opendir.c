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
#ident	"$Header: opendir.c,v 1.1.2.2 90/05/07 20:39:40 wje Exp $"

#include <sys/param.h>
#include <sys/stat.h>
#include <sys/dir.h>
#include <errno.h>

/*
 * open a directory.
 */
DIR *
opendir(name)
	char *name;
{
	register DIR *dirp;
	register int fd;
	struct stat sb;
	extern int errno;
	extern char *malloc();
	extern int open(), close(), fstat();

	if ((fd = open(name, 0)) == -1)
		return (NULL);
	if (fstat(fd, &sb) == -1) {
		(void) close(fd);
		return (NULL);
	}
#ifdef DIRECTORIES_ONLY
	if ((sb.st_mode & S_IFMT) != S_IFDIR) {
		errno = ENOTDIR;
		(void) close(fd);
		return (NULL);
	}
#endif
	if (((dirp = (DIR *)malloc(sizeof(DIR))) == NULL) ||
	    ((dirp->dd_buf = malloc((unsigned)sb.st_blksize)) == NULL)) {
		if (dirp) {
			free(dirp);
		}
		(void) close(fd);
		return (NULL);
	}
	dirp->dd_bsize = sb.st_blksize;
	dirp->dd_bbase = 0;
	dirp->dd_entno = 0;
	dirp->dd_fd = fd;
	dirp->dd_loc = 0;
	return (dirp);
}
