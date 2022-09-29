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
#ident	"$Header: opendir.c,v 1.7.1.2 90/05/10 01:18:13 wje Exp $"

/* Copyright (c) 1982 Regents of the University of California */

#include <bsd/sys/types.h>
#include <sys/stat.h>
#ifdef mips
#include <bsd/sys/dir.h>
#else
#include <ndir.h>
#endif
#include <errno.h>

/*
 * open a directory.
 */
DIR *
opendir(name)
	char *name;
{
	register DIR *dirp;
	struct stat sbuf;

	dirp = (DIR *)malloc(sizeof(DIR));
	if (dirp == NULL)
		return NULL;
	dirp->dd_fd = open(name, 0);
	if (dirp->dd_fd == -1) {
		free(dirp);
		return NULL;
	}
	fstat(dirp->dd_fd, &sbuf);
	if ((sbuf.st_mode & S_IFMT) != S_IFDIR) {
		errno = ENOTDIR;
		free(dirp);
		return (NULL);
	}
	dirp->dd_size = 0;
	return dirp;
}
