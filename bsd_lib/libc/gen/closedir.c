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
#ident	"$Header: closedir.c,v 1.1.2.2 90/05/07 20:35:05 wje Exp $"

#ifndef lint
#endif

#include <sys/param.h>
#include <sys/dir.h>

/*
 * close a directory.
 */
void
closedir(dirp)
	register DIR *dirp;
{
	extern void free();
	extern int close();

	(void) close(dirp->dd_fd);
	dirp->dd_fd = -1;
	dirp->dd_loc = 0;
	if (dirp->dd_buf) {
		free((char *)dirp->dd_buf);
	}
	free((char *)dirp);
}
