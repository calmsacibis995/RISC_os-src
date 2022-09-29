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
#ident	"$Header: closedir.c,v 1.7.2.2 90/05/10 01:14:06 wje Exp $"

/* Copyright (c) 1982 Regents of the University of California */

#include <bsd/sys/types.h>
#ifdef mips
#include <bsd/sys/dir.h>
#else
#include <ndir.h>
#endif

#undef BSDclosedir

/*
 * close a directory.
 */
void
BSDclosedir(dirp)
	register DIR *dirp;
{
	close(dirp->dd_fd);
	dirp->dd_fd = -1;
	dirp->dd_size = 0;
	free(dirp);
}
