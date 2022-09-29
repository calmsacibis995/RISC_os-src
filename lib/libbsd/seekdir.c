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
#ident	"$Header: seekdir.c,v 1.6.2.2 90/05/10 01:18:53 wje Exp $"

/* Copyright (c) 1982 Regents of the University of California */

#include <bsd/sys/types.h>
#ifdef mips
#include <bsd/sys/dir.h>
#else
#include <ndir.h>
#endif

/*
 * seek to an entry in a directory.
 * Only values returned by ``telldir'' should be passed to seekdir.
 */
void
seekdir(dirp, loc)
	register DIR *dirp;
	long loc;
{
	register long curloc;
	register struct dirent *dp;

	curloc = telldir(dirp);
	if (loc == curloc)
		return;
	
	/*
	 * If there is data in the current buffer, check whether the
	 * desired offset is within its limits.
	 * 
	 * In the new 'dirent' scheme, there is an offset value in
	 * the dirent structure that gives the actual offset in the
	 * disk directory file of the entry represented by that
	 * dirent structure.  The values returned by telldir are
	 * these offsets.
	 */
	if (dirp->dd_size > 0) {
		dp = (struct dirent *) dirp->dd_buf;
		curloc = 0;
		while (curloc < dirp->dd_size) {
			if (dp->d_off > loc)
				break;
			if (dp->d_off == loc) {
				dirp->dd_loc = curloc;
				return;
			}
			curloc += dp->d_reclen;
			dp = (struct dirent *) &dirp->dd_buf[curloc];
		}
	}

	/* 
	 * The requested location is not in the current buffer.
	 * Discard the buffer and reposition the file pointer to the
	 * requested location.
	 */
	dirp->dd_size = 0;
	lseek(dirp->dd_fd, loc, 0);
}
