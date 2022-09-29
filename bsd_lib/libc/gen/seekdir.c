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
#ident	"$Header: seekdir.c,v 1.1.2.2 90/05/07 20:41:14 wje Exp $"

#include <sys/param.h>
#include <sys/dir.h>

/*
 * seek to an entry in a directory.
 * Only values returned by "telldir" should be passed to seekdir.
 */
void
seekdir(dirp, tell)
	register DIR *dirp;
	register long tell;
{
	register struct direct *dp;
	register long entno;
	long base;
	long curloc;
	extern long lseek();

	curloc = telldir(dirp);
	if (curloc == tell)
		return;
	base = tell / dirp->dd_bsize;
	entno = tell % dirp->dd_bsize;
	(void) lseek(dirp->dd_fd, base, 0);
	dirp->dd_loc = 0;
	dirp->dd_entno = 0;
	while (dirp->dd_entno < entno) {
		dp = readdir(dirp);
		if (dp == NULL)
			return;
	}
}
