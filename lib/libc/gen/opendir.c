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
#ident	"$Header: opendir.c,v 1.6.2.2 90/05/10 01:35:54 wje Exp $"

/*	Copyright (c) 1984 AT&T	*/
/*	  All Rights Reserved  	*/

/*	THIS IS UNPUBLISHED PROPRIETARY SOURCE CODE OF AT&T	*/
/*	The copyright notice above does not evidence any   	*/
/*	actual or intended publication of such source code.	*/

/*
	opendir -- C library extension routine

*/

#include	<sys/types.h>
#include	<dirent.h>
#include	<sys/stat.h>
#include	<sys/errno.h>

extern char	*malloc();
extern int	open(), close(), fstat();
extern int	errno;

#define NULL	0

DIR *
opendir( filename )
char		*filename;	/* name of directory */
{
	register DIR	*dirp;		/* -> malloc'ed storage */
	register int	fd;		/* file descriptor for read */
	struct stat	sbuf;		/* result of fstat() */

	if ( (fd = open( filename, 0 )) < 0 )
		return NULL;
	if ( (fstat( fd, &sbuf ) < 0)
	  || ((sbuf.st_mode & S_IFMT) != S_IFDIR)
	  || ((dirp = (DIR *)malloc( sizeof(DIR) )) == NULL)
	  || ((dirp->dd_buf = (char *)malloc(DIRBUF)) == NULL)
	   )	{
		if ((sbuf.st_mode & S_IFMT) != S_IFDIR)
			errno = ENOTDIR;
		(void)close( fd );
		return NULL;		/* bad luck today */
		}

	dirp->dd_fd = fd;
	dirp->dd_loc = dirp->dd_size = 0;	/* refill needed */

	return dirp;
}
