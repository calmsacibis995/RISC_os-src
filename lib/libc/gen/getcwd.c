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
#ident	"$Header: getcwd.c,v 1.12.1.2 90/05/10 01:31:59 wje Exp $"

/*	Copyright (c) 1984 AT&T	*/
/*	  All Rights Reserved  	*/

/*	THIS IS UNPUBLISHED PROPRIETARY SOURCE CODE OF AT&T	*/
/*	The copyright notice above does not evidence any   	*/
/*	actual or intended publication of such source code.	*/

/*LINTLIBRARY*/
/*
 * Library routine to GET the Current Working Directory.
 * arg1 is a pointer to a character buffer into which the
 * path name of the current directory is placed by the
 * subroutine.  arg1 may be zero, in which case the 
 * subroutine will call malloc to get the required space.
 * arg2 is the length of the buffer space for the path-name.
 * If the actual path-name is longer than (arg2-2), or if
 * the value of arg2 is not at least 3, the subroutine will
 * return a value of zero, with errno set as appropriate.
 */

#include <stdio.h>
#include <sys/errno.h>

extern char *malloc();
extern int errno;

static char *getcwd_path();

char *
getcwd(arg1, arg2)
char	*arg1;
int	arg2;
{
  	int	allocated_arg1 = 0;

	if(arg2 == 0) {
		errno = EINVAL;
		return(0);
	}
	if(arg2 < 0) {
#ifdef	SYSTYPE_POSIX
		errno = EINVAL;
#else	/* !SYSTYPE_POSIX */
		errno = ERANGE;
#endif	/* !SYSTYPE_POSIX */
		return(0);
	}
	if(arg1 == 0) {
		if((arg1 = malloc((unsigned)arg2)) == 0) {
			errno = ENOMEM;
			return(0);
		};
		allocated_arg1 = 1;
	};
	errno = 0;
	if (getcwd_path(arg1,arg2) == NULL) {
		if (allocated_arg1)
			free(arg1);
		return(0);
	};
	return(arg1);
}

/*
 * getcwd_path() returns the pathname of the current working directory. 
 * On error, an error code is left in errno, an a null pointer is returned.
 * an error message is copied to pathname and null pointer is returned.
 */
#ifdef	SYSTYPE_POSIX
#include <limits.h>
#else	/* !SYSTYPE_POSIX */
#include <sys/limits.h>
#endif	/* !SYSTYPE_POSIX */
#include <sys/types.h>
#include <sys/stat.h>
#include <dirent.h>

char *strcpy();
static int pathsize;			/* pathname length */

static char *prepend();

static char *
getcwd_path(pathname,pathlen)
	char *pathname;
	int	pathlen;
{
	char pathbuf[PATH_MAX];		/* temporary pathname buffer */
	char *pnptr = &pathbuf[(sizeof pathbuf)-1]; /* pathname pointer */
	char curdir[PATH_MAX];	/* current directory buffer */
	char *dptr = curdir;		/* directory pointer */
	dev_t cdev, rdev;		/* current & root device number */
	ino_t cino, rino;		/* current & root inode number */
	DIR *dirp = NULL;		/* directory stream */
	struct dirent *dir;		/* directory entry struct */
	struct stat d, dd;		/* file status struct */
	int	saved_errno;
	DIR *prevdirp = NULL;

	pathsize = 0;
	*pnptr = '\0';
	if (stat("/", &d) < 0) {
		return (NULL);
	}
	rdev = d.st_dev;
	rino = d.st_ino;
	strcpy(dptr, "./");
	dptr += 2;
	if ((prevdirp = opendir(curdir)) == NULL) 
		goto baddir;
	if (stat(curdir, &d) < 0) 
		goto baddir;
	for (;;) {
		if (d.st_ino == rino && d.st_dev == rdev) {
			/* reached root directory */
			closedir(prevdirp);
			break;
		}
		cino = d.st_ino;
		cdev = d.st_dev;
		strcpy(dptr, "../");
		dptr += 3;
		errno = 0;
		if ((dirp = opendir(curdir)) == NULL) 
			goto baddir;
		fstat(dirp->dd_fd, &d);
		if (cdev == d.st_dev) {
			if (cino == d.st_ino) {
				/* reached root directory */
				closedir(prevdirp);
				closedir(dirp);
				break;
			}
			do {
				if ((dir = readdir(dirp)) == NULL) 
					goto baddir;
			} while (dir->d_ino != cino);
		} else
			do {
next_entry:
				if ((dir = readdir(dirp)) == NULL) 
					goto baddir;
				strcpy(dptr, dir->d_name);
				if (lstat(curdir, &dd) == -1)
					goto next_entry;
			} while(dd.st_ino != cino || dd.st_dev != cdev);
		closedir(prevdirp);
		prevdirp = dirp;
		dirp = NULL;
		pnptr = prepend("/", prepend(dir->d_name, pnptr));
	}
	if (*pnptr == '\0')		/* current dir == root dir */
		strcpy(pathname, "/");
	else {
		if (strlen(pnptr) >= pathlen) {
			errno = ERANGE;
			return(0);
		};
		strcpy(pathname, pnptr);
	};
	errno = 0;
	return (pathname);

baddir:
	saved_errno = errno;
	if (prevdirp != NULL)
		closedir(prevdirp);
	if (dirp != NULL)
		closedir(dirp);
	errno = saved_errno;
	if (errno == 0) 
		errno = EIO;
	return (NULL);
}

/*
 * prepend() tacks a directory name onto the front of a pathname.
 */
static char *
prepend(dirname, pathname)
	register char *dirname;
	register char *pathname;
{
	register int i;			/* directory name size counter */

	for (i = 0; *dirname != '\0'; i++, dirname++)
		continue;
	if ((pathsize += i) < PATH_MAX)
		while (i-- > 0)
			*--pathname = *--dirname;
	return (pathname);
}
