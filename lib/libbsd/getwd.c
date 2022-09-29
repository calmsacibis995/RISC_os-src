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
#ident	"$Header: getwd.c,v 1.7.2.2 90/05/10 01:16:57 wje Exp $"

/*
 * Copyright (c) 1980 Regents of the University of California.
 * All rights reserved.  The Berkeley software License Agreement
 * specifies the terms and conditions for redistribution.
 */

/*
 * getwd() returns the pathname of the current working directory. On error
 * an error message is copied to pathname and null pointer is returned.
 */

#ifdef SYSTYPE_SYSV
#include <bsd/sys/param.h>
#else SYSTYPE_SYSV
#include <sys/param.h>
#endif SYSTYPE_SYSV
#include <sys/stat.h>
#ifdef SYSTYPE_SYSV
#include <bsd/sys/dir.h>
#else SYSTYPE_SYSV
#include <sys/dir.h>
#endif SYSTYPE_SYSV

#define GETWDERR(s)	strcpy(pathname, (s));

char *strcpy();
static int pathsize;			/* pathname length */

char *
getwd(pathname)
	char *pathname;
{
	char pathbuf[MAXPATHLEN];		/* temporary pathname buffer */
	char *pnptr = &pathbuf[(sizeof pathbuf)-1]; /* pathname pointer */
	char curdir[MAXPATHLEN];	/* current directory buffer */
	char *dptr = curdir;		/* directory pointer */
	char *prepend();		/* prepend dirname to pathname */
	dev_t cdev, rdev;		/* current & root device number */
	ino_t cino, rino;		/* current & root inode number */
	DIR *dirp = NULL;		/* directory stream */
	struct direct *dir;		/* directory entry struct */
	struct stat d, dd;		/* file status struct */
	DIR *prevdirp = NULL;

	pathsize = 0;
	*pnptr = '\0';
	if (stat("/", &d) < 0) {
		GETWDERR("getwd: can't stat /");
		return (NULL);
	}
	rdev = d.st_dev;
	rino = d.st_ino;
	strcpy(dptr, "./");
	dptr += 2;
	if ((prevdirp = opendir(curdir)) == NULL)  {
	  	GETWDERR("getwd: can't open .");
		goto baddir;
	};
	if (stat(curdir, &d) < 0) {
		GETWDERR("getwd: can't stat .");
		goto baddir;
	}
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
		if ((dirp = opendir(curdir)) == NULL) {
			GETWDERR("getwd: can't open ..");
			goto baddir;
		}
		fstat(dirp->dd_fd, &d);
		if (cdev == d.st_dev) {
			if (cino == d.st_ino) {
				/* reached root directory */
				closedir(prevdirp);
				closedir(dirp);
				break;
			}
			do {
				if ((dir = readdir(dirp)) == NULL) {
					GETWDERR("getwd: read error in ..");
					goto baddir;
				}
			} while (dir->d_ino != cino);
		} else
			do {
next_entry:
				if ((dir = readdir(dirp)) == NULL) {
					GETWDERR("getwd: read error in ..");
					goto baddir;
				}
				strcpy(dptr, dir->d_name);
				if (lstat(curdir, &dd) == -1){
					goto next_entry;
				};
			} while(dd.st_ino != cino || dd.st_dev != cdev);
		closedir(prevdirp);
		prevdirp = dirp;
		dirp = NULL;
		pnptr = prepend("/", prepend(dir->d_name, pnptr));
	}
	if (*pnptr == '\0')		/* current dir == root dir */
		strcpy(pathname, "/");
	else
		strcpy(pathname, pnptr);
	return (pathname);

baddir:
	if (prevdirp != NULL)
		closedir(prevdirp);
	if (dirp != NULL)
		closedir(dirp);
	return(NULL);
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
	if ((pathsize += i) < MAXPATHLEN)
		while (i-- > 0)
			*--pathname = *--dirname;
	return (pathname);
}
