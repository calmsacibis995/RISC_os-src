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
#ident	"$Header: devnm.c,v 1.1.2.4 90/05/09 17:22:26 wje Locked $"

#include <stdio.h>
#include <sun/mntent.h>
#include <sys/types.h>
#include <sys/nami.h>
#include <sys/stat.h>
#include <sys/fstyp.h>
#include <sys/statfs.h>

extern char	MTAB[];


#include <dirent.h>

char *
devnm(file, dev)
	char *file;
	register dev_t dev;
{
	register int i;
	struct statfs sfsb;
	char fsid[FSTYPSZ];
	char *name, *searchdir();

	static struct devs {
		char	*dirname;
		DIR	*dirp;
	} devs[] = {		/* in order of desired search */
		"/dev/dsk",	NULL,
		"/dev",		NULL,
		"/dev/rdsk",	NULL
	};
	static char devname[MAXPATHLEN];

	/*
	 * Check whether file lives on a network filesystem.
	 */
	if (statfs(file, &sfsb, sizeof sfsb, 0) < 0
	    || sysfs(GETFSTYP, sfsb.f_fstyp, fsid) < 0) {
		return NULL;
	}
	if (!strcmp(fsid, FSID_NFS)) {
		register FILE *mtabp;
		register struct mntent *mntp;

		if ((mtabp = setmntent(MTAB, "r")) == NULL)
			return NULL;
		while ((mntp = getmntent(mtabp)) != NULL) {
			struct stat stb;

			if (stat(mntp->mnt_dir, &stb) < 0)
				continue;
			if (stb.st_dev == dev)
				return mntp->mnt_fsname;
		}
		endmntent(mtabp);
	}

	/*
	 * Not nfs, so assume a disk filesystem on a device whose node is
	 * in one of the search directories.
	 */
	if (devs[0].dirp == NULL) {
		for (i = 0; i < 3; i++)
			devs[i].dirp = opendir(devs[i].dirname);
	}

	for (i = 0; i < 3; i++) {
		if (devs[i].dirp != NULL
		    && chdir(devs[i].dirname) == 0
		    && (name = searchdir(devs[i].dirp, dev)) != NULL) {
			sprintf(devname, "%s/%s", devs[i].dirname, name);
			return devname;
		}
	}
	return NULL;
}

char *
searchdir(dirp, dev)
	register DIR *dirp;
	register dev_t dev;
{
	register struct dirent *dep;
	struct stat stb;

	rewinddir(dirp);
#ifdef RISCOS
	while ((dep = readdir(dirp)) != 0)
#else
	while ((dep = readdir(dirp)) > 0)
#endif
	{
		if (dep->d_ino == 0)
			continue;
		if (stat(dep->d_name, &stb) < 0)
			return NULL;
/* XXX stupid AT&T: why oh why are we excluding non-block inodes but */
/* XXX nonetheless searching /dev/rdsk? */
		if (dev != stb.st_rdev
		    || (stb.st_mode & S_IFMT) != S_IFBLK
		    || strcmp(dep->d_name, "swap") == 0
		    || strcmp(dep->d_name, "pipe") == 0) {
			continue;
		}
		return dep->d_name;
	}
	return NULL;
}
