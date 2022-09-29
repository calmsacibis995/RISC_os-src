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
#ident	"$Header: fs_lookup.c,v 1.4.2.2 90/05/09 15:42:31 wje Exp $"

#include <stdio.h>
#include <sun/mntent.h>
#include <sys/fstyp.h>
#include <sys/statfs.h>
#include <sys/types.h>
#include <sys/stat.h>
#include <errno.h>

extern char	MTAB[];

void	put_line(/* mntp */);

int
stat_entry(mntp, sfbp)
	register struct mntent *mntp;
	register struct statfs *sfbp;
{
	char *colon;

	if (statfs(mntp->mnt_dir, sfbp, sizeof(struct statfs), 0) < 0) {
		if (errno == ETIMEDOUT &&
		    strcmp(mntp->mnt_type, MNTTYPE_NFS) == 0) {
			colon = (char *)strchr(mntp->mnt_fsname, ':');
			if (colon == NULL) {	/* NFS, yet not NFS? */
				fprintf(stderr, "Bad NFS mount: %s\n",
					mntp->mnt_fsname);
			} else {
				*colon = '\0';
				mark_down(mntp->mnt_fsname);
				*colon = ':';
			}
		}
		return df_perror("cannot get filesystem information",
			mntp->mnt_dir);
	}
#ifdef DEBUG
	pstatfs(mntp->mnt_fsname, sfbp);
#endif

	return 0;
}

#ifdef DEBUG
pstatfs(name, sfbp)
	char *name;
	register struct statfs *sfbp;
{
	fprintf(stderr, "\
statfs(%s) = {\n\
	fstyp %d,\n\
	bsize %ld,\n\
	frsize %ld,\n\
	blocks %ld,\n\
	bfree %ld,\n\
	files %ld,\n\
	ffree %ld,\n\
	fname %.6s,\n\
	fpack %.6s\n\
}\n",
	    name,
	    sfbp->f_fstyp,
	    sfbp->f_bsize,
	    sfbp->f_frsize,
	    sfbp->f_blocks,
	    sfbp->f_bfree,
	    sfbp->f_files,
	    sfbp->f_ffree,
	    sfbp->f_fname,
	    sfbp->f_fpack);
}
#endif

/* We couldn't stat the file with the name given on the
 * command line, so we check if the file name is an 
 * nfs file system name of the form host:pathname, and find the
 * directory on which the file name is mounted.  If name is not of
 * the form host:pathname we return (-1).
 *
 * If a directory name is found, attempt to do the df on it.
 *
 */
struct mntent *
find_nfs_entry(fsname) 
char *fsname;
{
	register FILE *mtabp;
	register struct mntent *mnt;
	register struct stat  	statb;
	register char *cp = fsname;

	while (cp && *cp && *cp != ':') cp++;
	if (cp == NULL || *cp == '\0') return(NULL);

	if ((mtabp = setmntent(MOUNTED, "r")) == 0) {
		perror(MOUNTED);
		exit(1);
	}

	while ((mnt = getmntent(mtabp)) != NULL) {
		if (strcmp(mnt->mnt_type, MNTTYPE_IGNORE) == 0 ||
		    strcmp(mnt->mnt_type, MNTTYPE_SWAP) == 0)
			continue;
		if (strcmp(mnt->mnt_type, MNTTYPE_NFS) != 0 ) {
			continue;
		}
		if (strcmp(mnt->mnt_fsname, fsname) == 0) {
			/* Found it, now stat the directory fsname
			 * is mounted on.
			 */
			(void) endmntent(mtabp);
			return(mnt);
		}
	}

	/* If we got here, we didn't find a match for name, so
	 * we couldn't do a stat.
	 */
	(void) endmntent(mtabp);
	return(NULL);
}

extern struct mntent *get_ment();

find_entry(name)
	char *name;
{
	struct mntent *mntp;
	struct stat stb;

	if (stat(name, &stb) < 0) {
		if ((mntp = find_nfs_entry(name)) == NULL) {
			df_perror("", name);
			return;
		}
	} else {

		if ((stb.st_mode & S_IFMT) == S_IFBLK) {
			mntp = get_ment(stb.st_rdev);
	/*
	 * if mntp is NULL here, we should still try looking for an unmounted
	 * filesystem and fake the mount table entry
	 */
		} else {
			mntp = get_ment(stb.st_dev);
		}
	}
	if (mntp) {
		if (valid_type(mntp->mnt_type)) {
			put_line(mntp);
		}
	} else {
		fprintf(stderr,
		"df: %s: No filesystem found or server is down\n",
		name);
	}
}


