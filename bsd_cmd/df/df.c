/* |-----------------------------------------------------------|
 * | Copyright (c) 1990 MIPS Computer Systems, Inc.            |
 * | All Rights Reserved                                       |
 * |-----------------------------------------------------------|
 * |          Restricted Rights Legend                         |
 * | Use, duplication, or disclosure by the Government is      |
 * | subject to restrictions as set forth in                   |
 * | subparagraph (c)(1)(ii) of the Rights in Technical        |
 * | Data and Computer Software Clause of DFARS 52.227-7013.   |
 * |         MIPS Computer Systems, Inc.                       |
 * |         950 DeGuigne Drive                                |
 * |         Sunnyvale, CA 94086                               |
 * |-----------------------------------------------------------|
 */
#ident	"$Header: df.c,v 1.3.2.2.1.2 90/08/03 11:11:19 hawkes Exp $"

/*
 * Strategy note: There exist versions of this command that cache
 * mount table entries, and versions that look like they are trying
 * to cache them but aren't (like the previous revision of this one).
 *
 * This is probably a fine idea, but if you really need speed (this
 * is df, remember, which is not one of the top 10 most used commands),
 * what you want to do is to cache the mount directory device numbers,
 * and you only want to stat them if you have to. You see, if you
 * stat them all, you may end up stat'ing remotely mounted filesystems
 * for machines that are down, and that really screws things up.
 *
 * Here's some advice for someone that wants to do this right:
 *
 *	1. Write a routine to cache all of the mount table entries, make
 *	   it smart enough to be callable multiple times without reading
 *	   the entries multiple times, and call it only when it is
 *	   needed.
 *
 *	2. Write a routine to get directory device numbers, and have it
 *	   cache the results in case the same directory is examined
 *	   more than once.
 *
 *	3. Try to add some kind of timeout to avoid waiting forever
 *	   due to a server being down.
 *
 *	4. Add host-up caching so that you don't stat the same machine
 *	   once for each filesystem if the host is down. This should go
 *	   in the routine getmntpt().
 */

#include <sys/param.h>
#include <ufs/fs.h>
#include <sys/vfs.h>
#include <sys/stat.h>

#include <stdio.h>
#include <mntent.h>
#include <errno.h>

char	*mpath();
int	iflag;
int	type;
char	*typestr;
int	type_local = 0;
int	type_nfs = 0;

struct mntent *getmntpt(), *mntdup();
void	usage(), pheader();

union {
	struct fs iu_fs;
	char dummy[SBSIZE];
} sb;
#define sblock sb.iu_fs

int
main(argc, argv)
	int argc;
	char **argv;
{
	int i;
	struct stat statb;
	char tmpname[1024];

	/*
	 * Skip over command name, if present.
	 */
	if (argc > 0) {
		argv++;
		argc--;
	}

	while (argc > 0 && (*argv)[0]=='-') {
		switch ((*argv)[1]) {

		case 'i':
			iflag++;
			break;

		case 't':
			type++;
			argv++;
			argc--;
			if (argc <= 0)
				usage();
			typestr = *argv;
			if (strcmp(typestr, "local") == 0)
				type_local = 1;
			else if (strcmp(typestr, MNTTYPE_43) == 0) 
				type_local  = 1; 
			else if (strcmp(typestr, "ffs") == 0) 
				type_local  = 1; 
			else if (strcmp(typestr, MNTTYPE_NFS) == 0)
				type_nfs = 1;
			break;

		default:
			usage();
		}
		argc--, argv++;
	}
	if (argc > 0 && type) {
		usage();
	}
	sync();
	if (argc <= 0) {
		register FILE *mtabp;
		register struct mntent *mnt;

		if ((mtabp = setmntent(MOUNTED, "r")) == NULL) {
			(void) fprintf(stderr, "df: ");
			perror(MOUNTED);
			exit(1);
		}
		pheader();
		while ((mnt = getmntent(mtabp)) != NULL) {
			if (strcmp(mnt->mnt_type, MNTTYPE_IGNORE) == 0 ||
			    strcmp(mnt->mnt_type, MNTTYPE_SWAP) == 0)
				continue;
			if (type) {
				if (!type_local &&
				    (strcmp(typestr, mnt->mnt_type) != 0) ) 
					continue;
			    	if (type_local && 
				    strcmp(mnt->mnt_type, MNTTYPE_NFS) == 0 ) 
					continue;
			}
			if ((type_local && 
       			     strcmp(mnt->mnt_type, MNTTYPE_NFS) != 0) &&
			    (stat(mnt->mnt_fsname, &statb) >= 0)  &&
			    (((statb.st_mode & S_IFMT) == S_IFBLK) ||
			     ((statb.st_mode & S_IFMT) == S_IFCHR)) ) { 
				(void) strcpy(tmpname, mnt->mnt_fsname);
				dfreedev(tmpname);
			} else {
				dfreemnt(mnt->mnt_dir, mnt);
			}
		}
		(void) endmntent(mtabp);
	} else {
		pheader();
		for (i = 0; i < argc; i++) {
			register struct mntent *mnt;

			if (stat(argv[i], &statb) < 0) {
				if (df_nfs_fsname(argv[i]) < 0) {
					(void) fprintf(stderr, "df: ");
					perror(argv[i]);
				}
				continue;
			} 

			if ((statb.st_mode & S_IFMT) == S_IFBLK ||
			    (statb.st_mode & S_IFMT) == S_IFCHR) {
				dfreedev(argv[i]);
			} else {
				if (((mnt = getmntpt(argv[i])) != NULL) &&
				    (!type || 
				     strcmp(typestr, mnt->mnt_type) == 0)) {
					dfreemnt(argv[i], mnt);
				}
			}
		}
	}
	exit(0);
	/*NOTREACHED*/
}

/* We couldn't stat the file with the name given on the
 * command line, so we check if the file name is an 
 * nfs file system name of the form host:pathname, and find the
 * directory on which the file name is mounted.  If name is not of
 * the form host:pathname we return (-1).
 *
 * If a directory name is found, attempt to do the df on it.
 *
 */
df_nfs_fsname(fsname) 
char *fsname;
{
	register FILE *mtabp;
	register struct mntent *mnt;
	register struct stat  	statb;
	register char *cp = fsname;

	while (cp && *cp && *cp != ':') cp++;
	if (cp == NULL || *cp == '\0') return(-1);

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
			dfreemnt(mnt->mnt_dir, mnt);
			(void) endmntent(mtabp);
			return(1);
		}
	}

	/* If we got here, we didn't find a match for name, so
	 * we couldn't do a stat.
	 */
	(void) endmntent(mtabp);
	return(-1);
}


void
pheader()
{
	if (iflag)
		(void) printf("Filesystem             iused   ifree  %%iused");
	else
		(void) printf("Filesystem            kbytes    used   avail capacity");
	(void) printf("  Mounted on\n");
}

dfreedev(file)
	char *file;
{
	long totalblks, availblks, avail, free, used;
	int fi;

	fi = open(file, 0);
	if (fi < 0) {
		(void) fprintf(stderr, "df: ");
		perror(file);
		return;
	}
	if (bread(file, fi, SBLOCK, (char *)&sblock, SBSIZE) == 0) {
		(void) close(fi);
		return;
	}
	(void) printf("%-20.20s", file);
	if (iflag) {
		int inodes = sblock.fs_ncg * sblock.fs_ipg;
		used = inodes - sblock.fs_cstotal.cs_nifree;
		(void) printf("%8ld%8ld%6.0f%% ", used, sblock.fs_cstotal.cs_nifree,
		    inodes == 0 ? 0.0 : (double)used / (double)inodes * 100.0);
	} else {
		totalblks = sblock.fs_dsize;
		free = sblock.fs_cstotal.cs_nbfree * sblock.fs_frag +
		    sblock.fs_cstotal.cs_nffree;
		used = totalblks - free;
		availblks = totalblks * (100 - sblock.fs_minfree) / 100;
		avail = availblks > used ? availblks - used : 0;
		(void) printf("%8d%8d%8d",
		    totalblks * sblock.fs_fsize / 1024,
		    used * sblock.fs_fsize / 1024,
		    avail * sblock.fs_fsize / 1024);
		(void) printf("%6.0f%%",
		    availblks==0? 0.0: (double)used/(double)availblks * 100.0);
		(void) printf("  ");
	}
	(void) printf("  %s\n", mpath(file));
	(void) close(fi);
}

dfreemnt(file, mnt)
	char *file;
	struct mntent *mnt;
{
	struct statfs fs;

	if (statfs(file, &fs) < 0) {
		perror(file);
		return;
	}

	if (strlen(mnt->mnt_fsname) > 20) {
		(void) printf("%s\n", mnt->mnt_fsname);
		(void) printf("                    ");
	} else {
		(void) printf("%-20.20s", mnt->mnt_fsname);
	}
	if (iflag) {
		long files, used;

		files = fs.f_files;
		used = files - fs.f_ffree;
		if (fs.f_files == -1 || fs.f_ffree == -1) {
			(void) printf("     N/A     N/A    N/A ");
		} else {
			(void) printf("%8ld%8ld%6.0f%% ", used, fs.f_ffree,
			    files == 0?0.0:(double)used/(double)files*100.0);
		}
	} else {
		long totalblks, avail, free, used, reserved;

		totalblks = fs.f_blocks;
		free = fs.f_bfree;
		used = totalblks - free;
		avail = fs.f_bavail;
		reserved = free - avail;
		if (avail < 0)
			avail = 0;
		(void) printf("%8d%8d%8d", totalblks * fs.f_bsize / 1024,
		    used * fs.f_bsize / 1024, avail * fs.f_bsize / 1024);
		totalblks -= reserved;
		(void) printf("%6.0f%%",
		    totalblks==0? 0.0: (double)used/(double)totalblks * 100.0);
		(void) printf("  ");
	}
	(void) printf("  %s\n", mnt->mnt_dir);
}

/*
 * Given a name like /usr/src/etc/foo.c returns the mntent
 * structure for the file system it lives in.
 */
struct mntent *
getmntpt(file)
	char *file;
{
	FILE *mntp;
	struct mntent *mnt;
	struct stat filestat, dirstat;

	if (stat(file, &filestat) < 0) {
		perror(file);
		return(NULL);
	}

	if ((mntp = setmntent(MOUNTED, "r")) == 0) {
		perror(MOUNTED);
		exit(1);
	}

	while ((mnt = getmntent(mntp)) != 0) {
		if (strcmp(mnt->mnt_type, MNTTYPE_IGNORE) == 0 ||
		    strcmp(mnt->mnt_type, MNTTYPE_SWAP) == 0)
			continue;
		if ((stat(mnt->mnt_dir, &dirstat) >= 0) &&
		   (filestat.st_dev == dirstat.st_dev)) {
			(void) endmntent(mntp);
			return mnt;
		}
	}
	(void) endmntent(mntp);
	(void) fprintf(stderr, "Couldn't find mount point for %s\n", file);
	exit(1);
	/*NOTREACHED*/
}

/*
 * Given a name like /dev/rrp0h, returns the mounted path, like /usr.
 */
char *
mpath(file)
	char *file;
{
	FILE *mntp;
	register struct mntent *mnt;

	if ((mntp = setmntent(MOUNTED, "r")) == 0) {
		(void) fprintf(stderr, "df: ");
		perror(MOUNTED);
		exit(1);
	}

	while ((mnt = getmntent(mntp)) != 0) {
		if (strcmp(file, mnt->mnt_fsname) == 0) {
			(void) endmntent(mntp);
			return (mnt->mnt_dir);
		}
	}
	(void) endmntent(mntp);
	return "";
}

long lseek();

int
bread(file, fi, bno, buf, cnt)
	char *file;
	int fi;
	daddr_t bno;
	char *buf;
	int cnt;
{
	register int n;
	extern int errno;

	(void) lseek(fi, (long)(bno * DEV_BSIZE), 0);
	if ((n = read(fi, buf, (unsigned) cnt)) < 0) {
		/* probably a dismounted disk if errno == EIO */
		if (errno != EIO) {
			(void) fprintf(stderr, "df: read error on ");
			perror(file);
			(void) fprintf(stderr, "bno = %ld\n", bno);
		} else {
			(void) fprintf(stderr, "df: premature EOF on %s\n",
			    file);
			(void) fprintf("bno = %ld expected = %d count = %d\n",
			    bno, cnt, n);
		}
		return (0);
	}
	return (1);
}

void
usage()
{
	(void) fprintf(stderr, "usage: df [ -i ] [-t type | file... ]\n");
	(void) fprintf(stderr, "\ttype is one of: ffs, ufs, 4.3, nfs, proc\n");
	exit(0);
}
