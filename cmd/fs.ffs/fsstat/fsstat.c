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
#ident	"$Header: fsstat.c,v 1.3.2.2 90/05/09 15:56:25 wje Exp $"

#   include <stdio.h>
#   include <sys/types.h>
#   include <fcntl.h>
#   include <sys/stat.h>
#   include <ustat.h>

#   include <sys/fs/bfs_yech.h>
#   include <sys/fs/bfs_param.h>
#   include <sys/fs/bfs_fs.h>

/*
 * For regular file system: 
 * 	exit 0 - file system is unmounted and okay
 * 	exit 1 - file system is unmounted and needs checking
 * 	exit 2 - file system is mounted
 * 
 * For root file system:
 * 	exit 0 - mounted and okay
 * 	exit 1 - mounted and needs checking
 *
 * exit 3 - unexpected failures
 */

char fsbuf[BFS_SBSIZE];
extern int errno;

main(argc, argv)
int	argc;
char	*argv[];
{
	register dev;
	register char *fp;
	struct fs *fs;
	struct stat stbd, stbr;
	struct ustat usb;

	/*
	 * Argument check.
	 */
	if (argc != 2) {
		fprintf(stderr, "usage: fsstat special\n");
		exit(3);
	}
	fp = argv[1];

	/*
	 * Open disk device.
	 */
	if ((dev = open(fp, O_RDONLY)) < 0) {
		fprintf(stderr, "fsstat.ffs: cannot open %s\n", fp);
		exit(3);
	}
	fstat(dev, &stbd);
	if ((stbd.st_mode&S_IFMT) != S_IFBLK) {
		fprintf(stderr, "fsstat.ffs: %s not a block device\n", fp);
		exit(3);
	}

	/*
	 * Read the super-block (struct fs).
	 */
	if (lseek(dev, BFS_SBLOCK * BFS_DEV_BSIZE, 0) == -1) {
		fprintf(stderr, "fsstat.ffs: seek to superblock failed\n");
		exit(1);
	}
	fs = (struct fs *) fsbuf;
	if (read(dev, (char *) fs, BFS_SBSIZE) != BFS_SBSIZE) {
		fprintf(stderr, 
			"fsstat.ffs: read of superblock failed %d\n", errno);
		exit(3);
	}
	if (fs->fs_magic != BFS_FS_MAGIC) {
		fprintf(stderr, "fsstat.ffs: %s not a ffs file system.\n", fp);
		exit(3);
	}

	/*
	 * Handle case where dev is root file system.
	 */
	stat("/", &stbr);
	if (stbr.st_dev == stbd.st_rdev) {
		if ( fs->fs_clean & BFS_CLEAN ) {
			/* Mounted and okay. */
			fprintf(stderr, "fsstat: root file system okay\n");
			exit(0);
		} else {
			/* Mounted and NOT okay. */
			fprintf(stderr, 
				"fsstat: root file system needs checking\n");
			exit(1);
		}
	}

	/*
	 * Handle case where dev is NOT root file system.
	 */
	if (ustat(stbd.st_rdev, &usb) == 0) {
		fprintf(stderr, "fsstat: %s mounted\n", fp);
		exit(2);
	}
	if ( (fs->fs_clean&BFS_CLEAN) && !(fs->fs_clean&BFS_MOUNT) ) {
		fprintf(stderr, "fsstat: %s okay\n", fp);
		exit(0);
	}
	else {
		fprintf(stderr, "fsstat: %s needs checking\n", fp);
		exit(1);
	}
}
