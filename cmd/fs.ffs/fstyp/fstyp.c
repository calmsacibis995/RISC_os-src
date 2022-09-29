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
#ident	"$Header: fstyp.c,v 1.5.1.2 90/05/09 15:57:09 wje Exp $"


#include <stdio.h>
#include <fcntl.h>
#undef KERNEL
#define KERNEL 1
#include <sys/types.h>
#undef KERNEL
#include <sys/stat.h>
#include <sys/fsid.h>
#define KERNEL 1
#include <sys/param.h>
#undef KERNEL
#include <sys/buf.h>
#include <bsd/sys/time.h>

#define KERNEL 1
#include <sys/vfs.h>
#undef KERNEL
#include <sys/vnode.h>
#include <sys/stream.h>
#undef free
#undef malloc
#undef calloc
#define KERNEL 1
#include <sys/fs/ufs_inode.h>
#undef KERNEL
#include <sys/fs/ufs_fsdir.h>
#undef itoo
#undef itod
#undef INOPB
#include <sys/fs/ufs_fs.h>
#include <sys/fs/ufs_mount.h>

extern	void	exit();
extern	long	lseek();

main(argc,argv)
	int argc;
	char *argv[];
{

	int fd;
	char *dev;
	struct stat buf;
	union {
		char	fssb_sblock[SBSIZE];
		struct fs fssb_fs;
	} fssb;

	if (argc != 2) {
		fprintf(stderr, "Usage: fstyp.ffs special\n");
		exit(1);
	}

	dev = argv[1];
	if (stat(dev, &buf) < 0) {
		fprintf(stderr, "fstyp.ffs: cannot stat <%s>\n", dev);
		exit(1);
	}

	if (((buf.st_mode & S_IFMT) != S_IFBLK) &&
	    ((buf.st_mode & S_IFMT) != S_IFCHR)) {
		fprintf(stderr,
		    "fstyp.ffs: <%s> is not a block, or a character device\n",
		    dev);
		exit(1);
	}

	/* read the super block */
	if ((fd = open(dev, O_RDONLY)) < 0) {
		fprintf(stderr, "fstyp.ffs: cannot open <%s>\n", dev);
		exit(1);
	}
	if ((lseek(fd, (long)(SBLOCK*DEV_BSIZE), 0) < 0) ||
	    (read(fd, &fssb.fssb_fs, (sizeof fssb)) != (sizeof fssb))) {
		fprintf(stderr, "fstyp.ffs: cannot read superblock\n");
		close(fd);
		exit(1);
	}
	close(fd);
	
	/*
	 * Does this look like a berkeley files system?
	 */
	if (fssb.fssb_fs.fs_magic != FS_MAGIC)
		badsb("Bad magic number");
	if (fssb.fssb_fs.fs_ncg < 1)
		badsb("NCG out of range");
	if (fssb.fssb_fs.fs_cpg < 1 || fssb.fssb_fs.fs_cpg > MAXCPG)
		badsb("CPG out of range");
	if (fssb.fssb_fs.fs_sbsize > SBSIZE)
		badsb("Size preposterously large"); 

	printf("%s\n", FSID_BFS);
	exit(0);
}

badsb(msg)
char *msg;
{
	fprintf(stderr, "fstyp.ffs: %s.\n", msg);
	exit(1);
}
