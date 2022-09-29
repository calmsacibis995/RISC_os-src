/* --------------------------------------------------- */
/* | Copyright (c) 1986 MIPS Computer Systems, Inc.  | */
/* | All Rights Reserved.                            | */
/* --------------------------------------------------- */
/* $Header: newfs.c,v 1.10.1.7.1.2 90/07/20 14:56:38 hawkes Exp $ */

/*
 * Copyright (c) 1983 Regents of the University of California.
 * All rights reserved.  The Berkeley software License Agreement
 * specifies the terms and conditions for redistribution.
 */
/* @(#)newfs.c	1.4 88/06/21 4.0NFSSRC; from	5.2 (Berkeley) 9/11/85" */

/*
 * newfs: friendly front end to mkfs
 */
#include <sys/param.h>
#include <sys/stat.h>
#include <ufs/fs.h>
#include <sys/dir.h>

#include <stdio.h>
#include <disktab.h>

#ifdef RISCOS
#include <sysv/sys.s>		/* making SysV system call */
#include <sysv/sys/dkio.h>
#define ustat(_rdev, _ustatarea) syscall(SYS_utssys, _rdev, _ustatarea)
#define DEFAULT_BLOCK_SIZE (8*1024)
#endif
#ifndef mips
#define	BOOTDIR	"/usr/mdec"	/* directory for boot blocks */
#endif

int	Nflag;			/* run mkfs without writing file system */
int	verbose;		/* show mkfs line before exec */
int	noboot;			/* do not fill boot blocks */
int	fssize;			/* file system size */
int	fsize;			/* fragment size */
int	bsize;			/* block size */
int	ntracks;		/* # tracks/cylinder */
int	nsectors;		/* # sectors/track */
int	sectorsize;		/* bytes/sector */
int	cpg;			/* cylinders/cylinder group */
int	minfree = -1;		/* free space threshold */
int	opt;			/* optimization preference (space or time) */
int	rpm;			/* revolutions/minute of drive */
int	density;		/* number of bytes per inode */

char	device[MAXPATHLEN];
char	cmd[BUFSIZ];

char	*index();
char	*rindex();
char	*sprintf();

main(argc, argv)
	int argc;
	char *argv[];
{
	char *cp, *special;
	register struct disktab *dp;
	register struct partition *pp;
	struct stat st;
	register int i;
	int status;
#ifdef RISCOS
	struct {
		daddr_t tfree;
		ino_t tinode;
		char fname[6];
		char fpack[6];
	} ustatarea;
#endif

	argc--, argv++;
	while (argc > 0 && argv[0][0] == '-') {
		for (cp = &argv[0][1]; *cp; cp++)
			switch (*cp) {

			case 'v':
				verbose++;
				break;

			case 'N':
				Nflag++;
				/* fall through to */

			case 'n':
				noboot++;
				break;

			case 's':
				if (argc < 1)
					fatal("-s: missing file system size");
				argc--, argv++;
				fssize = atoi(*argv);
				if (fssize < 0)
					fatal("%s: bad file system size",
						*argv);
				goto next;

			case 't':
				if (argc < 1)
					fatal("-t: missing track total");
				argc--, argv++;
				ntracks = atoi(*argv);
				if (ntracks < 0)
					fatal("%s: bad total tracks", *argv);
				goto next;

			case 'o':
				if (argc < 1)
					fatal("-o: missing optimization preference");
				argc--, argv++;
				if (strcmp(*argv, "space") == 0)
					opt = FS_OPTSPACE;
				else if (strcmp(*argv, "time") == 0)
					opt = FS_OPTTIME;
				else
					fatal("%s: bad optimization preference %s",
					    *argv,
					    "(options are `space' or `time')");
				goto next;

			case 'b':
				if (argc < 1)
					fatal("-b: missing block size");
				argc--, argv++;
				bsize = atoi(*argv);
				if (bsize < 0 || bsize < MINBSIZE)
					fatal("%s: bad block size", *argv);
				goto next;

			case 'f':
				if (argc < 1)
					fatal("-f: missing frag size");
				argc--, argv++;
				fsize = atoi(*argv);
				if (fsize < 0)
					fatal("%s: bad frag size", *argv);
				goto next;

			case 'S':
				if (argc < 1)
					fatal("-S: missing sector size");
				argc--, argv++;
				sectorsize = atoi(*argv);
				if (sectorsize < 0)
					fatal("%s: bad sector size", *argv);
				goto next;

			case 'c':
				if (argc < 1)
					fatal("-c: missing cylinders/group");
				argc--, argv++;
				cpg = atoi(*argv);
				if (cpg < 0)
					fatal("%s: bad cylinders/group", *argv);
				goto next;

			case 'm':
				if (argc < 1)
					fatal("-m: missing free space %%\n");
				argc--, argv++;
				minfree = atoi(*argv);
				if (minfree < 0 || minfree > 99)
					fatal("%s: bad free space %%\n",
						*argv);
				goto next;

			case 'r':
				if (argc < 1)
					fatal("-r: missing revs/minute\n");
				argc--, argv++;
				rpm = atoi(*argv);
				if (rpm < 0)
					fatal("%s: bad revs/minute\n", *argv);
				goto next;

			case 'i':
				if (argc < 1)
					fatal("-i: missing bytes per inode\n");
				argc--, argv++;
				density = atoi(*argv);
				if (density < 0)
					fatal("%s: bad bytes per inode\n",
						*argv);
				goto next;

			default:
				fatal("-%c: unknown flag", cp);
			}
next:
		argc--, argv++;
	}
	if (argc < 2) {
		fprintf(stderr, "usage: newfs.ffs [ -v ] [ mkfs-options ] %s\n",
			"special-device device-type");
		fprintf(stderr, "where mkfs-options are:\n");
		fprintf(stderr, "\t-N do not create file system, %s\n",
			"just print out parameters");
		fprintf(stderr, "\t-s file system size (sectors)\n");
		fprintf(stderr, "\t-b block size\n");
		fprintf(stderr, "\t-f frag size\n");
		fprintf(stderr, "\t-t tracks/cylinder\n");
		fprintf(stderr, "\t-c cylinders/group\n");
		fprintf(stderr, "\t-m minimum free space %%\n");
		fprintf(stderr, "\t-o optimization preference %s\n",
			"(`space' or `time')");
		fprintf(stderr, "\t-r revolutions/minute\n");
		fprintf(stderr, "\t-S sector size\n");
		fprintf(stderr, "\t-i number of bytes per inode\n");
		exit(1);
	}
	special = argv[0];
#ifndef RISCOS
	/*
	 * Sys V doesn't follow the berkeley disk naming conventions.
	 * Make user specify the character device.
	 */
	cp = rindex(special, '/');
	if (cp != 0)
		special = cp + 1;
	if (*special == 'r' && special[1] != 'a' && special[1] != 'b')
		special++;
	special = sprintf(device, "/dev/r%s", special);
#endif
	if (stat(special, &st) < 0) {
		fprintf(stderr, "newfs.ffs: "); perror(special);
		exit(2);
	}
#ifdef RISCOS
	if((st.st_mode & S_IFMT) == S_IFBLK)
		if(ustat(st.st_rdev,&ustatarea) >= 0) {
			printf("*** MOUNTED FILE SYSTEM\n");
			exit(1);
		}
#else
	/* We no longer need the below check because our closing of
	 * block devices is guaranteed to flush all blocks to disk before
	 * they are invalidated, unlike other unixes.
	 */
	if ((st.st_mode & S_IFMT) != S_IFCHR)
		fatal("%s: not a character device", special);
#endif
	dp = getdiskbyname(argv[1]);
	if (dp == 0)
		fatal("%s: unknown disk type", argv[1]);
	cp = index(argv[0], '\0') - 1;
#ifdef RISCOS
	/* this code is INSERTED here to get the volume header */
	/* I did it this way so it is ISOLATED */
	if (fssize <= 0) {
		int fd,atoi();
		int part;
		struct stat	sb;
		struct volume_header vh;
		struct io_arg	ia;
		char *ptr, *buf, *calloc();
		buf = calloc(1,strlen(special)+12);	/* add slop */
		if (strncmp("/dev/r",special,6) != 0) {
			if (strncmp(special,"/dev/",5) == 0) {
				(void) strcpy(buf,"/dev/r");
				(void) strcat(buf,&special[5]);
			}
		}
		else {
			(void) strcpy(buf,special);
		}
		ptr = rindex(buf,'/');
		if (ptr == NULL)	/* no '/' */
			goto out;
		ptr++;ptr++;ptr++;
		if (*ptr != 'c')	/* no 'c' */
			goto out;
		ptr++;
		while (*ptr && *ptr >= '0' && *ptr <= '9') ptr++;
		if (*ptr != 'd')	/* no 'd' */
			goto out;
		ptr++;
		while (*ptr && *ptr >= '0' && *ptr <= '9') ptr++;
		if (*ptr != 's')	/* no 's' */
			goto out;
		ptr++;
		part = atoi(ptr);
		if (part < 0 || part > 15)
			goto out;
		if (stat(buf, &sb) < 0)
			fatal("%s: cannot stat `%s'", argv[1], buf);
		if ((fd = open(buf, 0)) < 0) 
			fatal("%s: cannot open `%s'", argv[1], buf);
	
		if (ioctl (fd, DIOCGETSIZE, &ia) >= 0) {
			close (fd);
			fssize = ia.retval;
		} else {
			ia.retval = ia.sectst = 0;
			ia.memaddr = (unsigned long)&vh;
			ia.datasz = sizeof(vh);
			if (ioctl(fd, DIOCGETVH, &ia) < 0) {
				(void) close(fd);
				fatal("%s: cannot get volume header on `%s'", argv[1], buf);
			}
			(void) close(fd);
			if (vh.vh_magic != VHMAGIC) {
				fatal("%s: bad volume header on `%s'", argv[1], buf);
				exit(1);
			}
			fssize = vh.vh_pt[part].pt_nblks;
		}
out:
		part++;		/* placeholder for the goto's above */
	}
	if (fssize <= 0) {
		fatal("%s: no default size for `%c' partition", argv[1], *cp);
	}
#else
	if (cp == 0 || *cp < 'a' || *cp > 'h')
		fatal("%s: can't figure out file system partition", argv[0]);
	pp = &dp->d_partitions[*cp - 'a'];
	if (fssize == 0) {
		fssize = pp->p_size;
		if (fssize < 0)
			fatal("%s: no default size for `%c' partition",
				argv[1], *cp);
	}
#endif
	if (nsectors == 0) {
		nsectors = dp->d_nsectors;
		if (nsectors < 0)
			fatal("%s: no default #sectors/track", argv[1]);
	}
	if (ntracks == 0) {
		ntracks = dp->d_ntracks;
		if (ntracks < 0)
			fatal("%s: no default #tracks", argv[1]);
	}
	if (sectorsize == 0) {
		sectorsize = dp->d_secsize;
		if (sectorsize < 0)
			fatal("%s: no default sector size", argv[1]);
	}
#ifdef RISCOS
	/* ~~
	 * Default partitions again ...
	 * I'll just make default be 8K/1K fs.
	 */
	if (bsize == 0) {
		bsize = DEFAULT_BLOCK_SIZE;
	}
	if (fsize == 0) {
		fsize = (1024);
	}
#else /* !RISCOS */
	if (bsize == 0) {
		bsize = pp->p_bsize;
		if (bsize < 0)
			fatal("%s: no default block size for `%c' partition",
				argv[1], *cp);
	}
	if (fsize == 0) {
		fsize = pp->p_fsize;
		if (fsize < 0)
			fatal("%s: no default frag size for `%c' partition",
				argv[1], *cp);
	}
#endif /* RISCOS */
	if (rpm == 0) {
		rpm = dp->d_rpm;
		if (rpm < 0)
			fatal("%s: no default revolutions/minute value",
				argv[1]);
	}
	/* XXX - following defaults are both here and in mkfs */
	if (density <= 0)
		density = 2048;
	if (minfree < 0)
		minfree = 10;
	if (minfree < 10 && opt != FS_OPTSPACE) {
		fprintf(stderr, "setting optimization for space ");
		fprintf(stderr, "with minfree less than 10%\n");
		opt = FS_OPTSPACE;
	}
	if (cpg == 0)
		cpg = 16;
#ifdef RISCOS
	/*
	 * Round the partition size down to to an integral number of
	 * filesystem blocks.
	 */
	fssize = (fssize / (bsize / DEV_BSIZE)) * (bsize / DEV_BSIZE);
#endif
	sprintf(cmd, "/etc/mkfs.ffs %s%s %d %d %d %d %d %d %d %d %d %s",
		Nflag ? "-N " : "", special,
		fssize, nsectors, ntracks, bsize, fsize, cpg, minfree, rpm/60,
		density, opt == FS_OPTSPACE ? "s" : "t");
	if (verbose)
		printf("%s\n", cmd);
	if (status = system(cmd))
		exit(status >> 8);
	if (Nflag)
		exit(0);
#ifdef RISCOS
	sprintf(cmd, "/etc/fsirand.ffs %s", special);
#else
	sprintf(cmd, "fsirand %s", special);
#endif
	if (status = system(cmd))
		printf("%s: failed, status = %d\n", cmd, status);
#ifdef RISCOS
	sprintf(cmd, "/etc/tunefs.ffs -d 0 %s\n", special);
	if (status = system(cmd))
		printf("%s: failed, status = %d\n", cmd, status);
#endif RISCOS
#ifndef mips
	if (*cp == 'a' && !noboot) {
		char type[3];
		struct stat sb;

		cp = rindex(special, '/');
		if (cp == NULL)
			fatal("%s: can't figure out disk type from name",
				special);
		if (stat(special, &sb) >= 0 && (sb.st_mode & S_IFMT) == S_IFCHR)
			cp++;
		type[0] = *++cp;
		type[1] = *++cp;
		type[2] = '\0';
		installboot(special, type);
	}
#endif /* mips */
	exit(0);
}


#ifndef mips
installboot(dev, type)
	char *dev, *type;
{
	int fd;
	char bootblock[MAXPATHLEN], standalonecode[MAXPATHLEN];
	char bootimage[BBSIZE];

	sprintf(bootblock, "%s/%sboot", BOOTDIR, type);
	sprintf(standalonecode, "%s/boot%s", BOOTDIR, type);
	if (verbose) {
		printf("installing boot code\n");
		printf("sector 0 boot = %s\n", bootblock);
		printf("1st level boot = %s\n", standalonecode);
	}
	fd = open(bootblock, 0);
	if (fd < 0) {
		fprintf(stderr, "newfs.ffs: "); perror(bootblock);
		exit(1);
	}
	if (read(fd, bootimage, DEV_BSIZE) < 0) {
		fprintf(stderr, "newfs.ffs: "); perror(bootblock);
		exit(2);
	}
	close(fd);
	fd = open(standalonecode, 0);
	if (fd < 0) {
		fprintf(stderr, "newfs.ffs: "); perror(standalonecode);
		exit(1);
	}
	if (read(fd, &bootimage[DEV_BSIZE], BBSIZE - DEV_BSIZE) < 0) {
		fprintf(stderr, "newfs.ffs: "); perror(standalonecode);
		exit(2);
	}
	close(fd);
	fd = open(dev, 1);
	if (fd < 0) {
		fprintf(stderr, "newfs.ffs: "); perror(dev);
		exit(1);
	}
	if (write(fd, bootimage, BBSIZE) != BBSIZE) {
		fprintf(stderr, "newfs.ffs: "); perror(dev);
		exit(2);
	}
	close(fd);
}
#endif

/*VARARGS*/
fatal(fmt, arg1, arg2)
	char *fmt;
{

	fprintf(stderr, "newfs.ffs: ");
	fprintf(stderr, fmt, arg1, arg2);
	putc('\n', stderr);
	exit(10);
}
