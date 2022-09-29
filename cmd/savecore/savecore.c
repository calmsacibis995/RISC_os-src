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
#ident	"$Header: savecore.c,v 1.10.2.4 90/05/09 18:38:17 wje Exp $"

/*
 * Copyright (c) 1980,1986 Regents of the University of California.
 * All rights reserved.  The Berkeley software License Agreement
 * specifies the terms and conditions for redistribution.
 */

/*
 * savecore
 */

#include <stdio.h>
#include <nlist.h>
#include <bsd/sys/param.h>
#include <dirent.h>
#include <sys/stat.h>
#include <sys/statfs.h>
#include <bsd/sys/time.h>
#include <bsd/sys/file.h>
#include <sys/immu.h>
#include <sys/sysmacros.h>
#include <bsd/syslog.h>
#include <sys/dump.h>
#include <sys/errno.h>

#define	DAY	(60L*60L*24L)
#define	LEEWAY	(3*DAY)

#define eq(a,b) 	(!strcmp(a,b))
#define max(a,b)	((a) > (b) ? (a) : (b))

#if defined(vax) || defined(mips)
#define ok(number) ((number)&0x7fffffff)
#else
#define ok(number) (number)
#endif

struct nlist current_nl[] = {	/* namelist for currently running system */
#define X_DUMPDEV	0
	{ "dumpdev" },
#define X_DUMPLO	1
	{ "dumplo" },
#define X_TIME		2
	{ "time" },
#define	X_DUMPSIZE	3
	{ "dumpsize" },
#define X_VERSION	4
	{ "id_string" },
#define X_PANICSTR	5
	{ "panicstr" },
#define	X_DUMPMAG	6
	{ "dumpmag" },
#define	X_KERNEL_MAGIC	7
	{ "kernel_magic" },
#define	X_END		8
	{ "end" },
	{ "" },
};

struct nlist dump_nl[] = {	/* name list for dumped system */
	{ "dumpdev" },		/* entries MUST be the same as */
	{ "dumplo" },		/*	those in current_nl[]  */
	{ "time" },
	{ "dumpsize" },
	{ "id_string" },
	{ "panicstr" },
	{ "dumpmag" },
	{ "kernel_magic" },
	{ "end" },
	{ "" },
};

typedef union dumphdr_max {
	struct dumphdr hdr;		/* header used when ordered dump */
					/* was taken by kernel */
	char fill[16384];		/* reserve at least Mips2 pagesize */
} hdr_t;
hdr_t	dumphdr;
int	ordered=0;			/* an ordered dump was taken */

char	*systemfile;
char	*dirname;			/* directory to save dumps in */
char	*ddname;			/* name of dump device */
char	*find_dev();
dev_t	dumpdev;			/* dump device */
time_t	dumptime;			/* time the dump was taken */
int	dumplo;				/* where dump starts on dumpdev */
unsigned int	dumpsize;		/* amount of memory dumped */
int	dumpmag;			/* magic number in dump */
time_t	now;				/* current date */
char	*path();
char	*malloc();
char	*ctime();
char	vers[80];
char	core_vers[80];
char	panic_mesg[80];
int	panicstr;
int	getpagesize();
int	pagesize;
off_t	lseek();
off_t	Lseek();
int	Verbose;
extern	int errno;

main(argc, argv)
	char **argv;
	int argc;
{
	char *cp;

	pagesize = getpagesize();

	argc--, argv++;
	while (argc > 0 && argv[0][0] == '-') {
		for (cp = &argv[0][1]; *cp; cp++) switch (*cp) {

		case 'v':
			Verbose++;
			break;

		default:
		usage:
			fprintf(stderr,
			    "usage: savecore [-v] dirname [ system ]\n");
			exit(1);
		}
		argc--, argv++;
	}
	if (argc != 1 && argc != 2)
		goto usage;
	dirname = argv[0];
	if (argc == 2)
		systemfile = argv[1];
	openlog("savecore", LOG_ODELAY, LOG_AUTH);
	if (access(dirname, W_OK) < 0) {
		int oerrno = errno;

		perror(dirname);
		errno = oerrno;
		syslog(LOG_ERR, "%s: %m", dirname);
		exit(1);
	}
	read_kmem();
	if (!dump_exists()) {
		if (Verbose)
			fprintf(stderr, "savecore: No dump exists.\n");
		exit(0);
	}
	(void) time(&now);
	if ( !ordered ) {
		check_kmem();
		if (panicstr)
			syslog(LOG_CRIT, "reboot after panic: %s", panic_mesg);
		else
			syslog(LOG_CRIT, "reboot");
		if (!check_space())
			exit(1);
	}
	if (!get_crashtime())
		exit(1);
	save_core();
	clear_dump();
	exit(0);
}

dump_exists()
{
	register int dumpfd;
	int word;

	dumpfd = Open(ddname, O_RDONLY);
	Lseek(dumpfd, (off_t)(dumplo + ok(dump_nl[X_DUMPMAG].n_value)), L_SET);
	Read(dumpfd, (char *)&word, sizeof (word));
	if (word != dumpmag) {
		Lseek(dumpfd, (off_t)dumplo, L_SET);
		Read(dumpfd, (char *)&dumphdr, pagesize);
		if ( eq( dumphdr.hdr.magic, "ordered" ) ) {
			ordered = 1;
			dumptime = dumphdr.hdr.timestamp;
		}
	} 
	if ( Verbose && (word != dumpmag) && !ordered ) {
		printf("dumplo = %d (%d bytes)\n", dumplo/NBPSCTR, dumplo);
		printf("magic number mismatch: %x != %x\n", word, dumpmag);
	}
	close(dumpfd);
	return ((word == dumpmag) || ordered);
}

clear_dump()
{
	register int dumpfd;
	int zero = 0;

	dumpfd = Open(ddname, O_WRONLY);
	if ( ordered ) {
		Lseek(dumpfd, (off_t)dumplo, L_SET);	/* zero magic string */
	} else {
 		Lseek(dumpfd, (off_t)(dumplo + ok(dump_nl[X_DUMPMAG].n_value)),
 			L_SET);				/* zero magic number */
	}
	Write(dumpfd, (char *)&zero, sizeof (zero));
	close(dumpfd);
}

char *
find_dev(dev, type)
	register dev_t dev;
	register int type;
{
	register DIR *dfd = opendir("/dev");
	struct dirent *dir;
	struct stat statb;
	static char devname[MAXPATHLEN + 1];
	char *dp;

	strcpy(devname, "/dev/");
	while ((dir = readdir(dfd))) {
		strcpy(devname + 5, dir->d_name);
		if (stat(devname, &statb)) {
			perror(devname);
			continue;
		}
		if ((statb.st_mode&S_IFMT) != type)
			continue;
		if (dev == statb.st_rdev) {
			closedir(dfd);
			dp = malloc(strlen(devname)+1);
			strcpy(dp, devname);
			return (dp);
		}
	}
	closedir(dfd);
	fprintf(stderr, "Can't find device %d/%d\n", major(dev), minor(dev));
	syslog(LOG_ERR, "Can't find device %d/%d", major(dev), minor(dev));
	exit(1);
	/*NOTREACHED*/
}

int	cursyms[] =
    { X_DUMPDEV, X_DUMPLO, X_VERSION, X_DUMPMAG, -1 };
int	dumpsyms[] =
    { X_TIME, X_DUMPSIZE, X_VERSION, X_PANICSTR, X_DUMPMAG, -1 };
read_kmem()
{
	register char *cp;
	FILE *fp;
	char *dump_sys;
	int kmem, i;
	long kernel_magic;
	
	dump_sys = systemfile ? systemfile : "/unix";
	nlist("/unix", current_nl);
	nlist(dump_sys, dump_nl);
	/*
	 * Some names we need for the currently running system,
	 * others for the system that was running when the dump was made.
	 * The values obtained from the current system are used
	 * to look for things in /dev/kmem that cannot be found
	 * in the dump_sys namelist, but are presumed to be the same
	 * (since the disk partitions are probably the same!)
	 */
	for (i = 0; cursyms[i] != -1; i++)
		if (current_nl[cursyms[i]].n_value == 0) {
			fprintf(stderr, "/unix: %s not in namelist\n",
			    current_nl[cursyms[i]].n_name);
			syslog(LOG_ERR, "/unix: %s not in namelist",
			    current_nl[cursyms[i]].n_name);
			exit(1);
		}
	for (i = 0; dumpsyms[i] != -1; i++)
		if (dump_nl[dumpsyms[i]].n_value == 0) {
			fprintf(stderr, "%s: %s not in namelist\n", dump_sys,
			    dump_nl[dumpsyms[i]].n_name);
			syslog(LOG_ERR, "%s: %s not in namelist", dump_sys,
			    dump_nl[dumpsyms[i]].n_name);
			exit(1);
		}
	kmem = Open("/dev/kmem", O_RDONLY);
	Lseek(kmem, (long)current_nl[X_DUMPDEV].n_value, L_SET);
	Read(kmem, (char *)&dumpdev, sizeof (dumpdev));
	Lseek(kmem, (long)current_nl[X_DUMPLO].n_value, L_SET);
	Read(kmem, (char *)&dumplo, sizeof (dumplo));
	Lseek(kmem, (long)current_nl[X_DUMPMAG].n_value, L_SET);
	Read(kmem, (char *)&dumpmag, sizeof (dumpmag));

	/*
	 * Check that /unix and /dev/kmem match
	 */
	if (current_nl[X_KERNEL_MAGIC].n_value) {
		Lseek(kmem, (long)current_nl[X_KERNEL_MAGIC].n_value, L_SET);
		Read(kmem, (char *)&kernel_magic, sizeof(kernel_magic));
		if (kernel_magic != current_nl[X_END].n_value) {
			fprintf(stderr, "savecore: /unix doesn't match /dev/kmem\n");
			syslog(LOG_ERR, "savecore: /unix doesn't match /dev/kmem");
			exit(1);
		}
	}

	dumplo *= NBPSCTR;
	ddname = find_dev(dumpdev, S_IFBLK);
	fp = fdopen(kmem, "r");
	if (fp == NULL) {
		syslog(LOG_ERR, "Couldn't fdopen kmem");
		exit(1);
	}
	if (systemfile)
		return;
	if (current_nl[X_VERSION].n_value != 0) {
		fseek(fp, (long)current_nl[X_VERSION].n_value, L_SET);
		fgets(vers, sizeof (vers), fp);
	} else {
		vers[0] = '\0';
	}
	fclose(fp);
}

check_kmem()
{
	FILE *fp;
	register char *cp;

	fp = fopen(ddname, "r");
	if (fp == NULL) {
		int oerrno = errno;

		perror(ddname);
		errno = oerrno;
		syslog(LOG_ERR, "%s: %m", ddname);
		exit(1);
	}
	if (dump_nl[X_VERSION].n_value != 0) {
		fseek(fp, (off_t)(dumplo+ok(dump_nl[X_VERSION].n_value)),
			L_SET);
		fgets(core_vers, sizeof (core_vers), fp);
	} else {
		core_vers[0] = '\0';
	}
	fclose(fp);
	if (!eq(vers, core_vers) && systemfile == 0) {
		fprintf(stderr,
		   "Warning: unix version mismatch:\n\t%sand\n\t%s",
		   vers, core_vers);
		syslog(LOG_WARNING,
		   "Warning: unix version mismatch: %s and %s",
		   vers, core_vers);
	}
	fp = fopen(ddname, "r");
	fseek(fp, (off_t)(dumplo + ok(dump_nl[X_PANICSTR].n_value)), L_SET);
	fread((char *)&panicstr, sizeof (panicstr), 1, fp);
	if (panicstr) {
		fseek(fp, dumplo + ok(panicstr), L_SET);
		cp = panic_mesg;
		do
			*cp = getc(fp);
		while (*cp++);
	}
	fclose(fp);
}

get_crashtime()
{
	int dumpfd;

	if ( ordered ) {
		dumptime = dumphdr.hdr.timestamp;
	} else {
		dumpfd = Open(ddname, O_RDONLY);
		Lseek(dumpfd,(off_t)(dumplo+ok(dump_nl[X_TIME].n_value)),L_SET);
		Read(dumpfd, (char *)&dumptime, sizeof dumptime);
		close(dumpfd);
	}
	if (dumptime == 0) {
		if (Verbose)
			printf("Dump time not found.\n");
		return (0);
	}
	printf("System went down at %s", ctime(&dumptime));
	syslog(LOG_NOTICE, "System went down at %s", ctime(&dumptime));
	if (dumptime < now - LEEWAY || dumptime > now + LEEWAY) {
		printf("dump time is unreasonable\n");
		return (0);
	}
	return (1);
}

char *
path(file)
	char *file;
{
	register char *cp = malloc(strlen(file) + strlen(dirname) + 2);

	(void) strcpy(cp, dirname);
	(void) strcat(cp, "/");
	(void) strcat(cp, file);
	return (cp);
}

check_space()
{
	struct stat dsb;
	register char *ddev;
	int dfd;
	int spacefree;	/* number of kbytes free */
	int minfree;	/* minimum number of kbytes free */
	struct statfs fs;

	if (stat(dirname, &dsb) < 0) {
		int oerrno = errno;

		perror(dirname);
		errno = oerrno;
		syslog(LOG_ERR, "%s: %m", dirname);
		exit(1);
	}
	statfs(dirname, &fs, sizeof(fs), 0);
	spacefree = fs.f_bfree / 2;
	minfree = read_number("minfree");
	if (Verbose) {
		printf("%d kbytes free, %d must be free for dump\n", spacefree,
			minfree);
	}
 	if (spacefree < minfree) {
		printf("Dump omitted, not enough space on device\n");
		syslog(LOG_WARNING, "Dump omitted, not enough space on device");
		return (0);
	}
	return (1);
}

read_number(fn)
	char *fn;
{
	char lin[80];
	register FILE *fp;

	fp = fopen(path(fn), "r");
	if (fp == NULL) {
		return (0);
	}
	if (fgets(lin, 80, fp) == NULL) {
		fclose(fp);
		return (0);
	}
	fclose(fp);
	return (atoi(lin));
}

save_core()
{
	register int n;
	register char *cp;
	register int ifd, ofd, bounds;
	register FILE *fp;

	cp = malloc(BUFSIZ);
	if (cp == 0) {
		fprintf(stderr, "savecore: Can't allocate i/o buffer.\n");
		return;
	}

	bounds = read_number("bounds");
	ifd = Open(systemfile?systemfile:"/unix", O_RDONLY);
	sprintf(cp, "unix.%d", bounds);
	ofd = Create(path(cp), 0644);
	while((n = Read(ifd, cp, BUFSIZ)) > 0)
		Write(ofd, cp, n);
	close(ifd);
	close(ofd);

	if ( ordered ) {
		ordered_save( bounds );
	} else {
		unordered_save( bounds );
	}
	fp = fopen(path("bounds"), "w");
	fprintf(fp, "%d\n", bounds+1);
	fclose(fp);
	free(cp);
}

ordered_save(bounds)
register int bounds;
{
	register char *cp;
	register int ifd, ofd;
	register int i, n;

	cp = malloc(pagesize);
	if (cp == 0) {
		fprintf(stderr, "savecore: Can't allocate i/o buffer.\n");
		return;
	}
	ifd = Open(ddname, O_RDONLY);
	sprintf(cp, "core.%d", bounds);
	ofd = Create(path(cp), 0644);
	Lseek(ifd, (off_t)dumplo, L_SET);
	dumpsize = 0;
	while (1) {
		/* 
		 * read dumphdr.  If return from read is less than
		 * the size of a dumphdr, or an error was returned, then
	 	 * we are probably at the end of the swap partition and
		 * cannot read an 8K block.  There is no way to tell for
		 * sure when we are done, so assume that if error is not
		 * EIO, then it is truly and error, otherwise we are done.
		 *
		 * We assume that the dumphdr is always one page in length,
		 * and that is the size of the native page on this machine.
		 * The dumphdr.page[] array is declared to have a number of
		 * elements computed from "NBPP", which is the Mips1 4KB page
		 * size.
		 */
		if ((n=read(ifd, &dumphdr, pagesize)) < pagesize){
			if ( (n < 0) && errno != EIO ) {
				int oerrno = errno;

				perror("read");
				errno = oerrno;
				syslog(LOG_ERR, "read: %m");
				exit(1);
			} else {
				break;
			}
		}
		if ( (strncmp( dumphdr.hdr.magic, "ordered", 8 ) == 0 ) &&
		     (dumphdr.hdr.timestamp == dumptime) ) {
			for ( i = 0 ; i < dumphdr.hdr.numpages ; i++ ) {
				if (Read(ifd, cp, pagesize) < pagesize) {
					syslog(LOG_WARNING,
					  "WARNING: core may be incomplete");
					printf(
					  "WARNING: core may be incomplete\n");
					break;
				}
				dumpsize = max( dumpsize, dumphdr.hdr.page[i] );
				Lseek(ofd, (off_t) dumphdr.hdr.page[i], L_SET);
				Write(ofd, cp, pagesize);
			}
		} else {
			break;
		}
	} 
	printf("Ordered: Saved %d bytes of image in core.%d\n", dumpsize,
		bounds);
	syslog(LOG_NOTICE, "Ordered: Saved %d bytes of image in core.%d",
		dumpsize, bounds);
	close(ifd);
	close(ofd);
	free(cp);
}

#define	BUFPAGES	(256*1024/pagesize)

unordered_save(bounds)
register int bounds;
{
	register int n;
	register char *cp;
	register int ifd, ofd;

	cp = malloc(BUFPAGES*pagesize);
	if (cp == 0) {
		fprintf(stderr, "savecore: Can't allocate i/o buffer.\n");
		return;
	}
	ifd = Open(ddname, O_RDONLY);
	Lseek(ifd, (off_t)(dumplo + ok(dump_nl[X_DUMPSIZE].n_value)), L_SET);
	Read(ifd, (char *)&dumpsize, sizeof (dumpsize));
	sprintf(cp, "core.%d", bounds);
	ofd = Create(path(cp), 0644);
	Lseek(ifd, (off_t)dumplo, L_SET);
	printf("Saving %d bytes of image in core.%d\n", pagesize*dumpsize,
		bounds);
	syslog(LOG_NOTICE, "Saving %d bytes of image in core.%d",
		pagesize*dumpsize, bounds);
	while (dumpsize > 0) {
		n = Read(ifd, cp,
		    (dumpsize > BUFPAGES ? BUFPAGES : dumpsize) * pagesize);
		if (n == 0) {
			syslog(LOG_WARNING,
			    "WARNING: core may be incomplete");
			printf("WARNING: core may be incomplete\n");
			break;
		}
		Write(ofd, cp, n);
		dumpsize -= n/pagesize;
	}
	close(ifd);
	close(ofd);
	free(cp);
}

/*
 * Versions of std routines that exit on error.
 */
Open(name, rw)
	char *name;
	int rw;
{
	int fd;

	fd = open(name, rw);
	if (fd < 0) {
		int oerrno = errno;

		perror(name);
		errno = oerrno;
		syslog(LOG_ERR, "%s: %m", name);
		exit(1);
	}
	return (fd);
}

Read(fd, buff, size)
	int fd, size;
	char *buff;
{
	int ret;

	ret = read(fd, buff, size);
	if (ret < 0) {
		int oerrno = errno;

		perror("read");
		errno = oerrno;
		syslog(LOG_ERR, "read: %m");
		exit(1);
	}
	return (ret);
}

off_t
Lseek(fd, off, flag)
	int fd, flag;
	long off;
{
	long ret;

	if (flag == L_SET)
		off = ok(off);
	ret = lseek(fd, off, flag);
	if (ret == -1) {
		int oerrno = errno;

		perror("lseek");
		errno = oerrno;
		syslog(LOG_ERR, "lseek: %m");
		exit(1);
	}
	return (ret);
}

Create(file, mode)
	char *file;
	int mode;
{
	register int fd;

	fd = creat(file, mode);
	if (fd < 0) {
		int oerrno = errno;

		perror(file);
		errno = oerrno;
		syslog(LOG_ERR, "%s: %m", file);
		exit(1);
	}
	return (fd);
}

Write(fd, buf, size)
	int fd, size;
	char *buf;
{

	if (write(fd, buf, size) < size) {
		int oerrno = errno;

		perror("write");
		errno = oerrno;
		syslog(LOG_ERR, "write: %m");
		exit(1);
	}
}
