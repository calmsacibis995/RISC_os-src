/* --------------------------------------------------- */
/* | Copyright (c) 1986 MIPS Computer Systems, Inc.  | */
/* | All Rights Reserved.                            | */
/* --------------------------------------------------- */
/* $Header: tar.c,v 1.17.1.3.1.1.1.2 90/10/05 10:04:55 beacker Exp $ */

/*
 * Copyright (c) 1980 Regents of the University of California.
 * All rights reserved.  The Berkeley software License Agreement
 * specifies the terms and conditions for redistribution.
 */

/*
 * Tape Archival Program
 */
#include <stdio.h>
#include <sys/types.h>
#include <sys/param.h>
#include <sys/stat.h>
#ifndef SYSTYPE_BSD43
#include <dirent.h>
#else SYSTYPE_BSD43
#include <sys/dir.h>
#endif SYSTYPE_BSD43
#include <sys/ioctl.h>
#include <sys/mtio.h>
#ifndef SYSTYPE_BSD43
#include <time.h>
#else SYSTYPE_BSD43
#include <sys/time.h>
#endif SYSTYPE_BSD43
#include <signal.h>
#include <errno.h>
#include <fcntl.h>
#include <tar.h>
#include <pwd.h>
#include <grp.h>

#ifndef	SYSTYPE_BSD43
#include <sysv/sys/sysmacros.h>
#endif	

#define TBLOCK	512
#define NBLOCK	20
#define NAMSIZ	100
#define PREFSIZ	155
#define	IDLEN	32

#define	writetape(b)	writetbuf(b, 1)
#define	min(a,b)  ((a) < (b) ? (a) : (b))
#define	max(a,b)  ((a) > (b) ? (a) : (b))

union hblock {
	char dummy[TBLOCK];
	struct header {
		char name[NAMSIZ];
		char mode[8];
		char uid[8];
		char gid[8];
		char size[12];
		char mtime[12];
		char chksum[8];
		char typeflag;
		char linkname[NAMSIZ];
		char magic[TMAGLEN];
		char version[TVERSLEN];
		char uname[IDLEN];
		char gname[IDLEN];
		char devmajor[8];
		char devminor[8];
		char prefix[PREFSIZ];
	} dbuf;
};

struct linkbuf {
	ino_t	inum;
	dev_t	devnum;
	int	count;
	char	pathname[NAMSIZ];
	struct	linkbuf *nextp;
};

union	hblock dblock;
union	hblock *tbuf;
struct	linkbuf *ihead;
struct	stat stbuf;
char	*namefile = "-";	/* For n option */
char	*rmthostname;		/* remote host name for N option */
char	*rmtprogramname = "tar";

#ifndef SYSTYPE_BSD43
struct utimbuf {
	time_t	actime;
	time_t	modtime;
};
#endif SYSTYPE_BSD43

int	rflag;
int	bsd43_sflag;
int	xflag;
int	vflag;
int	tflag;
int	cflag;
int	mflag;
int	fflag;
int	iflag;
int	sysv_oflag;
int	bsd43_oflag;
int	pflag;
int	wflag;
int	hflag;
int	Bflag;
int	Fflag;
int	nflag;
int	sysv_dflag;			/* put directories on the tape */
int	rmtflag;		/* use remote host */
int	Sflag;			/* archive special files */

int	mt;
int	term;
int	chksum;
int	recno;
int	first;
int	prtlinkerr;
int	freemem = 1;
int	nblock = 0;
int	onintr();
int	onquit();
int	onhup();
#ifdef notdef
int	onterm();
#endif

daddr_t	low;
daddr_t	high;
daddr_t	bsrch();

FILE	*vfile = stdout;
FILE	*tfile;
char	tname[] = "/tmp/tarXXXXXX";
char	*usefile;
#ifndef SYSTYPE_BSD43
char	magtape[] = DEFTAPE;
#else SYSTYPE_BSD43
char	magtape[] = "/dev/rmt8";
#endif SYSTYPE_BSD43
char	*malloc();
long	time();
off_t	lseek();
char	*mktemp();
char	*strcat();
char	*strcpy();
#ifndef SYSTYPE_BSD43
char	*strrchr();
#define	rindex	strrchr
#else SYSTYPE_BSD43
char	*rindex();
#endif SYSTYPE_BSD43
char	*getwdir();
#ifndef SYSTYPE_BSD43
char	*getcwd();
#else SYSTYPE_BSD43
char	*getwd();
#endif SYSTYPE_BSD43
char	*getmem();
struct	group	*getgrnam(), *getgrgid();
struct	passwd	*getpwnam(), *getpwuid();

main(argc, argv)
int	argc;
char	*argv[];
{
	char *cp;

	if (argc < 2)
		usage();

	tfile = NULL;
	usefile =  magtape;
	argv[argc] = 0;
	argv++;
	for (cp = *argv++; *cp; cp++) 
		switch(*cp) {

		case 'f':
			if (*argv == 0) {
				fprintf(stderr,
			"tar: tapefile must be specified with 'f' option\n");
				usage();
			}
			usefile = *argv++;
			fflag++;
			break;

		case 'c':
			cflag++;
			rflag++;
			break;

#ifndef SYSTYPE_BSD43
		case 'd':
			sysv_dflag++;
			break;
#endif SYSTYPE_BSD43

		case 'o':
#ifdef SYSTYPE_BSD43
			bsd43_oflag++;
#else SYSTYPE_BSD43			
			sysv_oflag++;
#endif SYSTYPE_BSD43
			break;

		case 'p':
			pflag++;
			break;
		
		case 'u':
			mktemp(tname);
			if ((tfile = fopen(tname, "w")) == NULL) {
				fprintf(stderr,
				 "tar: cannot create temporary file (%s)\n",
				 tname);
				done(1);
			}
			fprintf(tfile, "!!!!!/!/!/!/!/!/!/! 000\n");
			/*FALL THRU*/

		case 'r':
			rflag++;
			break;

#ifdef SYSTYPE_BSD43
		case 's':
			bsd43_sflag++;
			break;

#endif SYSTYPE_BSD43
		case 'v':
			vflag++;
			break;

		case 'w':
			wflag++;
			break;

		case 'x':
			xflag++;
			break;

		case 't':
			tflag++;
			break;

		case 'm':
			mflag++;
			break;

		case '-':
			break;

		case '0':
		case '1':
		case '4':
		case '5':
		case '7':
		case '8':
			magtape[strlen(magtape) - 1] = *cp;
			usefile = magtape;
			break;

		case 'b':
			if (*argv == 0) {
				fprintf(stderr,
			"tar: blocksize must be specified with 'b' option\n");
				usage();
			}
			nblock = atoi(*argv);
			if (nblock <= 0) {
				fprintf(stderr,
				    "tar: invalid blocksize \"%s\"\n", *argv);
				done(1);
			}
			argv++;
			break;

		case 'l':
			prtlinkerr++;
			break;

		case 'h':
			hflag++;
			break;

		case 'i':
			iflag++;
			break;

		case 'B':
			Bflag++;
			break;

		case 'F':
			Fflag++;
			break;

		case 'n':
			if (*argv == 0) {
				fprintf(stderr,
			"tar: namefile must be specified with 'n' option\n");
				usage();
			}
			namefile = *argv++;
			nflag++;
			break;

		case 'N':
			if (*argv == 0) {
				fprintf(stderr,
			"tar: network host must be specified with 'N' option\n");
				usage();
			}
			rmthostname = *argv++;
			rmtflag++;
			break;

		case 'S':
			Sflag++;
			break;
		
		default:
			fprintf(stderr, "tar: %c: unknown option\n", *cp);
			usage();
		}

	if (!rflag && !xflag && !tflag)
		usage();

#ifndef SYSTYPE_BSD43
	if (! sysv_dflag)
		bsd43_oflag = 1;
#else SYSTYPE_BSD43
	if (! bsd43_oflag)
		sysv_dflag = 1;
#endif SYSTYPE_BSD43
		
	if (nflag) {			/* read the names from a file	*/
		cr_vector(namefile, &argv);
	}

	if (rflag) {
		if (cflag && tfile != NULL)
			usage();
		if (signal(SIGINT, SIG_IGN) != SIG_IGN)
			(void) signal(SIGINT, onintr);
		if (signal(SIGHUP, SIG_IGN) != SIG_IGN)
			(void) signal(SIGHUP, onhup);
		if (signal(SIGQUIT, SIG_IGN) != SIG_IGN)
			(void) signal(SIGQUIT, onquit);
#ifdef notdef
		if (signal(SIGTERM, SIG_IGN) != SIG_IGN)
			(void) signal(SIGTERM, onterm);
#endif
		mt = openmt(usefile, 1);
		dorep(argv);
		done(0);
	}
	mt = openmt(usefile, 0);
	if (xflag)
		doxtract(argv);
	else
		dotable(argv);
	done(0);
}

usage()
{
	fprintf(stderr,
#ifndef SYSTYPE_BSD43
"tar: usage: tar -{txru}[cvfbdlmhopwBin] [tapefile] [blocksize] [listfile] file1 file2...\n");
#else SYSTYPE_BSD43
"tar: usage: tar -{txru}[cvfblmhopwBins] [tapefile] [blocksize] [listfile] file1 file2...\n");
#endif SYSTYPE_BSD43
	done(1);
}

int
openmt(tape, writing)
	char *tape;
	int writing;
{

	if (strcmp(tape, "-") == 0) {
		/*
		 * Read from standard input or write to standard output.
		 */
		if (writing) {
			if (cflag == 0) {
				fprintf(stderr,
			 "tar: can only create standard output archives\n");
				done(1);
			}
			vfile = stderr;
#ifdef SYSTYPE_BSD43
			setlinebuf(vfile);
#endif SYSTYPE_BSD43
			mt = dup(1);
		} else {
			mt = dup(0);
			Bflag++;
		}
	} else if ( rmtflag == 0) {
		/*
		 * Use file or tape on local machine.
		 */
		if (writing) {
#ifndef SYSTYPE_BSD43
			if (cflag)
				mt = open(tape,
					O_RDWR|O_CREAT|O_TRUNC|O_NDELAY, 0666);
			else
				mt = open(tape, O_RDWR|O_NDELAY);
		} else
			mt = open(tape, O_RDONLY|O_NDELAY);
#else SYSTYPE_BSD43
			if (cflag)
				mt = open(tape, O_RDWR|O_CREAT|O_TRUNC, 0666);
			else
				mt = open(tape, O_RDWR);
		} else
			mt = open(tape, O_RDONLY);
#endif SYSTYPE_BSD43
	} else {	/* remote host via rmt protocol */
		if (rmthost(rmthostname) == 0)
		{
			exit(1);
		}
		if (writing) {
#ifndef SYSTYPE_BSD43
			if (cflag)
				mt = rmtopen(tape,
					O_RDWR|O_CREAT|O_TRUNC|O_NDELAY);
			else
				mt = rmtopen(tape, O_RDWR|O_NDELAY);
		} else /* not writing */
			mt = rmtopen(tape, O_RDONLY|O_NDELAY);
#else SYSTYPE_BSD43
			if (cflag)
				mt = rmtopen(tape, O_RDWR|O_CREAT|O_TRUNC);
			else
				mt = rmtopen(tape, O_RDWR);
		} else /* not writing */
			mt = rmtopen(tape, O_RDONLY);
#endif SYSTYPE_BSD43
        }  /* end of else for rmt protocol */
	if (mt < 0) {
		fprintf(stderr, "tar: ");
		fflush(stderr);
		perror(tape);
		done(1);
	}
	return(mt);
}

dorep(argv)
	char *argv[];
{
	register char *cp, *cp2;
	char wdir[MAXPATHLEN], tempdir[MAXPATHLEN], *parent;

	if (!cflag) {
		getdir();
		do {
			passtape();
			if (term)
				done(0);
			getdir();
		} while (!endtape());
		backtape();
		if (tfile != NULL) {
			char buf[200];

			(void)sprintf(buf,
"sort +0 -1 +1nr %s -o %s; awk '$1 != prev {print; prev=$1}' %s >%sX; mv %sX %s",
				tname, tname, tname, tname, tname, tname);
			fflush(tfile);
			system(buf);
			freopen(tname, "r", tfile);
			fstat(fileno(tfile), &stbuf);
			high = stbuf.st_size;
		}
	}

	(void) getwdir(wdir, sizeof (wdir));
	while (*argv && ! term) {
		cp2 = *argv;
		if (!strcmp(cp2, "-C") && argv[1]) {
			argv++;
			if (chdir(*argv) < 0) {
				fprintf(stderr, "tar: can't change directories to ");
				fflush(stderr);
				perror(*argv);
			} else
				(void) getwdir(wdir, sizeof (wdir));
			argv++;
			continue;
		}
		parent = wdir;
		for (cp = *argv; *cp; cp++)
			if (*cp == '/')
				cp2 = cp;
		if (cp2 != *argv) {
			*cp2 = '\0';
			if (chdir(*argv) < 0) {
				fprintf(stderr, "tar: can't change directories to ");
				fflush(stderr);
				perror(*argv);
				continue;
			}
			parent = getwdir(tempdir);
			*cp2 = '/';
			cp2++;
		}
		putfile(*argv++, cp2, parent);
		if (chdir(wdir) < 0) {
			fprintf(stderr, "tar: cannot change back?: ");
			fflush(stderr);
			perror(wdir);
		}
	}
	putempty();
	putempty();
	flushtape();
	if (prtlinkerr == 0)
		return;
	for (; ihead != NULL; ihead = ihead->nextp) {
		if (ihead->count == 0)
			continue;
		fprintf(stderr, "tar: missing links to %s\n", ihead->pathname);
	}
}

endtape()
{
	return (dblock.dbuf.name[0] == '\0');
}

getdir()
{
	register struct stat *sp;
	int i;

top:
	readtape((char *)&dblock);
	if (dblock.dbuf.name[0] == '\0')
		return;
	sp = &stbuf;
	sscanf(dblock.dbuf.mode, "%o", &i);
	sp->st_mode = i;
	switch	(dblock.dbuf.typeflag) {
	case REGTYPE:
	case AREGTYPE:
		sp->st_mode |= S_IFREG;
		break;
	case DIRTYPE:
		sp->st_mode |= S_IFDIR;
		break;
	case SYMTYPE:
		sp->st_mode |= S_IFLNK;
		break;
	case CHRTYPE:
		sp->st_mode |= S_IFCHR;
		break;
	case BLKTYPE:
		sp->st_mode |= S_IFBLK;
		break;
	case FIFOTYPE:
		sp->st_mode |= S_IFIFO;
		break;
	}
	sscanf(dblock.dbuf.uid, "%o", &i);
	sp->st_uid = i;
	sscanf(dblock.dbuf.gid, "%o", &i);
	sp->st_gid = i;
	sscanf(dblock.dbuf.size, "%lo", &sp->st_size);
	sscanf(dblock.dbuf.mtime, "%lo", &sp->st_mtime);
	sscanf(dblock.dbuf.chksum, "%o", &chksum);
	if (chksum != (i = checksum())) {
		fprintf(stderr, "tar: directory checksum error (%d != %d)\n",
		    chksum, i);
		if (iflag)
			goto top;
		done(2);
	}
	if (strncmp(dblock.dbuf.magic, TMAGIC, TMAGLEN) == 0) {
		int 	devmajor, devminor;

		sscanf(dblock.dbuf.devmajor, "%o", &devmajor);
		sscanf(dblock.dbuf.devminor, "%o", &devminor);
		sp->st_rdev = makedev(devmajor, devminor);
	}
	/* strip off leading "/" if present */
	if (bsd43_sflag && dblock.dbuf.name[0] == '/') {
		register char *cp1, *cp2;
		for (cp1 = cp2 = dblock.dbuf.name; *cp2 && *cp2 == '/'; ++cp2);
		if (!*cp2)
			goto top;
		while (*cp1++ = *cp2++);
	}
	if (tfile != NULL)
		fprintf(tfile, "%s %s\n", dblock.dbuf.name, dblock.dbuf.mtime);
}

passtape()
{
	long blocks;
	char *bufp;

	if (dblock.dbuf.typeflag == LNKTYPE)
		return;
	blocks = stbuf.st_size;
	blocks += TBLOCK-1;
	blocks /= TBLOCK;

	while (blocks-- > 0)
		(void) readtbuf(&bufp, TBLOCK);
}

putfile(longname, shortname, parent)
	char *longname;
	char *shortname;
	char *parent;
{
	int infile = 0;
	long blocks;
	char buf[TBLOCK];
	char *bigbuf;
	register char *cp;
#ifndef SYSTYPE_BSD43
	struct dirent *dp;
#else SYSTYPE_BSD43
	struct direct *dp;
#endif SYSTYPE_BSD43
	DIR *dirp;
	register int i;
	long l;
	char newparent[NAMSIZ+64];
	extern int errno;
	int	maxread;
	int	hint;		/* amount to write to get "in sync" */

	if (!hflag)
		i = lstat(shortname, &stbuf);
	else
		i = stat(shortname, &stbuf);
	if (i < 0) {
		fprintf(stderr, "tar: ");
		fflush(stderr);
		perror(longname);
		return;
	}
	if (tfile != NULL && checkupdate(longname) == 0)
		return;
	if (checkw('r', longname) == 0)
		return;
	if (Fflag && checkf(shortname, stbuf.st_mode, Fflag) == 0)
		return;

	switch (stbuf.st_mode & S_IFMT) {

	case S_IFDIR:
		for (i = 0, cp = buf; *cp++ = longname[i++];)
			;
		*--cp = '/';
		*++cp = 0  ;
		if (!bsd43_oflag) {
			if ((cp - buf) >= NAMSIZ) {
				fprintf(stderr, "tar: %s: file name too long\n",
				    longname);
				return;
			}
			stbuf.st_size = 0;
			tomodes(&stbuf);
			strcpy(dblock.dbuf.name,buf);
			dblock.dbuf.typeflag = DIRTYPE;
			(void)sprintf(dblock.dbuf.chksum, "%.6o", checksum());
			(void) writetape((char *)&dblock);
		}
		if (*shortname == '/') {
			strcpy(newparent, shortname);
		} else {
			sprintf(newparent, "%s/%s", parent, shortname);
		}
		if (chdir(shortname) < 0) {
			fflush(stderr);
			perror(shortname);
			return;
		}
		if ((dirp = opendir(".")) == NULL) {
			fprintf(stderr, "tar: %s: directory read error\n",
			    longname);
			if (chdir(parent) < 0) {
				fprintf(stderr, "tar: cannot change back?: ");
				fflush(stderr);
				perror(parent);
			}
			return;
		}
		while ((dp = readdir(dirp)) != NULL && !term) {
			if (!strcmp(".", dp->d_name) ||
			    !strcmp("..", dp->d_name))
				continue;
			strcpy(cp, dp->d_name);
			l = telldir(dirp);
			closedir(dirp);
			putfile(buf, cp, newparent);
			dirp = opendir(".");
			seekdir(dirp, l);
		}
		closedir(dirp);
		if (chdir(parent) < 0) {
			fprintf(stderr, "tar: cannot change back?: ");
			fflush(stderr);
			perror(parent);
		}
		break;

	case S_IFLNK:
		tomodes(&stbuf);
		if (strlen(longname) >= NAMSIZ) {
			fprintf(stderr, "tar: %s: file name too long\n",
			    longname);
			return;
		}
		strcpy(dblock.dbuf.name, longname);
		if (stbuf.st_size + 1 >= NAMSIZ) {
			fprintf(stderr, "tar: %s: symbolic link too long\n",
			    longname);
			return;
		}
		i = readlink(shortname, dblock.dbuf.linkname, NAMSIZ - 1);
		if (i < 0) {
			fprintf(stderr, "tar: can't read symbolic link ");
			fflush(stderr);
			perror(longname);
			return;
		}
		dblock.dbuf.linkname[i] = '\0';
		dblock.dbuf.typeflag = SYMTYPE;
		if (vflag)
			fprintf(vfile, "a %s symbolic link to %s\n",
			    longname, dblock.dbuf.linkname);
		(void)sprintf(dblock.dbuf.size, "%.11lo", 0L);
		(void)sprintf(dblock.dbuf.chksum, "%.6o", checksum());
		(void) writetape((char *)&dblock);
		break;

	case S_IFREG:
		if ((infile = open(shortname, O_RDONLY)) < 0) {
			fprintf(stderr, "tar: ");
			fflush(stderr);
			perror(longname);
			return;
		}
		tomodes(&stbuf);
		if (strlen(longname) >= NAMSIZ) {
			fprintf(stderr, "tar: %s: file name too long\n",
			    longname);
			close(infile);
			return;
		}
		strcpy(dblock.dbuf.name, longname);
		if (stbuf.st_nlink > 1) {
			struct linkbuf *lp;
			int found = 0;

			for (lp = ihead; lp != NULL; lp = lp->nextp)
				if (lp->inum == stbuf.st_ino &&
				    lp->devnum == stbuf.st_dev) {
					found++;
					break;
				}
			if (found) {
				strcpy(dblock.dbuf.linkname, lp->pathname);
				dblock.dbuf.typeflag = LNKTYPE;
				(void)sprintf(dblock.dbuf.size, "%.11lo", 0L);
				(void)sprintf(dblock.dbuf.chksum, "%.6o", checksum());
				(void) writetape( (char *) &dblock);
				if (vflag)
					fprintf(vfile, "a %s link to %s\n",
					    longname, lp->pathname);
				lp->count--;
				close(infile);
				return;
			}
			lp = (struct linkbuf *) getmem(sizeof(*lp));
			if (lp != NULL) {
				lp->nextp = ihead;
				ihead = lp;
				lp->inum = stbuf.st_ino;
				lp->devnum = stbuf.st_dev;
				lp->count = stbuf.st_nlink - 1;
				strcpy(lp->pathname, longname);
			}
		}
		dblock.dbuf.typeflag = REGTYPE;
		blocks = (stbuf.st_size + (TBLOCK-1)) / TBLOCK;
		if (vflag)
			fprintf(vfile, "a %s %ld blocks\n", longname, blocks);
		(void)sprintf(dblock.dbuf.chksum, "%.6o", checksum());
		hint = writetape((char *)&dblock);
#ifndef SYSTYPE_BSD43
		maxread = (nblock * TBLOCK);
#else SYSTYPE_BSD43
		maxread = max(stbuf.st_blksize, (nblock * TBLOCK));
#endif SYSTYPE_BSD43
		if ((bigbuf = malloc((unsigned)maxread)) == 0) {
			maxread = TBLOCK;
			bigbuf = buf;
		}

		while ((i = read(infile, bigbuf, min((hint*TBLOCK), maxread))) > 0
		  && blocks > 0) {
		  	register int nblks;

			nblks = ((i-1)/TBLOCK)+1;
		  	if (nblks > blocks)
		  		nblks = blocks;
			hint = writetbuf(bigbuf, nblks);
			blocks -= nblks;
		}
		close(infile);
		if (bigbuf != buf)
			free(bigbuf);
		if (i < 0) {
			fprintf(stderr, "tar: Read error on ");
			fflush(stderr);
			perror(longname);
		} else if (blocks != 0 || i != 0)
			fprintf(stderr, "tar: %s: file changed size\n",
			    longname);
		while (--blocks >=  0)
			putempty();
		break;

	case S_IFIFO:
	case S_IFCHR:
	case S_IFBLK:
		if (!Sflag) {
			fprintf(stderr, "tar: %s is not a regular file. Not dumped\n",
			    longname);
			break;
		}

		tomodes(&stbuf);
		if (strlen(longname) >= NAMSIZ) {
			fprintf(stderr, "tar: %s: file name too long\n",
			    longname);
			close(infile);
			return;
		}
		strcpy(dblock.dbuf.name, longname);
		if (stbuf.st_nlink > 1) {
			struct linkbuf *lp;
			int found = 0;

			for (lp = ihead; lp != NULL; lp = lp->nextp)
				if (lp->inum == stbuf.st_ino &&
				    lp->devnum == stbuf.st_dev) {
					found++;
					break;
				}
			if (found) {
				strcpy(dblock.dbuf.linkname, lp->pathname);
				dblock.dbuf.typeflag = LNKTYPE;
				(void)sprintf(dblock.dbuf.chksum, "%.6o", checksum());
				(void) writetape( (char *) &dblock);
				if (vflag)
					fprintf(vfile, "a %s link to %s\n",
					    longname, lp->pathname);
				lp->count--;
				return;
			}
			lp = (struct linkbuf *) getmem(sizeof(*lp));
			if (lp != NULL) {
				lp->nextp = ihead;
				ihead = lp;
				lp->inum = stbuf.st_ino;
				lp->devnum = stbuf.st_dev;
				lp->count = stbuf.st_nlink - 1;
				strcpy(lp->pathname, longname);
			}
		}
		switch(stbuf.st_mode & S_IFMT) {
		case S_IFIFO:
			dblock.dbuf.typeflag = FIFOTYPE;
			break;
		case S_IFCHR:
			dblock.dbuf.typeflag = CHRTYPE;
			break;
		case S_IFBLK:
			dblock.dbuf.typeflag = BLKTYPE;
			break;
		}
		blocks = (stbuf.st_size + (TBLOCK-1)) / TBLOCK;
		if (vflag)
			fprintf(vfile, "a %s major %ld, minor %ld\n",
					longname, major(stbuf.st_rdev),
					minor(stbuf.st_rdev));
		(void)sprintf(dblock.dbuf.chksum, "%.6o", checksum());
		writetape((char *)&dblock);
		break;
	default:
		fprintf(stderr, "tar: %s is not a regular file. Not dumped\n",
		    longname);
		break;
	}
}

doxtract(argv)
	char *argv[];
{
	extern int errno;
	long blocks, bytes;
	int ofile, i;
	struct passwd	*pwd;
	struct group 	*grp;
	char name[256];

	for (;;) {
		if ((i = wantit(argv)) == 0)
			continue;
		if (i == -1)
			break;		/* end of tape */
		if (checkw('x', dblock.dbuf.name) == 0) {
			passtape();
			continue;
		}
		if (Fflag) {
			char *s;

			if ((s = rindex(dblock.dbuf.name, '/')) == 0)
				s = dblock.dbuf.name;
			else
				s++;
			if (checkf(s, stbuf.st_mode, Fflag) == 0) {
				passtape();
				continue;
			}
		}
		if (strncmp(dblock.dbuf.magic, TMAGIC, TMAGLEN) == 0) {
			/* if uname and gname are valid, find out what there
			 * corresponding uid and gid are for the system being
			 * tar'ed to and use those values instead of the stored 
			 * values
			 */
			if ((pwd = getpwnam(dblock.dbuf.uname)) != NULL) 
				stbuf.st_uid = pwd->pw_uid;
			if ((grp = getgrnam(dblock.dbuf.gname)) != NULL) 
				stbuf.st_gid = grp->gr_gid;
			if (dblock.dbuf.prefix[0] != '\0') {
				strncpy(name, dblock.dbuf.prefix, PREFSIZ);
				strncat(name, dblock.dbuf.name, NAMSIZ);
				name[NAMSIZ+PREFSIZ] = '\0';
			} else {
				strncpy(name, dblock.dbuf.name, NAMSIZ);
				name[NAMSIZ] = '\0';
			}
		} else { /* old archive */
			strncpy(name, dblock.dbuf.name, NAMSIZ);
			name[NAMSIZ] = '\0';
		}

		if (checkdir(name)) {	/* have a directory */
			if (mflag == 0)
				dodirtimes(&dblock, name);
			continue;
		}
		switch (dblock.dbuf.typeflag) {
		case CHRTYPE: /* character special file */
		case BLKTYPE: /* block special file */
		case FIFOTYPE: /* FIFO special file */
			if (!Sflag) return;
			/*
			 * only unlink non directories or empty
			 * directories
			 */
			if (rmdir(name) < 0) {
				if (errno == ENOTDIR)
					unlink(name);
			}
			if (mknod(name, stbuf.st_mode, stbuf.st_rdev)<0) {
				fprintf(stderr, "tar: %s: mknod failed: ",
				    name);
				fflush(stderr);
				perror("");
				continue;
			}
			if (vflag)
				fprintf(vfile, "x %s, major %ld, minor %ld\n",
					name, major(stbuf.st_rdev),
					minor(stbuf.st_rdev));
			if (mflag == 0)
				setimes(name, stbuf.st_mtime);
			if (pflag)
				chmod(name, stbuf.st_mode & 07777);
			continue;
		case SYMTYPE: /* symlink */
			/*
			 * only unlink non directories or empty
			 * directories
			 */
			if (rmdir(name) < 0) {
				if (errno == ENOTDIR)
					unlink(name);
			}
			if (symlink(dblock.dbuf.linkname, name)<0) {
				fprintf(stderr, "tar: %s: symbolic link failed: ",
				    name);
				fflush(stderr);
				perror("");
				continue;
			}
			if (vflag)
				fprintf(vfile, "x %s symbolic link to %s\n",
				    name, dblock.dbuf.linkname);
#ifdef notdef
			/* ignore alien orders */
			chown(name, stbuf.st_uid, stbuf.st_gid);
			if (mflag == 0)
				setimes(name, stbuf.st_mtime);
			if (pflag)
				chmod(name, stbuf.st_mode & 07777);
#endif
			continue;
		case LNKTYPE:	/* regular link */
			/*
			 * only unlink non directories or empty
			 * directories
			 */
			if (rmdir(name) < 0) {
				if (errno == ENOTDIR)
					unlink(name);
			}
			if (link(dblock.dbuf.linkname, name) < 0) {
				fprintf(stderr, "tar: can't link %s to %s: ",
				    name, dblock.dbuf.linkname);
				fflush(stderr);
				perror("");
				continue;
			}
			if (vflag)
				fprintf(vfile, "%s linked to %s\n",
				    name, dblock.dbuf.linkname);
			continue;
		default:
			fprintf(stderr, "tar: unrecognized type %d for %s: attempting to extract it as a regular file\n",
					dblock.dbuf.typeflag, name);
			fflush(stderr);
			/* fall thru */
		case REGTYPE:
		case AREGTYPE:
			if ((ofile = creat(name,stbuf.st_mode&0xfff)) < 0) {
				fprintf(stderr, "tar: can't create %s: ",
				    name);
				fflush(stderr);
				perror("");
				passtape();
				continue;
			}
			if (!sysv_oflag) {
				chown(name, stbuf.st_uid, stbuf.st_gid);
			}
			blocks = ((bytes = stbuf.st_size) + TBLOCK-1)/TBLOCK;
			if (vflag)
				fprintf(vfile, "x %s, %ld bytes, %ld tape blocks\n",
				    name, bytes, blocks);
			for (; blocks > 0;) {
				register int nread;
				char	*bufp;
				register int nwant;
				
				nwant = NBLOCK*TBLOCK;
				if (nwant > (blocks*TBLOCK))
					nwant = (blocks*TBLOCK);
				nread = readtbuf(&bufp, nwant);
				if (write(ofile, bufp, (int)min(nread, bytes)) < 0) {
					fprintf(stderr,
					    "tar: %s: HELP - extract write error: ",
					    name);
					fflush(stderr);
					perror("");
					done(2);
				}
				bytes -= nread;
				blocks -= (((nread-1)/TBLOCK)+1);
			}
			close(ofile);
			if (mflag == 0)
				setimes(name, stbuf.st_mtime);
			if (pflag)
				chmod(name, stbuf.st_mode & 07777);
			continue;
		}
	}
	if (mflag == 0) {
		dblock.dbuf.name[0] = '\0';	/* process the whole stack */
		dodirtimes(&dblock, name);
	}
}

dotable(argv)
	char *argv[];
{
	register int i;

	for (;;) {
		if ((i = wantit(argv)) == 0)
			continue;
		if (i == -1)
			break;		/* end of tape */
		if (vflag)
			longt(&stbuf);
		printf("%s", dblock.dbuf.name);
		if (dblock.dbuf.typeflag == LNKTYPE)
			printf(" linked to %s", dblock.dbuf.linkname);
		if (dblock.dbuf.typeflag == SYMTYPE)
			printf(" symbolic link to %s", dblock.dbuf.linkname);
		printf("\n");
		passtape();
	}
}

putempty()
{
	char buf[TBLOCK];

#ifndef SYSTYPE_BSD43
	memset(buf, '\0', sizeof (buf));
#else SYSTYPE_BSD43
	bzero(buf, sizeof (buf));
#endif SYSTYPE_BSD43
	(void) writetape(buf);
}

longt(st)
	register struct stat *st;
{
	register char *cp;
	char *ctime();

	pmode(st);
	printf("%3d/%1d", st->st_uid, st->st_gid);
	switch	(st->st_mode & S_IFMT) {
	case S_IFCHR:
	case S_IFBLK:
	case S_IFIFO:
		printf("%3d,%1d", major(st->st_rdev), minor(st->st_rdev));
		break;
	default:
		printf("%7ld", st->st_size);
	}
	cp = ctime(&st->st_mtime);
	printf(" %#-12.12s %#-4.4s ", cp+4, cp+20);
}

#define	SUID	04000
#define	SGID	02000
#define	ROWN	0400
#define	WOWN	0200
#define	XOWN	0100
#define	RGRP	040
#define	WGRP	020
#define	XGRP	010
#define	ROTH	04
#define	WOTH	02
#define	XOTH	01
#define	STXT	01000
int	m1[] = { 1, ROWN, 'r', '-' };
int	m2[] = { 1, WOWN, 'w', '-' };
int	m3[] = { 2, SUID, 's', XOWN, 'x', '-' };
int	m4[] = { 1, RGRP, 'r', '-' };
int	m5[] = { 1, WGRP, 'w', '-' };
int	m6[] = { 2, SGID, 's', XGRP, 'x', '-' };
int	m7[] = { 1, ROTH, 'r', '-' };
int	m8[] = { 1, WOTH, 'w', '-' };
int	m9[] = { 2, STXT, 't', XOTH, 'x', '-' };

int	*m[] = { m1, m2, m3, m4, m5, m6, m7, m8, m9};

pmode(st)
	register struct stat *st;
{
	register int **mp;

	for (mp = &m[0]; mp < &m[9];)
		selectbits(*mp++, st);
}

selectbits(pairp, st)
	int *pairp;
	struct stat *st;
{
	register int n, *ap;

	ap = pairp;
	n = *ap++;
	while (--n>=0 && (st->st_mode&*ap++)==0)
		ap++;
	putchar(*ap);
}

/*
 * Make all directories needed by `name'.  If `name' is itself
 * a directory on the tar tape (indicated by a trailing '/'),
 * return 1; else 0.
 */
checkdir(name)
	register char *name;
{
	register char *cp;

	/*
	 * Quick check for existence of directory.
	 */
	if ((cp = rindex(name, '/')) == 0)
		return (0);
	*cp = '\0';
	if (access(name, 0) == 0) {	/* already exists */
		*cp = '/';
		return (cp[1] == '\0');	/* return (lastchar == '/') */
	}
	*cp = '/';

	/*
	 * No luck, try to make all directories in path.
	 */
	for (cp = name; *cp; cp++) {
		if (*cp != '/')
			continue;
		*cp = '\0';
		if (name[0] != '\0' && access(name, 0) < 0) {
			if (mkdir(name, 0777) < 0) {
				fflush(stderr);
				perror(name);
				*cp = '/';
				return (0);
			}
			if (!sysv_oflag &&
			    cp[1] == '\0') {
				chown(name, stbuf.st_uid, stbuf.st_gid);
			}
			if (pflag && cp[1] == '\0')	/* dir on the tape */
				chmod(name, stbuf.st_mode & 07777);
		}
		*cp = '/';
	}
	return (cp[-1]=='/');
}

onintr()
{
	(void) signal(SIGINT, SIG_IGN);
	term++;
}

onquit()
{
	(void) signal(SIGQUIT, SIG_IGN);
	term++;
}

onhup()
{
	(void) signal(SIGHUP, SIG_IGN);
	term++;
}

#ifdef notdef
onterm()
{
	(void) signal(SIGTERM, SIG_IGN);
	term++;
}
#endif

tomodes(sp)
register struct stat *sp;
{
	register char *cp;
	struct passwd *pwd;
	struct group *grp;

	for (cp = dblock.dummy; cp < &dblock.dummy[TBLOCK]; cp++)
		*cp = '\0';
	(void)sprintf(dblock.dbuf.mode, "%.6o ", sp->st_mode & 07777);
	(void)sprintf(dblock.dbuf.uid, "%.6o ", sp->st_uid);
	(void)sprintf(dblock.dbuf.gid, "%.6o ", sp->st_gid);
	(void)sprintf(dblock.dbuf.size, "%.11lo ", sp->st_size);
	(void)sprintf(dblock.dbuf.mtime, "%.11lo ", sp->st_mtime);
	(void)sprintf(dblock.dbuf.devmajor, "%.6o ", major(sp->st_rdev));
	(void)sprintf(dblock.dbuf.devminor, "%.6o ", minor(sp->st_rdev));
	(void)strncpy(dblock.dbuf.magic, TMAGIC, TMAGLEN);
	(void)strncpy(dblock.dbuf.version, TVERSION, TVERSLEN);
	dblock.dbuf.prefix[0] = '\0';
	if ((pwd = getpwuid(sp->st_uid)) != NULL) {
		strncpy(dblock.dbuf.uname, pwd->pw_name, IDLEN);
		/* make sure name is null-terminated */
		dblock.dbuf.uname[IDLEN-1] = '\0'; 
	}
    	if ((grp = getgrgid(sp->st_gid)) != NULL) {
		strncpy(dblock.dbuf.gname, grp->gr_name, IDLEN);
		/* make sure name is null-terminated */
		dblock.dbuf.gname[IDLEN-1] = '\0'; 
	}

}

checksum()
{
	register i;
	register char *cp;

	for (cp = dblock.dbuf.chksum;
	     cp < &dblock.dbuf.chksum[sizeof(dblock.dbuf.chksum)]; cp++)
		*cp = ' ';
	i = 0;
	for (cp = dblock.dummy; cp < &dblock.dummy[TBLOCK]; cp++)
		i += *cp;
	return (i);
}

checkw(c, name)
	char *name;
{
	if (!wflag)
		return (1);
	printf("%c ", c);
	if (vflag)
		longt(&stbuf);
	printf("%s: ", name);
	return (response() == 'y');
}

response()
{
	char c;

	c = getchar();
	if (c != '\n')
		while (getchar() != '\n')
			;
	else
		c = 'n';
	return (c);
}

checkf(name, mode, howmuch)
	char *name;
	int mode, howmuch;
{
	int l;

	if ((mode & S_IFMT) == S_IFDIR){
		if ((strcmp(name, "SCCS")==0) || (strcmp(name, "RCS")==0)) 
			return(0); 
		return(1);
	}
	if ((l = strlen(name)) < 3)
		return (1);
	if (howmuch > 1 && name[l-2] == '.' && name[l-1] == 'o')
		return (0);
	if (strcmp(name, "core") == 0 ||
	    strcmp(name, "errs") == 0 ||
	    (howmuch > 1 && strcmp(name, "a.out") == 0))
		return (0);
	/* SHOULD CHECK IF IT IS EXECUTABLE */
	return (1);
}

/* Is the current file a new file, or the newest one of the same name? */
checkupdate(arg)
	char *arg;
{
	char name[100];
	long mtime;
	daddr_t seekp;
	daddr_t	lookup();

	rewind(tfile);
	for (;;) {
		if ((seekp = lookup(arg)) < 0)
			return (1);
		fseek(tfile, seekp, 0);
		fscanf(tfile, "%s %lo", name, &mtime);
		return (stbuf.st_mtime > mtime);
	}
}

done(n)
{
	unlink(tname);
	exit(n);
}

/* 
 * Do we want the next entry on the tape, i.e. is it selected?  If
 * not, skip over the entire entry.  Return -1 if reached end of tape.
 */
wantit(argv)
	char *argv[];
{
	register char **cp;

	getdir();
	if (endtape())
		return (-1);
	if (*argv == 0)
		return (1);
	for (cp = argv; *cp; cp++)
		if (prefix(*cp, dblock.dbuf.name))
			return (1);
	passtape();
	return (0);
}

/*
 * Does s2 begin with the string s1, on a directory boundary?
 */
prefix(s1, s2)
	register char *s1, *s2;
{
	while (*s1)
		if (*s1++ != *s2++)
			return (0);
	if (*s2)
		return (*s2 == '/');
	return (1);
}

#define	N	200
int	njab;

daddr_t
lookup(s)
	char *s;
{
	register i;
	daddr_t a;

	for(i=0; s[i]; i++)
		if (s[i] == ' ')
			break;
	a = bsrch(s, i, low, high);
	return (a);
}

daddr_t
bsrch(s, n, l, h)
	daddr_t l, h;
	char *s;
{
	register i, j;
	char b[N];
	daddr_t m, m1;

	njab = 0;

loop:
	if (l >= h)
		return ((daddr_t) -1);
	m = l + (h-l)/2 - N/2;
	if (m < l)
		m = l;
	fseek(tfile, m, 0);
	fread(b, 1, N, tfile);
	njab++;
	for(i=0; i<N; i++) {
		if (b[i] == '\n')
			break;
		m++;
	}
	if (m >= h)
		return ((daddr_t) -1);
	m1 = m;
	j = i;
	for(i++; i<N; i++) {
		m1++;
		if (b[i] == '\n')
			break;
	}
	i = cmp(b+j, s, n);
	if (i < 0) {
		h = m;
		goto loop;
	}
	if (i > 0) {
		l = m1;
		goto loop;
	}
	return (m);
}

cmp(b, s, n)
	char *b, *s;
{
	register i;

	if (b[0] != '\n')
		exit(2);
	for(i=0; i<n; i++) {
		if (b[i+1] > s[i])
			return (-1);
		if (b[i+1] < s[i])
			return (1);
	}
	return (b[i+1] == ' '? 0 : -1);
}

readtape(buffer)
	char *buffer;
{
	char *bufp;

	if (first == 0)
		getbuf();
	(void) readtbuf(&bufp, TBLOCK);
#ifndef SYSTYPE_BSD43
	memcpy(buffer, bufp, TBLOCK);
#else SYSTYPE_BSD43
	bcopy(bufp, buffer, TBLOCK);
#endif SYSTYPE_BSD43
	return(TBLOCK);
}

readtbuf(bufpp, size)
	char **bufpp;
	int size;
{
	register int i;

	if (recno >= nblock || first == 0) {
		if (rmtflag)
		{
		    if ((i = rmtread( (char *)tbuf, TBLOCK*nblock)) < 0)
			mterr("read", i, 3);
		}
		else
		{
		    if ((i = bread(mt, (char *)tbuf, TBLOCK*nblock)) < 0)
			mterr("read", i, 3);
		}
		if (first == 0) {
			if ((i % TBLOCK) != 0) {
				fprintf(stderr, "tar: tape blocksize error\n");
				done(3);
			}
			i /= TBLOCK;
			if (i != nblock) {
				fprintf(stderr, "tar: blocksize = %d\n", i);
				nblock = i;
			}
			first = 1;
		}
		recno = 0;
	}
	if (size > ((nblock-recno)*TBLOCK))
		size = (nblock-recno)*TBLOCK;
	*bufpp = (char *)&tbuf[recno];
	recno += (size/TBLOCK);
	return (size);
}

writetbuf(buffer, n)
	register char *buffer;
	register int n;
{
	int i;

	if (first == 0) {
		getbuf();
		first = 1;
	}
	if (recno >= nblock) {
		if (rmtflag)
		{
		    i = rmtwrite((char *)tbuf, TBLOCK*nblock);
		}
		else
		{
		    i = write(mt, (char *)tbuf, TBLOCK*nblock);
		}
		if (i != TBLOCK*nblock)
			mterr("write", i, 2);
		recno = 0;
	}

	/*
	 *  Special case:  We have an empty tape buffer, and the
	 *  users data size is >= the tape block size:  Avoid
	 *  the buffer copy and dma direct to tape.  BIG WIN.  Add the
	 *  residual to the tape buffer.
	 */
	while (recno == 0 && n >= nblock) {
		if (rmtflag)
		{
		    i = rmtwrite((char *)buffer, TBLOCK*nblock);
		}
		else
		{
		    i = write(mt, (char *)buffer, TBLOCK*nblock);
		}
		if (i != TBLOCK*nblock)
			mterr("write", i, 2);
		n -= nblock;
		buffer += (nblock * TBLOCK);
	}
		
	while (n-- > 0) {
#ifndef SYSTYPE_BSD43
		memcpy((char *)&tbuf[recno++], buffer, TBLOCK);
#else SYSTYPE_BSD43
		bcopy(buffer, (char *)&tbuf[recno++], TBLOCK);
#endif SYSTYPE_BSD43
		buffer += TBLOCK;
		if (recno >= nblock) {
		    if (rmtflag)
		    {
			i = rmtwrite((char *)tbuf, TBLOCK*nblock);
		    }
		    else
		    {
			i = write(mt, (char *)tbuf, TBLOCK*nblock);
		    }
		    if (i != TBLOCK*nblock)
			    mterr("write", i, 2);
		    recno = 0;
		}
	}

	/* Tell the user how much to write to get in sync */
	return (nblock - recno);
}

backtape()
{
	static int mtdev = 1;
	static struct mtop mtop = {MTBSR, 1};
	struct mtget mtget;
	
	if (mtdev == 1)
		if (rmtflag)
		{
		    mtdev = rmtioctl(MTIOCGET, (char *)&mtget);
		}
		else
		{
		    mtdev = ioctl(mt, MTIOCGET, (char *)&mtget);
		}
	if (mtdev == 0) {
		if (rmtflag)
		{
		    if (rmtioctl(MTIOCTOP, (char *)&mtop) < 0) {
			    fprintf(stderr, "tar: tape backspace error: ");
			    fflush(stderr);
			    perror("");
			    done(4);
		    }
		}
		else
		{
		    if (ioctl(mt, MTIOCTOP, (char *)&mtop) < 0) {
			    fprintf(stderr, "tar: tape backspace error: ");
			    fflush(stderr);
			    perror("");
			    done(4);
		    }
		}
	} else {
		if (rmtflag)
		{
		    rmtseek((daddr_t) -TBLOCK*nblock, 1);
		}
		else
		{
		    lseek(mt, (daddr_t) -TBLOCK*nblock, 1);
		}
	}
	recno--;
}

flushtape()
{
	int i;

	if (rmtflag)
	{
	    i = rmtwrite((char *)tbuf, TBLOCK*nblock);
	}
	else
	{
	    i = write(mt, (char *)tbuf, TBLOCK*nblock);
	}
	if (i != TBLOCK*nblock)
		mterr("write", i, 2);
}

mterr(operation, i, exitcode)
	char *operation;
	int i;
{
	fprintf(stderr, "tar: tape %s error: ", operation);
	if (i < 0) {
		fflush(stderr);
		perror("");
	} else
		fprintf(stderr, "unexpected EOF\n");
	done(exitcode);
}

bread(fd, buf, size)
	int fd;
	char *buf;
	int size;
{
	int count;
	static int lastread = 0;

	if (!Bflag)
	    if (rmtflag)
	    {
		return (rmtread(buf, size)); 
	    }
	    else
	    {
		return (read(fd, buf, size)); 
	    }

	for (count = 0; count < size; count += lastread) {
		if (rmtflag)
		{
		    lastread = rmtread(buf, size - count);
		}
		else
		{
		    lastread = read(fd, buf, size - count);
		}
		if (lastread <= 0) {
			if (count > 0)
				return (count);
			return (lastread);
		}
		buf += lastread;
	}
	return (count);
}

char *
getwdir(buf, size)
	char *buf;
	int size;
{
#ifndef SYSTYPE_BSD43
	if (getcwd(buf, size) == NULL) {
		perror("tar: can't get current directory:");
		exit(1);
	}
#else SYSTYPE_BSD43
	if (getwd(buf) == NULL) {
		fprintf(stderr, "tar: %s\n", buf);
		exit(1);
	}
#endif SYSTYPE_BSD43
	return (buf);
}

getbuf()
{
	
	if (nblock == 0) {
#ifndef SYSTYPE_BSD43
		nblock = NBLOCK;
#else SYSTYPE_BSD43
		if (rmtflag)
		{
			nblock = NBLOCK;	/* no way to find out */
		}
		else
		{
		    fstat(mt, &stbuf);
		    if ((stbuf.st_mode & S_IFMT) == S_IFCHR)
			    nblock = NBLOCK;
		    else {
			    nblock = stbuf.st_blksize / TBLOCK;
			    if (nblock == 0)
				    nblock = NBLOCK;
		    }
		}
#endif SYSTYPE_BSD43
	}
	tbuf = (union hblock *)malloc((unsigned)nblock*TBLOCK);
	if (tbuf == NULL) {
		fprintf(stderr, "tar: blocksize %d too big, can't get memory\n",
		    nblock);
		done(1);
	}
}

/*
 * Save this directory and its mtime on the stack, popping and setting
 * the mtimes of any stacked dirs which aren't parents of this one.
 * A null directory causes the entire stack to be unwound and set.
 *
 * Since all the elements of the directory "stack" share a common
 * prefix, we can make do with one string.  We keep only the current
 * directory path, with an associated array of mtime's, one for each
 * '/' in the path.  A negative mtime means no mtime.  The mtime's are
 * offset by one (first index 1, not 0) because calling this with a null
 * directory causes mtime[0] to be set.
 * 
 * This stack algorithm is not guaranteed to work for tapes created
 * with the 'r' option, but the vast majority of tapes with
 * directories are not.  This avoids saving every directory record on
 * the tape and setting all the times at the end.
 */
char dirstack[NAMSIZ];
#define NTIM (NAMSIZ/2+1)		/* a/b/c/d/... */
time_t mtime[NTIM];

dodirtimes(hp, name)
	union hblock *hp;
	char *name;
{
	register char *p = dirstack;
	register char *q = name;
	register int ndir = 0;
	char *savp;
	int savndir;

	/* Find common prefix */
	while (*p == *q && *p) {
		if (*p++ == '/')
			++ndir;
		q++;
	}

	savp = p;
	savndir = ndir;
	while (*p) {
		/*
		 * Not a child: unwind the stack, setting the times.
		 * The order we do this doesn't matter, so we go "forward."
		 */
		if (*p++ == '/')
			if (mtime[++ndir] >= 0) {
				*--p = '\0';	/* zap the slash */
				setimes(dirstack, mtime[ndir]);
				*p++ = '/';
			}
	}
	p = savp;
	ndir = savndir;

	/* Push this one on the "stack" */
	while (*p = *q++)	/* append the rest of the new dir */
		if (*p++ == '/')
			mtime[++ndir] = -1;
	mtime[ndir] = stbuf.st_mtime;	/* overwrite the last one */
}

setimes(path, mt)
	char *path;
	time_t mt;
{
#ifdef SYSTYPE_BSD43
	struct timeval tv[2];

	tv[0].tv_sec = time((time_t *) 0);
	tv[1].tv_sec = mt;
	tv[0].tv_usec = tv[1].tv_usec = 0;
	if (utimes(path, tv) < 0) {
		fprintf(stderr, "tar: can't set time on %s: ", path);
		fflush(stderr);
		perror("");
	}

#else SYSTYPE_BSD43
	struct utimbuf tb;

	tb.actime = time((time_t *) 0);
	tb.modtime = mt;

	/*
	 * It seems that it doesn't really matter in System V whether
	 * the time gets set or not when you give away a file.
	 */
	(void) utime(path, &tb);
#endif SYSTYPE_BSD43
}

char *
getmem(size)
{
	char *p = malloc((unsigned) size);

	if (p == NULL && freemem) {
		fprintf(stderr,
		    "tar: out of memory, link and directory modtime info lost\n");
		freemem = 0;
	}
	return (p);
}

/*
 * The subroutine cr_vector reates a vector from the contents of a file.
 */

#define MBUFSIZE	4096		/* 4K buffer */

cr_vector(file, argv)
	char	*file;
	char	**argv[];
{
	int	f;			/* input file pointer		*/
	struct  stat sb;		/* the stat buffer		*/
	int	size;			/* length of the file		*/
	char	*strings;		/* pointer to the strings	*/
	register char	*s;		/* local char pointer		*/
	int	nlines;			/* the number of lines in file	*/
	char	**a;
	char	*temp_file;		/* the temp file name for -	*/
	FILE	*fp;
	char	*buf, *malloc();
	int	n;
	int	delete = 0;

	temp_file = "/tmp/tar.XXXXXX";

	/*
	 * If the filename is -, open a temporary file and copy the
	 * standard input to the file.
	 */
	
	if (file[0] == '-' && file[1] == '\0') {
		mktemp(temp_file);
		f = open(temp_file, O_RDWR | O_CREAT | O_TRUNC | O_EXCL, 0644);
		if (f < 0) {
			fprintf(stderr, "tar: temporary file %s: ", temp_file);
			fflush(stderr);
			perror("");
			done(2);
		}
		buf = malloc(MBUFSIZE);
		if (buf == NULL) {
			fprintf(stderr, "tar: Out of memory\n");
			done(2);
		}

		while ((n = read(0, buf, sizeof(buf), stdin)) > 0) {
			write(f, buf, n);
		}
		free(buf);
		close(f);
		file = temp_file;
		delete = 1;
	}

	/*
	 * Determine the file size and allocate enough memory to hold
	 * everything.
	 */

	if (stat(file, &sb) < 0) {
		fprintf(stderr, "tar: %s: ",
			delete ? "could not reread temporary file" : file);
		fflush(stderr);
		perror("");
		if (delete) {
			unlink(file);
		}
		done(5);
	}

	size = sb.st_size;
	strings = malloc(size + 1);
	if (strings == NULL) {
		fprintf(stderr, "tar: Out of memory\n");
		done(2);
	}
	strings[size] = '\0';

	/*
	 * Read the file and determine the number of lines. Allocate the
	 * vector.
	 */

	f = open(file, O_RDONLY);
	if (f == 0) {
		fprintf(stderr, "tar: %s: ",
			delete ? "could not reread temporary file" : file);
		fflush(stderr);
		perror("");
		if (delete) {
			unlink(file);
		}
		done(5);
	}

	n = read(f, strings, size);
	if (n < 0) {
		fprintf(stderr, "tar: error reading file %s: ",
			delete ? "(temporary file)" : file);
		fflush(stderr);
		perror("");
		if (delete) {
			unlink(file);
		}
		done(5);
	}

	s = strings;
	nlines = 0;
	while (*s) {
		if (*s == '\n') {
			nlines++;
		}
		s++;
	}

	a = (char **)malloc((nlines + 1) * (sizeof(char *)));
	if (a == NULL) {
		fprintf(stderr, "tar: Out of memory\n");
		done(2);
	}

	/*
	 * Go through the data, replacing newlines with nulls and setting
	 * up the vector.
	 */

	*argv = a;
	s = strings;
	nlines = 1;
	a[0] = s;
	while (*s) {
		if (*s == '\n') {
			*s = '\0';
			a[nlines++] = s + 1;
		}
		s++;
	}

	a[nlines - 1] = NULL;
	if (delete) {
		unlink(file);
	}
}
