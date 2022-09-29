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
#ident	"$Header: mkboottape.c,v 1.3.2.2 90/05/09 16:47:40 wje Exp $"

/*
 * mkboottape [-f FILE_OR_DEVICE] file_list
 * exit(99) means major error
 * exit(<99) means error on individual file
 */

#define	LANGUAGE_C	1

#include "tpd.h"
#include <sys/types.h>
#include <sys/stat.h>
#include <sys/file.h>
#ifdef SYSV
#include <sys/fcntl.h>
#endif SYSV
#include <stdio.h>

#ifdef SYSV
char *tapedev = "/dev/rmt/m0";
#define index strchr
#define rindex strrchr
#define bzero(s,n)	memset(s, '\0', n)
#else
char *tapedev = "/dev/rmt8";
#endif SYSV

char *progname;
int errflg;
union tp_header th;

#define	MAXPATH		256

struct filelist {
	char name[TP_NAMESIZE];
	char fullname[MAXPATH];
	int len;
} filelist[TP_NENTRIES];

union endian {
	char c[4];
	int i;
} endian;
int leflag;	/* non-zero if little-endian */
#define	TO_BE(x)	(leflag ? nuxi(x) : x)

main(argc, argv)
int argc;
register char **argv;
{
	struct stat st;
	register struct filelist *fp, *fpt;
	register struct tp_entry *te;
	char namebuf[TP_NAMESIZE];
	char buf[TP_BLKSIZE];
	char *cp;
	int fd;
	int tfd;
	int type;
	int curblock;
	int bytes;
	extern char *rindex();

	init_endian();
#ifdef DEBUG
	fprintf(stderr, "this is a %s-endian machine\n",
	    leflag ? "little" : "big");
#endif
	fp = filelist;
	progname = *(argv++);
	argc--;

	/*
	 * process flags
	 */
	for (; argc > 0 && **argv == '-'; argc--, argv++) {

		switch ((*argv)[1]) {
		case 'f':
			if (--argc <= 0) {
				fprintf(stderr, "%s: -f DEV_OR_FILE\n");
				exit(99);
			}
			tapedev = *++argv;
			break;
		
		default:
			fprintf(stderr, "%s: unknown option: %s\n",
			    progname, *argv);
			exit(99);
		}
	}
	for (; argc > 0; argc--, argv++) {
		/*
		 * open file, stat it, add vitals to filelist
		 */
		fd = open(*argv, O_RDONLY);
		if (fd < 0) {
			fprintf(stderr, "%s: Can't open %s\n",
			    progname, *argv);
			errflg++;
			continue;
		}
		if (fstat(fd, &st) < 0) {
			fprintf(stderr, "%s: Can't stat %s\n",
			    progname, *argv);
			goto err;
		}
		type = st.st_mode & S_IFMT;
		if (type != S_IFREG) {
			fprintf(stderr, "%s: Not a regular file: %s\n",
			    progname, *argv);
			goto err;
		}
		cp = rindex(*argv, '/');
		if (cp == NULL)
			cp = *argv;
		else
			cp++;		/* skip slash */
		if (strlen(*argv) >= MAXPATH-1) {
			fprintf(stderr, "%s: Pathname too long: %s\n",
			    progname, *argv);
			goto err;
		}
		if (strlen(cp) >= TP_NAMESIZE) {
			strncpy(namebuf, *argv, TP_NAMESIZE-1);
			namebuf[TP_NAMESIZE-1] = 0;
			cp = namebuf;
			fprintf(stderr,
			   "%s: WARNING: filename %s truncated to %s\n",
			   progname, *argv, cp);
			goto err;
		}
		for (fpt = filelist; fpt < fp; fpt++)
			if (strcmp(cp, fpt->name) == 0) {
				fprintf(stderr,
				    "%s: Duplicate file name: %s\n",
				    progname, cp);
				goto err;
			}
		if (fp >= &filelist[TP_NENTRIES]) {
			fprintf(stderr,
			    "%s: Directory overflow: %s not on tape\n",
			    progname, *argv);
err:
			errflg++;
			close(fd);
			continue;
		}
		strcpy(fp->fullname, *argv);
		strcpy(fp->name, cp);
		fp->len = st.st_size;
		fp++;
		close(fd);
	}
	if (fp == filelist) {
		/*
		 * we assume this wasn't really intended
		 */
		fprintf(stderr, "%s: no files specified\n", progname);
		exit(99);
	}
	/*
	 * build tape directory
	 */
	bzero(&th, sizeof(th));
	th.th_td.td_magic = TO_BE(TP_MAGIC);
	te = th.th_td.td_entry;
	curblock = 1;	/* start data blocks after directory */
	for (fpt = filelist; fpt < fp; fpt++, te++) {
		strcpy(te->te_name, fpt->name);
		te->te_nbytes = TO_BE(fpt->len);
		te->te_lbn = TO_BE(curblock);
		curblock += (fpt->len + TP_BLKSIZE - 1) / TP_BLKSIZE;
	}
	th.th_td.td_cksum = cksum(&th, sizeof(th));
	th.th_td.td_cksum = TO_BE(th.th_td.td_cksum);
	/*
	 * open tape and write directory
	 */
	if ((tfd = open(tapedev, O_RDWR|O_CREAT, 0666)) < 0) {
		fprintf(stderr, "%s: Can't open %s\n", progname, tapedev);
		exit(99);
	}
	if (write(tfd, &th, sizeof(th)) != sizeof(th)) {
		fprintf(stderr, "%s: Can't write to %s\n", progname, tapedev);
		exit(99);
	}
	/*
	 * scan filelist and write files
	 */
	for (fpt = filelist; fpt < fp; fpt++) {
#ifdef DEBUG
		printf ("Processing %s\n", fpt->fullname);
#endif DEBUG		
		if ((fd = open(fpt->fullname, O_RDONLY)) < 0) {
			fprintf(stderr, "%s: Couldn't reopen %s\n",
			    progname, fpt->fullname);
			exit(99);
		}
		while (fpt->len) {
			bytes = fpt->len > TP_BLKSIZE ? TP_BLKSIZE : fpt->len;
			if (bytes != TP_BLKSIZE)
				bzero(buf, TP_BLKSIZE);
			if (read(fd, buf, bytes) != bytes) {
				fprintf(stderr, "%s: Can't read %s\n",
				    progname, fpt->fullname);
				exit(99);
			}
			if (write(tfd, buf, TP_BLKSIZE) != TP_BLKSIZE) {
				fprintf(stderr, "%s: Write to %s failed\n",
				    progname, tapedev);
				exit(99);
			}
			fpt->len -= bytes;
		}
		close(fd);
	}
	close(tfd);
	exit(errflg);
}

init_endian()
{
	endian.i = 0x01020304;
	leflag = (endian.c[0] == 4);
}

cksum(ip, bcnt)
register int *ip;
int bcnt;
{
	register sum;
	register int *ep;

	if (bcnt & 0x3) {
		fprintf(stderr, "%s: Internal error: bad tpd size\n",
		    progname);
		exit(99);
	}
	ep = ip + (bcnt >> 2);
	sum = 0;
	while (ip < ep) {
		sum += TO_BE(*ip);
		ip++;
	}
	return(-sum);
}

nuxi(x)
	unsigned x;
{
	return(
	    ((x&0xff)<<24)
	   |((x&0xff00)<<8)
	   |((x&0xff0000)>>8)
	   |((x&0xff000000)>>24)
	);
}
