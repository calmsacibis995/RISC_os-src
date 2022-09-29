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
#ident	"$Header: devinfo.c,v 1.7.2.2 90/05/09 15:41:41 wje Exp $"

/*	Copyright (c) 1984 AT&T	*/
/*	  All Rights Reserved  	*/

/*	THIS IS UNPUBLISHED PROPRIETARY SOURCE CODE OF AT&T	*/
/*	The copyright notice above does not evidence any   	*/
/*	actual or intended publication of such source code.	*/

#include <stdio.h>
#include <fcntl.h>
#include <sys/types.h>
#include <sys/stat.h>
#include <sys/dvh.h>
#include <sys/dkio.h>
 
#define BLKSIZE 512
#define DEFECTSZ 64
#define DEVICE	"/dev/rdsk/"
#define BACKUP	6
#define BOOT	7
#define FIOCTL	1
#define DRERR	2
#define FILERR	2
#define OPENERR	2
#define WRITERR 2

struct io_arg args;
extern int errno;
char *ProgName;
struct stat statbuf;

main(argc, argv)
	int argc;
	char **argv;
{
	register struct volume_header vh;
	extern char optarg;
	extern int optind, opterr;
	int errflg, exiterr, iflg, pflg, fd, i, c;
	char *device;
	iflg = 0;
	pflg = 0;
	errflg = 0;

	if ((ProgName = (char *)strrchr (*argv, '/')) == NULL) {
		ProgName = *argv;
	}
	else {
		ProgName++;
	}
	
	if (argc > 1) {
		while ((c = getopt(argc, argv, "ip")) != EOF) {
			switch (c) {
			case 'i':
				iflg = 1;
				i++;
				break;
			case 'p':
				pflg = 1;
				i++;
				break;
			case '?':
				errflg = 1;
				break;
			}
		}
	}
	else 
		errflg = 1;

	argv+=i;
	argc-=i;
	if ((argc == 1) || errflg) {
		fprintf(stderr,"Usage: %s [-i] [-p] device...\n",ProgName);
		exit(2);
	}

	if ((iflg == 0) && (pflg == 0)) pflg = 1;	/* default: pflg*/
	while (argc-- > 1) {
		device = *++argv;
		if ((fd = open(device,O_RDONLY)) < 0) {
			perror (device);
			exit(OPENERR);
		}

		if (stat(device,&statbuf) < 0) {
			perror(device);
			exit(FILERR);
		}
		else 
			if ((statbuf.st_mode & S_IFMT) != S_IFCHR) {
				fprintf(stderr,"%s: Not a raw device\n",device);
				exit(FILERR);
			}

	/* READ SECTOR 0 */

		args.sectst = 0x00;
		args.datasz = sizeof(vh);
		args.memaddr = (unsigned long)&vh;

		if ((ioctl (fd, DIOCGETVH, &args)) == -1) {
			perror (device);
			exit(FIOCTL);
		}

		if(iflg)
			devinfo(&vh, fd, device);
		if(pflg)
			partinfo(&vh, fd, device);
		close(fd);
	}
		exit(0);
}

partinfo (pvh, ofd, device)
	register struct volume_header *pvh;
	char *device;
	int ofd;
{
	int i;
	unsigned int startblock, noblocks, flag, tag, major, slice;

	major = 0;
	slice = 0;
	startblock = 0;
	noblocks = 0;

/*  already got stat above
	i = stat(device, &statbuf);
	if(i < 0) {
		perror (device);
		exit(FILERR);
	}
*/
	major = (statbuf.st_rdev >> 8) & 0xff;
	slice = statbuf.st_rdev & 0xff;
	startblock = pvh->vh_pt[slice].pt_firstlbn;
	noblocks = pvh->vh_pt[slice].pt_nblks;
	flag = pvh->vh_pt[slice].pt_type;
	tag = 0;
	fprintf(stdout,"%s	%x	%x	%d	%d	%x	%x\n", 
		device, major, slice, startblock, noblocks, flag, tag);
}

devinfo(pvh, ofd, device)
	register struct volume_header *pvh;
	char *device;
	int ofd;
{
	int i;
	unsigned int nopartitions, sectorcyl, version, driveid, bytes;

	nopartitions = 0;
	sectorcyl = 0;
	bytes = 0;
	version = 0;

	sectorcyl = pvh->vh_dp.dp_secs * pvh->vh_dp.dp_trks0;
	bytes = pvh->vh_dp.dp_secbytes;
	driveid = 0;
	version = 0;

	for (i=0; i < NPARTAB; i++) {
		if(pvh->vh_pt[i].pt_nblks > 0) {
			nopartitions++;
		}
	}
	fprintf(stdout,"%s	%0x	%0x	%d	%d	%d\n", 
		device, version, driveid, sectorcyl, bytes, nopartitions);
}
