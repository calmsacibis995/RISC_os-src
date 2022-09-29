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
#ident	"$Header: clri.c,v 1.5.1.2 90/05/09 15:51:42 wje Exp $"

	/* @(#)clri.c	1.2 88/03/08 4.0NFSSRC; from 1.8 88/02/07 SMI */
	/* from UCB 2.2 4/11/82 */
/*
 * clri filsys inumber ...
 */

#ifndef SIMFS
#include <sys/param.h>
#include <sys/time.h>
#include <sys/vnode.h>
#include <ufs/inode.h>
#include <ufs/fs.h>
#else
#include "../h/param.h"
#include "../h/time.h"
#include "../h/vnode.h"
#include "../ufs/inode.h"
#include "../ufs/fs.h"
#endif

#define ISIZE	(sizeof(struct dinode))
#define	NI	(MAXBSIZE/ISIZE)

struct	dinode	buf[NI];

union {
	char		dummy[SBSIZE];
	struct fs	sblk;
} sb_un;
#define sblock sb_un.sblk

int	status;

main(argc, argv)
	int argc;
	char *argv[];
{
	register i, f;
	unsigned n;
	int j, k;
	long off;
	long gen;

	if (argc < 3) {
		printf("usage: clri filsys inumber ...\n");
		exit(4);
	}
	f = open(argv[1], 2);
	if (f < 0) {
		printf("cannot open %s\n", argv[1]);
		exit(4);
	}
	lseek(f, SBLOCK * DEV_BSIZE, 0);
	if (read(f, &sblock, SBSIZE) != SBSIZE) {
		printf("cannot read %s\n", argv[1]);
		exit(4);
	}
	if (sblock.fs_magic != FS_MAGIC) {
		printf("bad super block magic number\n");
		exit(4);
	}
	for (i = 2; i < argc; i++) {
		if (!isnumber(argv[i])) {
			printf("%s: is not a number\n", argv[i]);
			status = 1;
			continue;
		}
		n = atoi(argv[i]);
		if (n == 0) {
			printf("%s: is zero\n", argv[i]);
			status = 1;
			continue;
		}
		off = fsbtodb(&sblock, itod(&sblock, n)) * DEV_BSIZE;
		lseek(f, off, 0);
		if (read(f, (char *)buf, sblock.fs_bsize) != sblock.fs_bsize) {
			printf("%s: read error\n", argv[i]);
			status = 1;
		}
	}
	if (status)
		exit(status);
	for (i = 2; i < argc; i++) {
		n = atoi(argv[i]);
		printf("clearing %u\n", n);
		off = fsbtodb(&sblock, itod(&sblock, n)) * DEV_BSIZE;
		lseek(f, off, 0);
		read(f, (char *)buf, sblock.fs_bsize);
		j = itoo(&sblock, n);
		gen = buf[j].di_gen;
		bzero((caddr_t)&buf[j], ISIZE);
		buf[j].di_gen = gen + 1;
		lseek(f, off, 0);
		write(f, (char *)buf, sblock.fs_bsize);
	}
	exit(status);
}

isnumber(s)
	char *s;
{
	register c;

	while(c = *s++)
		if (c < '0' || c > '9')
			return(0);
	return(1);
}
