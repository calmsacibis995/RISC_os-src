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
#ident	"$Header: mkfile.c,v 1.1.1.3 90/05/09 19:13:40 wje Exp $"
#ifndef lint
static  char sccsid[] = "@(#)mkfile.c	1.2 88/07/26 4.0NFSSRC; from 1.3 88/02/08 SMI";
#endif

/*
 * Copyright (c) 1986 by Sun Microsystems, Inc.
 */

#include <stdio.h>
#include <ctype.h>
#include <sys/file.h>

#define	MIN(a,b)	((a) < (b) ? (a) : (b))

int verbose = 0;
int nobytes = 0;

main(argc, argv)
	char **argv;
{
	int fd;
	char *opts;
	int size;
	int mult;
	int bytes;
	int wrote;
	int len;
	char buf[8192];
	int exitval;

	exitval = 0;

	if (argc == 1)
		usage();
	
	while (argv[1][0] == '-') {
		opts = &argv[1][0];
		while (*(++opts)) {
			switch(*opts) {
			case 'v':
				verbose++;
				break;
			case 'n':
				nobytes++;
				break;
			default:
				usage();
			}
		}
		argc--;
		argv++;
	}
	if (argc < 3)
		usage();

	mult = 1;
	len = strlen(argv[1]);
	if (isalpha(argv[1][len-1])) {
		switch (argv[1][len-1]) {
		case 'k':
		case 'K':
			mult = 1024;
			break;
		case 'b':
		case 'B':
			mult = 512;
			break;
		case 'm':
		case 'M':
			mult = 1048576;
			break;
		default:
			fprintf(stderr, "unknown size %s\n", argv[1]);
			usage();
		}
		argv[1][len-1] = '\0';
	}
	size = atoi(argv[1]) * mult;

	argv++;
	argc--;

	while (argc > 1) {
		if (verbose)
			fprintf(stdout, "%s %d bytes\n", argv[1], size);
		fd = open(argv[1], O_CREAT|O_TRUNC|O_RDWR, 01600);
		if (fd < 0) {
			perror(argv[1]);
			close(fd);
			exitval++;
			argv++;
			argc--;
			continue;
		}
		if (lseek(fd, size-1, 0) < 0) {
			perror(argv[1]);
			close(fd);
			exitval++;
			argv++;
			argc--;
			continue;
		} else if (write(fd, "", 1) != 1) {
			perror(argv[1]);
			close(fd);
			exitval++;
			argv++;
			argc--;
			continue;
		}

		if (!nobytes) {
			wrote = 0;
			if (lseek(fd, 0, 0) < 0) {
				perror(argv[1]);
				close(fd);
				exitval++;
				argv++;
				argc--;
				continue;
			}
			while (wrote < size) {
				bytes = MIN(sizeof(buf), size-wrote);
				if (write(fd, buf, bytes) != bytes) {
					perror(argv[1]);
					exitval++;
					break;
				}
				wrote += bytes;
			}
		}
		close(fd);
		argv++;
		argc--;
	}
    exit(exitval);
}

usage()
{
	fprintf(stderr, "mkfile: [-vn] <size> <name1> [<name2>] ...\n");
	exit(1);
}
