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
 * |-----------------------------------------------------------| */
#
# ident "$Header: biod.c,v 2.4.2.2.1.2 90/08/03 11:13:29 hawkes Exp $ "
#

#define sgi	1

#include <stdio.h>
#ifdef sgi
#include <bsd/sys/types.h>
#endif
#include <bsd/sys/file.h>
#include <bsd/sys/ioctl.h>

/*
 * This is the NFS asynchronous block I/O daemon
 */

main(argc, argv)
	int argc;
	char *argv[];
{
	extern int errno;
	int pid;
	int count;

	if (argc > 2) {
		usage(argv[0]);
	}

	if (argc == 2) {
		count = atoi(argv[1]);
		if (count < 0) {
			usage(argv[0]);
		}
	} else {
		count = 1;
	}

	{ int tt = open("/dev/tty", O_RDWR);
		if (tt > 0) {
			ioctl(tt, TIOCNOTTY, 0);
			close(tt);
		}
	}
	while (count--) {
		pid = fork();
		if (pid == 0) {
			async_daemon();		/* Should never return */
			fprintf(stderr, "%s: async_daemon ", argv[0]);
			perror("");
			exit(1);
		}
		if (pid < 0) {
			fprintf(stderr, "%s: cannot fork", argv[0]);
			perror("");
			exit(1);
		}
	}
}

usage(name)
	char	*name;
{

	fprintf(stderr, "usage: %s [<count>]\n", name);
	exit(1);
}
