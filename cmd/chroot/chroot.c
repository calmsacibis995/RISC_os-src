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
#ident	"$Header: chroot.c,v 1.5.2.2 90/05/09 15:22:39 wje Exp $"

/*	Copyright (c) 1984 AT&T	*/
/*	  All Rights Reserved  	*/

/*	THIS IS UNPUBLISHED PROPRIETARY SOURCE CODE OF AT&T	*/
/*	The copyright notice above does not evidence any   	*/
/*	actual or intended publication of such source code.	*/

# include <stdio.h>
# include <errno.h>

main(argc, argv)
char **argv;
{
	extern char *sys_errlist[];
	extern int sys_nerr;

	if(argc < 3) {
		printf("usage: chroot rootdir command arg ...\n");
		exit(1);
	}
	argv[argc] = 0;
	if(argv[argc-1] == (char *) -1) /* catches potential problems in
					 old 16 bit implimentations */
		argv[argc-1] = (char *) -2;
	if (chroot(argv[1]) < 0) {
		perror(argv[1]);
		exit(1);
	}
	if (chdir("/") < 0) {
		printf("Can't chdir to new root\n");
		exit(1);
	}
	execv(argv[2], &argv[2]);
	if((errno > 0) && (errno <= sys_nerr)) 
		printf("chroot: %s\n",sys_errlist[errno]);
	else printf("chroot: exec failed, errno = %d\n",errno);
	exit(1);
}
