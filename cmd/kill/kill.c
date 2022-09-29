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
#ident	"$Header: kill.c,v 1.7.2.2 90/05/09 16:17:53 wje Exp $"

/*	Copyright (c) 1984 AT&T	*/
/*	  All Rights Reserved  	*/

/*	THIS IS UNPUBLISHED PROPRIETARY SOURCE CODE OF AT&T	*/
/*	The copyright notice above does not evidence any   	*/
/*	actual or intended publication of such source code.	*/

/*
**	kill - send signal to process
*/

#include	<stdio.h>
#include	<signal.h>
#include	<sys/errno.h>

extern void exit();

main(argc, argv)
char **argv;
{
	register signo, pid, res;
	int	errlev = 0, neg = 0, zero = 0;
	extern	errno;
	char	*msg;

	if (argc > 1 && *argv[1] == '-') {
		if (argv[1][1] < '0' || argv[1][1] > '9') {
			usage();
		}
		signo = atoi(argv[1]+1);
		argc--;
		argv++;
	} else
		signo = SIGTERM;
	argv++;
	if (argc <= 1) {
		usage();
	}
	while (argc > 1) {
		if (**argv == '-') neg++;
		if (**argv == '0') zero++;
		pid = atoi(*argv);
		if (	((pid == 0) && !zero)
		     || ((pid < 0) && !neg)
		     || (pid > 32000)
		     || (pid < -32000)
		      ) usage();
		res = kill(pid, signo);
		if (res<0) {
			if(pid <= 0) {
				pid = abs(pid);
				msg = "not a killable process group";
			}
			else if (errno == EPERM)
				msg = "permission denied";
			else if (errno == EINVAL)
				msg = "invalid signal";
			else msg = "no such process";
			fprintf(stderr,"kill: %d: %s\n", pid, msg);
			errlev = 2;
		}
		argc--;
		argv++;
		neg = zero = 0;
	}
	return(errlev);
}
usage()
{
	fprintf(stderr, "usage: kill [ -signo ] pid ...\n");
	exit(2);
}
