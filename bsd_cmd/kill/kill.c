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
#ident	"$Header: kill.c,v 1.2.1.3 90/05/07 18:42:30 wje Exp $"

static	char *sccsid = "@(#)kill.c	4.4 (Berkeley) 4/20/86";
/*
 * kill - send signal to process
 */

#include <signal.h>
#include <ctype.h>

#ifdef RISCOS
char *signm[] = { 0,
"HUP", "INT", "QUIT", "ILL", "TRAP", "IOT", "EMT", "FPE",	/* 1-8 */
"KILL", "BUS", "SEGV", "SYS", "PIPE", "ALRM", "TERM", "USR1",	/* 9-16 */
"USR2", "CHLD", "XFSZ", "STOP", "TSTP", "POLL", "IO", "URG",	/* 17-24 */
"WINCH", "VTALRM", "PROF", "CONT", "TTIN", "TTOU", "LOST", 0	/* 25-31 */
};

/* Some alternative names */
char *signm2[] = { 0,
0, 0, 0, 0, 0, "ABRT", "XCPU", 0,	/* 1-8 */
0, 0, 0, 0, 0, 0, 0, 0,			/* 9-16 */
0, "CLD", "PWR", 0, 0, 0, 0, 0,		/* 17-24 */
0, 0, 0, 0, 0, 0, 0, 0			/* 25-31 */
};
#else
char *signm[] = { 0,
"HUP", "INT", "QUIT", "ILL", "TRAP", "IOT", "EMT", "FPE",	/* 1-8 */
"KILL", "BUS", "SEGV", "SYS", "PIPE", "ALRM", "TERM", "URG",	/* 9-16 */
"STOP", "TSTP", "CONT", "CHLD", "TTIN", "TTOU", "IO", "XCPU",	/* 17-24 */
"XFSZ", "VTALRM", "PROF", "WINCH", 0, "USR1", "USR2", 0,	/* 25-31 */
};
#endif

main(argc, argv)
char **argv;
{
	register signo, pid, res;
	int errlev;
	extern char *sys_errlist[];
	extern errno;

	errlev = 0;
	if (argc <= 1) {
	usage:
		printf("usage: kill [ -sig ] pid ...\n");
		printf("for a list of signals: kill -l\n");
		exit(2);
	}
	if (*argv[1] == '-') {
		if (argv[1][1] == 'l') {
			for (signo = 0; signo <= NSIG; signo++) {
				if (signm[signo])
					printf("%s ", signm[signo]);
				if (signo == 16)
					printf("\n");
			}
			printf("\n");
			exit(0);
		} else if (isdigit(argv[1][1])) {
			signo = atoi(argv[1]+1);
			if (signo < 0 || signo > NSIG) {
				printf("kill: %s: number out of range\n",
				    argv[1]);
				exit(1);
			}
		} else {
			char *name = argv[1]+1;

			for (signo = 0; signo <= NSIG; signo++)
#ifdef RISCOS
			     if ((signm[signo] && !strcmp(signm[signo],name))||
				 (signm2[signo] && !strcmp(signm2[signo],name)))
#else
			     if (signm[signo] && !strcmp(signm[signo],name))
#endif
					goto foundsig;

			printf("kill: %s: unknown signal; kill -l lists signals\n", name);
			exit(1);
foundsig:
			;
		}
		argc--;
		argv++;
	} else
		signo = SIGTERM;
	argv++;
	while (argc > 1) {
		if (!(isdigit(**argv) || **argv == '-'))
			goto usage;
		res = kill(pid = atoi(*argv), signo);
		if (res<0) {
			printf("%u: %s\n", pid, sys_errlist[errno]);
			errlev = 1;
		}
		argc--;
		argv++;
	}
	return(errlev);
}
