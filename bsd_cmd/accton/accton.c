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
#ident	"$Header: accton.c,v 1.1.2.4 90/05/07 18:01:53 wje Exp $"


#include <stdio.h>
#include <sys/signal.h>

int onsys();

main(argc, argv)
char **argv;
{
	extern errno;

	(void) signal(SIGSYS, onsys);
	if (argc > 1)
	{
#ifdef RISCOS
		/* map a supplied arg of usr/adm/acct to usr/adm/pacct
		 * to make the change in the structure of the acctg dirs
		 * in RISC/os 4.50 transparent
		 */
		if (strcmp(argv[1],"/usr/adm/acct") == 0 )
		{
		    acct("/usr/adm/pacct");
		}
		else
		    acct(argv[1]);
#else
		acct(argv[1]);
#endif
	}
	else
	/* no arg means turn it off */
		acct((char *)0);
	if (errno) {
		perror("accton");
		exit(1);
	}
	exit(0);
}

int
onsys()
{
	fprintf(stderr, "acct: Accounting is not enabled in this system.\n");
	exit(1);
}
