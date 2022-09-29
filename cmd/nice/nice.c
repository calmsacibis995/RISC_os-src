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
#ident	"$Header: nice.c,v 1.5.2.2 90/05/09 18:13:47 wje Exp $"

/*	Copyright (c) 1984 AT&T	*/
/*	  All Rights Reserved  	*/

/*	THIS IS UNPUBLISHED PROPRIETARY SOURCE CODE OF AT&T	*/
/*	The copyright notice above does not evidence any   	*/
/*	actual or intended publication of such source code.	*/

/*
**	nice
*/


#include	<stdio.h>
#include	<ctype.h>

main(argc, argv)
int argc;
char *argv[];
{
	int	nicarg = 10;
	extern	errno;
	extern	char *sys_errlist[];

	if(argc > 1 && argv[1][0] == '-') {
		register char	*p = argv[1];

		if(*++p != '-') {
			--p;
		}
		while(*++p)
			if(!isdigit(*p)) {
				fprintf(stderr, "nice: argument must be numeric.\n");
				exit(2);
			}
		nicarg = atoi(&argv[1][1]);
		argc--;
		argv++;
	}
	if(argc < 2) {
		fprintf(stderr, "nice: usage: nice [-num] command\n");
		exit(2);
	}
	nice(nicarg);
	execvp(argv[1], &argv[1]);
	fprintf(stderr, "%s: %s\n", sys_errlist[errno], argv[1]);
	exit(2);
}
