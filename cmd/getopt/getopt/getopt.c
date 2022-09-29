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
#ident	"$Header: getopt.c,v 1.5.2.2 90/05/09 16:06:48 wje Exp $"

/*	Copyright (c) 1984 AT&T	*/
/*	  All Rights Reserved  	*/

/*	THIS IS UNPUBLISHED PROPRIETARY SOURCE CODE OF AT&T	*/
/*	The copyright notice above does not evidence any   	*/
/*	actual or intended publication of such source code.	*/


#include	<stdio.h>

extern void exit();
extern char *strcat();
extern char *strchr();

main(argc, argv)
int argc;
char **argv;
{
	extern	int optind;
	extern	char *optarg;
	register int	c;
	int	errflg = 0;
	char	tmpstr[4];
	char	outstr[5120];
	char	*goarg;

	if(argc < 2) {
		fputs("usage: getopt legal-args $*\n", stderr);
		exit(2);
	}

	goarg = argv[1];
	argv[1] = argv[0];
	argv++;
	argc--;

	while((c=getopt(argc, argv, goarg)) != EOF) {
		if(c=='?') {
			errflg++;
			continue;
		}

		tmpstr[0] = '-';
		tmpstr[1] = c;
		tmpstr[2] = ' ';
		tmpstr[3] = '\0';

		strcat(outstr, tmpstr);

		if(*(strchr(goarg, c)+1) == ':') {
			strcat(outstr, optarg);
			strcat(outstr, " ");
		}
	}

	if(errflg) {
		exit(2);
	}

	strcat(outstr, "-- ");
	while(optind < argc) {
		strcat(outstr, argv[optind++]);
		strcat(outstr, " ");
	}

	(void) printf("%s\n", outstr);
	exit(0);	/*NOTREACHED*/
}
