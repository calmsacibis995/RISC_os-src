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
#ident	"$Header: main.c,v 1.10.2.2 90/05/09 15:12:28 wje Exp $"

/*	Copyright (c) 1984 AT&T	*/
/*	  All Rights Reserved  	*/

/*	THIS IS UNPUBLISHED PROPRIETARY SOURCE CODE OF AT&T	*/
/*	The copyright notice above does not evidence any   	*/
/*	actual or intended publication of such source code.	*/

#include <stdio.h>
#include <ctype.h>
#include <signal.h>
#include "awk.h"
#include "y.tab.h"

int	dbg	= 0;
int	svargc;
uchar	**svargv;
uchar	*cmdname;	/* gets argv[0] for error messages */
extern	FILE *yyin;	/* lex input file */
uchar	*lexprog;	/* points to program argument if it exists */
extern	int errorflag;	/* non-zero if any syntax errors; set by yyerror */
int	compile_time = 1;	/* 0 when machine starts.  for error printing */

main(argc, argv)
	int argc;
	uchar *argv[];
{
	uchar *progfile = NULL, *progarg = NULL, *fs = NULL, *freezename = NULL;
	extern int fpecatch();
	int c,i;
	extern char *optarg;
	extern int optind;
	int argindx;

	cmdname = argv[0];
	if (argc == 1) {
		usage();
		exit(2);
	}
	yyin = NULL;
	while ((c=getopt(argc,argv,"df:F:")) != -1) {
	    switch (c)  {
		case 'f': /* next arg is program filename */
		    if ((yyin = fopen(optarg, "r")) == NULL)  {
			perror(optarg);
			exit(2);
		    }
		    progfile = optarg;
		    break;
		case 'F':	/* set field separator */
		    if (strlen(optarg) > 1)  {  /* not a character */
			usage();
		        exit(2);
		    }
		    fs = (uchar *)optarg;
		    break;
		case 'd':
		    dbg = 1;
		    break;
		case '?':
		    usage();
		    exit(2);
	    }
	}

	argc = argc - optind + 1;
	if (yyin == NULL) {	/* no -f; first argument is program */
		if (argv[optind] == NULL)  {
			usage();
			exit(2);
		}
		dprintf("program = |%s|\n", argv[optind], NULL, NULL);
		progarg = lexprog = argv[optind];
		argc--;
/*
  		argv++;  
 */
		optind++;
	}
	while (argc > 1) {	/* do leading "name=val" */
		if (!isclvar(argv[optind]))
			break;
		setclvar(argv[optind]);
		argc--;
/*
		argv++;
 */
		optind++;

	}
	argv[0] = cmdname;	/* put prog name at front of arglist */
	svargc = argc;
	svargv = argv;
	dprintf("svargc=%d, svargv[0]=%s\n", svargc, svargv, NULL);
	for (i=0; i < argc; i++) svargv[i+1]=argv[optind+i];
	syminit(svargc, svargv);
	if (fs)
		*FS = tostring(fs);
	*FILENAME = svargv[1];	/* initial file name */
	if (argc == 1) {	/* no filenames; use stdin */
		initgetrec();
	}
	signal(SIGFPE, fpecatch);
	yyparse();
	dprintf("errorflag=%d\n", errorflag, NULL, NULL);
	if (errorflag == 0) {
		compile_time = 0;
		run(winner);
	} else
		bracecheck();
	exit(errorflag);
}

usage()
{
	fprintf(stderr, "Usage: %s [-d] [-Fc]  commands [parameters] [file...]\n", cmdname);
	fprintf(stderr, "       %s [-d] [-Fc] -f script [parameters] [file...]\n", cmdname);
}
