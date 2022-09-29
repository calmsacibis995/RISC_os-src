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
#ident	"$Header: refer1.c,v 1.1.2.2 90/05/07 19:12:32 wje Exp $"

#include <signal.h>
#include "refer..c"

main(argc,argv)		/* process command-line arguments */
char *argv[];
{
	char line[BUFSIZ], *s;
	int nodeflt = 0;

	signals();
	while (argv[1] && argv[1][0] == '-') {
		switch(argv[1][1]) {
		case 'e':
			endpush++; 
			break;
		case 's':
			sort++;
			endpush = 1;
			if (argv[1][2])
				keystr = argv[1]+2;
			break;
		case 'l': 
			labels++;
			s = argv[1]+2;
			nmlen = atoi(s);
			while (*s)
				if (*s++ == ',')
					break;
			dtlen = atoi(s);
			break;
		case 'k':
			keywant = (argv[1][2] ? argv[1][2] : 'L');
			labels++;
			break;
		case 'n':
			nodeflt = 1;
			break;
		case 'p':
			argc--; 
			argv++;
			*search++ = argv[1];
			if (search-rdata > NSERCH)
				err("too many -p options (%d)", NSERCH);
			break;
		case 'a':
			authrev = atoi(argv[1]+2);
			if (authrev<=0)
				authrev = 1000;
			break;
		case 'b':
			bare = (argv[1][2] == '1') ? 1 : 2;
			break;
		case 'c':
			smallcaps = argv[1]+2;
			break;
		case 'f':
			refnum = atoi(argv[1]+2) - 1;
			break;
		case 'B':
			biblio++;
			bare = 2;
			if (argv[1][2])
				convert = argv[1]+2;
			break;
		case 'S':
			science++;
			labels = 1;
			break;
		case 'P':
			postpunct++;
			break;
		}
		argc--; 
		argv++;
	}
	if (getenv("REFER") != NULL)
		*search++ = getenv("REFER");
	else if (nodeflt == 0)
		*search++ = "/usr/dict/papers/Ind";
	if (!labels) {
		sprintf(ofile, "/tmp/rj%db", getpid());
		ftemp = fopen(ofile, "w");
		if (ftemp == NULL) {
			fprintf(stderr, "Can't open scratch file\n");
			exit(1);
		}
	}
	if (endpush) {
		sprintf(tfile, "/tmp/rj%da", getpid());
		fo = fopen(tfile, "w");
		if (fo == NULL) {
			fo = ftemp;
			fprintf(stderr, "Can't open scratch file");
		}
		sep = 002; /* separate records without confusing sort..*/
	} else 
		fo = ftemp;
	do {
		if (argc > 1) {
			fclose(in);
			Iline = 0;
			in = fopen(Ifile = argv[1], "r");
			argc--; 
			argv++;
			if (in == NULL) {
				err("Can't read %s", Ifile);
				continue;
			}
		}
		while (input(line)) {
			Iline++;
			if (biblio && *line == '\n')
				doref(line);
			else if (biblio && Iline == 1 && *line == '%')
				doref(line);
			else if (!prefix(".[", line))
				output(line);
			else
				doref(line);
		}
	} while (argc > 1);

	if (endpush && fo != NULL)
		dumpold();
	output("");
	if (!labels)
		recopy(ofile);
	clfgrep();
	cleanup();
	exit(0);
}

extern int intr();

signals()
{
	if (signal(SIGINT, SIG_IGN) != SIG_IGN)
		signal(SIGINT, intr);
	signal(SIGHUP, intr);
	signal(SIGPIPE, intr);
	signal(SIGTERM, intr);
}

intr()
{
	signal(SIGINT, SIG_IGN);
	cleanup();
	exit(1);
}

cleanup()
{
	if (tfile[0])
		unlink(tfile);
	if (gfile[0])
		unlink(gfile);
	if (ofile[0])
		unlink(ofile);
	if (hidenam[0])
		unlink(hidenam);
}
