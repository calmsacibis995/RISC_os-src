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
#ident	"$Header: lookbib.c,v 1.1.2.2 90/05/07 18:46:43 wje Exp $"

#include <stdio.h>
#include <ctype.h>

main(argc, argv)	/* look in biblio for record matching keywords */
int argc;
char **argv;
{
	FILE *fp, *hfp, *fopen(), *popen();
	char s[BUFSIZ], hunt[64], *sprintf();
	int instructions = 1;

	if (argc > 1 && strcmp(argv[1],"-n") == 0)
	{
		argv++;
		argc--;
		instructions = 0;
	}

	if (argc == 1 || argc > 2)
	{
		fputs("Usage: lookbib [-n] database\n",
			stderr);
		fputs("\tfinds citations specified on standard input\n",
			stderr);
		exit(1);
	}
	if (!isatty(fileno(stdin)))
		fp = stdin;
	else if ((fp = fopen("/dev/tty", "r")) == NULL)
	{
		perror("lookbib: /dev/tty");
		exit(1);
	}
	sprintf(s, "%s.ia", argv[1]);
	if (access(s, 0) == -1) {
		sprintf (s, "%s", argv[1]);
		if (access(s, 0) == -1) {
			perror(s);
			fprintf(stderr, "\tNeither index file %s.ia ", s);
			fprintf(stderr, "nor reference file %s found\n", s);
			exit(1);
		}
	}
	sprintf(hunt, "/usr/lib/refer/hunt %s", argv[1]);

	if (instructions && isatty(fileno(fp)))
	{
		fprintf(stderr, "Instructions? ");
		fgets(s, BUFSIZ, fp);
		if (*s == 'y')
			instruct();
	}
   again:
	fprintf(stderr, "> ");
	if (fgets(s, BUFSIZ, fp))
	{
		if (*s == '\n')
			goto again;
		if ((hfp = popen(hunt, "w")) == NULL)
		{
			perror("lookbib: /usr/lib/refer/hunt");
			exit(1);
		}
		map_lower(s);
		fputs(s, hfp);
		pclose(hfp);
		goto again;
	}
	fclose(fp);
	fprintf(stderr, "EOT\n");
	exit(0);
}

map_lower(s)		/* map string s to lower case */
char *s;
{
	for ( ; *s; ++s)
		if (isupper(*s))
			*s = tolower(*s);
}

instruct()
{
	fputs("\nType keywords (such as author and date) after the > prompt.\n",
		stderr);
	fputs("References with those keywords are printed if they exist;\n",
		stderr);
	fputs("\tif nothing matches you are given another prompt.\n",
		stderr);
	fputs("To quit lookbib, press CTRL-d after the > prompt.\n\n",
		stderr);
}
