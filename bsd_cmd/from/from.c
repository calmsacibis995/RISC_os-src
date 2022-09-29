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
#ident	"$Header: from.c,v 1.2.2.2 90/05/07 18:35:02 wje Exp $"

#include <stdio.h>
#include <ctype.h>
#include <pwd.h>

struct	passwd *getpwuid();

main(argc, argv)
	int argc;
	register char **argv;
{
	char lbuf[BUFSIZ];
	char lbuf2[BUFSIZ];
	register struct passwd *pp;
	int stashed = 0;
	register char *name;
	char *sender;
	char *getlogin();

	if (argc > 1 && *(argv[1]) == '-' && (*++argv)[1] == 's') {
		if (--argc <= 1) {
			fprintf (stderr, "Usage: from [-s sender] [user]\n");
			exit (1);
		}
		--argc;
		sender = *++argv;
		for (name = sender; *name; name++)
			if (isupper(*name))
				*name = tolower(*name);

	} else
		sender = NULL;
	if (chdir("/usr/mail") < 0)
		exit(1);
	if (argc > 1)
		name = argv[1];
	else {
		name = getlogin ();
		if (name == NULL || strlen(name) == 0) {
			pp = getpwuid(getuid());
			if (pp == NULL) {
				fprintf(stderr, "Who are you?\n");
				exit(1);
			}
			name = pp->pw_name;
		}
	}
	if (freopen(name, "r", stdin) == NULL) {
#ifdef notdef
		/*
		 * This was added in 4.3, but it shouldn't have been.
		 * It is reasonable for mailboxes not to exist.
		 */

		fprintf(stderr, "Can't open /usr/spool/mail/%s\n", name);
#endif
		exit(0);
	}
	while (fgets(lbuf, sizeof lbuf, stdin) != NULL)
		if (lbuf[0] == '\n' && stashed) {
			stashed = 0;
			printf("%s", lbuf2);
		} else if (strncmp(lbuf, "From ", 5) == 0 &&
		    (sender == NULL || match(&lbuf[4], sender))) {
			strcpy(lbuf2, lbuf);
			stashed = 1;
		}
	if (stashed)
		printf("%s", lbuf2);
	exit(0);
}

match (line, str)
	register char *line, *str;
{
	register char ch;

	while (*line == ' ' || *line == '\t')
		++line;
	if (*line == '\n')
		return (0);
	while (*str && *line != ' ' && *line != '\t' && *line != '\n') {
		ch = isupper(*line) ? tolower(*line) : *line;
		if (ch != *str++)
			return (0);
		line++;
	}
	return (*str == '\0');
}
