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
#ident	"$Header: id.c,v 1.6.2.2 90/05/09 16:14:36 wje Exp $"

/*
 * id/whoami combination
 *
 * Written by David Koblas
 *
 * Usage: id [+format]
 * 
 * The format consists of %-specifiers, escapes, and other characters,
 * and is terminated by a newline.  The escapes \n, \b, \f, \r, \t, and
 * \\ are recognized as expected.  The %-specifiers are:
 *
 * %% - %
 * %u - user-id number
 * %U - effective user-id number
 * %g - group-id number
 * %G - effective group-id number
 * %l - user-name
 * %L - effective user-name
 * %n - group-name
 * %N - effective group-name
 *
 */

#include	<stdio.h>
#include	<pwd.h>
#include	<grp.h>

extern struct passwd *getpwuid();
extern struct group *getgrgid();

#define UNKNOWN	"unknown"

char           *strrchr();

main(argc, argv)
	int             argc;
	char          **argv;
{
	char           *cp;
	struct group   *gr;
	struct passwd  *pw;
	char            name[40];
	char            ename[40];
	char            grp[40];
	char            egrp[40];
	int             uid;
	int             gid;
	int             euid;
	int             egid;
	int             i;

	uid = getuid();
	gid = getgid();
	euid = geteuid();
	egid = getegid();

	pw = getpwuid(uid);
	if (pw == NULL) {
		strcpy(name, UNKNOWN);
	} else {
		strcpy(name, pw->pw_name);
	}
	pw = getpwuid(euid);
	if (pw == NULL) {
		strcpy(ename, UNKNOWN);
	} else {
		strcpy(ename, pw->pw_name);
	}
	gr = getgrgid(gid);
	if (gr == NULL) {
		strcpy(grp, UNKNOWN);
	} else {
		strcpy(grp, gr->gr_name);
	}
	gr = getgrgid(egid);
	if (gr == NULL) {
		strcpy(egrp, UNKNOWN);
	} else {
		strcpy(egrp, gr->gr_name);
	}

	if ((cp = strrchr(argv[0], '/')) == NULL) {
		cp = argv[0];
	} else {
		cp++;
	}

	if (strcmp(cp, "whoami") == 0) {
		printf("%s\n", name);
		exit(0);
	}
	if (argc == 1) {
		printf("uid=%d(%s)", uid, name);
		if (uid != euid)
			printf(" euid=%d(%s)", euid, ename);
		printf(" gid=%d(%s)", gid, grp);
		if (gid != egid)
			printf(" egid=%d(%s)", egid, egrp);
		putchar('\n');
	} else if (argc == 2 && argv[1][0] == '+') {
		for (i = 1; i < strlen(argv[1]); i++) {
			if (argv[1][i] == '%') {
				i++;
				if (i == strlen(argv[1]))
					putchar('%');
				else {
					switch (argv[1][i]) {
					case '%':
						putchar('%');
						break;
					case 'u':
						printf("%d", uid);
						break;
					case 'U':
						printf("%d", euid);
						break;
					case 'g':
						printf("%d", gid);
						break;
					case 'G':
						printf("%d", egid);
						break;
					case 'l':
						printf("%s", name);
						break;
					case 'L':
						printf("%s", ename);
						break;
					case 'n':
						printf("%s", grp);
						break;
					case 'N':
						printf("%s", egrp);
						break;
					default:
						putchar('%');
						putchar(argv[1][i]);
						break;
					}
				}
			} else if (argv[1][i] == '\\') {
				i++;
				if (i == strlen(argv[1]))
					putchar('\n');
				else {
					switch (argv[1][i]) {
					case 'n':
						putchar('\n');
						break;
					case 'b':
						putchar('\b');
						break;
					case 'f':
						putchar('\f');
						break;
					case 'r':
						putchar('\r');
						break;
					case 't':
						putchar('\t');
						break;
					case '\\':
						putchar('\\');
						break;
					default:
						putchar('\\');
						putchar(argv[1][i]);
						break;
					}
				}
			} else
				putchar(argv[1][i]);
		}
		putchar('\n');
	} else {
		fprintf(stderr, "usage: id [+format]\n");
		exit(0);
	}
	exit(0);
}
