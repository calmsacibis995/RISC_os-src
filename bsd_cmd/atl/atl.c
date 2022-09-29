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
#ident	"$Header: atl.c,v 1.1.2.2 90/05/07 18:03:26 wje Exp $"

/* 
 * The atl program 
 *
 * By: Roger S. Southwick 
 *
 * April 21, 1986 
 *
 * This program lists the jobs in the /usr/spool/at directory which belong to
 * him. 
 *
 * usage: atl job#...
 *
 * The job# is the job number reported by atq. 
 *
 * (This turns out to be the inode number of the file, but we don't tell the
 * user that!) 
 *
 */

#define ATDIR	"/usr/spool/at"

/*
 * Define STRICT if you require the user to be logged in Without this, being
 * su'ed will work. 
 */

/* #define STRICT	/* Strict login required	 */

#include <stdio.h>
#include <sys/types.h>
#include <sys/stat.h>
#include <sys/dir.h>
#include <pwd.h>
#include <ctype.h>

extern char    *sys_errlist[];
extern int      errno;
#define ERROR	sys_errlist[errno]

char           *myname = NULL;
FILE           *fp = NULL;

main(argc, argv)
	int             argc;
	char           *argv[];
{
	DIR            *opendir();
	struct direct  *readdir();
	int             chdir(), stat(), sscanf();
	register DIR   *dirp;
	register struct direct *dp;
	register int    i;
	int             job;
	struct stat     stb;

	if (chdir(ATDIR) == -1) {
		(void) fprintf(stderr, "atl: could not chdir %s - %s\n", ATDIR, ERROR);
		exit(1);
	}
	if (argc < 2) {
		(void) fprintf(stderr, "usage: atl job#...\n");
		exit(1);
	}
	whoami();

	if ((dirp = opendir(".")) == NULL) {
		(void) fprintf(stderr, "atl: could not opendir(%s)\n", ATDIR);
		exit(1);
	}
	for (dp = readdir(dirp); dp != NULL; dp = readdir(dirp)) {
		if (dp->d_ino == 0)
			continue;

		if (strcmp(".", dp->d_name) == 0 || strcmp("..", dp->d_name) == 0)
			continue;

		if (!isdigit(dp->d_name[0]))
			continue;

		if (stat(dp->d_name, &stb) == -1) {
			(void) fprintf(stderr, "atl: could not stat %s/%s - %s\n", ATDIR, dp->d_name, ERROR);
			exit(1);
		}
		if ((stb.st_mode & S_IFMT) != S_IFREG)
			continue;

		for (i = 1; i < argc; i++) {
			if (sscanf(argv[i], "%d", &job) != 1) {
				(void) fprintf(stderr, "atl: job number (%s) must be numeric\n", argv[i]);
				continue;
			}
			if (job < 0) {
				(void) fprintf(stderr, "atl: job number (%d) must be positive\n", job);
				continue;
			}
			if (job != stb.st_ino)
				continue;

			if (notowner(dp->d_name)) {
				(void) fprintf(stderr, "atl: must be job's owner\n");
				continue;
			}
			if (argc > 2)
				(void) printf("\n>>>>>> Job # %d <<<<<<\n", job);

			output();
		}
	}
	exit(0);
}

int             myuid;

whoami()
{

#ifdef STRICT
	char           *getlogin();
#endif
	int             getuid();
	struct passwd  *getpwuid();
	register struct passwd *pwd;
	extern char    *myname;
	extern int      myuid;

	myuid = getuid();

#ifdef STRICT
	if ((myname = getlogin()) == NULL) {
		if ((pwd = getpwuid(myuid)) == NULL) {
			(void) fprintf(stderr, "atl: Say, who are you, anyway?\n");
			exit(1);
		}
		myname = pwd->pw_name;
	}
#else
	if ((pwd = getpwuid(myuid)) == NULL) {
		(void) fprintf(stderr, "atl: Say, who are you, anyway?\n");
		exit(1);
	}
	myname = pwd->pw_name;
#endif
}

char            owner[80];

int
notowner(name)
	register char  *name;
{
	FILE           *fopen();
	int             fscanf();
	extern char    *myname;
	extern FILE    *fp;
	extern char     owner[];
	extern          myuid;

	if ((fp = fopen(name, "r")) == NULL) {
		(void) fprintf(stderr, "atl: could not open %s/%s for read\n", ATDIR, name);
		exit(1);
	}
	if (fscanf(fp, "# owner: %s\n", owner) != 1) {
		(void) fprintf(stderr, "atl: unknown owner %s/%s\n", ATDIR, name);
		return (1);
	}
	if (myuid == 0)
		return (0);

	if (strcmp(owner, myname) != 0) {
		(void) fclose(fp);
		fp = NULL;
		return (1);
	}
	return (0);
}

output()
{
	char           *fgets();
	extern FILE    *fp;
	extern int      myuid;
	extern char     owner[];
	char            buf[BUFSIZ];

	if (myuid == 0)
		(void) printf("# owner: %s\n", owner);

	while (fgets(buf, BUFSIZ, fp) != NULL)
		(void) fputs(buf, stdout);

	(void) fclose(fp);
}
