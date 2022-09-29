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
#ident	"$Header: touch.c,v 1.1.2.2 90/05/07 19:42:39 wje Exp $"

/*
 * attempt to set the modify date of a file to the current date. if the file
 * exists, read and write its first character. if the file doesn't exist,
 * create it, unless -c option prevents it. if the file is read-only, -f
 * forces chmod'ing and touch'ing. 
 */

#include <stdio.h>
#include <sys/types.h>
#include <sys/stat.h>
#include <sys/file.h>

int             dontcreate = 0;	/* set if -c option */
int             force = 0;	/* set if -f option */

int             Errs = 0;	/* Set if any errors */

char           *whoami = "touch";

char            Errbuf[1024];

main(argc, argv)
	int             argc;
	char          **argv;
{
	char           *argp;
	extern int      optind;
	int             c;

	while ((c = getopt(argc, argv, "cf")) != EOF) {
		switch (c) {

		case 'c':
			dontcreate = 1;
			break;
		case 'f':
			force = 1;
			break;
		default:
			fprintf(stderr, "%s: usage: %s [-c] [-f] name...\n",
				whoami, whoami);
			exit(1);
		}
	}

	if (optind == argc) {
		fprintf(stderr, "%s: usage: %s [-c] [-f] name...\n",
			whoami, whoami);
		exit(1);
	}
	while (optind < argc) {
		touch(argv[optind]);
		optind++;
	}
	exit(Errs);
}

touch(filename)
	char           *filename;
{
	struct stat     statbuffer;

	if (stat(filename, &statbuffer) < 0) {
		if (!dontcreate) {
			create_file(filename);
		} else {
			fprintf(stderr, "%s: %s: does not exist\n",
				whoami, filename);
			Errs = 1;
		}
		return;
	}
	if ((statbuffer.st_mode & S_IFMT) != S_IFREG) {
		fprintf(stderr, "%s: %s: can only touch regular files\n",
			whoami, filename);
		Errs = 1;
		return;
	}
	if (!access(filename, R_OK | W_OK)) {
		if (statbuffer.st_size == 0) {
			touch_empty(filename);
		} else {
			touch_file(filename);
		}
		return;
	}
	if (force) {
		if (chmod(filename, 0666)) {
			sprintf(Errbuf, "%s: can not change mode of %s",
				whoami, filename);
			perror(Errbuf);
			Errs = 1;
			return;
		}
		if (statbuffer.st_size == 0) {
			touch_empty(filename);
		} else {
			touch_file(filename);
		}
		if (chmod(filename, statbuffer.st_mode)) {
			sprintf(Errbuf, "%s: could not restore mode of %s",
				whoami, filename);
			perror(Errbuf);
			Errs = 1;
			return;
		}
	} else {
		fprintf(stderr, "%s: %s: cannot touch\n", whoami, filename);
		Errs = 1;
	}
}

/*
 * The subroutine create_file() creates a new file. 
 */

create_file(filename)
	char           *filename;
{
	int             fd;

	fd = open(filename, O_RDWR | O_CREAT, 0666);
	if (fd < 0) {
		sprintf(Errbuf, "%s: could not create %s", whoami, filename);
		perror(Errbuf);
		return;
	}
	if (close(fd) < 0) {
		sprintf(Errbuf, "%s: %s", whoami, filename);
		perror(Errbuf);
		return;
	}
}

/*
 * The subroutine touch_file() touches a file that is not empty. This is done
 * by reading the first character and writing it back. 
 */

touch_file(filename)
	char           *filename;
{
	int             fd;
	char            first;

	fd = open(filename, O_RDWR);
	if (fd < 0) {
		sprintf(Errbuf, "%s: %s", whoami, filename);
		perror(Errbuf);
		Errs = 1;
		return;
	}
	if (read(fd, &first, 1) != 1) {
		sprintf(Errbuf, "%s: %s", whoami, filename);
		perror(Errbuf);
		Errs = 1;
		return;
	}
	if (lseek(fd, 0L, L_SET) < 0) {
		sprintf(Errbuf, "%s: %s", whoami, filename);
		perror(Errbuf);
		Errs = 1;
		return;
	}
	if (write(fd, &first, 1) != 1) {
		sprintf(Errbuf, "%s: %s", whoami, filename);
		perror(Errbuf);
		Errs = 1;
		return;
	}
	if (close(fd) < 0) {
		sprintf(Errbuf, "%s: %s", whoami, filename);
		perror(Errbuf);
		Errs = 1;
		return;
	}
}

/*
 * The subroutine touch_empty() touches a file that is empty. This is done by
 * writing a character in the file and then truncating the file. 
 */

touch_empty(filename)
	char           *filename;
{
	int             fd;
	char            c;

	c = 'X';

	fd = open(filename, O_RDWR | O_CREAT, 0666);
	if (fd < 0) {
		sprintf(Errbuf, "%s: could not create %s", whoami, filename);
		perror(Errbuf);
		return;
	}
	if (write(fd, &c, sizeof(char)) != sizeof(char)) {
		sprintf(Errbuf, "%s: %s", whoami, filename);
		perror(Errbuf);
	}
	if (ftruncate(fd, 0) < 0) {
		sprintf(Errbuf, "%s: %s", whoami, filename);
		perror(Errbuf);
	}
	if (close(fd) < 0) {
		sprintf(Errbuf, "%s: %s", whoami, filename);
		perror(Errbuf);
		return;
	}
}
