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
#ident	"$Header: du.c,v 1.3.2.2 90/05/07 18:21:25 wje Exp $"

#include <stdio.h>
#include <sys/param.h>
#include <sys/stat.h>
#include <sys/dir.h>

char	path[BUFSIZ], name[BUFSIZ];
int	fflg;
int	aflg;
int	sflg;
char	*dot = ".";
int Current_blocks = 0;

#define ML	1024
typedef struct ml_struct {
	dev_t	dev;
	ino_t	ino;
	struct ml_struct *next;
} ml_entry;
ml_entry *ml[ML];

long	descend();
char	*index(), *rindex(), *strcpy(), *sprintf();

#define	kb(n)	(howmany(dbtob(n), 1024))

main(argc, argv)
	int argc;
	char **argv;
{
	long blocks = 0;
	register char *np;
	int pid;
	int c;
	extern int optind;

	while ((c = getopt(argc, argv, "afs")) != EOF) {
		switch (c) {

		case 'a':
			aflg = 1;
			break;
			;;

		case 'f':
			fflg = 1;
			break;
			;;

		case 's':
			sflg = 1;
			break;
			;;

		default:
			fprintf(stderr,
				"du: usage: du [-a] [-f] [-s] [name...]\n");
			exit(1);
			;;
		}
	}

	if (optind == argc) {
		argv = &dot;
		argc = 1;
		optind = 0;
	}
	do {
		if (argc > optind + 1) {
			pid = fork();
			if (pid == -1) {
				perror("No more processes");
				exit(1);
			}
			if (pid != 0)
				wait((int *)0);
		}
		if (argc - optind == 1 || pid == 0) {
			(void) strcpy(path, argv[optind]);
			(void) strcpy(name, argv[optind]);
			if (np = rindex(name, '/')) {
				*np++ = '\0';
				if (chdir(*name ? name : "/") < 0) {
					perror(*name ? name : "/");
					exit(1);
				}
			} else
				np = path;
			blocks = descend(path, *np ? np : ".");
			if (sflg) {
			    if (fflg)
				printf("%ld\t%s\n", kb(Current_blocks), path);
			    else
				printf("%ld\t%s\n", kb(blocks), path);
			    }
			if (argc - optind > 1)
				exit(1);
		}
		optind++;
	} while (optind < argc);
	exit(0);
}

DIR	*dirp = NULL;


long
descend(base, name)
	char *base, *name;
{
	char *ebase0, *ebase;
	struct stat stb;
	int i;
	long blocks = 0;
	long curoff = NULL;
	register struct direct *dp;

	ebase0 = ebase = index(base, 0);
	if (ebase > base && ebase[-1] == '/')
		ebase--;
	if (lstat(name, &stb) < 0) {
		perror(base);
		*ebase0 = 0;
		return (0);
	}
	if (stb.st_nlink > 1 && (stb.st_mode&S_IFMT) != S_IFDIR) {
		ino_t mle;
		ml_entry *mlp;

#if ((ML-1)&ML)==0
		mle = stb.st_ino & (ML-1);
#else
		mle = stb.st_ino % ML;
#endif
		for (mlp = ml[mle]; mlp != NULL; mlp = mlp->next) {
			if (mlp->ino == stb.st_ino && mlp->dev == stb.st_dev)
				return (0);
		}
		mlp = (ml_entry *) malloc (sizeof(ml_entry));
		if (mlp == NULL) {
			perror ("out of memory for inode links");
			exit (1);
		}
		mlp->ino = stb.st_ino;
		mlp->dev = stb.st_dev;
		mlp->next = ml[mle];
		ml[mle] = mlp;
	}
	blocks = stb.st_blocks;
	if ((stb.st_mode&S_IFMT) != S_IFDIR) {
		if (aflg)
			printf("%ld\t%s\n", kb(blocks), base);
		return (blocks);
	}
	if (dirp != NULL)
		closedir(dirp);
	dirp = opendir(name);
	if (dirp == NULL) {
		perror(base);
		*ebase0 = 0;
		return (0);
	}
	if (chdir(name) < 0) {
		perror(base);
		*ebase0 = 0;
		closedir(dirp);
		dirp = NULL;
		return (0);
	}
	while (dp = readdir(dirp)) {
		if (!strcmp(dp->d_name, ".") || !strcmp(dp->d_name, ".."))
			continue;
		(void) sprintf(ebase, "/%s", dp->d_name);
		curoff = telldir(dirp);
		blocks += descend(base, ebase+1);
		*ebase = 0;
		if (dirp == NULL) {
			dirp = opendir(".");
			if (dirp == NULL) {
				perror(".");
				return (0);
			}
			seekdir(dirp, curoff);
		}
	}
	closedir(dirp);
	dirp = NULL;
	if (sflg == 0)
		printf("%ld\t%s\n", kb(blocks), base);
	if (chdir("..") < 0) {
		(void) sprintf(index(base, 0), "/..");
		perror(base);
		exit(1);
	}
	*ebase0 = 0;
	Current_blocks = blocks;
	if (fflg)
	    return(0);
	else
	    return (blocks);
}
