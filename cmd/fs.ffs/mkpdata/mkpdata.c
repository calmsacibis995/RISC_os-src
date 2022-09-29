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
#ident	"$Header: mkpdata.c,v 1.3.2.2 90/05/09 15:57:39 wje Exp $"

/*
 * pl - proto line
 *
 * Given one argument pl will output a line useful to mkproto.
 * exit status:
 *	>= 0	number of levels down
 *	<  0	error
 */
#include <sys/param.h>
#include <sys/types.h>
#include <sys/sysmacros.h>
#include <dirent.h>
#include <sys/stat.h>

main (argc, argv)
	int argc;
	char **argv;
{
	/*
	 * Skip program name
	 */
	argc--;
	*argv++;

	search (".", 0);
}

search (DirName, level)
	register char *DirName;
	register int level;
{
	DIR *dirp;
	register struct dirent *dp;
	register char *FullName;
	register int pos;
	struct stat Stat;

	pl (DirName);
	if ((dirp = opendir (DirName)) == NULL) {
		printf ("# couldn't open directory %s\n", DirName);
		return;
	}

	for (dp = readdir (dirp); dp != NULL; dp = readdir (dirp)) {
		if (!strcmp (dp->d_name, "..") || !strcmp (dp->d_name, ".")) {
			continue;
		}
		FullName = (char *)malloc (strlen (DirName) + 
			strlen(dp->d_name) + 2);
		if (FullName == NULL) {
			printf ("# couldn't malloc space for %s/%s\n",
				DirName, dp->d_name);
			exit (1);
		}

		sprintf (FullName, "%s/%s", DirName, dp->d_name);
		if (lstat (FullName, &Stat) < 0) {
			printf ("# can't stat %s\n", FullName);
			return;
		}

		if ((Stat.st_mode & S_IFMT) == S_IFDIR) {
			pos = telldir (dirp);
			closedir (dirp);
			search (FullName, level + 1);
			dirp = opendir (DirName);
			seekdir (dirp, pos);
		}
		else {
			pl (FullName);
		}
		free (FullName);
	}
	printf ("%*s$\n", level * 8, " ");
}


pl (FullName)
	register char *FullName;
{
	register int i;
	struct stat Stat;
	char *s;

	if (lstat (FullName, &Stat) < 0) {
		printf ("#\terror: can't stat %s\n", FullName);
		exit (-1);
	}

	/*
	 * Print out one tab for each '/' found
	 * in the name.  This gives a nice "look"
	 * to the proto type file.
	 */
	s = FullName;
	while (*s != '\0')
		if (*s++ == '/')
			printf ("\t");

	/*
	 * Print just the file name, no path.
	 * If file name equals '.' don't print
	 * anything.
	 */
	s = (char *)strrchr (FullName, '/');
	if (s == 0)
		s = FullName;
	else
		*s++;
	
	if (!(strlen (s) == 1 && *s == '.')) {
		printf ("%s ", s);

		/*
		 * Pad with spaces to 14 characters.
		 * Used just to pretty print the file names
		 * and could be removed if you liked.
		 */
		i = 14 - strlen (s);
		if (i > 0)
			printf ("%*s", i, " ");
	}

	/*
	 * Now for the mode
	 */
	switch (Stat.st_mode & S_IFMT) {
		case S_IFDIR:
			s = "d";
			break;

		case S_IFCHR:
			s = "c";
			break;
		
		case S_IFBLK:
			s = "b";
			break;
		
		case S_IFREG:
			s = "-";
			break;
		
		case S_IFLNK:
			s = "l";
			break;
		
		default:
			printf ("\n#\terror: %s unknown mode\n", FullName, 
				Stat.st_mode);
			exit (-1);
	}

	printf ("%s%c%c", s,
		Stat.st_mode & S_ISUID ? 'u' : '-',
		Stat.st_mode & S_ISGID ? 'g' : '-');
	
	printf ("%o %d %d ", Stat.st_mode & 0777, Stat.st_uid, Stat.st_gid);
	switch (Stat.st_mode & S_IFMT) {
		case S_IFCHR:
		case S_IFBLK:
			printf ("%d %d", major(Stat.st_rdev), 
				minor(Stat.st_rdev));
			break;
		
		case S_IFREG:
		case S_IFLNK:
			printf ("%s", FullName);
			break;
	}
	printf ("\n");
}
