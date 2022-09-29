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
#ident	"$Header: getusershell.c,v 1.2.1.2 90/05/07 20:38:17 wje Exp $"

/*
 * Copyright (c) 1985 Regents of the University of California.
 * All rights reserved.  The Berkeley software License Agreement
 * specifies the terms and conditions for redistribution.
 */

#if defined(LIBC_SCCS) && !defined(lint)
static char sccsid[] = "@(#)getusershell.c	5.4 (Berkeley) 7/25/86";
#endif LIBC_SCCS and not lint

#include <sys/param.h>
#include <sys/file.h>
#include <sys/stat.h>
#include <ctype.h>
#include <stdio.h>

#define SHELLS "/etc/shells"

/*
 * Do not add local shells here.  They should be added in /etc/shells
 */
static char *okshells[] =
    { "/bin/sh", "/bin/csh", 0 };

static char **shells, *strings;
static char **curshell = NULL;
extern char **initshells();

/*
 * Get a list of shells from SHELLS, if it exists.
 */
char *
getusershell()
{
	char *ret;

	if (curshell == NULL)
		curshell = initshells();
	ret = *curshell;
	if (ret != NULL)
		curshell++;
	return (ret);
}

endusershell()
{
	
	if (shells != NULL)
		free((char *)shells);
	shells = NULL;
	if (strings != NULL)
		free(strings);
	strings = NULL;
	curshell = NULL;
}

setusershell()
{

	curshell = initshells();
}

static char **
initshells()
{
	register char **sp, *cp;
	register FILE *fp;
	struct stat statb;
	extern char *malloc(), *calloc();

	if (shells != NULL)
		free((char *)shells);
	shells = NULL;
	if (strings != NULL)
		free(strings);
	strings = NULL;
	if ((fp = fopen(SHELLS, "r")) == (FILE *)0)
		return(okshells);
	if (fstat(fileno(fp), &statb) == -1) {
		(void)fclose(fp);
		return(okshells);
	}
	if ((strings = malloc((unsigned)statb.st_size)) == NULL) {
		(void)fclose(fp);
		return(okshells);
	}
	shells = (char **)calloc((unsigned)statb.st_size / 3, sizeof (char *));
	if (shells == NULL) {
		(void)fclose(fp);
		free(strings);
		strings = NULL;
		return(okshells);
	}
	sp = shells;
	cp = strings;
	while (fgets(cp, MAXPATHLEN + 1, fp) != NULL) {
		while (*cp != '#' && *cp != '/' && *cp != '\0')
			cp++;
		if (*cp == '#' || *cp == '\0')
			continue;
		*sp++ = cp;
		while (!isspace(*cp) && *cp != '#' && *cp != '\0')
			cp++;
		*cp++ = '\0';
	}
	*sp = (char *)0;
	(void)fclose(fp);
	return (shells);
}
