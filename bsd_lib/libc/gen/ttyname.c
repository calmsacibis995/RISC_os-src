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
#ident	"$Header: ttyname.c,v 1.4.1.2 90/05/07 20:44:55 wje Exp $"

#if defined(LIBC_SCCS) && !defined(lint)
static char sccsid[] = "@(#)ttyname.c	5.2 (Berkeley) 3/9/86";
#endif LIBC_SCCS and not lint

/*
 * ttyname(f): return "/dev/ttyXX" which the the name of the
 * tty belonging to file f.
 *  NULL if it is not a tty
 */

#include <sys/param.h>
#include <sys/dir.h>
#include <sys/stat.h>

#ifndef NULL
#define	NULL	0
#endif

static	char	dev[]	= "/dev/";
char	*strcpy();
char	*strcat();

char *
ttyname(f)
{
	struct stat fsb;
	struct stat tsb;
	register struct direct *db;
	register DIR *df;
	static char rbuf[32];

	if (isatty(f)==0)
		return(NULL);
	if (fstat(f, &fsb) < 0)
		return(NULL);
	if ((fsb.st_mode&S_IFMT) != S_IFCHR)
		return(NULL);
	if ((df = opendir(dev)) == NULL)
		return(NULL);
	while ((db = readdir(df)) != NULL) {
		if (db->d_ino != fsb.st_ino)
			continue;
		strcpy(rbuf, dev);
		strcat(rbuf, db->d_name);
		if (stat(rbuf, &tsb) < 0)
			continue;
		if (tsb.st_dev == fsb.st_dev && tsb.st_ino == fsb.st_ino) {
			closedir(df);
			return(rbuf);
		}
	}
	closedir(df);
	return(NULL);
}
