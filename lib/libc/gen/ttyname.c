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
#ident	"$Header: ttyname.c,v 1.7.2.2 90/05/10 01:41:23 wje Exp $"

/*	Copyright (c) 1984 AT&T	*/
/*	  All Rights Reserved  	*/

/*	THIS IS UNPUBLISHED PROPRIETARY SOURCE CODE OF AT&T	*/
/*	The copyright notice above does not evidence any   	*/
/*	actual or intended publication of such source code.	*/

/*LINTLIBRARY*/
/*
 * ttyname(f): return "/dev/ttyXX" which the the name of the
 * tty belonging to file f.
 *
 * This program works in two passes: the first pass tries to
 * find the device by matching device and inode numbers; if
 * that doesn't work, it tries a second time, this time doing a
 * stat on every file in /dev and trying to match device numbers
 * only. If that fails too, NULL is returned.
 */

#define	NULL	0
#include <sys/types.h>
#include <dirent.h>
#include <sys/stat.h>

extern int open(), read(), close(), stat(), fstat(), isatty();
extern char *strcpy(), *strcat();
extern long lseek();

static char rbuf[32], dev[]="/dev/";

char *
ttyname(f)
int	f;
{
	struct stat fsb, tsb;
	struct dirent *db;
	DIR	*df;
	register int pass1;

	if(isatty(f) == 0)
		return(NULL);
	if(fstat(f, &fsb) < 0)
		return(NULL);
	if((fsb.st_mode & S_IFMT) != S_IFCHR)
		return(NULL);
	if((df = opendir(dev)) == NULL)
		return(NULL);
	pass1 = 1;
	do {
		while((db = readdir(df)) != NULL) {
			if(pass1 && db->d_ino != fsb.st_ino)
				continue;
			(void) strcpy(rbuf, dev);
			(void) strcat(rbuf, db->d_name);
			if(stat(rbuf, &tsb) < 0)
				continue;
			if(tsb.st_rdev == fsb.st_rdev &&
				(tsb.st_mode&S_IFMT) == S_IFCHR &&
				(!pass1 || tsb.st_ino == fsb.st_ino)) {
				(void) closedir(df);
				return(rbuf);
			}
		}
		rewinddir(df);
	} while(pass1--);
	(void) closedir(df);
	return(NULL);
}
