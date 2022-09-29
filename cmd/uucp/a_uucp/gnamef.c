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
#ident	"$Header: gnamef.c,v 1.5.2.2 90/05/10 00:35:49 wje Exp $"

/*	Copyright (c) 1984 AT&T	*/
/*	  All Rights Reserved  	*/

/*	THIS IS UNPUBLISHED PROPRIETARY SOURCE CODE OF AT&T	*/
/*	The copyright notice above does not evidence any   	*/
/*	actual or intended publication of such source code.	*/


#include "uucp.h"

/*
 * get next file name from directory
 *	p	 -> file description of directory file to read
 *	filename -> address of buffer to return filename in
 *		    must be of size DIRSIZ+1
 * returns:
 *	FALSE	-> end of directory read
 *	TRUE	-> returned name
 */
gnamef(p, filename)
register char *filename;
DIR *p;
{
	struct dirent dentry;
	register struct dirent *dp = &dentry;

	while (1) {
		if ((dp = readdir(p)) == NULL)
			return(FALSE);
		if (dp->d_ino != 0 && dp->d_name[0] != '.')
			break;
	}

	(void) strncpy(filename, dp->d_name, MAXBASENAME);
	filename[MAXBASENAME] = '\0';
	return(TRUE);
}

/*
 * get next directory name from directory
 *	p	 -> file description of directory file to read
 *	filename -> address of buffer to return filename in
 *		    must be of size DIRSIZ+1
 * returns:
 *	FALSE	-> end of directory read
 *	TRUE	-> returned dir
 */
gdirf(p, filename, dir)
register char *filename;
DIR *p;
char *dir;
{
	char statname[MAXNAMESIZE];

	while (1) {
		if(gnamef(p, filename) == FALSE)
			return(FALSE);
		(void) sprintf(statname, "%s/%s", dir, filename);
		DEBUG(4, "stat %s\n", statname);
		if (DIRECTORY(statname))
		    break;
	}

	return(TRUE);
}
