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
#ident	"$Header: gwd.c,v 1.4.2.2 90/05/10 00:36:01 wje Exp $"

/*	Copyright (c) 1984 AT&T	*/
/*	  All Rights Reserved  	*/

/*	THIS IS UNPUBLISHED PROPRIETARY SOURCE CODE OF AT&T	*/
/*	The copyright notice above does not evidence any   	*/
/*	actual or intended publication of such source code.	*/


#include "uucp.h"

/*
 *	gwd - get working directory
 *	Uid and Euid are global
 *	return
 *		0 - ok
 *	 	FAIL - failed
 */

gwd(wkdir)
char *wkdir;
{
	FILE *fp;
	char cmd[BUFSIZ];

	*wkdir = '\0';
	(void) sprintf(cmd, "%s pwd 2>&-", PATH);

#ifndef V7
	(void) setuid(Uid);
	fp = popen(cmd, "r");
	(void) setuid(Euid);
#else
	fp = popen(cmd, "r");
#endif

	if (fp == NULL)
		return(FAIL);

	if (fgets(wkdir, MAXFULLNAME, fp) == NULL) {
		(void) pclose(fp);
		return(FAIL);
	}
	if (wkdir[strlen(wkdir)-1] == '\n')
		wkdir[strlen(wkdir)-1] = '\0';
	(void) pclose(fp);
	return(0);
}


/*
 * uidstat(file, &statbuf)
 * This is a stat call with the uid set from Euid to Uid.
 * Used from uucp.c and uux.c to permit file copies
 * from directories that may not be searchable by other.
 * return:
 *	same as stat()
 */

int
uidstat(file, buf)
char *file;
struct stat *buf;
{
#ifndef V7
	register ret;

	(void) setuid(Uid);
	ret = stat(file, buf);
	(void) setuid(Euid);
	return(ret);
#else /* V7 */
	int	ret;

	if (vfork() == 0) {
		(void) setuid(Uid);
		_exit(stat(file, buf));
	}
	wait(&ret);
	return(ret);
#endif /* V7 */
}
