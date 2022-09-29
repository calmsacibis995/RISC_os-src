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
#ident	"$Header: gwd.c,v 1.4.2.2 90/05/09 16:26:50 wje Exp $"

/*	Copyright (c) 1984 AT&T	*/
/*	  All Rights Reserved  	*/

/*	THIS IS UNPUBLISHED PROPRIETARY SOURCE CODE OF AT&T	*/
/*	The copyright notice above does not evidence any   	*/
/*	actual or intended publication of such source code.	*/

/*
 *	gwd(wkdir) -- place name of working directory in wkdir
 *	returns 0 for success, -1 for failure
 */

#include	"lp.h"


gwd(wkdir)
char *wkdir;
{
	FILE *fp, *popen();
	char *c;

	if ((fp = popen("pwd 2>/dev/null", "r")) == NULL) {
		*wkdir = '\0';
		return(-1);
	}
	if (fgets(wkdir, FILEMAX, fp) == NULL) {
		pclose(fp);
		*wkdir = '\0';
		return(-1);
	}
	if (*(c = wkdir + strlen(wkdir) - 1) == '\n')
		*c = '\0';
	pclose(fp);
	return(0);
}
