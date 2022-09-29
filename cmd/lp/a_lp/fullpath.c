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
#ident	"$Header: fullpath.c,v 1.4.2.2 90/05/09 16:26:38 wje Exp $"

/*	Copyright (c) 1984 AT&T	*/
/*	  All Rights Reserved  	*/

/*	THIS IS UNPUBLISHED PROPRIETARY SOURCE CODE OF AT&T	*/
/*	The copyright notice above does not evidence any   	*/
/*	actual or intended publication of such source code.	*/

/*
 *	fullpath(name, curdir) -- returns full pathname of file "name" when
 *	in current directory "curdir"
*/

#include	"lp.h"


char *
fullpath(name, curdir)
char *name;
char *curdir;
{
	static char fullname[FILEMAX];
	char *strcpy();

	if(*name == '/')
		return(name);
	else if(*curdir == '\0')
		return(NULL);
	else {
		sprintf(fullname, "%s/%s", curdir, name);
		return(fullname);
	}
}
