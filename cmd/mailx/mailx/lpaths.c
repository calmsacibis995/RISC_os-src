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
#ident	"$Header: lpaths.c,v 1.3.2.2 90/05/09 16:40:45 wje Exp $"

/*	Copyright (c) 1984 AT&T	*/
/*	  All Rights Reserved  	*/

/*	THIS IS UNPUBLISHED PROPRIETARY SOURCE CODE OF AT&T	*/
/*	The copyright notice above does not evidence any   	*/
/*	actual or intended publication of such source code.	*/


/*
 * mailx -- a modified version of a University of California at Berkeley
 *	mail program
 */


/*
 *	libpath(file) - return the full path to the library file
 *	binpath(file) - return the full path to the library file
 *
 *	If MAILXTEST is defined in the environment, use that.
 */

#include "uparm.h"

char *
libpath (file)
char *file;		/* the file name */
{
static char buf[100];	/* build name here */
char *envexlib;		/* the pointer returned by getenv */
extern char *getenv();

	if ( (envexlib = getenv ("MAILXTEST")) != 0) {
		strcpy (buf, envexlib);
		strcat (buf, "/lib/mailx");
	} else {
		strcpy (buf, LIBPATH);
	}
	strcat (buf, "/");
	strcat (buf, file);
	return (buf);
}

/*
 * --  same thing for bin
 */

char *
binpath (file)
char *file;		/* the file name */
{
static char buf[100];	/* build name here */
char *envexlib;		/* the pointer returned by getenv */
extern char *getenv();

	if ( (envexlib = getenv ("MAILXTEST")) != 0) {
		strcpy (buf, envexlib);
		strcat (buf, "/bin");
	} else {
		strcpy (buf, BINPATH);
	}
	strcat (buf, "/");
	strcat (buf, file);
	return (buf);
}
