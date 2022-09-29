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
#ident	"$Header: tmpfile.c,v 1.6.2.2 90/05/10 01:48:51 wje Exp $"

/*	Copyright (c) 1984 AT&T	*/
/*	  All Rights Reserved  	*/

/*	THIS IS UNPUBLISHED PROPRIETARY SOURCE CODE OF AT&T	*/
/*	The copyright notice above does not evidence any   	*/
/*	actual or intended publication of such source code.	*/

/*LINTLIBRARY*/
/*
 *	tmpfile - return a pointer to an update file that can be
 *		used for scratch. The file will automatically
 *		go away if the program using it terminates.
 */
#include <stdio.h>

extern FILE *fopen();
extern int unlink();
extern char *tmpnam();
extern void perror();

FILE *
tmpfile()
{
	char	tfname[L_tmpnam];
	register FILE	*p;

	(void) tmpnam(tfname);
	if((p = fopen(tfname, "w+")) == NULL)
		return NULL;
	else
		(void) unlink(tfname);
	return(p);
}
