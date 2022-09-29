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
#ident	"$Header: fatal.c,v 1.4.2.2 90/05/09 16:25:57 wje Exp $"

/*	Copyright (c) 1984 AT&T	*/
/*	  All Rights Reserved  	*/

/*	THIS IS UNPUBLISHED PROPRIETARY SOURCE CODE OF AT&T	*/
/*	The copyright notice above does not evidence any   	*/
/*	actual or intended publication of such source code.	*/

/* fatal - prints error message preceded by the command name found in
	   the global string "f_name".

	   If code is non-zero, then the routine found in f_clean is called
	   and fatal exits with the code.
	   Otherwise, fatal returns to the caller.
*/

#include	"lp.h"


char *f_name = NULL;
int (*f_clean)() = NULL;

fatal(msg, code)
char *msg;
int code;
{
	if(f_name != NULL)
		fprintf(stderr, "%s: ", f_name);
	fprintf(stderr, "%s\n", msg);
	fflush(stderr);

	if(code != 0) {
		if(f_clean != NULL)
			(*f_clean)();
		exit(code);
	}
}
