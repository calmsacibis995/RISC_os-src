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
#ident	"$Header: findtty.c,v 1.4.2.2 90/05/09 16:26:08 wje Exp $"

/*	Copyright (c) 1984 AT&T	*/
/*	  All Rights Reserved  	*/

/*	THIS IS UNPUBLISHED PROPRIETARY SOURCE CODE OF AT&T	*/
/*	The copyright notice above does not evidence any   	*/
/*	actual or intended publication of such source code.	*/

/* findtty(user) -- find first tty that user is logged in to.
	returns: /dev/tty?? if user is logged in
		 NULL, if not
*/

#include	"lp.h"
#include	"lpsched.h"


char *
findtty(user)
char *user;
{
	struct utmp utmp;
	static char tty[15];
	FILE *u;

	if((u = fopen(UTMP, "r")) == NULL)
		return(NULL);
	while(fread((char *) &utmp, sizeof(struct utmp), 1, u) == 1)
		if(strcmp(utmp.ut_name, user) == 0) {
			sprintf(tty, "/dev/%s", utmp.ut_line);
			fclose(u);
			return(tty);
		}
	fclose(u);
	return(NULL);
}
